import assert from "node:assert/strict";
import { mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import {
	isEmptyProfilePatch,
	loadConfig,
	parseSessionPatch,
	resolveEffectiveProfile,
	toSrtConfig,
	type HallpassConfig,
	type Profile,
} from "../profiles.ts";
import { formatHallpassStatus, formatSessionEditPreview } from "../status.ts";

function load(toml: string, projectToml?: string): HallpassConfig {
	const dir = mkdtempSync(join(tmpdir(), "hallpass-profiles-"));
	const user = join(dir, "user.toml");
	writeFileSync(user, toml);
	const paths = [user];
	if (projectToml !== undefined) {
		const project = join(dir, "project.toml");
		writeFileSync(project, projectToml);
		paths.push(project);
	}
	return loadConfig(dir, paths);
}

function getProfile(config: HallpassConfig, name: string): Profile {
	const profile = config.profiles.get(name);
	assert.ok(profile, `expected profile ${name}; warnings: ${config.warnings.join("; ")}`);
	return profile;
}

test("inheritance resolves scalars, generic list deltas, and last tool operation", () => {
	const config = load(`
default = "child"
[profiles.base]
description = "base"
agentUsage = "base instructions"
sandboxUserBash = true
filesystem.allowWrite = ["/a", "/b"]
filesystem.denyRead = ["/secret"]
network.mode = "offline"
network.allowedDomains = ["example.com"]
network.allowUnixSockets = ["/run/base.sock"]
tmuxProfile = ["default"]
tools = ["-write", "-edit"]
[profiles.child]
extends = "base"
filesystem.allowWrite = ["-/a", "+/c"]
filesystem.denyRead = []
network.allowedDomains = ["+github.com"]
network.allowUnixSockets = ["+/run/child.sock"]
tmuxProfile = ["+focused"]
tools = ["+write"]
`);
	const base = getProfile(config, "base");
	const child = getProfile(config, "child");
	assert.deepEqual(child.inheritanceChain, ["base", "child"]);
	assert.equal(child.agentUsage, "base instructions");
	assert.equal(child.sandboxUserBash, true);
	assert.deepEqual(child.filesystem.allowWrite, ["/b", "/c"]);
	assert.deepEqual(child.filesystem.denyRead, []);
	assert.equal(child.network.mode, "offline");
	assert.deepEqual(child.network.allowedDomains, ["example.com", "github.com"]);
	assert.deepEqual(child.network.allowUnixSockets, ["/run/base.sock", "/run/child.sock"]);
	assert.deepEqual(child.tmuxProfiles, ["default", "focused"]);
	assert.deepEqual(child.tools, ["+write", "-edit"]);
	assert.deepEqual(base.filesystem.allowWrite, ["/a", "/b"], "child resolution must not mutate parent");
	assert.deepEqual(config.cycleProfiles, ["base", "child"]);
});

test("raw lists replace and empty lists clear inherited values", () => {
	const config = load(`
[profiles.base]
agentUsage = "keep"
filesystem.allowWrite = ["/a", "/b"]
network.allowedDomains = ["a.example", "b.example"]
[profiles.replace]
extends = "base"
filesystem.allowWrite = ["/x"]
network.allowedDomains = []
[profiles.clear]
extends = "replace"
filesystem.allowWrite = []
agentUsage = ""
`);
	const replace = getProfile(config, "replace");
	const clear = getProfile(config, "clear");
	assert.deepEqual(replace.filesystem.allowWrite, ["/x"]);
	assert.deepEqual(replace.network.allowedDomains, []);
	assert.equal(replace.agentUsage, "keep");
	assert.deepEqual(clear.filesystem.allowWrite, []);
	assert.equal(clear.agentUsage, "");
});

test("omitted child network mode inherits instead of receiving the root open default", () => {
	const config = load(`
[profiles.base]
network.mode = "offline"
[profiles.child]
extends = "base"
network.allowedDomains = ["example.com"]
`);
	assert.equal(getProfile(config, "child").network.mode, "offline");
});

test("mixed lists, unknown parents, and cycles invalidate profiles with warnings", () => {
	const config = load(`
[profiles.mixed]
filesystem.allowWrite = ["/raw", "+/delta"]
[profiles.orphan]
extends = "missing"
[profiles.a]
extends = "b"
[profiles.b]
extends = "a"
`);
	assert.equal(config.profiles.size, 4);
	for (const name of ["mixed", "orphan", "a", "b"]) {
		const profile = getProfile(config, name);
		assert.equal(profile.invalid, true);
		assert.equal(profile.network.mode, "offline");
		assert.deepEqual(profile.tools, ["-bash", "-write", "-edit", "-Agent"]);
	}
	assert.deepEqual(config.cycleProfiles, []);
	assert.ok(config.warnings.some((warning) => warning.includes("not both")));
	assert.ok(config.warnings.some((warning) => warning.includes("unknown parent")));
	assert.ok(config.warnings.some((warning) => warning.includes("inheritance cycle")));
});

test("cycleProfiles filters hotkeys without hiding profiles and preserves user setting across layers", () => {
	const config = load(`
default = "special"
cycleProfiles = ["dev", "missing", "dev", "audit"]
[profiles.dev]
[profiles.audit]
[profiles.special]
`, `
[profiles.project]
`);
	assert.deepEqual(config.cycleProfiles, ["dev", "audit"]);
	assert.deepEqual([...config.profiles.keys()], ["dev", "audit", "special", "project"]);
	assert.ok(config.warnings.some((warning) => warning.includes("cycleProfiles")));

	const disabled = load(`cycleProfiles = []\n[profiles.dev]`);
	assert.deepEqual(disabled.cycleProfiles, []);
	assert.ok(disabled.profiles.has("dev"));
});

test("same-named project profile replaces raw user definition; named child can extend it", () => {
	const config = load(`
cycleProfiles = ["dev"]
[profiles.dev]
agentUsage = "user instructions"
filesystem.allowWrite = ["/user"]
network.mode = "open"
`, `
[profiles.dev]
network.mode = "offline"
[profiles.project-dev]
extends = "dev"
filesystem.allowWrite = ["+/project"]
`);
	const dev = getProfile(config, "dev");
	assert.equal(dev.network.mode, "offline");
	assert.equal(dev.agentUsage, undefined);
	assert.deepEqual(dev.filesystem.allowWrite, []);
	assert.deepEqual(getProfile(config, "project-dev").filesystem.allowWrite, ["/project"]);
	assert.deepEqual(config.cycleProfiles, ["dev"]);
});

test("session parser accepts profile fragments and rejects persistent/global fields", () => {
	const patch = parseSessionPatch(`
agentUsage = "session only"
filesystem.allowWrite = ["+$CWD"]
network.mode = "allowlist"
network.allowedDomains = ["+github.com"]
network.allowUnixSockets = ["+$CWD/session.sock"]
tools = ["+write"]
`);
	assert.equal(patch.agentUsage, "session only");
	assert.deepEqual(patch.filesystem.allowWrite, ["+$CWD"]);
	assert.equal(patch.network.mode, "allowlist");
	assert.deepEqual(patch.network.allowUnixSockets, ["+$CWD/session.sock"]);
	assert.deepEqual(patch.tools, ["+write"]);
	assert.throws(() => parseSessionPatch(`extends = "dev"`), /extends is implicit|unknown field/);
	assert.throws(() => parseSessionPatch(`cycleProfiles = ["dev"]`), /unknown field/);
	assert.throws(() => parseSessionPatch(`[network]\ndeniedDomains = ["example.com"]`), /deniedDomains/);
	assert.throws(() => parseSessionPatch(`filesystem.allowWrite = ["/raw", "+/delta"]`), /not both/);
	assert.throws(() => parseSessionPatch(`filesystem.allowWrite = ["+"]`), /must not be empty/);
	assert.throws(() => parseSessionPatch(`network.allowedDomains = [""]`), /must not be empty/);
	assert.ok(isEmptyProfilePatch(parseSessionPatch("# comment-only overlay\n")));
});

test("a later invalid same-named definition fails closed instead of using the earlier valid profile", () => {
	const config = load(`
default = "dev"
[profiles.dev]
network.mode = "open"
filesystem.allowWrite = ["/user"]
`, `
[profiles.dev]
filesystem.allowWrite = ["+"]
`);
	const dev = getProfile(config, "dev");
	assert.equal(dev.invalid, true);
	assert.equal(dev.network.mode, "offline");
	assert.deepEqual(dev.filesystem.allowWrite, []);
	assert.deepEqual(dev.filesystem.denyWrite, ["/"]);
	assert.deepEqual(dev.filesystem.denyRead, ["/"]);
	const attemptedUnlock = resolveEffectiveProfile(dev, parseSessionPatch(`
sandbox = false
tools = ["+bash", "+write", "+edit", "+Agent"]
filesystem.denyRead = []
filesystem.denyWrite = []
network.mode = "open"
`));
	assert.equal(attemptedUnlock.invalid, true);
	assert.equal(attemptedUnlock.sandbox, true);
	assert.equal(attemptedUnlock.network.mode, "offline");
	assert.deepEqual(attemptedUnlock.tools, ["-bash", "-write", "-edit", "-Agent"]);
	assert.deepEqual(attemptedUnlock.filesystem.denyRead, ["/"]);
	assert.deepEqual(attemptedUnlock.filesystem.denyWrite, ["/"]);
});

test("session overlay is a non-mutating final child and can restore tool access", () => {
	const config = load(`
[profiles.audit]
filesystem.allowWrite = []
network.mode = "offline"
tools = ["-write", "-edit"]
agentUsage = "plan first"
`);
	const base = getProfile(config, "audit");
	const patch = parseSessionPatch(`
filesystem.allowWrite = ["+$CWD"]
network.mode = "allowlist"
network.allowedDomains = ["+github.com"]
network.allowUnixSockets = ["+$CWD/session.sock"]
tools = ["+write", "+edit"]
`);
	const effective = resolveEffectiveProfile(base, patch);
	assert.deepEqual(effective.filesystem.allowWrite, ["$CWD"]);
	assert.equal(effective.network.mode, "allowlist");
	assert.deepEqual(effective.network.allowedDomains, ["github.com"]);
	assert.deepEqual(effective.network.allowUnixSockets, ["$CWD/session.sock"]);
	assert.deepEqual(effective.tools, ["+write", "+edit"]);
	assert.equal(effective.agentUsage, "plan first");
	assert.deepEqual(effective.inheritanceChain, ["audit"]);
	assert.deepEqual(base.filesystem.allowWrite, []);
	assert.equal(base.network.mode, "offline");
	assert.deepEqual(base.tools, ["-write", "-edit"]);
	const srtConfig = toSrtConfig(effective, "/work");
	assert.deepEqual(srtConfig.network?.allowUnixSockets, ["/work/session.sock"]);
	assert.equal(srtConfig.network?.allowedDomains, undefined, "proxy domains must remain outside customConfig");
});

test("shared status includes session state and agentUsage; approval preview contains a policy diff", () => {
	const base = getProfile(load(`[profiles.audit]\nagentUsage = "review only"\nnetwork.mode = "offline"`), "audit");
	const proposed = resolveEffectiveProfile(base, parseSessionPatch(`network.mode = "open"`));
	const status = formatHallpassStatus({
		revision: 7,
		baseProfile: "audit",
		activeProfile: "audit",
		inheritanceChain: ["audit"],
		sessionOverrides: `network.mode = "open"`,
		deniedDomains: ["169.254.169.254"],
		warnings: [],
		isChild: false,
		effectiveProfile: proposed,
	});
	assert.match(status, /session revision: 7/);
	assert.match(status, /network\.mode = "open"/);
	assert.match(status, /agentUsage:\nreview only/);
	assert.match(status, /169\.254\.169\.254/);
	const preview = formatSessionEditPreview(base, proposed, `network.mode = "open"`);
	assert.match(preview, /- network: offline/);
	assert.match(preview, /\+ network: open/);
	assert.match(preview, /does not edit hallpass\.toml/);
});
