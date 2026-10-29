import assert from "node:assert/strict";
import { mkdirSync, mkdtempSync, symlinkSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { installFileToolGuard } from "../guard.ts";
import type { Profile } from "../profiles.ts";

interface GuardEvent {
	toolName: string;
	input: { path?: string };
}
type GuardDecision = { block: true; reason: string } | undefined;

function makeProfile(filesystem: Partial<Profile["filesystem"]>): Profile {
	return {
		name: "test",
		inheritanceChain: ["test"],
		sandbox: true,
		filesystem: {
			denyRead: [],
			allowRead: [],
			allowWrite: [],
			denyWrite: [],
			...filesystem,
		},
		network: { mode: "open", allowedDomains: [], allowUnixSockets: [] },
		sandboxUserBash: false,
	};
}

function guardFor(profile: Profile, cwd: string): (event: GuardEvent) => GuardDecision {
	let handler: ((event: GuardEvent) => GuardDecision) | undefined;
	const pi = {
		on: (name: string, callback: (event: GuardEvent) => GuardDecision) => {
			if (name === "tool_call") handler = callback;
		},
	} as unknown as ExtensionAPI;
	installFileToolGuard(pi, () => profile, () => cwd);
	return (event) => {
		assert.ok(handler);
		return handler(event);
	};
}

test("allowWrite accepts the tool-facing path of an allowed symlinked file", () => {
	const dir = mkdtempSync(join(tmpdir(), "hallpass-guard-file-"));
	const real = join(dir, "store", "settings.json");
	mkdirSync(join(dir, "store"));
	writeFileSync(real, "{}");
	const alias = join(dir, "settings.json");
	symlinkSync(real, alias);
	const call = guardFor(makeProfile({ allowWrite: [alias] }), dir);

	assert.equal(call({ toolName: "edit", input: { path: alias } }), undefined);
	assert.equal(call({ toolName: "edit", input: { path: join(dir, "other.json") } })?.block, true);
});

test("allowWrite resolves a symlinked parent for a file that does not exist yet", () => {
	const dir = mkdtempSync(join(tmpdir(), "hallpass-guard-parent-"));
	const realDir = join(dir, "real");
	mkdirSync(realDir);
	const aliasDir = join(dir, "alias");
	symlinkSync(realDir, aliasDir, "dir");
	const call = guardFor(makeProfile({ allowWrite: [aliasDir] }), dir);

	assert.equal(call({ toolName: "write", input: { path: join(aliasDir, "new.json") } }), undefined);
});

test("denyRead uses the same canonical path on both sides", () => {
	const dir = mkdtempSync(join(tmpdir(), "hallpass-guard-deny-"));
	const real = join(dir, "secret.json");
	writeFileSync(real, "secret");
	const alias = join(dir, "alias.json");
	symlinkSync(real, alias);
	const call = guardFor(makeProfile({ denyRead: [alias] }), dir);

	assert.equal(call({ toolName: "read", input: { path: alias } })?.block, true);
});
