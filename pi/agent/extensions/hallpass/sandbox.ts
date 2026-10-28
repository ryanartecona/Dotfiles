/**
 * hallpass sandbox lifecycle: owns the process-wide srt SandboxManager.
 *
 * SandboxManager is a module-level singleton (one per pi process), shared by
 * the main session and all in-process subagent sessions. Empirically
 * verified against srt 0.0.80 (see DESIGN.md §4):
 *
 * - initialize() is one-shot and skips Zod validation; the base config must
 *   carry COMPLETE arrays — the proxy filter iterates config.network.* per
 *   request and crashes (HTTP 500 for everything) on undefined.
 * - The base config is the restrictive floor (offline). Widening is per call:
 *   registerCommandNetworkLists(randomId, {allowedDomains}) + wrap with
 *   {commandId: randomId}. commandId must be random, >=128 bits — pi
 *   tool-call ids do NOT qualify.
 * - Per-call customConfig.network.allowedDomains does NOT reach the proxy
 *   filter in 0.0.80; it only toggles whether restriction applies. Domain
 *   policy is enforced by (a) per-command grants and (b) the ask callback.
 * - allowUnixSockets is also ignored from customConfig in 0.0.80. It is
 *   installed with updateConfig only while wrapping, under a process-wide
 *   queue, then restored before any other session can wrap.
 * - The proxy is in-process (ephemeral localhost port + unix-socket backend),
 *   dies with the pi process. Wrapped commands must be executed ASYNC (a sync
 *   exec in the proxy-hosting process self-deadlocks).
 *
 * NETWORK "open" MODE: srt has no per-call allow-all (bare "*" is rejected in
 * allow entries), and the ask callback receives only {host, port} with no
 * command identity. So "open" is decided in the ask callback from in-flight
 * state: deny whenever ANY restricted (offline/allowlist) command is running
 * (fail closed); otherwise allow if an open command is running, or, for
 * background processes that outlive their command, if the process-wide floor
 * (the main session's last profile) is open. Consequence: an open command that
 * overlaps a restricted one (e.g. parallel subagents on different profiles)
 * gets its non-granted network denied. Filesystem policy is unaffected: it is
 * baked per call and always exact.
 *
 * Subagent sessions never move the floor; only the main session does.
 *
 * STATE PLACEMENT: pi's jiti loader runs with moduleCache disabled, so
 * hallpass's own modules may be re-instantiated per session, while srt (plain
 * ESM in node_modules) is a true process-wide singleton. All hallpass
 * process-wide state therefore lives on globalThis, never module scope.
 */

import { randomBytes } from "node:crypto";
import {
	SandboxManager,
	type SandboxAskCallback,
	type SandboxRuntimeConfig,
} from "@anthropic-ai/sandbox-runtime";
import type { NetworkProfile } from "./profiles.ts";

// The base config is intentionally the restrictive floor (offline network, no
// fs rules): every grant is applied per call, because the manager is shared by
// concurrent sessions. Persistent process-global policy is limited to the
// always-deny domain list; per-call Unix socket grants are installed only
// under withWrapLock and restored before the next wrap.
function buildBaseConfig(deniedDomains: string[], allowUnixSockets: string[] = []): SandboxRuntimeConfig {
	// SAFETY: initialize()/updateConfig() perform no Zod validation (verified in
	// 0.0.80: the schema only runs on the CLI config-file path), so the
	// compile-time-required keys just need to be present and complete; the
	// filesystem lists are a deliberate empty floor.
	return {
		network: { allowedDomains: [], deniedDomains: [...deniedDomains], allowUnixSockets: [...allowUnixSockets] },
		filesystem: { denyRead: [], allowWrite: [], denyWrite: [] },
	} as unknown as SandboxRuntimeConfig;
}

// Process-wide shared state (survives jiti's per-session module reloads).
// The ask callback closes over this object, so the floor stays live no
// matter which module instance's initialize() won the race.
interface SharedSandboxState {
	initPromise?: Promise<void>;
	floorInitialized: boolean;
	floorOpen: boolean;
	deniedDomains: string[];
	/** Wrapped commands currently running under an "open" profile. */
	inFlightOpen: number;
	/** Wrapped commands currently running under offline/allowlist profiles. */
	inFlightRestricted: number;
	/** Serializes wrap-time global socket updates across all in-process sessions. */
	wrapQueue: Promise<void>;
	/** Socket grants currently installed in srt's global config (inside wrapQueue only). */
	allowUnixSockets: string[];
}
// SAFETY: widening globalThis with a namespaced slot; the cast only adds an
// optional property, and all access goes through `shared` below.
const shared = (globalThis as { __hallpassSandbox?: SharedSandboxState }).__hallpassSandbox ??= {
	floorInitialized: false,
	floorOpen: false,
	deniedDomains: [],
	inFlightOpen: 0,
	inFlightRestricted: 0,
	wrapQueue: Promise.resolve(),
	allowUnixSockets: [],
};
// Fields added after earlier hallpass versions may be absent on globalThis after /reload.
shared.wrapQueue ??= Promise.resolve();
shared.allowUnixSockets ??= [];

/** Wrap only, never command execution: socket config is baked before the lock is released. */
async function withWrapLock<T>(fn: () => Promise<T>): Promise<T> {
	const previous = shared.wrapQueue;
	let release = (): void => undefined;
	shared.wrapQueue = new Promise<void>((resolve) => {
		release = resolve;
	});
	await previous;
	try {
		return await fn();
	} finally {
		release();
	}
}

/**
 * Set the process-wide always-deny domain list (top-level `[network]
 * deniedDomains`). Applied at init; if the manager is already initialized,
 * live-swapped via updateConfig (a global, per-request-read swap — the right
 * tool for a global list; wrap-time socket updates use the same queue).
 */
export function setDeniedDomains(domains: string[]): void {
	const unchanged =
		domains.length === shared.deniedDomains.length && domains.every((d, i) => d === shared.deniedDomains[i]);
	if (unchanged) return;
	shared.deniedDomains = [...domains];
	const init = shared.initPromise;
	if (init) {
		void withWrapLock(async () => {
			await init;
			await SandboxManager.updateConfig(buildBaseConfig(shared.deniedDomains, shared.allowUnixSockets));
		}).catch(() => {
			// Init failure is surfaced by the next wrapCommand; nothing to update.
		});
	}
}

/** Set the process-wide network floor from an explicit user action. */
export function setNetworkFloor(mode: NetworkProfile["mode"]): void {
	shared.floorInitialized = true;
	shared.floorOpen = mode === "open";
}

/** Initialize the floor from the default profile on first session start. */
export function ensureNetworkFloor(mode: NetworkProfile["mode"] | undefined): void {
	if (!shared.floorInitialized && mode) {
		shared.floorInitialized = true;
		shared.floorOpen = mode === "open";
	}
}

const askCallback: SandboxAskCallback = ({ host, port }) => {
	if (shared.inFlightRestricted === 0 && (shared.inFlightOpen > 0 || shared.floorOpen)) {
		return Promise.resolve(true);
	}
	return Promise.resolve({
		allow: false as const,
		reason: `hallpass: network access to ${host}:${port} is not allowed by the active profile`,
	});
};

/** Idempotent across sessions; a failed init is retried on the next call. */
export function ensureSandboxInitialized(): Promise<void> {
	// srt's own initialize() is also one-shot (a second call returns early),
	// so this is belt-and-braces dedupe across module instances.
	if (!shared.initPromise) {
		shared.initPromise = SandboxManager.initialize(
			buildBaseConfig(shared.deniedDomains),
			askCallback,
			LOG_MONITOR_ENABLED,
		).catch(
			(err: unknown) => {
				shared.initPromise = undefined;
				throw err;
			},
		);
	}
	return shared.initPromise;
}

/**
 * srt's macOS violation log monitor (a `log stream` child). OFF by default:
 * verified on this machine (macOS, srt 0.0.80) that it never receives
 * filesystem denials (the OS does not log them under srt's tag; `log show`
 * confirms), delivering only noise such as `sysctl-read`. Network denials do
 * not need it (the proxy records them synchronously). Opt in with
 * HALLPASS_LOG_MONITOR=1 to experiment on other macOS versions.
 */
export const LOG_MONITOR_ENABLED = process.platform === "darwin" && process.env.HALLPASS_LOG_MONITOR === "1";

/**
 * Violation kinds that fire on essentially every sandboxed command (e.g.
 * `sysctl-read kern.iossupportversion`) and give the model nothing to act on.
 * Filtered here rather than via srt's ignoreViolations so they also do not
 * satisfy the settle poll in violations().
 */
const NON_ACTIONABLE_VIOLATION = /\b(?:sysctl-|mach-|iokit-|ipc-|system-|signal|pseudo-tty|user-preference-)/;

export interface WrappedCommand {
	command: string;
	/** Release per-call registrations. Call when the command finishes. */
	cleanup: () => void;
	/**
	 * srt's `<sandbox_violations>` block for this invocation, or "" when there
	 * are none. Proxy (network) denials are recorded synchronously; macOS
	 * filesystem denials arrive asynchronously via the log monitor, so pass
	 * `settleMs` to poll briefly for them. Call before cleanup().
	 */
	violations: (settleMs?: number) => Promise<string>;
}

/**
 * Wrap a shell command with the sandbox. `srtConfig` carries the filesystem
 * policy (baked per call, always exact); `network` drives per-call network
 * policy:
 *   offline  — no grant; the proxy denies everything not explicitly granted.
 *   allowlist — random per-call grant, revoked by cleanup().
 *   open     — no grant; the ask callback decides from in-flight state (see header).
 *
 * `attributionId` (pi's tool-call id) labels violations; it is NOT a grant
 * key (grant keys must be random).
 */
export async function wrapCommand(
	command: string,
	srtConfig: Partial<SandboxRuntimeConfig>,
	network: NetworkProfile,
	signal: AbortSignal | undefined,
	attributionId: string,
): Promise<WrappedCommand> {
	await ensureSandboxInitialized();

	let grantId: string | undefined;
	if (network.mode === "allowlist") {
		grantId = randomBytes(16).toString("base64url");
		SandboxManager.registerCommandNetworkLists(grantId, {
			allowedDomains: network.allowedDomains,
		});
	}

	try {
		// The attribution key srt files violations under is exactly this id.
		const key = grantId ?? attributionId;
		const wrapped = await withWrapLock(async () => {
			const allowUnixSockets = srtConfig.network?.allowUnixSockets ?? [];
			if (allowUnixSockets.length === 0) {
				return SandboxManager.wrapWithSandbox(command, undefined, srtConfig, signal, { commandId: key });
			}

			// srt 0.0.80 ignores customConfig.network.allowUnixSockets and reads its
			// global config instead. Install the per-call grant only while wrapping,
			// then restore the restrictive floor before another session can wrap.
			shared.allowUnixSockets = [...allowUnixSockets];
			try {
				await SandboxManager.updateConfig(buildBaseConfig(shared.deniedDomains, allowUnixSockets));
				return await SandboxManager.wrapWithSandbox(command, undefined, srtConfig, signal, { commandId: key });
			} finally {
				shared.allowUnixSockets = [];
				await SandboxManager.updateConfig(buildBaseConfig(shared.deniedDomains));
			}
		});
		const isOpen = network.mode === "open";
		if (isOpen) shared.inFlightOpen++;
		else shared.inFlightRestricted++;
		let released = false;
		return {
			command: wrapped,
			cleanup: () => {
				if (released) return;
				released = true;
				if (isOpen) shared.inFlightOpen--;
				else shared.inFlightRestricted--;
				if (grantId) SandboxManager.unregisterCommandNetworkLists(grantId);
			},
			violations: async (settleMs = 0) => {
				const read = () => {
					const lines = SandboxManager.getSandboxViolationStore()
						.getViolationsForCommand(key)
						.map((violation) => violation.line)
						.filter((line) => !NON_ACTIONABLE_VIOLATION.test(line));
					const unique = [...new Set(lines)];
					return unique.length > 0 ? `<sandbox_violations>\n${unique.join("\n")}\n</sandbox_violations>` : "";
				};
				const deadline = Date.now() + settleMs;
				let found = read();
				while (!found && Date.now() < deadline) {
					await new Promise((resolve) => setTimeout(resolve, 50));
					found = read();
				}
				return found;
			},
		};
	} catch (err) {
		if (grantId) SandboxManager.unregisterCommandNetworkLists(grantId);
		throw err;
	}
}
