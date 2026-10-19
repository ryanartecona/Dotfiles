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
 *   filter in 0.0.80; it only toggles whether restriction applies. All
 *   network policy below is enforced by (a) per-command grants and (b) the
 *   ask callback.
 * - updateConfig() is process-global — never used for per-session policy.
 * - The proxy is in-process (ephemeral localhost port + unix-socket backend),
 *   dies with the pi process. Wrapped commands must be executed ASYNC (a sync
 *   exec in the proxy-hosting process self-deadlocks).
 *
 * CHUNK-1 LIMITATION — network "open" mode: srt has no per-call allow-all
 * (bare "*" is rejected in allow entries), and the ask callback receives
 * only {host, port} with no command identity. So "open" is decided by the
 * ask callback reading a PROCESS-WIDE floor that tracks the last explicit
 * /hallpass switch (initialized from the default profile). Correct for
 * single-session use; with concurrent in-process sessions on divergent
 * network modes, the floor is shared (fail-open toward "open"). Revisit in
 * chunk 3 when per-session identity lands. Filesystem policy is unaffected:
 * it is baked per call and always exact.
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

// SAFETY: initialize() performs no Zod validation (verified in 0.0.80 — the
// schema only runs on the CLI config-file path), so the compile-time-required
// keys just need to be present and complete. The base config is intentionally
// the restrictive floor (offline network, no fs rules): every grant is applied
// per call, because the manager is shared by concurrent sessions.
const BASE_CONFIG = {
	network: { allowedDomains: [], deniedDomains: [] },
	filesystem: { denyRead: [], allowWrite: [], denyWrite: [] },
} as unknown as SandboxRuntimeConfig;

// Process-wide shared state (survives jiti's per-session module reloads).
// The ask callback closes over this object, so the floor stays live no
// matter which module instance's initialize() won the race.
interface SharedSandboxState {
	initPromise?: Promise<void>;
	floorInitialized: boolean;
	floorOpen: boolean;
}
// SAFETY: widening globalThis with a namespaced slot; the cast only adds an
// optional property, and all access goes through `shared` below.
const shared = (globalThis as { __hallpassSandbox?: SharedSandboxState }).__hallpassSandbox ??= {
	floorInitialized: false,
	floorOpen: false,
};

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
	if (shared.floorOpen) return Promise.resolve(true);
	return Promise.resolve({
		allow: false as const,
		reason: `hallpass: network access denied — no active profile allows it (${host}:${port}). Switch with /hallpass switch <name>.`,
	});
};

/** Idempotent across sessions; a failed init is retried on the next call. */
export function ensureSandboxInitialized(): Promise<void> {
	// srt's own initialize() is also one-shot (a second call returns early),
	// so this is belt-and-braces dedupe across module instances.
	if (!shared.initPromise) {
		shared.initPromise = SandboxManager.initialize(BASE_CONFIG, askCallback, false).catch(
			(err: unknown) => {
				shared.initPromise = undefined;
				throw err;
			},
		);
	}
	return shared.initPromise;
}

export interface WrappedCommand {
	command: string;
	/** Release per-call registrations. Call when the command finishes. */
	cleanup: () => void;
}

/**
 * Wrap a shell command with the sandbox. `srtConfig` carries the filesystem
 * policy (baked per call, always exact); `network` drives per-call network
 * policy:
 *   offline  — no grant; the proxy denies everything not explicitly granted.
 *   allowlist — random per-call grant, revoked by cleanup().
 *   open     — no grant; the ask callback consults the process-wide floor.
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
		const wrapped = await SandboxManager.wrapWithSandbox(command, undefined, srtConfig, signal, {
			commandId: grantId ?? attributionId,
		});
		return {
			command: wrapped,
			cleanup: () => {
				if (grantId) SandboxManager.unregisterCommandNetworkLists(grantId);
			},
		};
	} catch (err) {
		if (grantId) SandboxManager.unregisterCommandNetworkLists(grantId);
		throw err;
	}
}
