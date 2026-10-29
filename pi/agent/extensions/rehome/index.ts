/**
 * rehome — change a live session's embedded working directory.
 *
 * pi bakes the startup cwd into the session header, the built-in tool
 * definitions, the system prompt, and the extension runtime, with no API to
 * change it in a live session. Session *replacement*, however, rebuilds the
 * entire runtime from the replacement session file's header cwd. rehome uses
 * that door:
 *
 *   1. fork the session file into the target cwd's session directory with a
 *      rewritten header (SessionManager.forkFrom: entries copied verbatim,
 *      parentSession backlink to the source file),
 *   2. process.chdir(targetCwd) so anything reading the process cwd follows,
 *   3. switchSession() to the forked file — the same in-place mechanism
 *      /resume uses, so no exit/re-enter is needed,
 *   4. append a marker message to the NEW session (it records the move);
 *      the original file is left byte-for-byte untouched — it was never
 *      rehomed, the fork was.
 *
 * History stays intact: the copied first system message still carries the
 * original `cwd` prompt section, and pi's section diffing persists a system
 * patch message with the new `cwd` section before the next request, so the
 * exact cwd of every tool call is derivable from the system-message replay
 * alone. The custom_message marker (customType "rehome") makes the boundary
 * explicit for rehome-aware tooling and for the model.
 *
 * Two behaviors need a session switch from *outside* a command handler, where
 * pi only exposes switchSession on command contexts:
 *
 *   - startup: session was opened (--continue/--resume/--session) from a cwd
 *     different from the one embedded in it -> offer rehome,
 *   - /resume intercept: the picked session belongs to another cwd -> offer
 *     rehome instead of pi's native "stay tied to the original cwd".
 *
 * For those, rehome captures pi's own command-context actions by wrapping
 * ExtensionRunner.prototype.bindCommandContext (extensions import the same
 * module instance pi runs on), then invokes the captured switchSession in a
 * macrotask after the event handler returns (calling it inside the handler
 * could deadlock the runtime). If the internals change and capture fails, the
 * flows degrade to a notification plus a pre-filled `/rehome` in the editor.
 */

import { appendFileSync, closeSync, existsSync, openSync, readSync, realpathSync } from "node:fs";
import { unlink } from "node:fs/promises";
import { dirname, isAbsolute, join, resolve } from "node:path";
import {
	ExtensionRunner,
	SessionManager,
	type ExtensionAPI,
	type ExtensionCommandContextActions,
	type ExtensionContext,
	type ReplacedSessionContext,
} from "@earendil-works/pi-coding-agent";

const CUSTOM_TYPE = "rehome";

/** Opt-in debug logging: set PI_REHOME_DEBUG to a writable file path. */
const DEBUG_FILE = process.env.PI_REHOME_DEBUG;
function debug(...parts: unknown[]): void {
	if (!DEBUG_FILE) return;
	try {
		appendFileSync(
			DEBUG_FILE,
			`${new Date().toISOString()} ${parts.map((p) => (typeof p === "string" ? p : JSON.stringify(p))).join(" ")}\n`,
		);
	} catch {
		/* never break the session over debug logging */
	}
}

type RehomeMode = "fork" | "move";

const FORK_LABEL = "Fork: rehome a copy here, keep the old session file";
const MOVE_LABEL = "Move: rehome a copy here, delete the old session file";
const KEEP_LABEL = "Keep the session's original cwd";
const CANCEL_LABEL = "Cancel";

type SwitchSessionFn = (
	sessionPath: string,
	options?: { withSession?: (ctx: ReplacedSessionContext) => Promise<void> },
) => Promise<{ cancelled: boolean }>;

/**
 * Wait until startup-time UI churn is over before showing a dialog.
 *
 * ctx.ui.select renders into the editor slot, which pi and other extensions
 * clear or replace during startup (e.g. setCustomEditorComponent, called by
 * editor-replacing extensions such as pi-open-tui, does
 * editorContainer.clear()) — destroying the dialog *without resolving its
 * promise*, so the prompt vanishes and the flow hangs forever.
 *
 * Such extensions install a *new* editor factory closure on every re-apply,
 * so an unchanged ctx.ui.getEditorComponent() reference across a full quiet
 * window means the churn is over. There is no fixed base delay: when nothing
 * is churning, the wait is just the quiet window itself. A hard cap
 * guarantees the prompt is shown even if something churns indefinitely.
 */
async function waitForUiToSettle(ctx: ExtensionContext): Promise<void> {
	const QUIET_MS = 500;
	const POLL_MS = 100;
	const MAX_WAIT_MS = 8000;
	type EditorFactory = () => unknown;
	const getFactory = (): EditorFactory | undefined => {
		try {
			return ctx.ui.getEditorComponent?.() as EditorFactory | undefined;
		} catch {
			return undefined;
		}
	};
	const start = Date.now();
	let last = getFactory();
	let quietSince = start;
	let swaps = 0;
	while (Date.now() - start < MAX_WAIT_MS) {
		await new Promise((r) => setTimeout(r, POLL_MS));
		const current = getFactory();
		if (current !== last) {
			last = current;
			quietSince = Date.now();
			swaps++;
			continue;
		}
		if (Date.now() - quietSince >= QUIET_MS) break;
	}
	debug("startup: ui settled", { waitedMs: Date.now() - start, editorSwaps: swaps, capped: Date.now() - start >= MAX_WAIT_MS });
}

// ---------------------------------------------------------------------------
// Command-action capture (see header comment). The cell lives on globalThis
// so it is shared if this module is evaluated again for a replacement
// extension runtime.
// ---------------------------------------------------------------------------

interface RehomeSharedState {
	patched: boolean;
	actions: ExtensionCommandContextActions | undefined;
}

const SHARED_KEY = Symbol.for("rehome.commandActionsCapture");
const shared: RehomeSharedState = ((globalThis as Record<symbol, unknown>)[SHARED_KEY] ??= {
	patched: false,
	actions: undefined,
}) as RehomeSharedState;

function installActionsCapture(): void {
	if (shared.patched) return;
	try {
		const proto = ExtensionRunner.prototype as {
			bindCommandContext?: (actions?: ExtensionCommandContextActions) => void;
		};
		const original = proto.bindCommandContext;
		if (typeof original !== "function") return;
		proto.bindCommandContext = function (this: unknown, actions?: ExtensionCommandContextActions) {
			if (actions) shared.actions = actions;
			return original.call(this, actions);
		};
		shared.patched = true;
	} catch {
		// Best effort; flows that need captured actions degrade gracefully.
	}
}

installActionsCapture();
debug("rehome loaded; patched =", shared.patched);

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Resolve + realpath (macOS /tmp symlinks etc.), tolerating missing paths. */
function normalizeCwd(p: string): string {
	const r = resolve(p);
	try {
		return realpathSync.native(r);
	} catch {
		return r;
	}
}

/** Read only the header line of a session file (bounded). */
function readSessionFileCwd(sessionFile: string): string | undefined {
	let fd: number | undefined;
	try {
		fd = openSync(sessionFile, "r");
		const buf = Buffer.alloc(8192);
		const n = readSync(fd, buf, 0, buf.length, 0);
		const firstLine = buf.subarray(0, n).toString("utf8").split("\n", 1)[0];
		const header: unknown = JSON.parse(firstLine);
		if (
			typeof header === "object" &&
			header !== null &&
			(header as { type?: unknown }).type === "session" &&
			typeof (header as { cwd?: unknown }).cwd === "string"
		) {
			return (header as { cwd: string }).cwd;
		}
		return undefined;
	} catch {
		return undefined;
	} finally {
		if (fd !== undefined) {
			try {
				closeSync(fd);
			} catch {
				/* ignore */
			}
		}
	}
}

function errMessage(err: unknown): string {
	return err instanceof Error ? err.message : String(err);
}

function markerContent(from: string, to: string, mode: RehomeMode): string {
	return `Session rehomed from ${from} to ${to} (${mode})`;
}

/**
 * Session directory for a target cwd, mirroring the layout of the current
 * session's directory so custom sessionDir setups keep working. Falls back
 * to forkFrom's built-in default when the current layout is unrecognized.
 */
function targetSessionDir(currentSessionDir: string | undefined, currentCwd: string, targetCwd: string): string | undefined {
	if (!currentSessionDir) return undefined;
	const encode = (cwd: string) => `--${resolve(cwd).replace(/^[/\\]/, "").replace(/[/\\:]/g, "-")}--`;
	// Only derive when the current dir actually looks like a per-cwd dir of
	// the current session; otherwise the file may live somewhere arbitrary.
	if (currentSessionDir.endsWith(encode(currentCwd))) {
		return join(dirname(currentSessionDir), encode(targetCwd));
	}
	return undefined;
}

// ---------------------------------------------------------------------------
// Core flows
// ---------------------------------------------------------------------------

export default function (pi: ExtensionAPI) {
	/**
	 * Rehome the *current* live session to targetCwd: fork the file, chdir,
	 * switch, then mark history in the NEW session. The original session file
	 * is left byte-for-byte untouched — it was never rehomed, the fork was.
	 * Used by /rehome and the startup prompt.
	 */
	async function rehomeCurrentSession(
		ctx: ExtensionContext,
		targetCwd: string,
		mode: RehomeMode,
		switchSession: SwitchSessionFn,
	): Promise<void> {
		const sourceFile = ctx.sessionManager.getSessionFile();
		if (!sourceFile) {
			ctx.ui.notify("rehome: this session is not persisted (--no-session), nothing to rehome", "error");
			return;
		}
		const fromCwd = ctx.sessionManager.getCwd();
		let forkedFile: string;
		try {
			const sessionDir = targetSessionDir(ctx.sessionManager.getSessionDir(), fromCwd, targetCwd);
			const forked = SessionManager.forkFrom(sourceFile, targetCwd, sessionDir);
			forkedFile = forked.getSessionFile()!;
		} catch (err) {
			ctx.ui.notify(`rehome: failed to create the rehomed session: ${errMessage(err)}`, "error");
			return;
		}
		const previousProcessCwd = process.cwd();
		try {
			process.chdir(targetCwd);
		} catch {
			// Non-fatal: the session cwd comes from the header, not process.cwd().
		}
		let result: { cancelled: boolean };
		try {
			result = await switchSession(forkedFile, {
				withSession: async (nctx) => {
					// Marker only in the rehomed session; details carries the
					// machine-readable form.
					await nctx.sendMessage({
						customType: CUSTOM_TYPE,
						content: markerContent(fromCwd, targetCwd, mode),
						display: true,
						details: { from: fromCwd, to: targetCwd, mode, sourceFile },
					});
					nctx.ui.notify(`Rehomed to ${targetCwd} (${mode})`, "info");
				},
			});
		} catch (err) {
			try {
				process.chdir(previousProcessCwd);
			} catch {
				/* ignore */
			}
			ctx.ui.notify(`rehome: switch failed: ${errMessage(err)}`, "error");
			return;
		}
		if (result.cancelled) {
			try {
				process.chdir(previousProcessCwd);
			} catch {
				/* ignore */
			}
			// The switch never happened, so the fork is an orphan copy.
			try {
				await unlink(forkedFile);
			} catch {
				/* ignore */
			}
			ctx.ui.notify("rehome: switch cancelled", "info");
			return;
		}
		if (mode === "move") {
			try {
				await unlink(sourceFile);
			} catch {
				// Keep the old file on failure; the rehome itself succeeded.
			}
		}
	}

	/**
	 * Rehome a *foreign* session file (picked in /resume) into the ambient
	 * cwd, then drive the switch ourselves after cancelling pi's native one.
	 */
	function rehomeForeignSession(
		ctx: ExtensionContext,
		sourceFile: string,
		sourceCwd: string,
		targetCwd: string,
		mode: RehomeMode,
	): { cancel: boolean } {
		const switchSession = shared.actions?.switchSession as SwitchSessionFn | undefined;
		if (!switchSession) {
			// Capture unavailable: let pi open the session natively and offer to
			// finish the rehome with one keystroke afterwards.
			ctx.ui.setEditorText("/rehome");
			ctx.ui.notify(`rehome: session belongs to ${sourceCwd} — press Enter after it opens to rehome here`, "warning");
			return { cancel: false };
		}
		let forkedFile: string;
		try {
			const sessionDir = targetSessionDir(ctx.sessionManager.getSessionDir(), ctx.sessionManager.getCwd(), targetCwd);
			forkedFile = SessionManager.forkFrom(sourceFile, targetCwd, sessionDir).getSessionFile()!;
		} catch (err) {
			ctx.ui.notify(`rehome: failed to create the rehomed session: ${errMessage(err)}`, "error");
			return { cancel: true };
		}
		setImmediate(() => {
			void (async () => {
				const result = await switchSession(forkedFile, {
					withSession: async (nctx) => {
						await nctx.sendMessage({
							customType: CUSTOM_TYPE,
							content: markerContent(sourceCwd, targetCwd, mode),
							display: true,
							details: { from: sourceCwd, to: targetCwd, mode, sourceFile },
						});
						nctx.ui.notify(`Rehomed to ${targetCwd} (${mode})`, "info");
					},
				});
				if (!result.cancelled && mode === "move") {
					try {
						await unlink(sourceFile);
					} catch {
						/* keep on failure */
					}
				}
			})().catch(() => {
				/* switch errors surface through pi's own error handling */
			});
		});
		return { cancel: true };
	}

	// -----------------------------------------------------------------------
	// /rehome command
	// -----------------------------------------------------------------------

	pi.registerCommand("rehome", {
		description:
			"Rehome this session to a new working directory. Usage: /rehome [path] [--fork|--move] " +
			"(default path: current directory; prompts for fork/move when no flag is given)",
		handler: async (args, ctx) => {
			const words = args.split(/\s+/).filter((w) => w.length > 0);
			let modeFlag: RehomeMode | undefined;
			let pathArg: string | undefined;
			for (const w of words) {
				if (w === "--fork") modeFlag = "fork";
				else if (w === "--move") modeFlag = "move";
				else if (!pathArg) pathArg = w;
				else {
					ctx.ui.notify(`rehome: unexpected argument: ${w}`, "error");
					return;
				}
			}
			const targetCwd = normalizeCwd(pathArg ? (isAbsolute(pathArg) ? pathArg : resolve(process.cwd(), pathArg)) : process.cwd());
			const currentCwd = normalizeCwd(ctx.sessionManager.getCwd());
			if (targetCwd === currentCwd) {
				ctx.ui.notify("rehome: session already lives in that directory", "info");
				return;
			}
			if (!existsSync(targetCwd)) {
				ctx.ui.notify(`rehome: target directory does not exist: ${targetCwd}`, "error");
				return;
			}
			let mode = modeFlag;
			if (!mode) {
				if (!ctx.hasUI) {
					mode = "fork";
				} else {
					const choice = await ctx.ui.select(`Rehome session to ${targetCwd}?`, [FORK_LABEL, MOVE_LABEL, CANCEL_LABEL]);
					if (choice === FORK_LABEL) mode = "fork";
					else if (choice === MOVE_LABEL) mode = "move";
					else {
						ctx.ui.notify("rehome cancelled", "info");
						return;
					}
				}
			}
			await rehomeCurrentSession(ctx, targetCwd, mode, (p, o) => ctx.switchSession(p, o));
		},
	});

	// -----------------------------------------------------------------------
	// Startup detection: session opened from a cwd different from its header
	// -----------------------------------------------------------------------

	pi.on("session_start", async (event, ctx) => {
		debug("session_start", { reason: event.reason, mode: ctx.mode, hasUI: ctx.hasUI, file: ctx.sessionManager.getSessionFile() });
		if (event.reason !== "startup") return;
		if (!ctx.hasUI) return debug("startup: skip, no UI");
		if (!ctx.sessionManager.getSessionFile()) return debug("startup: skip, session not persisted");
		const sessionCwd = ctx.sessionManager.getCwd();
		const normSession = normalizeCwd(sessionCwd);
		const ambient = normalizeCwd(process.cwd());
		if (normSession === ambient) return debug("startup: skip, cwd matches", normSession);
		if (!existsSync(normSession)) return debug("startup: skip, session cwd missing (pi owns this)", normSession);
		debug("startup: mismatch, scheduling prompt", { sessionCwd, ambient });
		// Deferred: switching from inside the handler can deadlock the runtime.
		setImmediate(() => {
			void (async () => {
				await waitForUiToSettle(ctx);
				debug("startup: showing prompt");
				const choice = await ctx.ui.select(
					`This session belongs to ${sessionCwd},\nbut pi was started in ${ambient}. Rehome it?`,
					[KEEP_LABEL, FORK_LABEL, MOVE_LABEL],
				);
				debug("startup: choice =", choice);
				const mode: RehomeMode | undefined = choice === FORK_LABEL ? "fork" : choice === MOVE_LABEL ? "move" : undefined;
				if (!mode) return;
				const switchSession = shared.actions?.switchSession as SwitchSessionFn | undefined;
				debug("startup: captured switchSession available =", !!switchSession);
				if (!switchSession) {
					ctx.ui.setEditorText("/rehome");
					ctx.ui.notify("rehome: press Enter to rehome this session here", "warning");
					return;
				}
				await rehomeCurrentSession(ctx, ambient, mode, (p, o) => switchSession(p, o));
			})().catch((err) => {
				debug("startup: ERROR", errMessage(err), (err as Error)?.stack);
			});
		});
	});

	// -----------------------------------------------------------------------
	// /resume intercept: picked session belongs to another cwd
	// -----------------------------------------------------------------------

	pi.on("session_before_switch", async (event, ctx) => {
		debug("session_before_switch", { reason: event.reason, target: event.targetSessionFile, mode: ctx.mode, hasUI: ctx.hasUI });
		if (event.reason !== "resume" || !event.targetSessionFile) return;
		if (!ctx.hasUI) return debug("intercept: skip, no UI");
		const targetCwdRaw = readSessionFileCwd(event.targetSessionFile);
		if (!targetCwdRaw) return debug("intercept: skip, no header cwd in target");
		const normTarget = normalizeCwd(targetCwdRaw);
		const ambient = normalizeCwd(process.cwd());
		if (normTarget === ambient) return debug("intercept: skip, cwd matches", normTarget);
		if (!existsSync(normTarget)) return debug("intercept: skip, target cwd missing (pi owns this)", normTarget);
		debug("intercept: mismatch, showing prompt", { targetCwdRaw, ambient });
		const choice = await ctx.ui.select(
			`That session belongs to ${targetCwdRaw},\ncurrent directory is ${ambient}. Rehome it?`,
			[KEEP_LABEL, FORK_LABEL, MOVE_LABEL],
		);
		debug("intercept: choice =", choice);
		const mode: RehomeMode | undefined = choice === FORK_LABEL ? "fork" : choice === MOVE_LABEL ? "move" : undefined;
		if (!mode) return; // undefined (dismissed) and KEEP both mean: pi's native behavior
		return rehomeForeignSession(ctx, event.targetSessionFile, targetCwdRaw, ambient, mode);
	});
}
