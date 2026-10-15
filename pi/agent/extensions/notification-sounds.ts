/**
 * Notification Sounds + Tab Badges
 *
 * Mirrors the global Claude Code hook sounds from ~/.claude/settings.json:
 *
 *   PermissionRequest -> afplay /System/Library/Sounds/Funk.aiff
 *   Stop              -> afplay /System/Library/Sounds/Blow.aiff
 *
 * Pi has no PermissionRequest event, so Funk plays whenever Pi blocks on an
 * extension UI prompt (select / confirm / input / editor) opened from *inside
 * a tool call* — i.e. the model or a tool is waiting on you. Prompts you open
 * yourself stay silent, even if you fire a command while the model is still
 * streaming (extension commands execute immediately, outside any tool call).
 * Blow plays when the agent run has fully settled (Claude's Stop).
 *
 * The terminal tab title also shows which tab needs your attention:
 *
 *   (no input yet)           -> "π - <cwd>" (no badge)
 *   turn processing          -> "⏳ π - <session> - <cwd>"
 *   blocked on input prompt  -> "🔔 π - <session> - <cwd>"
 *   turn complete            -> "✓ π - <session> - <cwd>"
 *
 * Badges clear before the session exits so they don't outlast Pi.
 */

import { spawn } from "node:child_process";
import { basename } from "node:path";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

const PERMISSION_SOUND = "/System/Library/Sounds/Funk.aiff";
const STOP_SOUND = "/System/Library/Sounds/Blow.aiff";

// afplay volume: 1.0 is full, so 0.7 is 70%.
const VOLUME = "0.7";

// Tab-title badges. Override with env vars if these emoji do not render well
// in your terminal.
const IN_PROGRESS_MARKER = process.env.PI_NOTIFY_IN_PROGRESS_MARKER ?? "⏳ ";
const ATTENTION_MARKER = process.env.PI_NOTIFY_ATTENTION_MARKER ?? "🔔 ";
const COMPLETION_MARKER = process.env.PI_NOTIFY_COMPLETION_MARKER ?? "✓ ";

// Pi titles the terminal "π - <session> - <cwd>" (APP_TITLE is "π" for the
// stock package; it is not exported). Override for a custom pi build.
const APP_TITLE = process.env.PI_APP_TITLE ?? "π";

type BadgeKind = "in-progress" | "attention" | "completion";

function play(path: string): void {
	if (process.platform !== "darwin") return;
	if (process.env.PI_NOTIFY_SOUNDS === "0") return;
	try {
		// Fire-and-forget: never block the agent or keep Pi alive on exit.
		spawn("afplay", ["-v", VOLUME, path], { stdio: "ignore", detached: true }).unref();
	} catch {
		// Ignore a missing player or sound file.
	}
}

/** Rebuild the title Pi itself would set, so a badge can be removed cleanly. */
function baseTitle(ctx: ExtensionContext): string {
	const cwd = basename(ctx.cwd);
	const session = ctx.sessionManager.getSessionName();
	return session ? `${APP_TITLE} - ${session} - ${cwd}` : `${APP_TITLE} - ${cwd}`;
}

export default function (pi: ExtensionAPI) {
	// Tool calls the agent is currently executing. A prompt opened from within a
	// tool call (e.g. the ask_user_question tool) is model-triggered. A prompt
	// opened by a command you run yourself happens outside any tool call, even
	// when it executes while the model is streaming.
	const activeToolCalls = new Set<string>();

	// Which badge is currently painted on the tab title, if any.
	let activeBadge: BadgeKind | null = null;

	function writeTitle(ctx: ExtensionContext, title: string): void {
		if (ctx.mode !== "tui") return;
		if (process.env.PI_NOTIFY_TITLE === "0") return;
		try {
			ctx.ui.setTitle(title);
		} catch {
			// Ignore terminals or modes that cannot set a title.
		}
	}

	function applyBadge(ctx: ExtensionContext, kind: BadgeKind): void {
		activeBadge = kind;
		const marker =
			kind === "in-progress" ? IN_PROGRESS_MARKER :
			kind === "attention" ? ATTENTION_MARKER :
			COMPLETION_MARKER;
		writeTitle(ctx, marker + baseTitle(ctx));
	}

	function clearBadge(ctx: ExtensionContext): void {
		activeBadge = null;
		writeTitle(ctx, baseTitle(ctx));
	}

	pi.on("tool_execution_start", (event) => {
		activeToolCalls.add(event.toolCallId);
	});

	pi.on("tool_execution_end", (event) => {
		activeToolCalls.delete(event.toolCallId);
	});

	// Start of turn: show in-progress badge to signal thinking/tools.
	pi.on("turn_start", (_event, ctx) => {
		applyBadge(ctx, "in-progress");
	});

	// Claude `PermissionRequest` analog: Pi is blocked waiting on the user.
	// Elevate to attention badge. `ctx.isIdle()` guards against a stale tool
	// entry after an abort.
	pi.on("ui_prompt_start", (_event, ctx) => {
		if (ctx.isIdle() || activeToolCalls.size === 0) return;
		play(PERMISSION_SOUND);
		applyBadge(ctx, "attention");
	});

	// Prompt answered: revert to in-progress badge (turn continues).
	pi.on("ui_prompt_end", (_event, ctx) => {
		if (activeBadge === "attention") {
			applyBadge(ctx, "in-progress");
		}
	});

	// Claude `Stop` analog: the full run settled. Show completion badge.
	pi.on("agent_settled", (_event, ctx) => {
		activeToolCalls.clear();
		play(STOP_SOUND);
		applyBadge(ctx, "completion");
	});

	// Session exit: reset title to badgeless so it doesn't outlast Pi.
	pi.on("session_shutdown", (_event, ctx) => {
		clearBadge(ctx);
	});
}
