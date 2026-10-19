/**
 * tmux-mcp fine-grain policy unification.
 *
 * tmux-mcp's pi package already gates every native `bash` call against its own
 * policy (it spawns `tmux-mcp policy-check` per call and blocks / asks / allows),
 * so hallpass does NOT run a second gate. What it adds is *selection*: a
 * hallpass profile's `tmuxProfile` names the tmux-mcp policy profile(s) to use,
 * and hallpass sets them through tmux-mcp's own pi.events interface:
 *
 *   tmux-mcp:shell-profile/get         emit { reply(state) }  -> sync state
 *   tmux-mcp:shell-profile/set-active  emit { names: string[] }
 *
 * The pi event bus is per session (each DefaultResourceLoader creates its own,
 * and pi-subagents does not share one), and each session has its own tmux-mcp
 * instance, so a subagent's selection never touches its parent's.
 *
 * Semantics:
 * - Precedence: the hallpass profile wins whenever it is chosen (session start,
 *   `/hp-profile`, reload), including re-selecting the already-active one. In
 *   between, the user may cycle tmux-mcp's profile directly (shift+tab) and
 *   that sticks until the next hallpass selection.
 * - A profile WITHOUT tmuxProfile leaves tmux-mcp alone, except that if the
 *   current tmux selection is still the one hallpass set, the pre-hallpass
 *   selection is restored. A selection the user cycled to since is kept.
 * - If tmux-mcp is not loaded the `get` emit gets no reply and sync is a no-op.
 * - Extension load order can make tmux-mcp's own session_start restore run
 *   after hallpass's, so the session-start sync is repeated once at the first
 *   prompt (`syncAtStart` / `syncIfPending`). Explicit selections do NOT arm
 *   that re-check, or it would undo a cycle made right after a switch.
 * - A tmuxProfile naming a profile tmux-mcp does not know makes tmux-mcp's
 *   policy check error, which blocks bash with its error text. hallpass does
 *   not validate names against `tmux-mcp profiles`.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import type { Profile } from "./profiles.ts";

interface ShellProfileState {
	active: string[];
	scoped: string[];
}

function sameSet(a: string[], b: string[]): boolean {
	return a.length === b.length && [...a].sort((x, y) => x.localeCompare(y)).join("\0") ===
			[...b].sort((x, y) => x.localeCompare(y)).join("\0");
}

export function createTmuxProfileSync(pi: ExtensionAPI): {
	/** Explicit selection (switch/reload): hallpass wins now; nothing is re-checked later. */
	sync: (profile: Profile | undefined) => void;
	/** Session start: sync now and arm one re-check at the first prompt (load-order race). */
	syncAtStart: (profile: Profile | undefined) => void;
	syncIfPending: (profile: Profile | undefined) => void;
} {
	let baseline: string[] | undefined;
	/** What hallpass last set, to tell its own selection from one the user cycled to. */
	let lastSet: string[] | undefined;
	let pending = false;

	function readState(): ShellProfileState | undefined {
		let state: ShellProfileState | undefined;
		pi.events.emit("tmux-mcp:shell-profile/get", {
			reply: (reply: ShellProfileState) => {
				state = reply;
			},
		});
		return state;
	}

	function sync(profile: Profile | undefined): void {
		const current = readState();
		if (!current) return;
		const wanted = profile?.tmuxProfiles;
		if (wanted && wanted.length > 0) {
			baseline ??= [...current.active];
			if (!sameSet(current.active, wanted)) {
				pi.events.emit("tmux-mcp:shell-profile/set-active", { names: wanted });
			}
			lastSet = [...wanted];
		} else {
			if (baseline && lastSet && sameSet(current.active, lastSet) && !sameSet(current.active, baseline)) {
				pi.events.emit("tmux-mcp:shell-profile/set-active", { names: baseline });
			}
			baseline = undefined;
			lastSet = undefined;
		}
	}

	return {
		sync(profile) {
			pending = false;
			sync(profile);
		},
		syncAtStart(profile) {
			sync(profile);
			pending = true;
		},
		syncIfPending(profile) {
			if (!pending) return;
			pending = false;
			sync(profile);
		},
	};
}
