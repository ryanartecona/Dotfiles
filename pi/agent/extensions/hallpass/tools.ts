/**
 * Tool-selection profiles: a profile's `tools = ["-write", "+grep"]` deltas.
 *
 * Two layers, because they fail differently:
 * - Visibility (main session only): pi.setActiveTools() hides disabled tools
 *   from the model and adds enabled ones. Skipped in subagent sessions because
 *   pi-subagents re-derives each child's tool scope every turn and would
 *   undo it.
 * - Enforcement (every session): a tool_call handler blocks a call to any
 *   tool the profile disables with `-name`. This is the backstop for children,
 *   for nested calls (codemode), and for anything that re-activates a tool.
 *
 * Deltas are applied relative to the CURRENT active set, and undone before the
 * next profile applies, so the user's own /tools choices survive a switch.
 * `+name` only activates tools already registered when it is applied; tools
 * that register later (MCP servers) are not picked up until the next apply.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import type { Profile } from "./profiles.ts";

/** Entries must be signed: a bare name would mean "replace the selection" in pi, which we avoid. */
export function parseToolDeltas(value: unknown, label: string, warnings: string[]): string[] | undefined {
	if (value === undefined) return undefined;
	if (!Array.isArray(value) || value.some((v) => typeof v !== "string")) {
		warnings.push(`${label}: expected an array of strings, ignoring`);
		return undefined;
	}
	const valid: string[] = [];
	for (const entry of value as string[]) {
		if (/^[+-]\S+$/.test(entry)) {
			valid.push(entry);
		} else {
			warnings.push(`${label}: "${entry}" must look like "+name" or "-name", ignoring it`);
		}
	}
	return valid;
}

export function createToolSelection(pi: ExtensionAPI): { apply: (profile: Profile | undefined) => void } {
	let removed = new Set<string>();
	let added = new Set<string>();

	function apply(profile: Profile | undefined): void {
		const active = new Set(pi.getActiveTools());
		// Undo whatever the previous profile did.
		for (const name of removed) active.add(name);
		for (const name of added) active.delete(name);
		removed = new Set();
		added = new Set();

		const registered = new Set(pi.getAllTools().map((tool) => tool.name));
		for (const delta of profile?.tools ?? []) {
			const name = delta.slice(1);
			if (delta.startsWith("-")) {
				if (active.delete(name)) removed.add(name);
			} else if (registered.has(name) && !active.has(name)) {
				active.add(name);
				added.add(name);
			}
		}
		pi.setActiveTools([...active]);
	}

	return { apply };
}

/** Enforcement layer: block calls to tools the active profile disables. */
export function installToolBlocker(pi: ExtensionAPI, getProfile: () => Profile | undefined): void {
	pi.on("tool_call", (event) => {
		const profile = getProfile();
		if (profile?.tools?.includes(`-${event.toolName}`)) {
			return {
				block: true,
				reason: `hallpass profile "${profile.name}" disables the ${event.toolName} tool. Ask the user to switch profiles (/hp-profile) if it is needed.`,
			};
		}
		return undefined;
	});
}
