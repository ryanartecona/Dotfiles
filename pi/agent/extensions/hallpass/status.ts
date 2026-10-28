import { summarizeProfile, type Profile } from "./profiles.ts";

/** Serializable status shared by /hp-status and the hallpass_status tool. */
export interface HallpassStatus {
	revision: number;
	baseProfile?: string;
	activeProfile?: string;
	inheritanceChain: string[];
	sessionOverrides: string;
	/** True when an invalid base is retaining but ignoring the overlay. */
	sessionOverridesIgnored?: boolean;
	deniedDomains: string[];
	warnings: string[];
	isChild: boolean;
	effectiveProfile?: Profile;
}

export function formatHallpassStatus(status: HallpassStatus): string {
	const lines = [
		`base profile: ${status.baseProfile ?? "(none)"}`,
		`inheritance: ${status.inheritanceChain.length ? status.inheritanceChain.join(" -> ") : "(none)"}`,
		`session revision: ${status.revision}`,
		`session overrides: ${status.sessionOverrides ? `\n${status.sessionOverrides}` : "(none)"}`,
		`global denied domains: ${status.deniedDomains.length ? status.deniedDomains.join(", ") : "(none)"}`,
	];
	if (status.sessionOverridesIgnored) {
		lines.push("session overrides ignored: yes (invalid base is fail-closed; fix config and /hp-reload)");
	}
	if (status.isChild) lines.push("subagent session: yes");
	if (status.effectiveProfile) lines.push("", summarizeProfile(status.effectiveProfile));
	else lines.push("", "hallpass: no active profile (bash unsandboxed)");
	if (status.warnings.length) lines.push("", `warnings:\n${status.warnings.map((warning) => `- ${warning}`).join("\n")}`);
	return lines.join("\n");
}

/** Small line-based diff used in the session-edit approval dialog. */
function diffLines(before: string[], after: string[]): string[] {
	const rows = before.length;
	const cols = after.length;
	const lcs = Array.from({ length: rows + 1 }, () => new Array<number>(cols + 1).fill(0));
	for (let i = rows - 1; i >= 0; i--) {
		for (let j = cols - 1; j >= 0; j--) {
			lcs[i]![j] = before[i] === after[j]
				? (lcs[i + 1]?.[j + 1] ?? 0) + 1
				: Math.max(lcs[i + 1]?.[j] ?? 0, lcs[i]?.[j + 1] ?? 0);
		}
	}

	const output: string[] = [];
	let i = 0;
	let j = 0;
	while (i < rows && j < cols) {
		if (before[i] === after[j]) {
			output.push(`  ${before[i]}`);
			i++;
			j++;
		} else if ((lcs[i + 1]?.[j] ?? 0) >= (lcs[i]?.[j + 1] ?? 0)) {
			output.push(`- ${before[i]}`);
			i++;
		} else {
			output.push(`+ ${after[j]}`);
			j++;
		}
	}
	while (i < rows) output.push(`- ${before[i++]}`);
	while (j < cols) output.push(`+ ${after[j++]}`);
	return output;
}

export function formatSessionEditPreview(current: Profile, proposed: Profile, toml: string): string {
	const diff = diffLines(summarizeProfile(current).split("\n"), summarizeProfile(proposed).split("\n"));
	return [
		"Effective policy diff (- current, + proposed):",
		"",
		...diff,
		"",
		"Complete session overlay:",
		"",
		toml.trim() || "(empty; clear all session overrides)",
		"",
		"This changes only the current in-memory session. It does not edit hallpass.toml.",
	].join("\n");
}
