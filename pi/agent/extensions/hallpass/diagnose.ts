/**
 * hallpass failure explanations.
 *
 * A sandboxed command that fails with a bare "Operation not permitted" or a
 * proxy "CONNECT ... 403" gives the model nothing to act on, and it tends to
 * retry or work around the block. This appends (a) srt's own violation block
 * when it recorded one and (b) a short hallpass hint that names the active
 * profile and says the block is policy, so the model reports it instead of
 * hunting for a bypass.
 *
 * Violation sources: proxy (network) denials are recorded synchronously by
 * srt. Filesystem denials are NOT available from srt on macOS: its `log
 * stream` monitor never receives them (see LOG_MONITOR_ENABLED in
 * sandbox.ts), so the pattern-based hint below is the primary fs explanation,
 * and it also carries the profile context srt does not know.
 */

import type { Profile } from "./profiles.ts";

const FS_DENIAL = /Operation not permitted|Permission denied|EPERM|EACCES|Read-only file system/i;
const NET_DENIAL = /CONNECT tunnel failed, response 40[34]|Received HTTP code 40[34] from proxy|Could not resolve proxy/i;

/** True when output looks like a filesystem denial (worth waiting for srt's log monitor). */
export function looksLikeFsDenial(output: string): boolean {
	return FS_DENIAL.test(output);
}

/** Keep only the tail of streamed output, enough for pattern checks. */
export class OutputTail {
	private text = "";
	private readonly limit: number;
	constructor(limit = 8192) {
		this.limit = limit;
	}
	push(chunk: Buffer | string): void {
		this.text += chunk.toString();
		if (this.text.length > this.limit) this.text = this.text.slice(-this.limit);
	}
	toString(): string {
		return this.text;
	}
}

/**
 * Text to append to a failed command's output, or "" when there is nothing
 * useful to add. `output` is the command's (possibly truncated) output.
 */
export function explainFailure(profile: Profile, output: string, srtViolations: string): string {
	const parts: string[] = [];
	if (srtViolations) parts.push(srtViolations);

	const hints: string[] = [];
	if (FS_DENIAL.test(output)) {
		const writable = profile.filesystem.allowWrite.length
			? profile.filesystem.allowWrite.join(", ")
			: "nothing";
		hints.push(
			`filesystem access may have been blocked by the hallpass sandbox (profile "${profile.name}"; writable: ${writable}).`,
		);
	}
	if (NET_DENIAL.test(output) || srtViolations.includes("allow list")) {
		let detail: string;
		if (srtViolations.includes("deny list")) {
			detail = "host is on hallpass's global always-deny list";
		} else if (profile.network.mode === "allowlist") {
			detail = `allowed domains: ${profile.network.allowedDomains.join(", ") || "none"}`;
		} else {
			detail = `network mode: ${profile.network.mode}`;
		}
		hints.push(`network access may have been blocked by the hallpass sandbox (profile "${profile.name}"; ${detail}).`);
	}
	if (hints.length > 0) {
		parts.push(
			`<hallpass>\n${hints.join("\n")}\n` +
				`This is an enforced policy, not a transient error: do not retry or try to bypass it. ` +
				`Use hallpass_status to inspect the effective policy. If the access is needed, explain which path or host ` +
				`and request a user-approved session change with hallpass_session_edit, or ask the user to switch profiles (/hp-profile).\n</hallpass>`,
		);
	}
	return parts.join("\n");
}
