import { fileURLToPath } from "node:url";
import { Type } from "@earendil-works/pi-ai";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { requestSessionEditApproval } from "./approval.ts";
import { formatHallpassStatus, type HallpassStatus } from "./status.ts";

export interface ReflectionHooks {
	getStatus: () => HallpassStatus;
	/** Validate revision + TOML and build an approval diff without changing state. */
	previewSessionEdit: (toml: string, expectedRevision: number) => string;
	/** Revalidate revision + TOML, then apply the overlay and its side effects. */
	applySessionEdit: (toml: string, expectedRevision: number, ctx: ExtensionContext) => string;
}

export function registerReflectionTools(pi: ExtensionAPI, hooks: ReflectionHooks): void {
	pi.registerTool({
		name: "hallpass_status",
		label: "Hallpass status",
		description:
			"Read the active hallpass profile, inheritance chain, effective filesystem/network/tool policy, " +
			"agentUsage instructions, global domain denies, and ephemeral session overrides. " +
			"The returned session revision is required by hallpass_session_edit.",
		parameters: Type.Object({}),
		annotations: { readOnlyHint: true, destructiveHint: false, idempotentHint: true, openWorldHint: false },
		async execute(_id, _params, _signal, _onUpdate, _ctx) {
			const status = hooks.getStatus();
			return {
				content: [{ type: "text", text: formatHallpassStatus(status) }],
				details: status,
			};
		},
	});

	pi.registerTool({
		name: "hallpass_session_edit",
		label: "Edit hallpass session policy",
		description:
			"Request user approval for a complete replacement of the ephemeral per-session hallpass overlay. " +
			"First call hallpass_status and pass its current session revision. Send the complete desired overlay " +
			"as TOML profile fields (without a [profiles.name] table); an empty string clears all overrides. " +
			"Every request requires interactive user approval. Load the hallpass skill for schema, inheritance, " +
			"signed-list delta syntax, and examples rather than guessing field semantics.",
		promptGuidelines: [
			"Use hallpass_session_edit when a task needs a session-only policy change; the user must approve every request.",
		],
		parameters: Type.Object({
			toml: Type.String({ description: "Complete desired session overlay as TOML, or an empty string to clear it" }),
			expectedRevision: Type.Integer({
				minimum: 0,
				description: "Current session revision returned by hallpass_status; prevents stale approvals",
			}),
		}),
		executionMode: "sequential",
		async execute(_id, params, signal, _onUpdate, ctx) {
			if (!ctx.hasUI) {
				return {
					content: [{
						type: "text",
						text: "hallpass_session_edit is unavailable without interactive UI; no changes were made. Ask the user to edit /hp-session or hallpass.toml instead.",
					}],
					details: { applied: false, reason: "noninteractive" },
					isError: true,
				};
			}

			let preview: string;
			try {
				preview = hooks.previewSessionEdit(params.toml, params.expectedRevision);
			} catch (err) {
				const message = err instanceof Error ? err.message : String(err);
				return {
					content: [{ type: "text", text: `hallpass session edit rejected before approval: ${message}` }],
					details: { applied: false, reason: message },
					isError: true,
				};
			}

			if (signal?.aborted) {
				return {
					content: [{ type: "text", text: "hallpass session edit was aborted before approval; no changes were made." }],
					details: { applied: false, reason: "aborted" },
					isError: true,
				};
			}
			const approval = await requestSessionEditApproval(ctx, preview, signal);
			if (signal?.aborted || approval.aborted) {
				return {
					content: [{ type: "text", text: "hallpass session edit was aborted; no changes were made." }],
					details: { applied: false, reason: "aborted" },
					isError: true,
				};
			}
			if (!approval.approved) {
				const decision = approval.cancelled ? "cancelled" : "rejected";
				const note = approval.note ? `\nUser rejection reason: ${approval.note}` : "";
				return {
					content: [{ type: "text", text: `The user ${decision} the hallpass session edit; no changes were made.${note}` }],
					details: { applied: false, reason: decision, note: approval.note },
				};
			}

			try {
				const message = hooks.applySessionEdit(params.toml, params.expectedRevision, ctx);
				const note = approval.note ? `\nUser approval note: ${approval.note}` : "";
				return {
					content: [{ type: "text", text: `${message}${note}\n\n${formatHallpassStatus(hooks.getStatus())}` }],
					details: { applied: true, note: approval.note, status: hooks.getStatus() },
				};
			} catch (err) {
				const message = err instanceof Error ? err.message : String(err);
				return {
					content: [{ type: "text", text: `hallpass session edit was not applied: ${message}` }],
					details: { applied: false, reason: message },
					isError: true,
				};
			}
		},
	});

	// Ship the skill with the extension in both package and direct-extension installs.
	// Using resource discovery instead of also declaring pi.skills avoids duplicate loading.
	pi.on("resources_discover", () => ({
		skillPaths: [fileURLToPath(new URL("./skills", import.meta.url))],
	}));
}
