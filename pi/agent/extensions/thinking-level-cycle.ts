/**
 * Cycle the active model's supported thinking levels in both directions.
 *
 * Ctrl+I moves forward; Shift+Ctrl+I moves backward. Pi's existing built-in
 * thinking-level binding remains unchanged.
 */

import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { getSupportedThinkingLevels } from "@earendil-works/pi-ai";
import { Key } from "@earendil-works/pi-tui";

type Direction = "forward" | "backward";

function cycleThinkingLevel(
	pi: ExtensionAPI,
	ctx: ExtensionContext,
	direction: Direction,
): void {
	const model = ctx.model;
	if (!model?.reasoning) {
		ctx.ui.notify("Current model does not support thinking", "info");
		return;
	}

	const levels = getSupportedThinkingLevels(model);
	const currentIndex = levels.indexOf(pi.getThinkingLevel());
	const startIndex = currentIndex >= 0 ? currentIndex : 0;
	const step = direction === "forward" ? 1 : -1;
	const nextIndex = (startIndex + step + levels.length) % levels.length;
	const nextLevel = levels[nextIndex];

	if (nextLevel === undefined) return;
	pi.setThinkingLevel(nextLevel);
	ctx.ui.notify(`Thinking level: ${nextLevel}`, "info");
}

export default function thinkingLevelCycleExtension(pi: ExtensionAPI): void {
	pi.registerShortcut(Key.ctrl("i"), {
		description: "Cycle thinking level forward",
		handler: (ctx) => cycleThinkingLevel(pi, ctx, "forward"),
	});

	pi.registerShortcut(Key.shiftCtrl("i"), {
		description: "Cycle thinking level backward",
		handler: (ctx) => cycleThinkingLevel(pi, ctx, "backward"),
	});
}
