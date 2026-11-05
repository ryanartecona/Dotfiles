/**
 * TUI component for the /rehome directory picker. Rendered through
 * ctx.ui.custom(), which swaps it into the editor slot and focuses it.
 * Modeled on pi's SessionSelectorComponent: query line with type-to-filter,
 * navigable list, Enter to descend, Esc to cancel.
 */

import { dirname } from "node:path";
import { fuzzyFilter, matchesKey, type Component, type Focusable, type TUI } from "@earendil-works/pi-tui";
import type { Theme } from "@earendil-works/pi-coding-agent";
import { buildPickerEntries, type PickerEntry } from "./dir-picker.ts";

const MAX_VISIBLE = 10;

export class DirectoryPickerComponent implements Component, Focusable {
	/** Focusable interface — set by the TUI when focus changes. */
	focused = false;

	private entries: PickerEntry[] = [];
	private readError = false;
	private selectedIndex = 0;
	private scrollOffset = 0;
	private query = "";

	constructor(
		private tui: TUI,
		private theme: Theme,
		private currentDir: string,
		private done: (result: string | undefined) => void,
	) {
		this.refresh();
	}

	private refresh(): void {
		const { entries, error } = buildPickerEntries(this.currentDir, this.query, fuzzyFilter);
		this.entries = entries;
		this.readError = error;
		if (this.selectedIndex >= entries.length) {
			this.selectedIndex = Math.max(0, entries.length - 1);
		}
	}

	private move(delta: number): void {
		const n = this.entries.length;
		if (n === 0) return;
		this.selectedIndex = (this.selectedIndex + delta + n) % n;
	}

	private activate(): void {
		const entry = this.entries[this.selectedIndex];
		if (!entry) return;
		if (entry.action === "select") {
			this.done(entry.path);
			return;
		}
		this.currentDir = entry.path;
		this.query = "";
		this.selectedIndex = 0;
		this.scrollOffset = 0;
		this.refresh();
	}

	private goUp(): void {
		this.currentDir = dirname(this.currentDir);
		this.query = "";
		this.selectedIndex = 0;
		this.scrollOffset = 0;
		this.refresh();
	}

	handleInput(data: string): void {
		if (matchesKey(data, "escape")) {
			this.done(undefined);
			return;
		}
		if (matchesKey(data, "up")) {
			this.move(-1);
		} else if (matchesKey(data, "down")) {
			this.move(1);
		} else if (matchesKey(data, "return")) {
			this.activate();
			return;
		} else if (matchesKey(data, "left")) {
			this.goUp();
		} else if (matchesKey(data, "right")) {
			const entry = this.entries[this.selectedIndex];
			if (entry?.action === "enter") {
				this.activate();
				return;
			}
		} else if (matchesKey(data, "backspace")) {
			if (this.query.length > 0) {
				this.query = this.query.slice(0, -1);
				this.selectedIndex = 0;
				this.scrollOffset = 0;
				this.refresh();
			} else {
				this.goUp();
			}
		} else if (data.length === 1 && data.charCodeAt(0) >= 32) {
			this.query += data;
			this.selectedIndex = 0;
			this.scrollOffset = 0;
			this.refresh();
		}
		this.tui.requestRender();
	}

	render(_width: number): string[] {
		const t = this.theme;
		const lines: string[] = [];
		lines.push(`${t.bold(t.fg("accent", "Rehome to directory"))}  ${t.fg("muted", this.currentDir)}`);
		lines.push(t.fg("muted", "type to filter · ↑/↓ navigate · enter/→ open · ←/backspace up · ./ rehome here · esc cancel"));
		lines.push(`> ${this.query}${this.focused ? "█" : ""}`);
		if (this.readError) {
			lines.push(t.fg("error", "(directory not readable)"));
		}
		// Keep the selection inside the visible window.
		if (this.selectedIndex < this.scrollOffset) this.scrollOffset = this.selectedIndex;
		if (this.selectedIndex >= this.scrollOffset + MAX_VISIBLE) {
			this.scrollOffset = this.selectedIndex - MAX_VISIBLE + 1;
		}
		const visible = this.entries.slice(this.scrollOffset, this.scrollOffset + MAX_VISIBLE);
		for (let i = 0; i < visible.length; i++) {
			const idx = this.scrollOffset + i;
			const entry = visible[i]!;
			const selected = idx === this.selectedIndex;
			const marker = selected ? t.fg("accent", "> ") : "  ";
			let label = entry.label;
			if (entry.action === "select") label = `${label} (rehome here)`;
			const styled = entry.action === "select" ? t.fg("accent", label) : entry.pinned ? t.fg("muted", label) : label;
			lines.push(marker + (selected ? t.bold(styled) : styled));
		}
		if (!this.readError && this.entries.every((e) => e.pinned)) {
			lines.push(t.fg("muted", "  (no subdirectories)"));
		}
		return lines;
	}
}
