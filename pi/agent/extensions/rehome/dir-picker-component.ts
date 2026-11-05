/**
 * Interactive directory-picker component for /rehome, rendered via
 * ctx.ui.custom() in the editor slot. Key handling is modeled on pi's
 * SessionSelectorComponent; the query line is a pi-tui Input component
 * (full emacs-style editing: ^a/^e/^b/^f, word moves, kill ring).
 */
import {
	fuzzyFilter,
	Input,
	matchesKey,
	truncateToWidth,
	type Component,
	type Theme,
	type TUI,
} from "@earendil-works/pi-tui";
import { buildPickerEntries, findExactDirEntry, type PickerEntry } from "./dir-picker.ts";

const MAX_VISIBLE = 10;

export class DirectoryPickerComponent implements Component {
	private currentDir: string;
	private readonly theme: Theme;
	private readonly queryInput = new Input({ prompt: "> " });
	private entries: PickerEntry[] = [];
	private error = false;
	private selectedIndex = 0;
	private scrollOffset = 0;
	private readonly onDone: (dir: string | undefined) => void;

	constructor(
		_tui: TUI,
		theme: Theme,
		currentDir: string,
		onDone: (dir: string | undefined) => void,
	) {
		this.currentDir = currentDir;
		this.theme = theme;
		this.onDone = onDone;
		this.queryInput.focused = true;
		this.refresh();
	}

	private get query(): string {
		return this.queryInput.getValue();
	}

	private refresh(): void {
		const { entries, error } = buildPickerEntries(this.currentDir, this.query, fuzzyFilter);
		this.entries = entries;
		this.error = error;
		if (this.selectedIndex >= this.entries.length) this.selectedIndex = Math.max(0, this.entries.length - 1);
	}

	handleInput(data: string): void {
		if (matchesKey(data, "escape")) {
			this.onDone(undefined);
			return;
		}
		if (matchesKey(data, "up")) {
			if (this.selectedIndex > 0) this.selectedIndex--;
			else if (this.scrollOffset > 0) this.scrollOffset--;
			return;
		}
		if (matchesKey(data, "down")) {
			if (this.selectedIndex + 1 < this.entries.length) this.selectedIndex++;
			return;
		}
		if (matchesKey(data, "tab")) {
			// A single option: Tab activates it, like Enter.
			if (this.entries.length === 1) {
				this.activate(this.entries[0]);
			} else if (this.selectedIndex + 1 < this.entries.length) {
				this.selectedIndex++;
			}
			return;
		}
		if (matchesKey(data, "shift+tab")) {
			if (this.selectedIndex > 0) this.selectedIndex--;
			else if (this.scrollOffset > 0) this.scrollOffset--;
			return;
		}
		if (matchesKey(data, "enter")) {
			this.activate(this.entries[this.selectedIndex]);
			return;
		}
		if (matchesKey(data, "right")) {
			const entry = this.entries[this.selectedIndex];
			if (entry && entry.action === "enter") this.activate(entry);
			return;
		}
		if (matchesKey(data, "left")) {
			this.goUp();
			return;
		}
		if (matchesKey(data, "backspace")) {
			if (this.query.length === 0) {
				this.goUp();
			} else {
				this.delegateToInput(data);
			}
			return;
		}
		if (data === "/") {
			// "ra/" descends into the exactly-matching directory, like Enter.
			const match = findExactDirEntry(this.entries, this.query);
			if (match) {
				this.currentDir = match.path;
				this.queryInput.setValue("");
				this.selectedIndex = 0;
				this.scrollOffset = 0;
				this.refresh();
				return;
			}
			// No exact match: treat the slash as ordinary query input.
		}
		// Everything else — printable characters, paste, and editing keys such
		// as ^a/^e/^b/^f/^u/^k, home/end, word moves — goes to the Input.
		this.delegateToInput(data);
	}

	/** Forward a key to the query Input; reset selection only when the text changed. */
	private delegateToInput(data: string): void {
		const before = this.query;
		this.queryInput.handleInput(data);
		if (this.query !== before) this.afterQueryEdit();
	}

	private afterQueryEdit(): void {
		this.selectedIndex = 0;
		this.scrollOffset = 0;
		this.refresh();
	}

	private activate(entry: PickerEntry | undefined): void {
		if (!entry) return;
		if (entry.action === "select") {
			this.onDone(entry.path);
			return;
		}
		this.currentDir = entry.path;
		this.queryInput.setValue("");
		this.selectedIndex = 0;
		this.scrollOffset = 0;
		this.refresh();
	}

	private goUp(): void {
		const parent = buildPickerEntries(this.currentDir, "", (i) => i).entries[1];
		if (parent) this.activate(parent);
	}

	render(width: number): string[] {
		const t = this.theme.fg.bind(this.theme);
		const lines: string[] = [];
		lines.push(t("accent", "─".repeat(width)));
		lines.push("");
		lines.push(`${t("accent", "Rehome to directory")}  ${t("muted", this.currentDir)}`);
		lines.push(...this.queryInput.render(width).map((line) => this.theme.bg("selectedBg", line)));
		if (this.error) {
			lines.push(t("error", "  (cannot read directory)"));
		}
		if (this.entries.length === 0) {
			if (!this.error) lines.push(t("muted", "  (no subdirectories)"));
		} else {
			if (this.selectedIndex < this.scrollOffset) this.scrollOffset = this.selectedIndex;
			if (this.selectedIndex >= this.scrollOffset + MAX_VISIBLE) this.scrollOffset = this.selectedIndex - MAX_VISIBLE + 1;
			const visible = this.entries.slice(this.scrollOffset, this.scrollOffset + MAX_VISIBLE);
			visible.forEach((entry, i) => {
				const selected = this.scrollOffset + i === this.selectedIndex;
				const styled = entry.pinned ? t("dim", entry.label) : entry.label;
				const marker = selected ? t("accent", "> ") : "  ";
				lines.push(marker + (selected ? this.theme.bold(styled) : styled));
			});
		}
		lines.push("");
		lines.push(
			t("muted", 'type to filter · ↑/↓/⇥ navigate · enter/→/"/" open · ←/backspace up · ./ rehome here · esc cancel'),
		);
		lines.push(t("accent", "─".repeat(width)));
		return lines.map((line) => truncateToWidth(line, width));
	}

	invalidate(): void {
		this.queryInput.invalidate();
	}
}
