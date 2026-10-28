import type { ExtensionContext } from "@earendil-works/pi-coding-agent";
import {
	decodeKittyPrintable,
	Editor,
	type EditorTheme,
	Key,
	matchesKey,
	wrapTextWithAnsi,
} from "@earendil-works/pi-tui";

export interface SessionEditApproval {
	approved: boolean;
	/** Optional user note attached to an explicit approval or rejection. */
	note?: string;
	cancelled?: boolean;
	aborted?: boolean;
}

function isPlainTextInput(data: string): boolean {
	if (decodeKittyPrintable(data) !== undefined) return true;
	if (data.startsWith("\x1b")) return false;
	return [...data].every((char) => char >= " " && char !== "\x7f");
}

/**
 * TUI: approve/reject immediately with Enter, or type on either option to attach
 * a note. RPC/other dialog UIs retain the standard boolean confirmation.
 */
export async function requestSessionEditApproval(
	ctx: ExtensionContext,
	preview: string,
	signal: AbortSignal | undefined,
): Promise<SessionEditApproval> {
	if (ctx.mode !== "tui") {
		const approved = await ctx.ui.confirm("Apply hallpass session overrides?", preview, { signal });
		return { approved, aborted: signal?.aborted };
	}

	return ctx.ui.custom<SessionEditApproval>((tui, theme, _keybindings, done) => {
		const choices = [
			{ label: "Approve", approved: true },
			{ label: "Reject", approved: false },
		];
		let selected = 1; // Reject is the safe default for a policy escalation.
		let editingNote = false;
		let componentFocused = false;
		let finished = false;
		let cachedWidth: number | undefined;
		let cachedLines: string[] | undefined;
		const editorTheme: EditorTheme = {
			borderColor: (text) => theme.fg("borderMuted", text),
			selectList: {
				selectedPrefix: (text) => theme.fg("accent", text),
				selectedText: (text) => theme.fg("accent", text),
				description: (text) => theme.fg("muted", text),
				scrollInfo: (text) => theme.fg("dim", text),
				noMatch: (text) => theme.fg("warning", text),
			},
		};
		const editor = new Editor(tui, editorTheme);

		function invalidate(): void {
			cachedWidth = undefined;
			cachedLines = undefined;
		}

		function refresh(): void {
			invalidate();
			tui.requestRender();
		}

		function finish(result: SessionEditApproval): void {
			if (finished) return;
			finished = true;
			signal?.removeEventListener("abort", onAbort);
			done(result);
		}

		function onAbort(): void {
			finish({ approved: false, aborted: true });
		}

		editor.onSubmit = (value) => {
			const choice = choices[selected] ?? choices[1]!;
			const note = value.trim();
			finish({ approved: choice.approved, note: note || undefined });
		};

		if (signal?.aborted) onAbort();
		else signal?.addEventListener("abort", onAbort, { once: true });

		return {
			get focused(): boolean {
				return componentFocused;
			},
			set focused(value: boolean) {
				componentFocused = value;
				editor.focused = value && editingNote;
				invalidate();
			},
			render(width: number): string[] {
				if (cachedLines && cachedWidth === width) return cachedLines;
				const renderWidth = Math.max(1, width);
				const contentWidth = Math.max(1, renderWidth - 2);
				const lines: string[] = [];

				function border(): void {
					lines.push(theme.fg("border", "─".repeat(renderWidth)));
				}

				function spacer(): void {
					lines.push("");
				}

				/** Match ExtensionEditor/Selector: one leading column and wrapped content. */
				function padded(rendered: string): void {
					for (const line of wrapTextWithAnsi(rendered, contentWidth)) lines.push(` ${line}`);
				}

				function previewLine(line: string): void {
					if (!line) {
						spacer();
						return;
					}
					if (line.startsWith("+ ")) {
						padded(theme.fg("success", line));
					} else if (line.startsWith("- ")) {
						padded(theme.fg("error", line));
					} else if (line.startsWith("  ")) {
						padded(theme.fg("muted", line));
					} else if (line === "Effective policy diff (- current, + proposed):" || line === "Complete session overlay:") {
						padded(theme.fg("accent", theme.bold(line)));
					} else if (line.startsWith("This changes only the current in-memory session.")) {
						padded(theme.fg("muted", line));
					} else {
						padded(theme.fg("text", line));
					}
				}

				border();
				spacer();
				padded(theme.fg("accent", theme.bold("Apply hallpass session overrides?")));
				spacer();
				for (const line of preview.split("\n")) previewLine(line);
				spacer();

				for (let i = 0; i < choices.length; i++) {
					const choice = choices[i]!;
					const active = i === selected;
					const prefix = active ? theme.fg("accent", "→ ") : "  ";
					const color = choice.approved ? "success" : "warning";
					padded(`${prefix}${theme.fg(active ? color : "text", choice.label)}`);
				}

				if (editingNote) {
					const choice = choices[selected] ?? choices[1]!;
					spacer();
					padded(theme.fg("muted", `Optional ${choice.label.toLowerCase()} note:`));
					spacer();
					lines.push(...editor.render(renderWidth));
				}

				spacer();
				const hint = editingNote
					? "Enter submit  Shift+Enter newline  Esc back (discard note)"
					: "↑↓ navigate  Enter select  type to add a note  Esc cancel";
				padded(theme.fg("dim", hint));
				spacer();
				border();

				cachedWidth = width;
				cachedLines = lines;
				return cachedLines;
			},
			handleInput(data: string): void {
				if (finished) return;
				if (editingNote) {
					if (matchesKey(data, Key.escape)) {
						editingNote = false;
						editor.focused = false;
						editor.setText("");
					} else {
						editor.handleInput(data);
					}
					refresh();
					return;
				}
				if (matchesKey(data, Key.up) || matchesKey(data, Key.left) || matchesKey(data, Key.shift("tab"))) {
					selected = (selected - 1 + choices.length) % choices.length;
				} else if (matchesKey(data, Key.down) || matchesKey(data, Key.right) || matchesKey(data, Key.tab)) {
					selected = (selected + 1) % choices.length;
				} else if (matchesKey(data, Key.enter)) {
					const choice = choices[selected] ?? choices[1]!;
					finish({ approved: choice.approved });
					return;
				} else if (matchesKey(data, Key.escape)) {
					finish({ approved: false, cancelled: true });
					return;
				} else if (isPlainTextInput(data)) {
					editingNote = true;
					editor.focused = componentFocused;
					editor.setText("");
					editor.handleInput(data);
				} else {
					return;
				}
				refresh();
			},
			invalidate,
			dispose: () => signal?.removeEventListener("abort", onAbort),
		};
	});
}
