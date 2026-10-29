// Test probe: records what cwd other extensions see on session_start.
import { appendFileSync } from "node:fs";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
	pi.on("session_start", async (event, ctx) => {
		appendFileSync(
			"/tmp/rehome-test/cwd-probe.log",
			`${event.reason}\tctx.cwd=${ctx.cwd}\tprocess.cwd()=${process.cwd()}\tsm.getCwd()=${ctx.sessionManager.getCwd()}\n`,
		);
	});
}
