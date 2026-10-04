// Why JavaScript: thin entry point; the logic is cljs in dashboard/src/dashboard/rcon_tools.cljs, run from the compiled bundle (tools/rcon-bundle.mjs).
// Quick raw RCON passthrough for the owner's live tests. Not for agents by default; use tools/rcon-test.mjs for the allow-listed tool.
//   node tools/rcon-raw.mjs <command...>      e.g. node tools/rcon-raw.mjs list
// Exit codes: 0 response, 1 connection/auth failure, 2 no command. Password: ~/.config/minecraft-claude/rcon-password.
import { runRconTool } from './rcon-bundle.mjs'

await runRconTool('rawMain', process.argv.slice(2))
