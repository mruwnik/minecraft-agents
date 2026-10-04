// Why JavaScript: thin entry point; the logic (a client that can only send `whitelist add <valid player name>`) is cljs in dashboard/src/dashboard/rcon_tools.cljs, run from the compiled bundle (tools/rcon-bundle.mjs).
//   node tools/rcon.mjs Aviendha
// Needs enable-rcon=true in server.properties and the password in ~/.config/minecraft-claude/rcon-password (chmod 600).
// This is a guard rail against accidents, not a security boundary.
import { runRconTool } from './rcon-bundle.mjs'

await runRconTool('whitelistMain', process.argv.slice(2))
