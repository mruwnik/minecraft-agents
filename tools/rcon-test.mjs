// Why JavaScript: thin entry point; the allow-list and builders are cljs in dashboard/src/dashboard/rcon_tools.cljs, run from the compiled bundle (tools/rcon-bundle.mjs).
// Sets up live tests of a Minecraft bot against the local server: sends a small allow-list of RCON commands
// (time, weather, tp, give, effects, damage, summon, setblock, fill, ...) aimed at allowed test players only.
//   node tools/rcon-test.mjs <subcommand> [args...]      e.g. node tools/rcon-test.mjs give ClaudeProbe bread 3
// Extra target players: RCON_TEST_TARGETS=Name1,Name2. To extend, edit the lists and `builders` in rcon_tools.cljs, then rebuild:
//   tools/compile dashboard rcon-tools --release
// This is a guard rail against accidents, not a security boundary.
import { runRconTool } from './rcon-bundle.mjs'

await runRconTool('testMain', process.argv.slice(2))
