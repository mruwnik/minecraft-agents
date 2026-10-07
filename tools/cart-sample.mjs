// Why JavaScript: thin entry point; the sampling and the speed maths are cljs (dashboard/src/dashboard/rcon_cart.cljs), run from the compiled bundle (tools/rcon-bundle.mjs).
// Follows one minecart over RCON (game time, Pos, Motion per tick, as fast as RCON answers) for the live rail tests.
//   node tools/cart-sample.mjs (--uuid <uuid> | --rider <player> | --near <x> <y> <z>) [--secs 30] [--until-stop] [--window 10]
//        [--cell <x> <z> --threshold 0.3] [--out samples.jsonl]
// JSON lines to --out (or stdout), summary to stdout. Exit codes: 0 done, 1 RCON failure, 2 bad arguments. Rebuild after cljs edits: tools/compile dashboard rcon-tools --release
import { runRconTool } from './rcon-bundle.mjs'

await runRconTool('cartSampleMain', process.argv.slice(2))
