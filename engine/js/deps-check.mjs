// Are the source patches in node_modules (tools/patch-deps.mjs) still there? An npm install silently undoes them.
// [path from the repo root, marker substring, title]
export const PATCHES = Object.freeze([
  ['node_modules/mineflayer-pathfinder/lib/goto.js', "results.status !== 'partial') { // patched by bot/patch-deps.mjs", 'goto waits out a partial search'],
  ['node_modules/mineflayer-pathfinder/index.js', 'patched by bot/patch-deps.mjs: centre gate interaction waypoints', 'centred gate waypoints'],
  ['node_modules/mineflayer-pathfinder/index.js', 'if (!placingBlock) placing = false // patched by bot/patch-deps.mjs', 'gate fix'],
  ['node_modules/mineflayer-pathfinder/lib/movements.js', 'patched by bot/patch-deps.mjs: no parkour over a fence', 'no parkour over fences'],
  ['node_modules/prismarine-physics/index.js', 'patched by bot/patch-deps.mjs: context-dependent scaffolding collision', 'scaffolding collision'],
  ['node_modules/prismarine-physics/index.js', 'patched by bot/patch-deps.mjs: open trapdoor over a ladder', 'trapdoor over a ladder'],
  ['node_modules/mineflayer/lib/plugins/entities.js', 'patched by bot/patch-deps.mjs: my own air', 'own air only'],
  ['node_modules/mineflayer-pathfinder/lib/physics.js', 'patched by bot/patch-deps.mjs: scaffold descent preview', 'scaffold descent preview'],
  ['node_modules/mineflayer-pathfinder/index.js', 'patched by bot/patch-deps.mjs: scaffold descent driver', 'scaffold descent driver']
].map(Object.freeze))

// `read(relPath)` returns the file's text or null; the titles whose marker is absent (or whose file is missing).
export function missingPatches (read) {
  return PATCHES.filter(([path, marker]) => !(read(path) ?? '').includes(marker)).map(([, , title]) => title)
}

// A body that cannot do what its planner plans is worse than none: these patches are refused at start, not just reported.
export const REQUIRED = Object.freeze(['trapdoor over a ladder'])

export const missingRequired = read => missingPatches(read).filter(title => REQUIRED.includes(title))
