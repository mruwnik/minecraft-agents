// The three mineflayer-pathfinder monkey-patches (gate shift/guard, goto-empty-path, parkour scan) as data.

// mineflayer-pathfinder 2.4.5, monitorMovement: after opening a gate it takes the next thing to place, and when there is none "placing" stays true:
// the next tick reads placingBlock.y of undefined, every tick, until the path is reset. Only bites with a scaffolding block in the inventory
const GATE_SHIFT = "          placingBlock = nextPoint.toPlace.shift()\n        }, err => {\n"
const GATE_GUARD = "          placingBlock = nextPoint.toPlace.shift()\n          if (!placingBlock) placing = false // patched by bot/patch-deps.mjs\n        }, err => {\n"
export const patchPathfinder = source => source.includes(GATE_GUARD)
  ? { status: 'already', source }
  : source.includes(GATE_SHIFT) ? { status: 'patched', source: source.replace(GATE_SHIFT, GATE_GUARD) } : { status: 'anchor missing', source }

// mineflayer-pathfinder 2.4.5 lib/goto.js resolves on the first path_update with an empty path, a partial one included. At a dead end that is
// the nearest cell to the goal (the alley by Perrin's sheep pen gate) the first 40 ms slice has no step yet: goto failed in a second with
// "no walkable path" while the pathfinder, its goal still set, searched on and walked the body away after the task had ended
const GOTO_EMPTY = "      if (results.path.length === 0) {\n        cleanup()\n"
const GOTO_FIXED = "      if (results.path.length === 0 && results.status !== 'partial') { // patched by bot/patch-deps.mjs\n        cleanup()\n"
// mineflayer-pathfinder 2.4.5 lib/movements.js getMoveParkourForward: a fence is `physical: false` (nothing stands on it), so from a
// start node one above a fence row (a body mid-jump in a gate cell) three fence posts in a row look like a gap to jump: Chani's stalled
// walks all had "113.5,73,-70.5" as their first node, four cells along her fence line from the gate, and the body bounced against the
// first post at y 72.8-73.2 until the 12 s alarm. Nothing parkours over a fence, a wall or a gate: the scan stops at the first one
const PARKOUR_START = "    if ((block1.physical && block1.height >= block0.height) ||\n"
const PARKOUR_START_FIXED = "    if (this.fences.has(block1.type) || // patched by bot/patch-deps.mjs: no parkour over a fence, wall or gate\n      (block1.physical && block1.height >= block0.height) ||\n"
const PARKOUR_SCAN = "      const blockD = this.getBlock(node, dx, -1, dz)\n"
const PARKOUR_SCAN_FIXED = "      const blockD = this.getBlock(node, dx, -1, dz)\n      if (this.fences.has(blockD.type)) break // patched by bot/patch-deps.mjs: no parkour over a fence, wall or gate\n"
export const patchParkourFences = source => source.includes(PARKOUR_START_FIXED) && source.includes(PARKOUR_SCAN_FIXED)
  ? { status: 'already', source }
  : source.includes(PARKOUR_START) && source.includes(PARKOUR_SCAN)
    ? { status: 'patched', source: source.replace(PARKOUR_START, PARKOUR_START_FIXED).replace(PARKOUR_SCAN, PARKOUR_SCAN_FIXED) }
    : { status: 'anchor missing', source }

export const patchGotoPartial = source => source.includes(GOTO_FIXED)
  ? { status: 'already', source }
  : source.includes(GOTO_EMPTY) ? { status: 'patched', source: source.replace(GOTO_EMPTY, GOTO_FIXED) } : { status: 'anchor missing', source }

// prismarine-item, `get enchants`: with item components it returns the raw data ({enchantments: [{id, level}]}) instead of the [{name, lvl}] list that mineflayer's
// digTime (`enchantments.concat`) and everyone else expect: an enchanted tool in hand broke harvest and made digs crawl (Kettricken, right after the first ./mc enchant)
const ENCHANTS_RAW = "        return this.componentMap.get('enchantments').data\n"
const ENCHANTS_LIST = "        const raw = this.componentMap.get('enchantments').data // patched by patch-deps.mjs\n        return (Array.isArray(raw) ? raw : raw?.enchantments ?? []).map(e => ({ lvl: e.level ?? e.lvl, name: e.name ?? registry.enchantments[e.id]?.name ?? `unknown_${e.id}` }))\n"
export const patchItemEnchants = source => source.includes(ENCHANTS_LIST)
  ? { status: 'already', source }
  : source.includes(ENCHANTS_RAW) ? { status: 'patched', source: source.replace(ENCHANTS_RAW, ENCHANTS_LIST) } : { status: 'anchor missing', source }

// postProcessPath stopped at a gate-opening action, leaving that node and every
// following node at integer corners. The body then aimed into the neighbouring
// fence. A gate interaction changes no floor: centre it and continue the normal
// floor-height processing, while real digging/scaffolding still stops the pass.
const GATE_PATH_START = "      if (curPoint.toBreak.length > 0 || curPoint.toPlace.length > 0) break\n      const b = bot.blockAt(new Vec3(curPoint.x, curPoint.y, curPoint.z))\n"
const GATE_PATH_FIXED = `      if (curPoint.toBreak.length > 0 || curPoint.toPlace.some(p => !p.useOne)) break
      const b = bot.blockAt(new Vec3(curPoint.x, curPoint.y, curPoint.z))
      if (curPoint.toPlace.length > 0) { // patched by bot/patch-deps.mjs: centre gate interaction waypoints
        if (!b?.name?.endsWith('_fence_gate')) break
        curPoint.x = Math.floor(curPoint.x) + 0.5
        curPoint.y = Math.floor(curPoint.y)
        curPoint.z = Math.floor(curPoint.z) + 0.5
        continue
      }
`
// The terrain adapter may insert its hook before the block read; the gate
// guard and interaction branch remain intact and should still be recognised.
export const patchGateWaypoints = source => source.includes(GATE_PATH_FIXED) ||
  source.includes('      if (curPoint.toBreak.length > 0 || curPoint.toPlace.some(p => !p.useOne)) break') &&
  source.includes(GATE_PATH_FIXED.slice(GATE_PATH_FIXED.indexOf('      if (curPoint.toPlace.length > 0)')))
  ? { status: 'already', source }
  : source.includes(GATE_PATH_START) ? { status: 'patched', source: source.replace(GATE_PATH_START, GATE_PATH_FIXED) } : { status: 'anchor missing', source }
