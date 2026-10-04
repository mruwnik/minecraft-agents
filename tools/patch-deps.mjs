// fixes to node_modules that cannot be made from outside: run when preparing installed dependencies, so an npm install cannot quietly undo them
import fs from 'node:fs'
import path from 'node:path'
import { patchPathfinder, patchGotoPartial, patchParkourFences, patchItemEnchants, patchGateWaypoints } from './dependency-patches/patches.mjs'
import { patchScaffoldingPhysics, patchScaffoldingPreview, patchScaffoldingDriver } from './dependency-patches/scaffolding.mjs'
import { patchOwnBreath } from './dependency-patches/airlog.mjs'
import { patchTerrainWaypoints, patchTerrainStart, patchTerrainStop, patchPathNodeCopies } from './dependency-patches/terrain.mjs'
import { patchAttributeProtocol } from './dependency-patches/attribute-protocol.mjs'
import { patchClimbableTrapdoor } from './dependency-patches/trapdoor-ladder.mjs'

const PATCHES = [
  ['node_modules/mineflayer-pathfinder/index.js', patchPathNodeCopies, 'mineflayer-pathfinder immutable search nodes', 'path rendering must not mutate an unfinished A* search'],
  ['node_modules/minecraft-data/minecraft-data/data/pc/26.1/protocol.json', patchAttributeProtocol, '26.1 canonical attribute wire IDs', 'attribute schema changed; mounted movement requires verified negotiated IDs'],
  ['node_modules/mineflayer-pathfinder/lib/physics.js', patchScaffoldingPreview, 'mineflayer-pathfinder scaffold descent preview', 'scaffold sneak descent preview anchor changed'],
  ['node_modules/mineflayer-pathfinder/index.js', patchScaffoldingDriver, 'mineflayer-pathfinder scaffold descent controls', 'scaffold sneak descent driver anchor changed'],
  ['node_modules/prismarine-physics/index.js', patchClimbableTrapdoor, 'prismarine-physics climbs an open trapdoor over a ladder', 'read tools/dependency-patches/trapdoor-ladder.mjs: a body may stop under a hatch vanilla climbs'],
  ['node_modules/prismarine-physics/index.js', patchScaffoldingPhysics, 'prismarine-physics scaffold collision', 'read tools/dependency-patches/scaffolding.mjs: scaffolding climb simulation may treat its outline as solid'],
  ['node_modules/mineflayer-pathfinder/index.js', patchGateWaypoints, 'mineflayer-pathfinder centred gate waypoints', 'read patchGateWaypoints in tools/dependency-patches/patches.mjs: gate paths may aim into adjacent fences'],
  ['node_modules/mineflayer-pathfinder/index.js', patchTerrainWaypoints, 'mineflayer-pathfinder terrain waypoints', 'read patchTerrainWaypoints in tools/dependency-patches/terrain.mjs: partial blocks and climbing need matching route heights'],
  ['node_modules/mineflayer-pathfinder/index.js', patchTerrainStop, 'mineflayer-pathfinder preserve terrain stops', 'terrain stop anchor changed'],
  ['node_modules/mineflayer-pathfinder/index.js', patchTerrainStart, 'mineflayer-pathfinder terrain start', 'read patchTerrainStart in tools/dependency-patches/terrain.mjs: fractional block states need matching starting feet height'],
  ['node_modules/mineflayer-pathfinder/index.js', patchPathfinder, 'mineflayer-pathfinder gate fix', 'read patchPathfinder in tools/dependency-patches/patches.mjs: walks through gates may crash every tick'],
  ['node_modules/mineflayer-pathfinder/lib/movements.js', patchParkourFences, 'mineflayer-pathfinder no parkour over fences', 'read patchParkourFences in tools/dependency-patches/patches.mjs: a walk replanned mid-jump beside a fence row may try to jump along it and stall'],
  ['node_modules/mineflayer-pathfinder/lib/goto.js', patchGotoPartial, 'mineflayer-pathfinder goto waits out a partial search', 'read patchGotoPartial in tools/dependency-patches/patches.mjs: a goto from a dead end may fail at once and the body walk on after'],
  ['node_modules/prismarine-item/index.js', patchItemEnchants, 'prismarine-item enchants list', 'read patchItemEnchants in tools/dependency-patches/patches.mjs: an enchanted tool in hand may break harvest and slow every dig'],
  ['node_modules/mineflayer/lib/plugins/entities.js', patchOwnBreath, 'mineflayer own air only', 'read patchOwnBreath in tools/dependency-patches/airlog.mjs: every swimmer and squid in sight may set this body\'s oxygen (card 962beec2)']
]
for (const [relative, patch, title, warning] of PATCHES) {
  const file = path.join(import.meta.dirname, '..', relative)
  const { status, source } = patch(fs.readFileSync(file, 'utf8'))
  if (status === 'patched') {
    // several bodies may start at once: write beside it and rename
    fs.writeFileSync(`${file}.${process.pid}.tmp`, source)
    fs.renameSync(`${file}.${process.pid}.tmp`, file)
  }
  console.log(`[patch-deps] ${title}: ${status}${status === 'anchor missing' ? ` (a new version? ${warning})` : ''}`)
}
