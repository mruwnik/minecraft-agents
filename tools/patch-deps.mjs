// fixes to node_modules that cannot be made from outside: run at every body start (start-body), so an npm install cannot quietly undo them
import fs from 'node:fs'
import path from 'node:path'
import { patchPathfinder, patchGotoPartial, patchParkourFences, patchItemEnchants } from '../src/lib.mjs'

const PATCHES = [
  ['node_modules/mineflayer-pathfinder/index.js', patchPathfinder, 'mineflayer-pathfinder gate fix', 'read patchPathfinder in src/lib.mjs: walks through gates may crash every tick'],
  ['node_modules/mineflayer-pathfinder/lib/movements.js', patchParkourFences, 'mineflayer-pathfinder no parkour over fences', 'read patchParkourFences in src/lib.mjs: a walk replanned mid-jump beside a fence row may try to jump along it and stall'],
  ['node_modules/mineflayer-pathfinder/lib/goto.js', patchGotoPartial, 'mineflayer-pathfinder goto waits out a partial search', 'read patchGotoPartial in src/lib.mjs: a goto from a dead end may fail at once and the body walk on after'],
  ['node_modules/prismarine-item/index.js', patchItemEnchants, 'prismarine-item enchants list', 'read patchItemEnchants in src/lib.mjs: an enchanted tool in hand may break harvest and slow every dig']
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
