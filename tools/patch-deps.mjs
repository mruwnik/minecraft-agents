// fixes to node_modules that cannot be made from outside: run at every body start (start-body), so an npm install cannot quietly undo them
import fs from 'node:fs'
import path from 'node:path'
import { patchPathfinder, patchItemEnchants } from '../src/lib.mjs'

const PATCHES = [
  ['node_modules/mineflayer-pathfinder/index.js', patchPathfinder, 'mineflayer-pathfinder gate fix', 'read patchPathfinder in src/lib.mjs: walks through gates may crash every tick'],
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
