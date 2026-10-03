// The visual models in a client jar: assets/minecraft/blockstates/<name>.json and assets/minecraft/models/block/<x>.json,
// as the maps tools/view/block-issues.mjs reads. Parent resolution happens there (resolveModel).
import fs from 'node:fs'
import { zipEntries, entryContent } from './jar-read.mjs'

const BLOCKSTATE = /^assets\/minecraft\/blockstates\/([^/]+)\.json$/
const BLOCK_MODEL = /^assets\/minecraft\/models\/(block\/[^/]+)\.json$/

export function loadModels (jarPath) {
  const buf = fs.readFileSync(jarPath)
  const blockstates = new Map()
  const models = new Map()
  for (const entry of zipEntries(buf)) {
    const state = BLOCKSTATE.exec(entry.name)
    const model = BLOCK_MODEL.exec(entry.name)
    if (!state && !model) continue
    const json = JSON.parse(entryContent(buf, entry).toString('utf8'))
    if (state) blockstates.set(state[1], json)
    else models.set(model[1], json)
  }
  return { blockstates, models }
}
