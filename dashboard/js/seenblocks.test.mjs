// Why JavaScript: tests the JavaScript seen-block reader against the binary seen file.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { saveSeen } from '../../engine/js/seen-file.mjs'
import { createSeenBlocks } from './seenblocks.mjs'

const withSeen = async (dim, body) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'seenblocks-'))
  try {
    const ids = new Uint16Array(4096)
    ids[0] = 2
    await saveSeen(path.join(root, 'w', 'agents', 'B', 'engine', 'seen.bin'), {
      version: '1.21.4',
      sections: [{ dim, cx: 0, sy: 0, cz: 0, seen: 1, base: 0, ids, times: new Uint16Array(4096) }]
    })
    return body(root)
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
}

// the body writes its dimension as "overworld"; a plan's dimension may be named with or without the prefix
for (const [stored, asked] of [['overworld', undefined], ['overworld', 'minecraft:overworld'], ['minecraft:overworld', 'overworld'], ['the_nether', 'minecraft:the_nether']]) {
  test(`dim ${asked} reads sections stored as ${stored}`, () => withSeen(stored, root => {
    const memory = createSeenBlocks({ stateDir: { worldsDir: root }, world: 'w', body: 'B', dim: asked })
    assert.equal(memory.sections, 1)
  }))
}
