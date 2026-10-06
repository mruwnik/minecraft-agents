// Why JavaScript: node:test file for the JS view writer (engine/js/view.mjs).
// A reloaded column whose content did not change is not rewritten (the timestamp does not count).
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { makeChunkClass } from '../tools/view/columns.mjs'
import { createView, columnFile } from '../engine/js/view.mjs'

const VERSION = '1.21.4'
const Chunk = makeChunkClass(VERSION)
const stone = Chunk.registry.blocksByName.stone.defaultState
const dirt = Chunk.registry.blocksByName.dirt.defaultState

const setup = () => {
  const stateDir = fs.mkdtempSync(path.join(os.tmpdir(), 'view-dedupe-'))
  const column = new Chunk({ minY: -64, worldHeight: 384 })
  column.setBlockStateId({ x: 1, y: 70, z: 2 }, stone)
  const handlers = {}
  const bot = {
    version: VERSION,
    registry: {},
    world: { getColumn: () => column, getColumns: () => [] },
    on: (name, fn) => { handlers[name] = fn },
    removeListener: () => {}
  }
  let clock = 1000
  const view = createView({ stateDir, agent: 'Bob', world: 'w', enabled: true, now: () => (clock += 1000), poseHz: 0 })
  view.attach(bot)
  const file = columnFile(stateDir, 'w', 0, 0)
  const flush = async () => { await view.flushColumns(); await view.idle() }
  return { stateDir, column, handlers, view, file, flush }
}

test('reloading an unchanged column writes no new file; a changed one does', async () => {
  const { stateDir, column, handlers, view, file, flush } = setup()
  try {
    handlers.chunkColumnLoad({ x: 0, z: 0 })
    await flush()
    assert.equal(view.stats().columns, 1)
    const first = fs.statSync(file).mtimeMs
    handlers.chunkColumnLoad({ x: 0, z: 0 })
    await flush()
    assert.equal(view.stats().columns, 0, 'identical content rewritten')
    assert.equal(fs.statSync(file).mtimeMs, first)
    column.setBlockStateId({ x: 1, y: 70, z: 2 }, dirt)
    handlers.chunkColumnLoad({ x: 0, z: 0 })
    await flush()
    assert.equal(view.stats().columns, 1)
  } finally {
    view.stop()
    fs.rmSync(stateDir, { recursive: true })
  }
})
