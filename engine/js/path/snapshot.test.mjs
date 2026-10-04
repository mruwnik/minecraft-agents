// Why JavaScript: tests snapshot.mjs, which stays JS: binary data; section id arrays and prismarine chunk loading.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import vec3 from 'vec3'
import prismarineChunk from 'prismarine-chunk'
import prismarineRegistry from 'prismarine-registry'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import zlib from 'node:zlib'
import { encodeColumn } from '../view.mjs'
import { createSnapshot, columnSections, loadColumn, loadRecordedWorld, UNLOADED } from './snapshot.mjs'

const { Vec3 } = vec3
const registry = prismarineRegistry('26.1')
const ChunkColumn = prismarineChunk(registry)
const id = name => registry.blocksByName[name].defaultState

const mixedColumn = () => {
  const column = new ChunkColumn()
  const names = ['stone', 'dirt', 'oak_planks', 'glass', 'water', 'gravel', 'sand', 'netherrack']
  // 8 palette entries in one section, plus another with > 256 distinct ids to force a direct palette
  for (let i = 0; i < 400; i++) column.setBlockStateId(new Vec3(i & 15, 0 + ((i >> 4) & 15), (i * 7) & 15), id(names[i % names.length]))
  for (let i = 0; i < 300; i++) column.setBlockStateId(new Vec3(i & 15, 100 + ((i >> 4) & 15), (i >> 2) & 15), 100 + i)
  column.setBlockStateId(new Vec3(3, -64, 4), id('bedrock'))
  column.setBlockStateId(new Vec3(5, 319, 6), id('stone'))
  return column
}

test('columnSections matches getBlockStateId at random cells', () => {
  const column = mixedColumn()
  const sections = columnSections(column)
  assert.equal(sections.length, 24)
  let seed = 7
  const rnd = n => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed % n }
  for (let i = 0; i < 2000; i++) {
    const x = rnd(16), z = rnd(16), y = -64 + rnd(384)
    const sy = (y + 64) >> 4
    const local = (((y + 64) & 15) * 16 + z) * 16 + x
    assert.equal(sections[sy][local], column.getBlockStateId(new Vec3(x, y, z)))
  }
})

test('columnSections covers the busy cells exactly', () => {
  const column = mixedColumn()
  const sections = columnSections(column)
  for (let y = 0; y < 16; y++) for (let z = 0; z < 16; z++) for (let x = 0; x < 16; x++) {
    const sy = (y + 64) >> 4
    assert.equal(sections[sy][(((y + 64) & 15) * 16 + z) * 16 + x], column.getBlockStateId(new Vec3(x, y, z)))
  }
})

test('sections are Uint16Array(4096), distinct objects', () => {
  const sections = columnSections(mixedColumn())
  assert.ok(sections.every(s => s instanceof Uint16Array && s.length === 4096))
  assert.notEqual(sections[0], sections[1])
})

test('stateAt reads loaded cells, UNLOADED elsewhere', () => {
  const snap = createSnapshot({})
  loadColumn(snap, -1, 2, mixedColumn())
  assert.equal(snap.stateAt(-16 + 3, -64, 32 + 4), id('bedrock'))
  assert.equal(snap.stateAt(-16 + 5, 319, 32 + 6), id('stone'))
  assert.equal(snap.stateAt(0, 0, 0), UNLOADED)
  assert.equal(snap.stateAt(-16, -65, 32), UNLOADED)
  assert.equal(snap.stateAt(-16, 320, 32), UNLOADED)
  assert.equal(snap.stateAt(-16, 50, 32), 0)
})

test('setState updates a loaded section and ignores an unloaded one', () => {
  const snap = createSnapshot({})
  loadColumn(snap, 0, 0, new ChunkColumn())
  snap.setState(5, 70, 5, 99)
  assert.equal(snap.stateAt(5, 70, 5), 99)
  snap.setState(500, 70, 5, 99)
  assert.equal(snap.stateAt(500, 70, 5), UNLOADED)
})

test('hasColumn, columns and dropColumn', () => {
  const snap = createSnapshot({})
  snap.setSection(0, 0, 0, new Uint16Array(4096))
  snap.setSection(-3, 5, 7, new Uint16Array(4096))
  assert.ok(snap.hasColumn(-3, 7))
  assert.ok(!snap.hasColumn(1, 1))
  assert.deepEqual([...snap.columns()].sort(), [[-3, 7], [0, 0]])
  snap.dropColumn(-3, 7)
  assert.ok(!snap.hasColumn(-3, 7))
  assert.equal(snap.stateAt(-48, -64 + 5 * 16, 112), UNLOADED)
})

test('a missing section inside a loaded column is UNLOADED', () => {
  const snap = createSnapshot({})
  snap.setSection(0, 3, 0, new Uint16Array(4096))
  assert.equal(snap.stateAt(0, -64, 0), UNLOADED)
  assert.equal(snap.stateAt(0, -64 + 48, 0), 0)
})

test('loadRecordedWorld reads column files, honours the range, and returns the count', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'snapshot-test-'))
  const file = (cx, cz) => path.join(dir, `${cx}.${cz}.bin`)
  const buffer = zlib.deflateSync(encodeColumn({ column: mixedColumn(), x: 0, z: 0, t: 0, body: 'b', mcVersion: '26.1' }))
  ;[[0, 0], [1, 0], [5, 5]].forEach(([cx, cz]) => fs.writeFileSync(file(cx, cz), buffer))
  const all = createSnapshot({})
  assert.equal(loadRecordedWorld(all, dir), 3)
  assert.equal(all.stateAt(5, 319, 6), id('stone'))
  const some = createSnapshot({})
  assert.equal(loadRecordedWorld(some, dir, { cxMin: 0, cxMax: 1, czMin: 0, czMax: 0 }), 2)
  assert.ok(!some.hasColumn(5, 5))
})

// ---- sectionHas: does a section hold any state flagged in a per-state table? cached, and kept honest by writes ----

const flagFor = ids => { const t = new Uint8Array(100); ids.forEach(i => { t[i] = 1 }); return t }

test('sectionHas: true when the section holds a flagged state, false otherwise or when absent', () => {
  const snap = createSnapshot({})
  const ids = new Uint16Array(4096)
  ids[5] = 7
  snap.setSection(0, 4, 0, ids)
  snap.setSection(0, 5, 0, new Uint16Array(4096))
  const table = flagFor([7])
  assert.deepEqual([snap.sectionHas(table, 0, 4, 0), snap.sectionHas(table, 0, 5, 0), snap.sectionHas(table, 0, 6, 0), snap.sectionHas(table, 3, 4, 3)], [true, false, false, false])
})

test('sectionHas: a write into the section is seen, as is replacing the section, and a different table', () => {
  const snap = createSnapshot({})
  snap.setSection(0, 4, 0, new Uint16Array(4096))
  const table = flagFor([7])
  assert.equal(snap.sectionHas(table, 0, 4, 0), false)
  snap.setState(1, snap.minY + 64 + 1, 1, 7)
  assert.equal(snap.sectionHas(table, 0, 4, 0), true)
  snap.setState(1, snap.minY + 64 + 1, 1, 0)
  assert.equal(snap.sectionHas(table, 0, 4, 0), false)
  const other = new Uint16Array(4096)
  other[0] = 9
  snap.setSection(0, 4, 0, other)
  assert.deepEqual([snap.sectionHas(table, 0, 4, 0), snap.sectionHas(flagFor([9]), 0, 4, 0)], [false, true])
})
