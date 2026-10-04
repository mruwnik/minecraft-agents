// The block scanner runs only when asked: a /block-issues request, or the blockScan option (--block-scan).
// Why JavaScript: tests the JS view server (tools/view/serve.mjs).
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import zlib from 'node:zlib'
import { createViewServer } from '../tools/view/serve.mjs'
import { makeChunkClass } from '../tools/view/columns.mjs'

const realTextures = path.join(path.dirname(fileURLToPath(import.meta.url)), '..', 'textures')
const Chunk = makeChunkClass('1.21.4')
const sand = Chunk.registry.blocksByName.suspicious_sand.defaultState
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms))

const columnFile = () => {
  const column = new Chunk({ minY: -64, worldHeight: 384 })
  column.setBlockStateId({ x: 1, y: 70, z: 2 }, sand)
  const sections = column.dump()
  const header = Buffer.from(JSON.stringify({ v: 1, x: 0, z: 0, t: 1, body: 'Ann', mcVersion: '1.21.4', minY: -64, worldHeight: 384, parts: [{ name: 'sections', len: sections.length }] }))
  const length = Buffer.alloc(4)
  length.writeUInt32LE(header.length)
  return zlib.deflateSync(Buffer.concat([length, header, sections]))
}

// a world w1 with one column and one online body; `act` talks to the server; resolves whether the scan wrote its file
async function scanWritten ({ options = {}, act, waitMs }) {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'view-scan-'))
  const world = path.join(root, 'worlds', 'w1')
  fs.mkdirSync(path.join(world, 'chunks'), { recursive: true })
  fs.mkdirSync(path.join(world, 'agents', 'Bob', 'view'), { recursive: true })
  fs.writeFileSync(path.join(world, 'chunks', '0.0.bin'), columnFile())
  fs.writeFileSync(path.join(world, 'agents', 'Bob', 'view', 'pose.json'), JSON.stringify({ v: 1, t: 5, world: 'w1', status: 'online', eye: { x: 8, y: 70, z: 8 }, yaw: 0, pitch: 0 }))
  const server = createViewServer({ stateDir: root, textureDir: realTextures, webDir: root, blockJar: null, blockSweepMs: 50, blockWriteMs: 50, ...options })
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve))
  try {
    await act(`http://127.0.0.1:${server.address().port}`)
    const file = path.join(world, 'view-block-issues.json')
    const deadline = Date.now() + waitMs
    while (!fs.existsSync(file) && Date.now() < deadline) await sleep(50)
    return fs.existsSync(file)
  } finally {
    server.closeAllConnections()
    await new Promise(resolve => server.close(resolve))
    fs.rmSync(root, { recursive: true, force: true })
  }
}

const openStream = async base => {
  const response = await fetch(`${base}/pose/w1/Bob`)
  await response.body.getReader().read()
}

test('opening a pose stream does not start the block scan', async () => {
  assert.equal(await scanWritten({ act: openStream, waitMs: 3000 }), false)
})

test('opening a pose stream starts the block scan with the blockScan option', async () => {
  assert.equal(await scanWritten({ options: { blockScan: true }, act: openStream, waitMs: 15000 }), true)
})

test('a /block-issues request starts the block scan', async () => {
  assert.equal(await scanWritten({ act: async base => (await fetch(`${base}/block-issues/w1`)).text(), waitMs: 15000 }), true)
})
