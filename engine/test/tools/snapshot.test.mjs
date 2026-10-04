// Why JavaScript: tests the engine/tools launchers, which stay JS; a JS test is the honest check of a JS entry point (process, argv, exit code).
// The snapshot launcher end to end on the view fixture world (tools/view/fixture.mjs): a real render, a real PNG.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import zlib from 'node:zlib'
import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { writeFixture, poseFor, WORLD, AGENT } from '../../../tools/view/fixture.mjs'
import { readEDN } from './edn.mjs'

const cli = fileURLToPath(new URL('../../tools/snapshot.mjs', import.meta.url))

function fixture (t) {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'snapshot-'))
  t.after(() => fs.rmSync(dir, { recursive: true, force: true }))
  const worlds = path.join(dir, 'worlds')
  writeFixture({ worldsDir: worlds })
  fs.writeFileSync(path.join(worlds, WORLD, 'world.json'), '{}')
  const workspace = path.join(dir, 'ws')
  const run = (...args) => {
    const result = spawnSync(process.execPath, [cli, AGENT, '--world', WORLD, '--worlds', worlds, '--workspace', workspace, ...args], { encoding: 'utf8' })
    return { ...result, edn: readEDN(result.stdout) }
  }
  const pose = overrides => fs.writeFileSync(path.join(worlds, WORLD, 'agents', AGENT, 'view', 'pose.json'), JSON.stringify({ ...poseFor(Date.now()), ...overrides }))
  return { workspace, run, pose }
}

// the decoded RGBA pixels of an 8-bit RGBA PNG written by encodePng (filter 0 rows)
function pixels (file) {
  const buf = fs.readFileSync(file)
  const width = buf.readUInt32BE(16)
  const height = buf.readUInt32BE(20)
  const idat = []
  for (let at = 8; at < buf.length;) {
    const len = buf.readUInt32BE(at)
    if (buf.toString('ascii', at + 4, at + 8) === 'IDAT') idat.push(buf.subarray(at + 8, at + 8 + len))
    at += 12 + len
  }
  const raw = zlib.inflateSync(Buffer.concat(idat))
  const rows = Array.from({ length: height }, (_, y) => raw.subarray(y * (width * 4 + 1) + 1, (y + 1) * (width * 4 + 1)))
  return { width, height, rgba: Buffer.concat(rows) }
}
const colours = ({ rgba }) => new Set(Array.from({ length: rgba.length / 4 }, (_, i) => rgba.readUInt32BE(i * 4))).size

test('snapshot renders the fixture wall into the workspace and names the block under the crosshair', t => {
  const f = fixture(t)
  const result = f.run('--width', '160', '--height', '90')
  assert.equal(result.status, 0, result.stdout + result.stderr)
  assert.match(result.edn.png, /^snapshots\/snap-.*\.png$/)
  const image = pixels(path.join(f.workspace, result.edn.png))
  assert.deepEqual([image.width, image.height], [160, 90])
  assert.ok(colours(image) > 20, 'a scene, not one colour')
  assert.equal(result.edn.crosshair.block, 'stone')
  assert.deepEqual(result.edn.crosshair.at, [8, 66, 4])
  assert.equal(result.edn.crosshair.distance, 10.5)
  assert.equal(result.edn.facing.compass, 'north')
  assert.match(result.edn.text, /Crosshair: stone at 8 66 4, 10.5 blocks/)
})

test('a direction override changes the picture and the crosshair but not the pose file', t => {
  const f = fixture(t)
  f.pose({})
  const before = f.run('--width', '64', '--height', '36')
  const poseFile = path.join(path.dirname(f.workspace), 'worlds', WORLD, 'agents', AGENT, 'view', 'pose.json')
  const poseText = fs.readFileSync(poseFile, 'utf8')
  const turned = f.run('--width', '64', '--height', '36', '--yaw', '180')
  assert.equal(turned.status, 0, turned.stdout)
  assert.equal(turned.edn.facing.compass, 'south')
  assert.equal(turned.edn.crosshair, null)
  assert.notDeepEqual(pixels(path.join(f.workspace, before.edn.png)).rgba, pixels(path.join(f.workspace, turned.edn.png)).rgba)
  assert.equal(fs.readFileSync(poseFile, 'utf8'), poseText)
  const aimed = f.run('--width', '64', '--height', '36', '--look-at', '13,66,4')
  assert.equal(aimed.edn.crosshair.block, 'diamond_ore')
})

test('an offline body and bad arguments are refused with a reason', t => {
  const f = fixture(t)
  f.pose({ status: 'offline' })
  const offline = f.run()
  assert.equal(offline.status, 1)
  assert.equal(offline.edn.reason.key, 'body-offline')
  for (const bad of [['--width', '0'], ['--look-at', '1,2'], ['--yaw', 'x']]) {
    const result = f.run(...bad)
    assert.equal(result.status, 2, bad.join(' '))
    assert.equal(result.edn.reason.key, 'invalid-option')
  }
  assert.equal(fs.existsSync(path.join(f.workspace, 'snapshots')), false)
})

test('--help prints the usage without rendering', () => {
  const result = spawnSync(process.execPath, [cli, '--help'], { encoding: 'utf8' })
  assert.equal(result.status, 0)
  assert.match(result.stdout, /snapshot/)
  assert.match(result.stdout, /--look-at/)
})
