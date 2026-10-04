// renderView lights each hit from the dumped sky and block light (vanilla lightmap), not from the time of day alone.
// The view fixture holds a lit stone stripe, a stone stripe behind an unlit air pocket, and a torch-lit floor patch.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { FIXTURE, AGENT, WORLD, writeFixture } from '../tools/view/fixture.mjs'
import { renderView, readPose } from '../tools/view/render.mjs'
import { decodePng } from '../tools/view/renderer.mjs'

const stateDir = { worldsDir: path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'view-light-')), 'worlds') }
writeFixture(stateDir)
const basePose = readPose(WORLD, AGENT, stateDir)

const W = 65 // odd sizes: the centre pixel is the centre ray
const H = 37
const eye = FIXTURE.eye
const aim = ([x, y, z]) => ({ yaw: Math.atan2(-(x - eye.x), -(z - eye.z)), pitch: Math.atan2(y - eye.y, Math.hypot(x - eye.x, z - eye.z)) })
const luminance = (png, px, py) => {
  const img = decodePng(png)
  const at = (py * img.width + px) * 4
  return 0.2126 * img.rgba[at] + 0.7152 * img.rgba[at + 1] + 0.0722 * img.rgba[at + 2]
}
const look = (target, pose = {}) => renderView({ world: WORLD, agentName: AGENT, stateDir, width: W, height: H, pose: { ...basePose, ...pose }, override: aim(target), aim: true })
const centre = r => luminance(r.png, (W - 1) / 2, (H - 1) / 2)

const LIT_WALL = [2.5, 66.5, 5] // south face of the lit stone stripe
const DARK_WALL = [6.5, 66.5, 5] // the same stone, its face onto an air pocket with no light
const TORCH_FLOOR = [7.5, 65, 9] // floor top under block light 14 and no sky
const OPEN_FLOOR = [11.5, 65, 12] // floor top under open sky

test('a stone face onto an unlit pocket is drawn much darker than the same stone in daylight', () => {
  const lit = look(LIT_WALL)
  const dark = look(DARK_WALL)
  assert.equal(lit.center.name, 'stone')
  assert.equal(dark.center.name, 'stone')
  assert.ok(centre(lit) > 3 * centre(dark), `lit ${centre(lit)} dark ${centre(dark)}`)
})

test('the centre hit reports the light on its face: sky and block level and how well it can be seen', () => {
  assert.deepEqual(look(LIT_WALL).center.light, { sky: 15, block: 0, seeing: look(LIT_WALL).center.light.seeing })
  assert.ok(look(LIT_WALL).center.light.seeing > 0.9)
  const dark = look(DARK_WALL).center.light
  assert.deepEqual([dark.sky, dark.block], [0, 0])
  assert.ok(dark.seeing > 0.05 && dark.seeing < 0.15, `light 0 is faint but not black: ${dark.seeing}`)
  assert.equal(look(TORCH_FLOOR).center.light.block, 14)
})

test('at midnight a torch-lit floor is brighter than the open floor, and the open floor is darker than at noon', () => {
  const midnight = { timeOfDay: 18000 }
  const torch = look(TORCH_FLOOR, midnight)
  const open = look(OPEN_FLOOR, midnight)
  assert.ok(centre(torch) > 1.5 * centre(open), `torch ${centre(torch)} open ${centre(open)}`)
  assert.ok(centre(look(OPEN_FLOOR)) > 1.5 * centre(open))
  assert.ok(open.center.light.seeing < look(OPEN_FLOOR).center.light.seeing)
})

test('a mob in the unlit pocket is drawn darker than the same mob in daylight and reports its light', () => {
  const zombieAt = (x, z) => ({ entities: [{ name: 'zombie', type: 'hostile', pos: { x, y: 65, z }, width: 0.6, height: 1.95 }] })
  const inDark = look([6.5, 66, 5.5], zombieAt(6.5, 5.5))
  const inLight = look([2.5, 66, 5.5], zombieAt(2.5, 5.5))
  const [d] = inDark.seen
  const [l] = inLight.seen
  assert.deepEqual([d.light.sky, d.light.block, l.light.sky, l.light.block], [0, 0, 15, 0])
  assert.ok(luminance(inLight.png, l.px, l.py) > 2 * luminance(inDark.png, d.px, d.py))
})
