import test from 'node:test'
import assert from 'node:assert/strict'
import { cameraBasis, rayDir } from '../tools/view/web/camera.mjs'
import { projectPoint, faceRegion } from '../tools/view/project.mjs'

const W = 640
const H = 360
const cameras = [
  { yaw: 0, pitch: 0, fov: 70 },
  { yaw: 0.7, pitch: -0.3, fov: 90 },
  { yaw: -2.1, pitch: 0.4, fov: 50 }
]
const pixels = [[0, 0], [320, 180], [639, 359], [17, 301], [500, 42]]
const cases = cameras.flatMap(camera => pixels.map(pixel => ({ camera, pixel })))

test('projectPoint inverts rayDir', () => {
  for (const { camera, pixel: [px, py] } of cases) {
    const basis = cameraBasis(camera)
    const d = rayDir(basis, px, py, W, H)
    const point = { x: d.x * 7.3, y: d.y * 7.3, z: d.z * 7.3 }
    const back = projectPoint(basis, point, W, H)
    assert.ok(Math.abs(back.px - px) < 1e-6, `px ${back.px} vs ${px}`)
    assert.ok(Math.abs(back.py - py) < 1e-6, `py ${back.py} vs ${py}`)
  }
})

test('projectPoint is null behind and at the camera plane', () => {
  const basis = cameraBasis({ yaw: 0, pitch: 0, fov: 70 })
  for (const point of [{ x: 0, y: 0, z: 1 }, { x: 3, y: 1, z: 0 }, { x: 0, y: 0, z: 0 }]) {
    assert.equal(projectPoint(basis, point, W, H), null)
  }
})

test('faceRegion shrinks the projected face by the inset', () => {
  const basis = cameraBasis({ yaw: 0, pitch: 0, fov: 70 })
  const eye = { x: 8, y: 66, z: 15 }
  const face = { axis: 'z', at: 5, x: [4, 12], y: [64, 68] }
  const full = faceRegion(basis, eye, face, W, H, 0)
  const inset = faceRegion(basis, eye, face, W, H, 0.25)
  const grew = [inset.x0 > full.x0, inset.x1 < full.x1, inset.y0 > full.y0, inset.y1 < full.y1]
  assert.deepEqual(grew, [true, true, true, true])
  // centre is the same
  assert.ok(Math.abs((full.x0 + full.x1) - (inset.x0 + inset.x1)) <= 2)
})

test('faceRegion handles faces on each axis and a face behind the camera', () => {
  const basis = cameraBasis({ yaw: 0, pitch: 0, fov: 70 })
  const eye = { x: 8, y: 66, z: 15 }
  const table = [
    [{ axis: 'z', at: 5, x: [4, 12], y: [64, 68] }, true],
    [{ axis: 'y', at: 65, x: [6, 10], z: [8, 11] }, true],
    [{ axis: 'x', at: 12, y: [64, 68], z: [5, 9] }, true],
    [{ axis: 'z', at: 20, x: [4, 12], y: [64, 68] }, false]
  ]
  for (const [face, visible] of table) {
    assert.equal(faceRegion(basis, eye, face, W, H) !== null, visible)
  }
})
