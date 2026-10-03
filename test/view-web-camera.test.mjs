import test from 'node:test'
import assert from 'node:assert/strict'
import { directionFor, cameraBasis, rayDir } from '../tools/view/web/camera.mjs'
import { directionFor as rendererDirectionFor, makeGrid, render } from '../src/vision/renderer.mjs'

const near = (a, b, msg) => assert.ok(Math.abs(a - b) < 1e-9, `${msg}: ${a} vs ${b}`)
const angles = [-3, -1.2, 0, 0.7, Math.PI / 2, 2.5, Math.PI, 5]
const pitches = [-1.2, -0.3, 0, 0.4, 1.3]

test('camera: directionFor equals the renderer\'s', () => {
  for (const yaw of angles) {
    for (const pitch of pitches) assert.deepEqual(directionFor(yaw, pitch), rendererDirectionFor(yaw, pitch))
  }
})

test('camera: the centre ray of an odd-sized image is forward', () => {
  for (const yaw of angles) {
    for (const pitch of pitches) {
      const basis = cameraBasis({ yaw, pitch, fov: 90 })
      const ray = rayDir(basis, 10, 6, 21, 13)
      for (const axis of ['x', 'y', 'z']) near(ray[axis], basis.forward[axis], `${yaw} ${pitch} ${axis}`)
    }
  }
})

test('camera: rays are unit length and half comes from the horizontal fov', () => {
  const basis = cameraBasis({ yaw: 1, pitch: 0.2, fov: 90 })
  near(basis.half, 1, 'half')
  const ray = rayDir(basis, 0, 0, 16, 9)
  near(Math.hypot(ray.x, ray.y, ray.z), 1, 'length')
})

const facing = [
  ['north', 0, { x: 0, z: -1 }],
  ['west', Math.PI / 2, { x: -1, z: 0 }],
  ['south', Math.PI, { x: 0, z: 1 }]
]
for (const [name, yaw, want] of facing) {
  test(`camera: yaw ${yaw.toFixed(2)} faces ${name}`, () => {
    const f = cameraBasis({ yaw, pitch: 0, fov: 90 }).forward
    near(f.x, want.x, 'x')
    near(f.z, want.z, 'z')
  })
}

test('camera: facing north the left of the image is west and the top is up', () => {
  const basis = cameraBasis({ yaw: 0, pitch: 0, fov: 90 })
  assert.ok(rayDir(basis, 0, 4, 9, 9).x < 0)
  assert.ok(rayDir(basis, 8, 4, 9, 9).x > 0)
  assert.ok(rayDir(basis, 4, 0, 9, 9).y > rayDir(basis, 4, 4, 9, 9).y)
  assert.ok(rayDir(basis, 4, 8, 9, 9).y < 0)
})

test('camera: positive pitch looks up', () => {
  assert.ok(cameraBasis({ yaw: 0, pitch: 0.5, fov: 90 }).forward.y > 0)
})

// a stone block to the camera's left (west of a camera facing north): the pixels the renderer paints with it are on the
// side of the image our rays point at it
test('camera: the renderer shows a block on the side our rays point at', () => {
  const grid = makeGrid({ x: -8, y: -4, z: -8 }, { x: 16, y: 8, z: 16 })
  grid.set(-3, 0, -4, 1)
  const width = 33
  const height = 17
  const eye = { x: 0.5, y: 0.5, z: 0.5 }
  const img = render({
    grid, info: id => [null, { kind: 'cube', name: 'rock' }][id], texture: () => ({ width: 1, height: 1, rgba: Uint8Array.from([255, 0, 0, 255]) }),
    eye, timeOfDay: 6000, width, height, yaw: 0, pitch: 0, fov: 90, maxDist: 12
  })
  const red = []
  for (let i = 0; i < width * height; i++) {
    const r = img.rgba[i * 4]
    if (r > img.rgba[i * 4 + 1] + 60 && r > img.rgba[i * 4 + 2] + 60) red.push(i)
  }
  assert.ok(red.length > 0)
  const basis = cameraBasis({ yaw: 0, pitch: 0, fov: 90 })
  const lo = { x: -3, y: 0, z: -4 }
  const hi = { x: -2, y: 1, z: -3 }
  const hits = ray => {
    const spans = ['x', 'y', 'z'].map(a => [(lo[a] - eye[a]) / ray[a], (hi[a] - eye[a]) / ray[a]].sort((p, q) => p - q))
    return Math.max(...spans.map(s => s[0])) <= Math.min(...spans.map(s => s[1])) + 1e-9
  }
  for (const i of red) assert.ok(hits(rayDir(basis, i % width, Math.floor(i / width), width, height)), `ray of pixel ${i} misses the block`)
  assert.ok(!hits(rayDir(basis, width - 1, 8, width, height)), 'the right edge does not see it')
})
