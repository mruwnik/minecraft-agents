import test from 'node:test'
import assert from 'node:assert/strict'
import { celestialAngle, skyDarken, brightness, lightColor, faceUV, faceShade, cornerLight, sceneTime, SHADING_GLSL } from '../tools/view/web/shading.mjs'

const near = (a, b, eps = 1e-6) => assert.ok(Math.abs(a - b) < eps, `${a} vs ${b}`)

for (const [time, rain, thunder, want] of [
  [6000, 0, 0, 1], [18000, 0, 0, 0.2], [6000, 1, 0, (1 - 5 / 16) * 0.8 + 0.2], [6000, 0, 1, (1 - 5 / 16) * 0.8 + 0.2]
]) {
  test(`skyDarken(${time}, ${rain}, ${thunder}) = ${want}`, () => near(skyDarken(time, rain, thunder), want))
}

test('celestialAngle: noon is 0, midnight 0.5, wraps with the day', () => {
  near(celestialAngle(6000), 0)
  near(celestialAngle(18000), 0.5)
  near(celestialAngle(6000 + 24000), celestialAngle(6000))
})

test('skyDarken falls monotonically through dusk', () => {
  const values = []
  for (let t = 12000; t <= 13800; t += 100) values.push(skyDarken(t, 0))
  values.slice(1).forEach((v, i) => assert.ok(v <= values[i], `step ${i}`))
})

for (const [level, want] of [[0, 0], [15, 1], [7.5, 0.5 / (4 - 1.5)]]) {
  test(`brightness(${level})`, () => near(brightness(level), want))
}

test('lightColor: full sun is bright, darkness is dark and grey', () => {
  assert.ok(lightColor(15, 0, 1).every(c => c > 0.95))
  const dark = lightColor(0, 0, 1)
  assert.ok(dark.every(c => c < 0.1))
  assert.deepEqual(dark.map(c => c.toFixed(6)), [dark[0], dark[0], dark[0]].map(c => c.toFixed(6)))
})

test('lightColor: block light is warm, night sky is bluish and dimmer', () => {
  const [r, , b] = lightColor(0, 6, 1)
  assert.ok(r > b)
  const night = lightColor(15, 0, 0.2)
  const day = lightColor(15, 0, 1)
  assert.ok(night.every((c, i) => c < day[i]))
  assert.ok(night[2] > night[0])
})

for (const [axis, sign, local, want] of [
  [1, -1, [0.25, 0.9, 0.75], { face: 'top', u: 0.25, v: 0.75 }],
  [1, 1, [0.25, 0.1, 0.75], { face: 'bottom', u: 0.25, v: 0.25 }],
  [2, -1, [0.25, 0.3, 1], { face: 'side', u: 0.25, v: 0.7 }], // normal +z, south
  [2, 1, [0.25, 0.3, 0], { face: 'side', u: 0.75, v: 0.7 }], // north
  [0, -1, [1, 0.3, 0.2], { face: 'side', u: 0.8, v: 0.7 }], // east
  [0, 1, [0, 0.3, 0.2], { face: 'side', u: 0.2, v: 0.7 }] // west
]) {
  test(`faceUV axis ${axis} step ${sign}`, () => {
    const got = faceUV(axis, sign, local)
    assert.equal(got.face, want.face)
    near(got.u, want.u)
    near(got.v, want.v)
  })
}

for (const [axis, sign, want] of [[1, -1, 1], [1, 1, 0.5], [2, 1, 0.8], [2, -1, 0.8], [0, 1, 0.6], [0, -1, 0.6]]) {
  test(`faceShade axis ${axis} step ${sign}`, () => assert.equal(faceShade(axis, sign), want))
}

const cell = (sky, block, opaque = false) => ({ light: [sky, block], opaque })

for (const [name, args, want] of [
  ['all open', [cell(10, 4), cell(10, 4), cell(10, 4), cell(10, 4)], { sky: 10, block: 4, ao: 1 }],
  ['one side opaque takes the center light', [cell(8, 0), cell(0, 0, true), cell(8, 0), cell(8, 0)], { sky: 8, block: 0, ao: (1 + 0.2 + 1 + 1) / 4 }],
  ['both sides opaque occlude the corner', [cell(8, 0), cell(0, 0, true), cell(0, 0, true), cell(15, 0)], { sky: 9.75, block: 0, ao: (1 + 0.2 + 0.2 + 0.2) / 4 }],
  ['opaque corner', [cell(8, 0), cell(8, 0), cell(8, 0), cell(0, 0, true)], { sky: 8, block: 0, ao: (1 + 1 + 1 + 0.2) / 4 }],
  ['mean of the four', [cell(4, 8), cell(8, 4), cell(12, 0), cell(0, 12)], { sky: 6, block: 6, ao: 1 }]
]) {
  test(`cornerLight: ${name}`, () => {
    const got = cornerLight(...args)
    near(got.sky, want.sky)
    near(got.block, want.block)
    near(got.ao, want.ao)
  })
}

for (const name of ['float brightness(', 'vec3 lightColor(', 'vec3 faceUV(', 'vec3 cornerLight(', 'float skyDarken(', 'float faceShade(', 'float celestialAngle(']) {
  test(`SHADING_GLSL defines ${name}`, () => assert.ok(SHADING_GLSL.includes(name)))
}

const params = query => new URLSearchParams(query)
for (const [name, pose, query, want] of [
  ['pose values', { timeOfDay: 13000, rain: 0.5 }, '', { time: 13000, rain: 0.5 }],
  ['no pose values: noon, dry', {}, '', { time: 6000, rain: 0 }],
  ['time param overrides', { timeOfDay: 13000, rain: 0.5 }, 'time=18000', { time: 18000, rain: 0.5 }],
  ['time 0 is a value', { timeOfDay: 13000 }, 'time=0', { time: 0, rain: 0 }],
  ['time rounds', {}, 'time=100.6', { time: 101, rain: 0 }],
  ['bad time ignored', { timeOfDay: 13000 }, 'time=abc', { time: 13000, rain: 0 }],
  ['empty time ignored', { timeOfDay: 13000 }, 'time=', { time: 13000, rain: 0 }],
  ['rain param overrides', { rain: 0 }, 'rain=1', { time: 6000, rain: 1 }],
  ['rain clamps high', {}, 'rain=3', { time: 6000, rain: 1 }],
  ['rain clamps low', { rain: 0.4 }, 'rain=-2', { time: 6000, rain: 0 }],
  ['bad rain ignored', { rain: 0.4 }, 'rain=x', { time: 6000, rain: 0.4 }]
]) test(`sceneTime: ${name}`, () => assert.deepEqual(sceneTime(pose, params(query)), want))
