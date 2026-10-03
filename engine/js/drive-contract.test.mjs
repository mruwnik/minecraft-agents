import { test } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { createControl } from './control.mjs'

const START = 1000000
const scenarios = JSON.parse(fs.readFileSync(new URL('../test/contract/drive-contract.json', import.meta.url), 'utf8'))

const makeRig = () => {
  const world = { offline: false, settling: false }
  const look = { yaw: 0, pitch: 0 }
  const clock = { t: START }
  const pos = { x: 0, y: 64, z: 0 }
  const body = {
    status: () => ({ ...world, pos }),
    take: () => ({ ok: true }),
    release: () => {},
    deadman: () => {},
    stopDriving: () => {},
    drive: ({ look: l }) => {
      Object.assign(look, l ?? {})
      return { pos, yaw: look.yaw, pitch: look.pitch }
    }
  }
  const control = createControl({ socketPath: '/unused', body, releaseMs: 1000, idleMs: 15000, now: () => clock.t })
  return { world, clock, control }
}

const runStep = async ({ world, clock, control }, step) => {
  clock.t = START + step.at
  if (step.status) return Object.assign(world, step.status)
  if (step.tick) return control.tick()
  const res = await control.handle(step.req)
  assert.deepStrictEqual({ status: res.status, json: res.json }, step.expect, `at ${step.at} ${JSON.stringify(step.req)}`)
}

for (const { name, steps } of scenarios) {
  test(name, async () => {
    const rig = makeRig()
    for (const step of steps) await runStep(rig, step)
  })
}
