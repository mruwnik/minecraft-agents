// The fake body every composite test drives: records each act() call as one rendered line, answers from a table
import { compact } from '../src/lib.mjs'

export const fakeApi = ({ world = {}, place, places = [], items = {}, drops = [], freeSlots = 27, answers = {} } = {}) => {
  const calls = []
  const report = {}
  const checkpoints = []
  const api = {
    act: async (name, args = {}) => {
      calls.push(`${name} ${compact(args, false)}`.trim())
      const answer = typeof answers[name] === 'function' ? answers[name](args) : answers[name]
      if (answer instanceof Error) throw answer
      return answer ?? {}
    },
    until: async () => true,
    checkpoint: async (extra = {}) => { checkpoints.push(extra) },
    clock: () => ({ time: 1000, day: true, night: false, elapsedDays: 0 }),
    inv: () => items,
    pos: () => ({ x: 0, y: 64, z: 0 }),
    block: (x, y, z) => {
      const name = world[`${x},${y},${z}`]
      const tag = String(name).split('#')[1]
      return name === undefined ? null : { name: String(name).split('#')[0], properties: { age: Number(tag) || 0, open: tag === 'open' }, solid: name !== 'air' && name !== 'water' }
    },
    plan: () => place,
    places: () => places,
    drops: () => drops,
    freeSlots: () => freeSlots,
    solid: name => Boolean(name) && name !== 'air' && name !== 'water',
    pen: () => null,
    pause: async () => {},
    note: line => calls.push(`note ${line}`),
    report: partial => Object.assign(report, partial)
  }
  return { api, calls, report, checkpoints }
}
