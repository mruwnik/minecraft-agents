// The fake body every composite test drives: records each act() call as one rendered line, answers from a table
import { compact } from '../src/lib.mjs'

export const fakeApi = ({ world = {}, place, places = [], items = {}, drops = [], freeSlots = 27, answers = {} } = {}) => {
  const calls = []
  const report = {}
  const checkpoints = []
  const events = []
  const progress = {}
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
    me: () => 'Tester',
    pos: () => ({ x: 0, y: 64, z: 0 }),
    block: (x, y, z) => {
      const raw = world[`${x},${y},${z}`]
      if (raw === undefined) return null
      // a trailing '~' is a waterlogged block: a slab laid in a channel still holds its water
      const name = String(raw).replace('~', '').split('#')[0]
      const tag = String(raw).replace('~', '').split('#')[1]
      // a #tag reused for whichever property the block actually has: age for a crop, open for a gate, level for
      // water (0 is a settled source; 1-7 is flow still spreading, and can recede a tick after this is read), the
      // half for a slab (top, else bottom: the half a slab is unless placed the other way up)
      const properties = {
        age: Number(tag) || 0,
        open: tag === 'open',
        level: Number(tag) || 0,
        ...(String(raw).includes('~') ? { waterlogged: 'true' } : {}),
        ...(/_slab$/.test(name) ? { type: tag === 'top' ? 'top' : 'bottom' } : {})
      }
      return { name, properties, solid: name !== 'air' && name !== 'water' }
    },
    plan: () => place,
    places: () => places,
    drops: () => drops,
    freeSlots: () => freeSlots,
    solid: name => Boolean(name) && name !== 'air' && name !== 'water',
    pen: () => null,
    pause: async () => {},
    note: line => calls.push(`note ${line}`),
    report: partial => Object.assign(report, partial),
    // an event of the composite's own (routine_day, routine_stopped), and what it tells the stuck watch about itself
    emit: (type, data = {}) => events.push({ type, ...data }),
    progress: data => Object.assign(progress, data)
  }
  return { api, calls, report, checkpoints, events, progress }
}
