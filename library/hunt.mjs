// Hunt: find one kind of creature, kill it, pick the drops up, find the next, and come home. The body already fights by
// reflex; what this adds is the going and the stopping - it walks to each target instead of waiting to be attacked, and
// it stops rather than emptying a valley (huntPick leaves a breeding pair of anything that breeds).
import { huntPick } from '../src/lib.mjs'

const RANGE = 48
const LEASH = 24
const ROUNDS = 40

// the body knows farm animals by id and age (`animals`); anything else is only a name and a distance (`look_around`)
async function inSight (api, mob, range) {
  const { found = [] } = await api.act('animals', { mob, within: range })
  if (found.length) return found
  const { each } = await api.act('look_around', { mob, range })
  const here = api.pos()
  return String(each ?? '').split(' ').filter(c => /^-?\d+,-?\d+,-?\d+$/.test(c))
    .map(c => {
      const [x, y, z] = c.split(',').map(Number)
      return { mob, x, y, z, grown: true, dist: Math.round(Math.hypot(x - here.x, y - here.y, z - here.z)) }
    })
    .sort((a, b) => a.dist - b.dist)
}

export default {
  doc: 'hunt mob= [count=1] [range=48] [leash=24] [home=<place>]: walk to one kind of creature, kill it, pick up the drops, and go on to the next',
  stops: 'the count is reached, nothing of that kind is in sight twice over, or only a breeding pair is left',
  args: { mob: 'string!', count: 'number', range: 'number', leash: 'number', home: 'string' },

  async run (api, a) {
    const mob = a.mob
    const want = a.count ?? 1
    const range = a.range ?? RANGE
    const killed = []
    let misses = 0
    let stopped = null

    for (let round = 0; round < ROUNDS && killed.length < want && !stopped; round++) {
      const found = await inSight(api, mob, range)
      const { target, stop } = huntPick(mob, found)
      // "none in sight" is worth one more look (they wander, and a chunk loads late); a second empty look is the answer
      if (stop && /^no /.test(stop) && misses === 0) { misses++; await api.pause(2); continue }
      if (stop) { stopped = stop; break }
      misses = 0
      const at = target.at ? Object.fromEntries(['x', 'y', 'z'].map((k, i) => [k, Number(target.at.split(',')[i])])) : { x: target.x, y: target.y, z: target.z }
      await api.act('goto', { ...at, range: 3 }).catch(() => ({}))
      await api.checkpoint()
      // by id where there is one: the nearest of a kind is not the one that was picked, and a calf must not be
      const verdict = await api.act('attack', { mob, ...(target.id === undefined ? {} : { id: target.id }), leash: a.leash ?? LEASH })
        .catch(e => ({ killed: false, gaveUp: e.message }))
      if (verdict.killed) killed.push(`${at.x},${at.y},${at.z}`)
      else if (verdict.gaveUp) api.note(`${mob} at ${at.x},${at.y},${at.z}: ${verdict.gaveUp}`)
      // the drops fall where the fight ended, not where it started
      await api.act('collect', { range: 12 }).catch(() => ({}))
      api.report({ killed: killed.length })
      await api.checkpoint()
    }

    if (a.home) await api.act('goto', { place: a.home }).catch(e => api.note(`could not get home to ${a.home}: ${e.message}`))
    return {
      mob,
      killed: killed.length,
      ...(killed.length ? { at: killed.join(' ') } : {}),
      ...(stopped ? { stopped } : killed.length >= want ? {} : { stopped: `gave up after ${ROUNDS} rounds` })
    }
  }
}
