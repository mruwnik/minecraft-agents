// The fake's leads: animals on the body's lead follow it when it walks, unless the entity spec says `snaps: true`,
// which breaks the lead on the first walk (it drops as an item). A tie to a fence post is `knot` on the entity.
// A led animal is dragged in a straight line and does not path: anything at its feet but open air (a fence, a gate
// open or shut: a cow jams in a 1-wide gateway, measured live) stops it on its side.
import { walkLine, openAir } from './fake-walkline.mjs'

const FOLLOW_GAP = 2

export function dragLeashed (s) {
  for (const e of s.entities.filter(x => x.leashedToMe)) {
    if (e.snaps) {
      Object.assign(e, { leashed: false, leashedToMe: false })
      s.entities.push({ id: s.nextEntityId++, name: 'item', kind: 'item', pos: { ...e.pos }, item: { name: 'lead', count: 1 } })
      continue
    }
    e.pos = walkLine([e.pos, { ...s.self.pos, x: s.self.pos.x - FOLLOW_GAP }], openAir(s)).pos
  }
}

const cell = ({ x, y, z }) => `${x},${y},${z}`

// A click on a fence post (empty hand or a lead; the server ties either way) ties every animal the body leads to a
// leash_knot entity there. Returns how many animals changed.
export function tieToPost (s, pos) {
  const led = s.entities.filter(e => e.leashedToMe)
  if (led.length === 0) return 0
  const knot = s.entities.find(e => e.name === 'leash_knot' && cell(e.pos) === cell(pos))
    ?? (s.entities.push({ id: s.nextEntityId++, name: 'leash_knot', kind: 'other', pos: { ...pos } }), s.entities.at(-1))
  led.forEach(e => Object.assign(e, { leashedToMe: false, leashHolder: knot.id }))
  return led.length
}

// An empty hand on the knot removes it and hands what was tied to it back to the body's own lead (26.1 does the
// same; a second click on the animal then drops the lead). Returns how many animals changed.
export function untieKnot (s, knot) {
  const tied = s.entities.filter(e => e.leashHolder === knot.id)
  tied.forEach(e => Object.assign(e, { leashedToMe: true, leashHolder: null }))
  s.entities.splice(s.entities.indexOf(knot), 1)
  return tied.length
}
