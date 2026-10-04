// Watches: "tell me when ...".
import fs from 'node:fs'
import path from 'node:path'
import { compact, matchesProps, checkWatch } from '../lib.mjs'
import { HOME } from './home.mjs'
import { emit } from './events.mjs'
import { matcher, inventoryCounts, findBlocksNear } from './helpers.mjs'
import { Vec3, bot, mcData, ready, roundVec } from './state.mjs'

// A watch is checked every 5 s and writes one `watch_hit` event when its condition becomes true, so waiting costs
// the driver nothing. Kinds: block (with optional `where` properties), mob (any entity or player name), item (in
// my inventory). Centre is a fixed x,y,z or, without one, wherever I am.
const WATCH_FILE = path.join(HOME, 'watches.json')
let watches = fs.existsSync(WATCH_FILE) ? JSON.parse(fs.readFileSync(WATCH_FILE, 'utf8')) : []
const saveWatches = () => fs.writeFileSync(WATCH_FILE, JSON.stringify(watches, null, 1) + '\n')
const watchTarget = w => w.block ? `block ${w.block}` : w.mob ? `mob ${w.mob}` : `item ${w.item}`
const describeWatch = w => `${w.name}: ${w.atMost ? 'at most' : 'at least'} ${w.count ?? 1} ${watchTarget(w)}${w.where ? ' ' + compact(w.where) : ''}${w.item ? '' : ` within ${w.within ?? 16}${w.x === undefined ? ' of me' : ` of ${w.x},${w.y},${w.z}`}`}${w.repeat ? ' (repeats)' : ''}`

function countForWatch (w) {
  if (w.item) return { seen: inventoryCounts()[w.item] ?? 0 }
  const centre = w.x === undefined ? bot.entity.position : new Vec3(w.x, w.y, w.z)
  const within = w.within ?? 16
  if (w.mob) {
    const m = matcher(w.mob)
    const hits = Object.values(bot.entities).filter(e => e !== bot.entity && e.position && m(e.username ?? e.name ?? '') && e.position.distanceTo(centre) <= within)
    return { seen: hits.length, at: hits[0] && roundVec(hits[0].position) }
  }
  const m = matcher(w.block)
  const ids = Object.values(mcData.blocksByName).filter(b => m(b.name)).map(b => b.id)
  const hits = findBlocksNear({ matching: ids, maxDistance: within, count: 512, point: centre }).filter(p => matchesProps(bot.blockAt(p)?.getProperties(), w.where))
  return { seen: hits.length, at: hits[0] && roundVec(hits[0]) }
}

setInterval(() => {
  if (!ready || !watches.length) return
  const before = JSON.stringify(watches)
  watches = watches.flatMap(w => {
    const { seen, at } = countForWatch(w)
    const { fire, met } = checkWatch(w, seen)
    if (fire) emit('watch_hit', { name: w.name, seen, what: watchTarget(w), at })
    return fire && !w.repeat ? [] : [{ ...w, met }]
  })
  if (JSON.stringify(watches) !== before) saveWatches()
}, 5000)

export const watchesQuick = {
  watch: (a) => {
    if (!a.name || [a.block, a.mob, a.item].filter(Boolean).length !== 1) throw new Error('watch needs name= and exactly one of block=, mob=, item=')
    if (a.block && !Object.keys(mcData.blocksByName).some(matcher(a.block))) throw new Error(`unknown block name: ${a.block}`)
    const { name, block, mob, item, where, count, atMost, within, x, y, z, repeat } = a
    watches = [...watches.filter(w => w.name !== name), { name, block, mob, item, where, count, atMost, within, x, y, z, repeat }]
    saveWatches()
    return { watching: describeWatch(a), now: countForWatch(a).seen }
  },
  unwatch: (a) => { watches = watches.filter(w => w.name !== a.name); saveWatches(); return { watches: watches.length } },
  watches: () => ({ text: watches.map(describeWatch).join('\n') || 'no watches' })
}
