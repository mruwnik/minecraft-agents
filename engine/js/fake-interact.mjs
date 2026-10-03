// The fake's `interact` primitive: use an item (or an empty hand) on an entity.
import { refusal } from './interact.mjs'

const dist = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)

const REACH = 3.5

const flowers = ['dandelion', 'poppy', 'blue_orchid', 'allium', 'azure_bluet', 'red_tulip', 'orange_tulip', 'white_tulip', 'pink_tulip', 'oxeye_daisy', 'cornflower', 'lily_of_the_valley', 'sunflower', 'lilac', 'rose_bush', 'peony', 'torchflower', 'pink_petals', 'wildflowers']

export const BREEDING_FOOD = {
  cow: ['wheat'],
  mooshroom: ['wheat'],
  sheep: ['wheat'],
  goat: ['wheat'],
  pig: ['carrot', 'potato', 'beetroot'],
  chicken: ['wheat_seeds', 'melon_seeds', 'pumpkin_seeds', 'beetroot_seeds', 'torchflower_seeds'],
  rabbit: ['carrot', 'golden_carrot', 'dandelion'],
  bee: flowers
}

const none = { consumed: 0, worn: 0, love: false, leash: null, changed: {} }

const finish = (r) => {
  const used = r.consumed > 0 || r.worn > 0 || r.love || r.leash !== null || Object.keys(r.changed).length > 0
  return { status: used ? 'used' : 'no-effect', ...r }
}

export function fakeInteract (s) {
  const carried = (name) => s.inventory.some(i => i.name === name)

  const takeOne = (name) => {
    const have = s.inventory.find(i => i.name === name)
    have.count -= 1
    if (have.count === 0) s.inventory.splice(s.inventory.indexOf(have), 1)
    return 1
  }

  const spawnItem = (pos, name) => {
    s.entities.push({ id: s.nextEntityId++, name: 'item', kind: 'item', pos: { ...pos }, item: { name, count: 1 } })
  }

  const feed = (e, item) => {
    if (e.baby) return { ...none, consumed: takeOne(item) }
    if (e.inLove || e.cooldown) return none
    const consumed = takeOne(item)
    e.inLove = true
    return { ...none, consumed, love: true }
  }

  const shear = (e) => {
    if (e.name !== 'sheep' || e.sheared || e.baby) return none
    e.sheared = true
    spawnItem(e.pos, 'white_wool')
    return { ...none, worn: 1, changed: { sheared: [false, true] } }
  }

  const leash = (e) => {
    if (e.leashed) return none
    const consumed = takeOne('lead')
    e.leashed = true
    return { ...none, consumed, leash: 'attached' }
  }

  const unleash = (e) => {
    e.leashed = false
    spawnItem(e.pos, 'lead')
    return { ...none, leash: 'detached' }
  }

  const effect = (e, item) => {
    if (e.accepts === false) return none
    if (item && (BREEDING_FOOD[e.name] ?? []).includes(item)) return feed(e, item)
    if (item === 'shears') return shear(e)
    if (item === 'lead') return leash(e)
    if (!item && e.leashed) return unleash(e)
    return none
  }

  return async (token, { id, item = null }) => {
    const e = s.entities.find(x => x.id === id)
    if (!e) return { status: 'gone', ...none }
    const refused = refusal(e.name)
    if (refused) return { status: 'cannot', reason: refused, ...none }
    if (item && !carried(item)) return { status: 'no-item', ...none }
    if (dist(s.self.pos, e.pos) > REACH) return { status: 'out-of-reach', ...none }
    if (!item && s.self.held && s.inventory.length >= 36) return { status: 'full', ...none }
    s.self.held = item
    if (e.mounts) return { status: 'failed', reason: 'mounted', ...none }
    if (e.opens) return { status: 'failed', reason: 'opened-window', ...none }
    return finish(effect(e, item))
  }
}
