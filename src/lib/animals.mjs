// Breeding and hunting judgements: pairing animals, flock surplus, and what food a creature wants.

import { within } from './world.mjs'
// shearing: nearest sheep within reach of a walk that this errand has not shorn yet (wool takes minutes to grow back)
export const nextSheep = (sheep, shorn, within) => sheep.filter(s => !shorn.has(s.id) && s.dist <= within).sort((a, b) => a.dist - b.dist)[0]
export const isBaby = metadata => metadata?.[16] === true

// what puts each farm animal in the mood
export const BREEDING_FOOD = {
  cow: ['wheat'], mooshroom: ['wheat'], sheep: ['wheat'], goat: ['wheat'],
  pig: ['carrot', 'potato', 'beetroot'],
  chicken: ['wheat_seeds', 'melon_seeds', 'pumpkin_seeds', 'beetroot_seeds', 'torchflower_seeds'],
  rabbit: ['carrot', 'golden_carrot', 'dandelion']
}
export const breedingFood = (mob, carried) => (BREEDING_FOOD[mob] ?? []).find(food => carried.includes(food)) ?? null

// Bees belong to an apiary, not a pen: flock.lead/maintain must never accept them, but the low-level feed and animals
// senses still need to know what they eat. Keep this beside, rather than inside, BREEDING_FOOD for that reason.
export const BEE_FLOWERS = [
  'dandelion', 'poppy', 'blue_orchid', 'allium', 'azure_bluet', 'red_tulip', 'orange_tulip', 'white_tulip',
  'pink_tulip', 'oxeye_daisy', 'cornflower', 'lily_of_the_valley', 'sunflower', 'lilac', 'rose_bush', 'peony',
  'torchflower', 'pink_petals', 'wildflowers', 'flowering_azalea', 'flowering_azalea_leaves'
]
export const CREATURE_FOOD = { ...BREEDING_FOOD, bee: BEE_FLOWERS }
export const creatureFood = (mob, carried) => (CREATURE_FOOD[mob] ?? []).find(food => carried.includes(food)) ?? null

// `hunt` picks its next target: the nearest GROWN one. A calf is next year's herd, and a kind that breeds is left alone
// once only a pair of grown ones is in sight - the starter pen's house rule, applied to the wild so that a hunt cannot
// empty a valley and leave nothing to come back to. A monster is not a herd, so nothing is kept back from one.
export const HUNT_KEEP = 2
export const huntPick = (mob, found, { keep = BREEDING_FOOD[mob] ? HUNT_KEEP : 0 } = {}) => {
  if (!found.length) return { stop: `no ${mob} in sight` }
  const grown = found.filter(f => f.grown !== false)
  if (grown.length <= keep) return { stop: `only ${grown.length} grown ${mob} in sight and a breeding pair stays: move on, or breed them up first` }
  return { target: [...grown].sort((a, b) => a.dist - b.dist)[0] }
}

export const apiaryGoods = items => Object.fromEntries(Object.entries(items)
  .filter(([name, count]) => count > 0 && ['honeycomb', 'honey_bottle'].includes(name)))

// The pen census is a line, not a map ("cow:2 sheep:1"): how many of one kind it says are in there.
export const insideCount = (inside, mob) =>
  Number(String(inside ?? '').split(' ').find(part => part.split(':')[0] === mob)?.split(':')[1] ?? 0)

// How many animals a pen still needs for a breeding pair, counting the ones already in it, and what to say when the
// country around cannot supply them. Babies do not breed, so `grown` is the count of grown ones NOT already inside.
export function pairPlan ({ mob, inside = 0, grown = 0, want = 2 }) {
  const fetch = want - inside
  if (fetch <= 0) return { fetch: 0, note: `the pen already holds ${inside} grown ${mob}` }
  if (grown >= fetch) return { fetch }
  const has = inside ? `${inside} grown ${mob}` : `no grown ${mob}`
  const near = grown ? `there is ${grown} within reach to fetch` : 'there is nothing within reach to fetch'
  return { fetch: 0, refuse: `breeding takes two: the pen holds ${has} and ${near}. Look further afield (within=), or bring one in by hand` }
}

// One round of keeping a flock: breed it up to the size asked for, cull what is over, and never cull the breeding pair
// itself. Babies count towards the size (they grow) but are never the ones culled.
export function flockPlan ({ mob, grown = 0, young = 0, size = 4, cull = true }) {
  const total = grown + young
  const why = `${total} ${mob} of ${size}`
  if (total < size && grown >= 2) return { do: 'breed', why }
  if (total < size) return { do: 'wait', why: `${why}, and breeding takes two grown ones` }
  if (total > size && cull && grown > 2) return { do: 'cull', count: Math.min(total - size, grown - 2), why }
  return { do: 'nothing', why }
}

// what a flock produces and a pen chest should hold. Meat is food first: enough to live on stays in my pockets
const FLOCK_GOODS = new Set(['mutton', 'beef', 'porkchop', 'chicken', 'rabbit', 'cooked_mutton', 'cooked_beef', 'cooked_porkchop',
  'cooked_chicken', 'cooked_rabbit', 'rabbit_hide', 'rabbit_foot', 'leather', 'feather', 'egg', 'string', 'bone'])
const MEAT = new Set(['mutton', 'beef', 'porkchop', 'chicken', 'rabbit', 'cooked_mutton', 'cooked_beef', 'cooked_porkchop', 'cooked_chicken', 'cooked_rabbit'])
export const flockSurplus = (items, keepFood = 8) => Object.fromEntries(Object.entries(items)
  .filter(([item]) => FLOCK_GOODS.has(item) || item.endsWith('_wool'))
  .map(([item, n]) => [item, MEAT.has(item) ? n - keepFood : n])
  .filter(([, n]) => n > 0))
