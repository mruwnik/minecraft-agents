// Eating: what to eat and when, backing off after a failed meal, and the refusals around a body's hunger.

import { BREEDING_FOOD } from './animals.mjs'

// Shared default blacklist for ordinary inventory and auto-eat decisions. Explicit
// desperate/anyway paths remain decisions of eatAllowed, not exceptions here.
export const BANNED_FOOD = Object.freeze(['rotten_flesh', 'spider_eye', 'poisonous_potato', 'pufferfish', 'chicken'])
// A meal on the way is not a loss: whatever the body ate comes off lost= and is said as ate= (my goto said "lost bread:1")
export function mealTally ({ gained, lost, ate }) {
  if (!Object.keys(ate).length) return { gained, lost }
  const left = Object.fromEntries(Object.entries(lost).map(([k, n]) => [k, n - (ate[k] ?? 0)]).filter(([, n]) => n > 0))
  return { gained, lost: left, ate }
}

// auto-eat starts below this food level: health only comes back at food 18 and up, so a hurt body eats sooner than a whole one
export const eatBelow = health => health < 20 ? 18 : 15

// food in my hand draws every animal that sees it after me, and out through any gate I open: put it away at a gate unless that is the plan
// luring: a lead is on, from its first step TOWARDS the animal (not only once it follows: started beside a gate, lead lost its wheat and gave up with=0)
// eating: a meal is running. The food in my hand is going into my mouth: taking it out cancelled every meal by a gate (Chani, food 7, 1743 [food away] lines)
export const foodAway = ({ held, luring, feeding, gateNear, eating = false }) => gateNear && !luring && !feeding && !eating && Object.values(BREEDING_FOOD).some(foods => foods.includes(held))

// mineflayer-auto-eat marks itself eating BEFORE it equips the food and only unmarks after the meal: an equip that throws leaves it "eating" for
// ever and the body starves with bread in its pockets (Jizo). A meal takes 1.6 s
export const eatJammed = eatingForMs => eatingForMs >= 15000

// #149(a). `eatBelow(health)` raises minHunger as health drops, which sends a HURT body to eat - and a hurt body is
// usually a body in a fight. Mariel died at food 17 with a sword in her pack: 42 meals in the minute the zombie took
// to kill her, each one putting bread in the hand the sword needed, and the fight and the run both went unarmed.
// Food is for after. Nothing here is about hunger: a body clear of all three eats as it always did.
export const eatHold = ({ hostileNear = false, fighting = false, fleeing = false } = {}) => {
  if (fighting) return 'a fight of my own is running: the sword stays in my hand until the mob is dead'
  if (fleeing) return 'a run is on: nothing goes in my hand until it is clear'
  if (hostileNear) return 'a hostile is within reach: the sword stays in my hand, food is for afterwards'
  return null
}

// #149(c): Mariel's "jammed, reset" arrived on the tick she died, and until then the meal held the sword out of her hand:
// every equip waits for a meal in flight (afterTheMeal). So a meal is dropped the moment eatHold would not have started
// it. Damage alone is not a reason: a starving body is hurt every 4 s, and the meal is its cure
export const mealToDrop = ({ eating = false, hostileNear = false, fighting = false, fleeing = false } = {}) =>
  eating ? eatHold({ hostileNear, fighting, fleeing }) : null
// strictErrors is off, so a meal that fails RESOLVES and reports only through eatFail. A meal I cancelled myself for a
// fight is not a failure and must not grow the backoff
export const mealFailed = error => !/manually cancel/i.test(String(error?.message ?? error ?? ''))

// #149(b): a meal that will not go down is not worth asking for every 3 s. Each consecutive failure waits twice as
// long, to half a minute; one meal that works clears the count (see eatFailedAt), so a fed body is never held back
export const EAT_BACKOFF_FIRST = 5000
export const EAT_BACKOFF_CAP = 30000
export const eatBackoff = failures => Math.min(EAT_BACKOFF_FIRST * 2 ** Math.max(0, Number(failures) - 1), EAT_BACKOFF_CAP)

// SAFETY. The plugin's reflex ends in `catch {}`: across every body's log there are 140 jam lines and not one word of why
// a meal never finished, while Chani's body walked 20 minutes at health 7 / food 4 with five carrots in its pockets. Its
// errors name its own internals and append the whole item object after a newline, so the advice comes first and the real
// first line after it: whoever reads eat_failed has to know what to DO.
const EAT_ADVICE = [
  [/couldn't find a choice/i, 'nothing I carry is food I am willing to eat'],
  [/^Failed to equip/i, 'the food would not go into my hand'],
  [/^Already eating/i, 'a meal was already running'],
  [/timed out/i, 'the server never said the meal finished'],
  [/switched early/i, 'my hand was emptied mid-meal (a reflex that re-equips?)'],
  [/manually canceled/i, 'the meal was called off']
]
// The reflex runs on every physics tick. While every meal was timing out that was 20 failed eats a second, each one a pair
// of window clicks at the server: after a failure it waits. A meal that works needs no cooldown, a fed body stops asking.
export const eatRetryDue = (failedAt, now, wait = 5000) => failedAt === null || now - failedAt >= wait

// A meal is 1.6 s of holding the food still, and anything that swaps my hand or right-clicks a block inside that window
// cancels it where the server counts: on ClaudeProbe an `equip wheat` 0.9 s into a meal left the bread uneaten and the
// food number where it was. The pathfinder does both whenever it is stuck -- a tool in hand before every dig, a gate
// worked by hand on every retry -- which is why Chani stood at health 7 with five carrots in her pockets until she dug
// the gate out, and ate five in a row the moment the stall ended. So the meal goes first: these calls wait for it. A
// meal always settles, it has a timeout of its own, so nothing here waits for ever.
export const afterTheMeal = (mealInFlight, fn) => async (...args) => {
  await (mealInFlight() ?? Promise.resolve()).catch(() => {})
  return fn(...args)
}

// What I carry, sorted the way the eat reflex sorts it. "nothing edible carried" told an agent nothing: a body holding
// rotten flesh and a body holding cobblestone got the same line, and neither knew whether to cook, to hunt, or to walk to
// a chest (backlog #134). isFood: does the game call it food at all. banned: what this body will never eat.
export function foodSort (carried, isFood, banned) {
  const seen = [...new Set(carried)]
  return {
    edible: seen.filter(name => isFood(name) && !banned.includes(name)),
    banned: seen.filter(name => isFood(name) && banned.includes(name)),
    notFood: seen.filter(name => !isFood(name))
  }
}

// The never-eat list is a preference, and a preference that outlives the body holding it is a bug. At food 3 with 8
// rotten flesh in its pockets this body was told it carried nothing edible, over and over, and walked 95 blocks to a
// bread chest to answer it (backlog #139). Hunger damage on Normal stops at half health, so the flesh costs nothing
// the starving was not already costing, and its own hunger cannot take a body below what the empty belly already
// would. So below the floor, with nothing better in the pockets, the banned food IS the food; and `anyway=true` lets
// the agent make that call itself at any hunger, because the agent can see reasons this code cannot.
export const HUNGER_FLOOR = 6
export const eatAllowed = ({ food, carried, anyway = false, floor = HUNGER_FLOOR }) => {
  const desperate = Boolean(anyway) || (food <= floor && !carried.edible.length)
  return { desperate, allowed: desperate ? [...carried.edible, ...carried.banned] : [...carried.edible] }
}

// backlog: a body with no food at all re-said "nothing I carry is food" on every backoff tick for as long as its
// pockets stayed empty, walling the events file the same way the old jam loop did (see errorRepeat). Fires on the
// edge, like checkWatch: true only the tick empty pockets are first seen, so the reflex can say it once and then
// leave the body alone until something edible or banned turns up to carry again.
export const noFoodEdge = (carried, hadNone) => {
  const hasNone = !carried.edible.length && !carried.banned.length
  return { fire: hasNone && !hadNone, hasNone }
}

// ./mc eat: why I will not, said before the plugin is touched at all -- its own refusals name its internals. It names what
// it passed over, so the answer says what to do next: cook the flesh away, go and find real food, or say anyway=true.
const floorNote = floor => `, which I eat only at food ${floor} or less or with anyway=true`
const whyNot = ({ banned, notFood }, item, floor) => banned.includes(item) ? `it is on the never-eat list${floorNote(floor)}` : notFood.includes(item) ? 'it is not food' : 'I do not carry it'
export function eatRefusal ({ food, item, carried, anyway = false, floor = HUNGER_FLOOR }) {
  const { banned, notFood } = carried
  const { allowed } = eatAllowed({ food, carried, anyway, floor })
  const have = allowed.length ? `what I carry is ${allowed.join(', ')}` : 'I carry nothing edible'
  if (item && !allowed.includes(item)) return `no ${item} I would eat: ${whyNot(carried, item, floor)}; ${have}`
  if (!item && !allowed.length) {
    const skipped = [...(banned.length ? [`never eaten: ${banned.join(', ')}${floorNote(floor)}`] : []), ...(notFood.length ? [`not food: ${notFood.join(', ')}`] : [])]
    return skipped.length ? `nothing I carry is food (${skipped.join('; ')})` : 'nothing I carry is food: my pockets are empty'
  }
  return food >= 20 ? 'food is already 20: the game refuses a meal at a full belly' : null
}

// ./mc eat said ok with nothing eaten: a meal counts only if the item left my pockets
export const uneatenMeal = ({ item, before, after }) => !item || after < before ? null
  : `the meal did not happen: I still carry ${after} ${item}, as many as before. Something took the food out of my hand mid-meal (the gate reflex puts tempting food away; see [food away] in bot.log) or the server never finished it. Step 5 blocks clear of any fence gate and eat again`

// edible: what I carry that I would eat. With food in my pockets the plugin's "couldn't find a choice" is about my hand, not my pockets
export function eatFailure (error, edible = []) {
  const first = String(error?.message ?? error ?? '').split('\n')[0].trim()
  if (!first) return 'the eat failed and said nothing'
  if (edible.length && /couldn't find a choice/i.test(first)) return `I carry ${edible.join(', ')}, but the eat reflex found no meal to take (it looked while the food was being moved and in no slot: near a fence gate the gate reflex puts tempting food away): ${first}`
  const advice = EAT_ADVICE.find(([pattern]) => pattern.test(first))
  return advice ? `${advice[1]}: ${first}` : first
}
