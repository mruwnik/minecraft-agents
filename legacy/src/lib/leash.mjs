// Leads (card 43a32481). A lead on an animal pulls it after the body: nothing has to see food, a gate only has to
// open, and the walk holds when one falls behind, because a lead breaks at LEAD_BREAK blocks. The plan here says
// which animals get a lead, when the walk goes or waits, and what to say about leads borrowed from a chest.

export const LEAD_BREAK = 10
// the walk stands still when an animal on a lead is further off than this, and goes on once it has been pulled in
export const LEAD_SLACK = 6

const LEASHABLE = new Set(['cow', 'sheep', 'pig', 'chicken', 'goat', 'horse', 'donkey', 'mule', 'llama', 'trader_llama', 'mooshroom', 'wolf', 'cat', 'fox', 'ocelot', 'parrot', 'rabbit', 'camel', 'sniffer', 'bee', 'axolotl', 'frog', 'polar_bear', 'panda', 'hoglin', 'strider', 'zoglin', 'dolphin', 'squid', 'glow_squid', 'iron_golem', 'snow_golem', 'allay'])
export const leashable = mob => LEASHABLE.has(mob)

export const NO_LEAD = 'no lead carried: take one from a chest (withdraw item=lead count=2) or craft one (4 string + 1 slimeball), or lead with food instead'

// which animals get a lead: candidates [{ id, dist, grown, penned }], the grown and the near first, never one in a
// pen unless allowPenned, at most one per lead carried. note= says when fewer leads than asked for decide the count
export function leashPlan ({ mob, leads, count = 1, candidates, allowPenned = false }) {
  if (!leashable(mob)) return { error: `a ${mob} takes no lead` }
  if (leads <= 0) return { error: NO_LEAD }
  const free = candidates.filter(c => allowPenned || !c.penned)
  if (!free.length) return { error: candidates.length ? `every ${mob} in reach stands in a pen: penned=true to take one from a pen of your own` : `no ${mob} within reach` }
  const ordered = [...free].sort((a, b) => Number(b.grown) - Number(a.grown) || a.dist - b.dist)
  const take = ordered.slice(0, Math.min(count, leads)).map(c => c.id)
  return { take, ...(leads < count ? { note: `only ${leads} lead${leads === 1 ? '' : 's'} carried: leashing ${take.length}, not ${count}` } : {}) }
}

// the walk with animals on leads: distances of the ones still on a lead; held: how many were leashed at the start
export function leashVerdict ({ distances, held, noPath = false }) {
  if (distances.length < held) return 'broke'
  if (!distances.length) return 'lost'
  if (Math.max(...distances) > LEAD_SLACK) return 'hold'
  return noPath ? 'noway' : 'go'
}

// what to say when a lead came off on the way: where the animal is and that its lead lies there
export const leadBroke = (mob, loose) => `a lead came off on the way: the ${mob} at ${loose.map(l => `${l.x},${l.y},${l.z}`).join(' ')} walks free and its lead lies about there (collect it). Leash it again from closer, and walk in shorter legs`

// leads borrowed from a chest for a walk: how many to take out, given what is carried and what the walk wants
export const leadBorrow = ({ carried, want }) => Math.max(0, want - carried)

// the line said after a walk that borrowed leads: they go back into the same chest, and a lost one is named
export function leadReturn ({ borrowed, carried, chest }) {
  if (!borrowed) return null
  const back = Math.min(borrowed, carried)
  const where = `the chest at ${chest.x},${chest.y},${chest.z}`
  if (back === borrowed) return { deposit: back, line: `${back} lead${back === 1 ? '' : 's'} back in ${where}` }
  return { deposit: back, line: `${back} of ${borrowed} leads back in ${where}: ${borrowed - back} lost on the way (a lead that comes off drops where the animal was; collect it and deposit it there)` }
}

// leads=x,y,z on flock.lead and flock.bring_pair: the chest to borrow leads from and return them to
export function leadsChest (value) {
  if (value === undefined || value === null) return null
  const parts = String(value).split(',').map(Number)
  if (parts.length !== 3 || parts.some(n => !Number.isInteger(n))) return { error: `leads= wants the chest cell as x,y,z (got ${value})` }
  const [x, y, z] = parts
  return { x, y, z }
}

// leashed=: which animals are on my leads now, as mob#id@x,y,z
export const leashedLine = held => held.map(e => `${e.name}#${e.id}@${Math.floor(e.x)},${Math.floor(e.y)},${Math.floor(e.z)}`).join(' ')
