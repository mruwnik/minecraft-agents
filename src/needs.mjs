// What a primitive cannot run without, checked before it runs. A primitive reads its arguments straight off the request
// (composites declare theirs, src/lib/composite.mjs checkArgs), so a missing one used to reach the action as undefined
// and come back inside its answer: find_blocks name=oak_log answered "unknown block name: undefined". The reply now
// names the argument, and takes the other spelling a driver reaches for: {key: [aliases]}, an alias is folded into
// the key when the key itself is not given.
export const NEEDS = {
  find_blocks: { block: ['name'] },
  craft: { item: [] },
  enchant: { item: [] },
  equip: { item: [] },
  toss: { item: [] },
  give: { player: [], item: [] },
  feed: { mob: [] },
  escort: { mob: [] },
  follow: { player: [] },
  whisper: { player: [] },
  trade: { offer: [] },
  villager_food: { uuid: [], item: [] },
  unmark: { name: [] },
  boat_mount: { id: [] },
  boat_leash: { id: [] },
  boat_unleash: { id: [] },
  boat_release: { id: [] },
  boat_recover: { id: [] },
  fill: { x: [], y: [], z: [] },
  pour: { x: [], y: [], z: [] },
  use: { x: [], y: [], z: [] },
  toggle: { x: [], y: [], z: [] },
  unleash: { x: [], y: [], z: [] },
  look_at: { x: [], y: [], z: [] }
}

const missing = value => value === undefined || value === null
const POINT = ['x', 'y', 'z']

// the given arguments with every alias folded into its key, and the line naming what is still missing (or null)
export function neededArgs (action, given, needs = NEEDS) {
  // Unleash without a point detaches animals from this player; a point
  // instead names a fence knot and still needs all three coordinates.
  if (action === 'unleash' && !POINT.some(key => Object.hasOwn(given, key))) return { args: given, error: null }
  const wanted = needs[action]
  if (!wanted) return { args: given, error: null }
  const args = Object.entries(wanted).reduce((acc, [key, aliases]) => {
    const alias = aliases.find(name => !missing(acc[name]))
    if (alias === undefined) return acc
    const { [alias]: value, ...rest } = acc
    return missing(acc[key]) ? { ...rest, [key]: value } : rest
  }, { ...given })
  const lacking = Object.keys(wanted).filter(key => missing(args[key]))
  if (!lacking.length) return { args, error: null }
  // a point is one argument to whoever types it: x= alone still leaves the whole of x= y= z= to give
  const named = Object.keys(wanted).filter(key => lacking.includes(key) || (POINT.includes(key) && lacking.some(k => POINT.includes(k))))
  const aliased = named.filter(key => wanted[key].length).map(key => `${wanted[key].map(a => `${a}=`).join(' ')} is taken as ${key}= too`)
  return { args, error: `${action} needs ${named.map(key => `${key}=`).join(' ')}${aliased.length ? ` (${aliased.join('; ')})` : ''}` }
}
