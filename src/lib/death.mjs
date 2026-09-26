// Death and respawn: naming what killed the body, its kit, and the walk back to unclaimed drops.

// what a hurt event cannot show by naming who is nearby. fell: blocks dropped just before; sinceCreeperMs: since a creeper was last seen close
export function hurtCause ({ lost, nearby, sinceCreeperMs, fell, food, oxygen, fledFrom, sinceFledMs }) {
  if (nearby.length) return null
  if (oxygen <= 0) return 'drowning: get to air'
  if (fell >= 4) return `a fall of ${fell} blocks`
  if (lost >= 5 && sinceCreeperMs < 5000) return 'a creeper blew up (it is gone now)'
  if (food <= 0) return 'starving: eat'
  return fledFrom && sinceFledMs < 30000 ? `hit while the body fled from a ${fledFrom} by itself: that run is why you have moved` : null
}

// Item 13 (#109). A death has to leave a line that says where the body fell and what did it: Claude's body died
// unattended on 09-22 and all the file holds is the jump to the world spawn, so nobody could go and fetch the iron kit.
//
// The server says exactly what happened, in a system message addressed to nobody: "Claude was slain by Zombie". That
// beats every guess, so it is read first. A line only counts as mine when it OPENS with my name: a player's chat is
// wrapped ("<Chani> ...") and anything else that merely contains the name is somebody talking about me, not the server
// announcing my death. "Claude joined the game" opens with it too, so the tail has to look like a death.
const DEATH_TAILS = /^(was |fell |drowned|burned |went up in flames|tried to swim in lava|starved |suffocated |froze |blew up|hit the ground|experienced kinetic energy|discovered the floor|withered away|died)/
export function deathBy (text, username) {
  const head = `${username} `
  if (typeof text !== 'string' || !text.startsWith(head)) return null
  const tail = text.slice(head.length).trim()
  if (!DEATH_TAILS.test(tail)) return null
  return tail.startsWith('was ') ? tail.slice(4) : tail
}

// A wound from a minute ago is not evidence of anything: a body that stood unhurt and then died did not drown a minute
// ago. Within the window, whatever the hurt reflex worked out is used, and failing that the mobs that were on me.
const mobList = names => names.length === 1
  ? `a ${names[0]} was on me`
  : `${names.slice(0, -1).map(n => `a ${n}`).join(', ')} and a ${names[names.length - 1]} were on me`
export function deathReport ({ pos, said, wound, now, window = 10000 }) {
  const fresh = wound && now - wound.at <= window ? wound : null
  const cause = said ?? fresh?.cause ?? (fresh?.nearby?.length ? mobList(fresh.nearby) : null)
  return {
    // where it STOOD: the respawn point is the world spawn and tells nobody anything
    ...(pos ? { pos } : {}),
    ...(cause ? { cause } : {}),
    ...(pos ? {} : { where: 'unknown: I was already gone when the death arrived' })
  }
}

// What fell with me. The drops lie where the body died for five minutes, so a line that names the kit is the difference
// between a run back and a re-smelt ("the iron kit was lost", #109). Tools, weapons and armour are what hurts to lose,
// so they are named; the rubble behind them is counted. Six names is as long a line as anyone reads.
const KIT = /_(pickaxe|axe|shovel|hoe|sword|helmet|chestplate|leggings|boots)$|^(bow|crossbow|shield|trident|elytra|flint_and_steel|bucket|water_bucket|lava_bucket)$/
export function deathKit (items) {
  const held = Object.entries(items).filter(([, n]) => n > 0)
  const kit = held.filter(([name]) => KIT.test(name)).map(([name, n]) => n > 1 ? `${name}:${n}` : name)
  const rubble = held.filter(([name]) => !KIT.test(name)).reduce((n, [, count]) => n + count, 0)
  const named = kit.slice(0, 6).join(', ')
  const more = kit.length > 6 ? `${named} and ${kit.length - 6} more` : named
  if (!kit.length) return rubble ? `${rubble} blocks` : null
  return rubble ? `${more} and ${rubble} other blocks` : more
}

// mineflayer's `death` event is the usual source, but the respawn always arrives. A respawn that no death preceded is a
// death that went unwritten, which is what the 09-22 file looks like: write it from what is known rather than nothing.
export const deathUnannounced = ({ diedAt, now, window = 5000 }) => !diedAt || now - diedAt > window

export const droppedWalk = ({ hasGoal, moving, digging, seconds, pathAgeMs }) =>
  hasGoal && !moving && !digging && seconds >= 4 && pathAgeMs !== null && pathAgeMs > 3000
