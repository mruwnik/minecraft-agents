// Pure helpers, kept apart from bot.mjs so they can be tested without a server.

const LONG_FIELDS = ['task', 'action', 'seconds', 'gained', 'lost', 'pos', 'ok', 'error']
const isPos = v => Object.keys(v).length === 3 && ['x', 'y', 'z'].every(k => typeof v[k] === 'number')
const isEmpty = v => v == null || v === false || v === '' || (typeof v === 'object' && Object.keys(v).length === 0)
const bracket = v => typeof v === 'object' && !Array.isArray(v) && !isPos(v) ? `(${compact(v)})` : compact(v)

// JSON without the punctuation tax: positions are x,y,z, counts are name:count, true is a bare key and
// null/false/empty simply vanish. Meant to be read by an LLM that pays per token, not parsed.
export function compact (v, counts = true) {
  if (isEmpty(v)) return ''
  if (typeof v === 'number') return String(Math.round(v * 10) / 10)
  if (typeof v !== 'object') return String(v)
  if (Array.isArray(v)) return v.filter(i => !isEmpty(i)).map(bracket).join(' ')
  if (isPos(v)) return ['x', 'y', 'z'].map(k => Math.floor(v[k])).join(',')
  const entries = Object.entries(v).filter(([, val]) => !isEmpty(val))
  if (counts && entries.every(([, val]) => typeof val === 'number')) return entries.map(([k, n]) => n === 1 ? k : `${k}:${n}`).join(' ')
  return entries.map(([k, val]) => val === true ? k : typeof val === 'object' && !Array.isArray(val) && !isPos(val) ? `${k}(${compact(val)})` : `${k}=${compact(val)}`).join(' ')
}

const signed = (sign, obj = {}) => Object.entries(obj).map(([k, n]) => `${sign}${k}:${n}`)

// One short line per API result: every token of output costs the driver context.
export function terse (r) {
  if (typeof r.map === 'string') return r.map
  if (typeof r.text === 'string') return r.text
  const head = r.ok ? 'ok' : 'FAIL'
  const error = r.error ? [`error: ${r.error}`] : []
  if (r.status === 'running') return `ok running task=${r.task} (still going: block on ./mc wait for its task_done, do not end your turn)`
  const extras = compact(Object.fromEntries(Object.entries(r).filter(([k]) => !LONG_FIELDS.includes(k))), false)
  if (!r.action) return [head, ...(extras ? [extras] : []), ...(r.pos ? [`pos=${compact(r.pos)}`] : []), ...error].join(' ')
  const at = r.pos ? [`@${compact(r.pos)}`] : []
  return [head, r.action, `${r.seconds}s`, ...signed('+', r.gained), ...signed('-', r.lost), ...at, ...(extras ? [extras] : []), ...error].join(' ')
}

export const capOutput = (text, limit = 1500) => text.length <= limit
  ? text
  : `${text.slice(0, limit)}\n[+${text.length - limit} chars cut: narrow the query, or delegate reading the full output (-v) to a subagent]`

const FALLBACK_SYMBOLS = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ#%&*+='
const range = (a, b) => Array.from({ length: Math.abs(b - a) + 1 }, (_, i) => Math.min(a, b) + i)

// ASCII slices of a box of the world, top layer first; air is '.', everything else gets a letter and a legend.
export function renderScan (nameAt, { x1, y1, z1, x2, y2, z2 }) {
  const symbols = new Map()
  const symbolFor = name => {
    if (name === 'air') return '.'
    if (symbols.has(name)) return symbols.get(name)
    const taken = new Set(symbols.values())
    const symbol = [...name.replaceAll('_', ''), ...FALLBACK_SYMBOLS].find(c => !taken.has(c)) ?? '?'
    symbols.set(name, symbol)
    return symbol
  }
  const xs = range(x1, x2)
  const zs = range(z1, z2)
  // labels padded to one width, or rows like -9 and -10 shift against each other and columns get misread
  const labelWidth = Math.max(...zs.map(z => String(z).length))
  const layers = range(y1, y2).reverse().flatMap(y => {
    const rows = zs.map(z => `${String(z).padStart(labelWidth)} ${xs.map(x => symbolFor(nameAt(x, y, z))).join('')}`)
    return rows.every(r => /^ *-?\d+ \.+$/.test(r)) ? [`y=${y} all air`] : [`y=${y}`, ...rows]
  })
  const ruler = `${' '.repeat(labelWidth)} ${xs.map(x => Math.abs(x) % 10).join('')}`
  const legend = [...symbols].map(([name, s]) => `${s}=${name}`).join(' ')
  return [`x ${xs[0]}..${xs.at(-1)} across (ruler: last digit of x), z down`, ruler, ...layers, legend].join('\n')
}

const between = (v, a, b) => v >= Math.min(a, b) && v <= Math.max(a, b)

// Is pos inside any of the protected boxes ({x1,y1,z1,x2,y2,z2}, corners in any order, inclusive)?
export const inAnyZone = (zones, pos) =>
  zones.some(z => between(pos.x, z.x1, z.x2) && between(pos.y, z.y1, z.y2) && between(pos.z, z.z1, z.z2))

// A Minecraft username is 3-16 of [A-Za-z0-9_]; squeeze a character's name into that, or null if it can't be done.
export function minecraftName (raw) {
  const squeezed = raw.normalize('NFD').replace(/[̀-ͯ]/g, '').replace(/[\s'’.\-]/g, '')
  return /^[A-Za-z0-9_]{3,16}$/.test(squeezed) ? squeezed : null
}

// Output of ~/.claude/hooks/choose_name.py: "✨ Name", "   Source: X", optionally "   a note".
export function parseChosenName (output) {
  const [first = '', ...rest] = output.split('\n').map(l => l.trim()).filter(Boolean)
  const source = rest.find(l => l.startsWith('Source:'))?.slice('Source:'.length).trim() ?? ''
  return { name: first.replace(/^\W+/u, '').trim(), source, note: rest.find(l => !l.startsWith('Source:')) ?? '' }
}

const FIRST_API_PORT = 3777
export const nextPort = used => {
  const taken = new Set(used)
  for (let port = FIRST_API_PORT; ; port++) if (!taken.has(port)) return port
}

// new-agent.mjs's command line: an optional name and `--harness <name>` (one of the notes files in harness/, default claude-code).
// Returns { name, harness } (name null when it should be drawn) or { error }.
const DEFAULT_HARNESS = 'claude-code'
export function newAgentArgs (argv, harnessesAvailable) {
  const known = harnessesAvailable.join(', ')
  let name = null
  let harness = DEFAULT_HARNESS
  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i]
    if (arg === '--harness' || arg.startsWith('--harness=')) {
      const value = arg === '--harness' ? argv[++i] : arg.slice('--harness='.length)
      if (!value) return { error: `--harness needs a name: one of ${known}` }
      harness = value
      continue
    }
    if (arg.startsWith('-')) return { error: `unknown option ${arg} (only --harness <name> is understood)` }
    if (name) return { error: `one name only: got "${name}" and "${arg}"` }
    name = arg
  }
  if (!harnessesAvailable.includes(harness)) return { error: `unknown harness "${harness}": the ones with notes in harness/ are ${known}` }
  return { name, harness }
}

const awayFrom = from => p => Math.round(Math.hypot(p.x - from.x, p.y - from.y, p.z - from.z))
const holds = (text, want) => String(text ?? '').toLowerCase().includes(String(want).toLowerCase())

// The search behind `places`: every marked point that matches, nearest first. places.json is shared by every body and
// passed 60 entries in a fortnight, so nobody should ever read it whole - q= (name or note), by=, kind= and within= are
// how you find one. Separate from describePlaces so the caller can say how many it did not show.
export const matchPlaces = (places, from, { q, by, kind, within = Infinity, maxDist = within } = {}) => {
  const dist = awayFrom(from)
  return places
    .filter(p => (!kind || p.kind === kind) && (!by || holds(p.by, by)) && (!q || holds(p.name, q) || holds(p.note, q)) && dist(p) <= maxDist)
    .sort((a, b) => dist(a) - dist(b))
}

// Shared points of interest, nearest first: "name kind 12m @x,y,z (who: note)".
export function describePlaces (places, from, options = {}) {
  const { limit = 12, notes = true } = options
  const dist = awayFrom(from)
  return matchPlaces(places, from, options)
    .slice(0, limit)
    .map(p => `${p.name} ${p.kind} ${dist(p)}m @${p.x},${p.y},${p.z}${notes ? ` (${p.by}${p.note ? `: ${p.note}` : ''})` : ''}`)
}

// One marked place, whole: what `places name=` answers. A plan is reported by its size, never printed - farm.plan name= prints it.
export const describePlace = (places, name, from) => {
  const place = places.find(p => p.name === name)
  if (!place) return null
  const rows = place.plan ? place.plan.split('\n') : []
  return {
    name: place.name,
    kind: place.kind,
    at: `${place.x},${place.y},${place.z}`,
    away: `${awayFrom(from)(place)}m`,
    by: place.by,
    ...(place.note ? { note: place.note } : {}),
    ...(rows.length ? { plan: `${Math.max(...rows.map(r => r.length))}x${rows.length}` } : {})
  }
}

// items smelted per unit of fuel, best first
const FUELS = [[/^(coal|charcoal)$/, 8], [/_(planks|log|wood)$/, 1.5], [/^stick$/, 0.5]]

// Which fuel to put in a furnace to smelt `count` items, from inventory `items` [{name,count}]: {name,count} or null
export function pickFuel (items, count) {
  for (const [re, perUnit] of FUELS) {
    const item = items.find(i => re.test(i.name))
    if (item) return { name: item.name, count: Math.min(item.count, Math.ceil(count / perUnit)) }
  }
  return null
}

// The server silently puts the body back every tick when its hitbox touches a block just so (see the hitbox
// note in README). One window of {resets, moved blocks}: many resets and no progress means it is wedged.
export const isWedged = ({ resets, moved }) => resets >= 30 && moved < 0.5

// Call attempt(stillWanted) until `count` is gathered. attempt resolves to how many it got, or rejects (counted as
// none). Two empty rounds in a row, or maxRounds, end it: {got, rounds, gaveUp?: why}
export async function retryUntilCount (attempt, count, maxRounds = 6) {
  let got = 0
  let empty = 0
  let lastError = 'nothing gained'
  for (let rounds = 1; rounds <= maxRounds; rounds++) {
    const gained = await attempt(count - got).catch(e => { lastError = e.message; return 0 })
    got += gained
    empty = gained ? 0 : empty + 1
    if (got >= count) return { got, rounds }
    if (empty >= 2) return { got, rounds, gaveUp: lastError }
  }
  return { got, rounds: maxRounds, gaveUp: 'round limit' }
}

// Does a block's properties object satisfy a `where` condition such as {age: 7}? Values compare as strings.
export const matchesProps = (props, where) => Object.entries(where ?? {}).every(([k, v]) => String(props?.[k]) === String(v))

// One check of a watch against how many matching things are there now. Fires on the edge (condition just became
// true), so a standing condition doesn't repeat itself. {count=1, atMost=false, met} -> {fire, met}
export function checkWatch (watch, seen) {
  const wanted = watch.count ?? 1
  const met = watch.atMost ? seen <= wanted : seen >= wanted
  return { fire: met && !watch.met, met }
}

// Some server replies never come (window opens through the version bridge, mostly); never wait forever for one.
export function within (ms, promise, what) {
  let timer
  const late = new Promise((resolve, reject) => { timer = setTimeout(() => reject(new Error(`${what} took longer than ${ms / 1000}s`)), ms) })
  return Promise.race([promise, late]).finally(() => clearTimeout(timer))
}

// why a long action may not start now, or null. Moving while the server has us in bed desyncs the body.
export function refuseReason ({ name, health, force, sleeping }) {
  if (sleeping && name !== 'run') return 'you are asleep: `wake` first'
  if (!['mine.get', 'dig'].includes(name) || health > 8 || force) return null
  return `health is ${Math.round(health)}: eat/rest first, or pass force=true`
}

// can a block be placed at `block` without walking? Within reach of the eyes, and not where the body (0.6 wide, 1.8 tall) is.
export function canPlaceFromHere (feet, block, reach = 4) {
  const overlaps = (lo, hi, cell) => lo < cell + 1 && hi > cell
  const inBody = overlaps(feet.x - 0.3, feet.x + 0.3, block.x) && overlaps(feet.z - 0.3, feet.z + 0.3, block.z) && overlaps(feet.y, feet.y + 1.8, block.y)
  if (inBody) return false
  return Math.hypot(block.x + 0.5 - feet.x, block.y + 0.5 - (feet.y + 1.62), block.z + 0.5 - feet.z) <= reach
}

// hostile-looking mobs the flee/fight reflexes should leave alone: endermen always, spiders in bright daylight (neutral then)
export const ignorableMob = (name, { day, skyLight }) => name === 'enderman' || (name === 'spider' && day && skyLight >= 12)

// the pathfinder's "goal was changed"/"path was stopped" usually means a reflex took over the legs: say which one
export function explainInterrupt (error, reflex) {
  if (!reflex || reflex.agoMs > 10000 || !/goal was changed|path was stopped/i.test(error)) return error
  const what = reflex.kind === 'leashed'
    ? `breaking off a fight with ${reflex.mob} that pulled me too far: walking back to where it started`
    : reflex.kind === 'fleeing' ? `fleeing from ${reflex.mob}` : `fighting ${reflex.mob}`
  const advice = reflex.kind === 'fleeing' && reflex.mob !== 'creeper' ? ' (unarmed or badly hurt bodies flee: carry a sword, keep your health up)' : ''
  return `interrupted: ${what}. Wait until it is over, then retry${advice}`
}

// what `place` should do about the block already at a target: leaves in a forest are cleared, anything else solid is somebody's
export function occupiedBy (existing, wanted) {
  if (!existing || existing.boundingBox !== 'block') return 'free'
  if (existing.name === wanted) return 'skip'
  return /_leaves$/.test(existing.name) ? 'clear' : 'blocked'
}

// shearing: nearest sheep within reach of a walk that this errand has not shorn yet (wool takes minutes to grow back)
export const nextSheep = (sheep, shorn, within) => sheep.filter(s => !shorn.has(s.id) && s.dist <= within).sort((a, b) => a.dist - b.dist)[0]

// farming: the seed to replant a ripe crop with, or null when it is not a crop or not ripe yet
const CROPS = { wheat: [7, 'wheat_seeds'], carrots: [7, 'carrot'], potatoes: [7, 'potato'], beetroots: [3, 'beetroot_seeds'], cocoa: [2, 'cocoa_beans'] }
export const ripeCrop = (name, age) => CROPS[name] && Number(age) >= CROPS[name][0] ? CROPS[name][1] : null
export const cropNames = Object.keys(CROPS)

// boustrophedon: row by row (z), alternating direction, so the farmer sweeps a field instead of criss-crossing it
export function harvestOrder (positions) {
  const rows = [...new Set(positions.map(p => p.z))].sort((a, b) => a - b)
  return rows.flatMap((z, i) => positions.filter(p => p.z === z).sort((a, b) => i % 2 ? b.x - a.x : a.x - b.x))
}

// `./mc dawn` blocks until morning so that its exit wakes a logged-off agent; it must never wait for ever
export function dawnVerdict (clock, now, waitedSeconds) {
  if (!clock || now - clock.at > 90000) return 'stale'
  if (clock.day) return 'day'
  return waitedSeconds >= 8 * 60 ? 'long' : 'wait'
}

// what to take from a chest and what it cannot give: a withdraw that silently skips a missing item reads as success
// a right-click on these opens or uses them instead of placing a block against them
const CLICKABLE = /(_bed|_door|_trapdoor|_fence_gate|_button|chest|barrel|shulker_box|crafting_table|furnace|smoker|hopper|dispenser|dropper|anvil|lever|loom|stonecutter|grindstone|smithing_table|cartography_table|brewing_stand|enchanting_table|beacon|note_block|repeater|comparator|composter|cauldron)$/

// which neighbour of a free spot to place against: a plain solid one if there is any, else a clickable one while sneaking
export function placeAgainst (neighbours) {
  const solid = neighbours.map((b, index) => ({ b, index })).filter(n => n.b?.boundingBox === 'block')
  const plain = solid.find(n => !CLICKABLE.test(n.b.name))
  if (plain) return { index: plain.index, sneak: false }
  return solid.length ? { index: solid[0].index, sneak: true } : null
}

// a healthy body sits near 200 MB and start-body caps the heap at 1536 MB; a runaway path search gets there within half a minute
export const overMemory = heapMb => heapMb >= 800

// items= comes as [{name, count}] or, shorter, as {name: count}
const itemList = wanted => Array.isArray(wanted) ? wanted : Object.entries(wanted).map(([name, count]) => ({ name, count }))

// collect with a full inventory walks to drops it cannot pick up: tell the driver why they stayed
export const leftLying = (freeSlots, left, inDeepWater = []) => ({
  ...(freeSlots > 0 || !left.length ? {} : { inventoryFull: `left lying: ${[...new Set(left)].join(' ')}. Deposit or toss something first` }),
  ...(inDeepWater.length ? { inWater: `left in deep water: ${[...new Set(inDeepWater)].join(' ')}. Fetch it from a boat or the shore, or collect wet=true and watch your air` } : {})
})

// the chests' items= or, as every other action says it, item= (count=)
export const itemsArg = a => a.items ?? (a.item ? [{ name: a.item, count: a.count }] : undefined)

export function withdrawPlan (wanted, inChest) {
  if (!wanted) throw new Error(`withdraw needs items='{"coal":4}' or item=coal count=4`)
  // count 'all': whatever is there, and none of it is no shortfall
  const rows = itemList(wanted).map(w => ({ name: w.name, want: w.count === 'all' ? inChest[w.name] ?? 0 : w.count ?? inChest[w.name] ?? 1, have: inChest[w.name] ?? 0 }))
  return {
    take: rows.filter(r => r.have > 0).map(r => ({ name: r.name, count: Math.min(r.want, r.have) })),
    short: rows.filter(r => r.have < r.want).map(r => `${r.name}:${r.have}/${r.want}`)
  }
}

// night is when beds work; state, the shared clock, night_fell and the reflexes must all agree on it
export const isNight = tick => tick > 12542 && tick < 23460

// every running body writes the game time to clock.json; a logged-off agent reads it (./mc clock) instead of reconnecting to look,
// which would stop the night from skipping
export function describeClock (clock, now) {
  const age = clock ? Math.round((now - clock.at) / 1000) : null
  if (!clock || age > 60) return `time unknown: no body has reported ${clock ? `for ${age}s` : 'yet'} (nobody online); start your body and check state`
  return `time=${clock.day ? 'day' : 'night'} ${clock.timeOfDay} (seen ${age}s ago by ${clock.by})`
}

// suffocating (died once under gravel that fell while a digging walk tunnelled beneath it): the block to dig free is the one
// round our head; a solid block at the feet alone does no harm
// only blocks that fall: a door or a slab at head height is no burial, and digging it would wreck someone's house
const FALLS = /^(gravel|sand|red_sand|suspicious_sand|suspicious_gravel|.*_concrete_powder)$/
export const buriedIn = ([head]) => head?.boundingBox === 'block' && FALLS.test(head.name) ? head : undefined

// the pathfinder snaps each waypoint onto the top of the block it is in; in a doorway that is the top of the door panel (a block
// up, off-centre), which no body can reach from the doorstep. Put such a waypoint back on the floor in the middle of the doorway.
export function doorwayNode (node, door) {
  if (!door) return node
  return { ...node, x: door.x + 0.5, y: door.half === 'upper' ? door.y - 1 : door.y, z: door.z + 0.5 }
}

// walks are walk-only (no digging, no scaffold: they used to tunnel through hills and leave pillars) unless asked; mining must dig
export const mayDig = (name, args) => args.dig === true

// ./mc wait: an idle subagent is not woken by its monitor (the events only reach it with the next message), so drivers wait inside a
// blocking command instead. These are the events worth ending the wait for
const WAKE_TYPES = new Set(['tool_broke', 'whisper', 'died', 'kicked', 'body_down', 'error', 'task_done', 'task_cancelled', 'wedged', 'stalled', 'buried',
  'watch_hit', 'night_fell', 'dawn', 'woke_up', 'bedtime_failed', 'code_updated'])
export const wakeWorthy = (event, me) => WAKE_TYPES.has(event.type) ||
  (event.type === 'chat' && event.from !== me) || (event.type === 'hurt' && event.health <= 8)

const eventLine = ({ seq, t, type, ...rest }) => [type, ...Object.entries(rest).map(([k, v]) => v === true ? k : `${k}=${typeof v === 'string' ? v : JSON.stringify(v)}`)].join(' ')

// text: what was appended to events.jsonl since the last look. consumed: how many bytes of it were whole lines (the rest is read next time)
// what happened between two waits is reported by the second one: anything older than a minute says so ("(3m ago) died"), or a driver
// reads an old death as a new one
const aged = (event, now) => {
  const minutes = Math.floor((now - Date.parse(event.t)) / 60000)
  return minutes >= 1 ? `(${minutes}m ago) ` : ''
}
export function waitReport (text, me, now = Date.now()) {
  const whole = text.slice(0, text.lastIndexOf('\n') + 1)
  const events = whole.split('\n').filter(Boolean).map(line => JSON.parse(line))
  return { lines: events.filter(e => wakeWorthy(e, me)).map(e => aged(e, now) + eventLine(e)), consumed: Buffer.byteLength(whole) }
}

// bedtime reflex: a body whose driver is away (monitor expired, waiting, asleep itself) still goes to bed, so one absent driver does not
// keep the night going for everyone. A driver who is at work (a task, or a command in the last 90 s) is left alone
export const bedtime = s => s.night && !s.busy && !s.asleep && s.bedNear && !s.hostileNear && s.reflexes && s.idleMs >= 90000 &&
  s.sinceTryMs >= Math.min(30000 * 2 ** s.failures, 300000)

// what a `place` run reports. Cells it could not reach or attach are skipped, not fatal: one awkward cell used to end a 60-block list
// Did the block really land? mineflayer's placeBlock resolves as soon as the server answers anything, and a placement the
// server dislikes is dropped without a word: the cell must have CHANGED (a seed lands as a crop, so its name is no help).
export const placeMissed = (before, after) => after === before
  ? 'the server dropped it without a word (something in the way, or you moved out of reach): nothing was placed'
  : null

// Where did the bucket really empty? A click that misses pours at the player's own eye level instead, and the flood then
// breaks every crop it runs over. `cells` is what stands around me now, `before` the keys of the sources that were
// already there: what is new, and is a source (level 0), is the one to take back.
export const strayFluid = (before, cells, fluid) =>
  cells.find(c => c.name === fluid && Number(c.level) === 0 && !before.has(`${c.x},${c.y},${c.z}`)) ?? null

// cells: what really stands there now, read back off the world, {x,y,z,name}. The @x,y,z of any reply is where the BODY
// stands, and AhuraMazda took it for the block he had just placed: he dug what he thought was his own bed remnant and hit
// someone else's pressure plate. Grouped by block, because a batch usually lays one kind
export const placedAt = cells => {
  if (!cells.length) return null
  const kinds = [...new Set(cells.map(c => c.name))]
  return kinds.map(kind => {
    const mine = cells.filter(c => c.name === kind).map(c => `${c.x},${c.y},${c.z}`)
    const shown = mine.slice(0, 6).join(' ')
    return `${shown}${mine.length > 6 ? ` and ${mine.length - 6} more` : ''} (${kind})`
  }).join(' ')
}

export function placeOutcome (placed, skipped, verb = 'placed', already = 0, cells = []) {
  // one line per different reason: a batch that hit solid ground AND had nothing to attach to showed only the first, and the second stayed a riddle
  const reasons = [...new Set(skipped.map(s => s.why))]
  const why = reasons.map(r => `${skipped.filter(s => s.why === r).length} ${r} (first ${skipped.find(s => s.why === r).at})`).join('; ')
  if (!placed && why) return { error: `${verb} nothing: ${why}${already ? `; ${already} were already there` : ''}` }
  const there = already ? { alreadyThere: already } : {}
  const at = placedAt(cells) ? { at: placedAt(cells) } : {}
  return why ? { [verb]: placed, skipped: skipped.length, why, ...there, ...at } : { [verb]: placed, ...there, ...at }
}

// the server tells a player which of its items just broke with an entity status (47 main hand .. 52 boots). Nothing else does: my axe
// wore out unnoticed and the body, unarmed without knowing it, ran from a zombie it should have fought
const BROKEN = { 47: 'hand', 48: 'off-hand', 49: 'head', 50: 'torso', 51: 'legs', 52: 'feet' }
export const brokenSlot = status => BROKEN[status] ?? null

// where an item goes when `equip` is given no destination: a helmet "equipped" into the hand answered ok and protected nothing
const SLOTS = [[/_helmet$|^carved_pumpkin$/, 'head'], [/_chestplate$|^elytra$/, 'torso'], [/_leggings$/, 'legs'], [/_boots$/, 'feet'], [/^shield$/, 'off-hand']]
export const equipSlot = itemName => SLOTS.find(([pattern]) => pattern.test(itemName))?.[1] ?? 'hand'

// fight or run. Running at 8 health was too late for a body without armour: two zombies took 20 health in 12 s, and it died fleeing.
// So run earlier the less armour it wears and the more there are of them (never above 16: a fresh body may always try)
export function shouldFlee (s) {
  if (!s.armed) return true
  return s.health <= Math.min(8 + (4 - s.armorPieces) + 3 * (s.attackers - 1), 16)
}

// archers shoot from beyond the 7 blocks the other reflexes watch: a body that stood still was shot dead without ever reacting.
// Just hurt, not in a fight, an archer in sight: charge it (it cannot be outwaited), or run when unarmed or badly hurt
export const ARCHERS = new Set(['skeleton', 'stray', 'bogged', 'pillager'])
export const crowdSize = hostiles => hostiles.filter(h => h.dist <= 5 || (ARCHERS.has(h.name) && h.dist <= 24)).length
export function rangedThreat (s) {
  if (s.hurtMsAgo > 5000 || s.fighting || s.meleeNear || !s.archerNear) return null
  return s.armed && s.health >= 14 && !s.inWater ? 'charge' : 'flee'
}

// how many of the wanted blocks a round of mining really brought in: the blocks that are gone, if anything at all was picked up
// (drops from tree tops are often still falling when we count, so the items gained are no upper limit).
// Counting every gained item called a round that only dug dirt on the way a success
export const minedCount = (gone, gained) => gained > 0 ? gone : 0

// the weakest tool that can harvest a block, when none of the carried item types can; null when the block needs no tool or one is carried.
// mineflayer-tool recurses for ever (until the heap is gone) when asked to equip for a block nothing carried can harvest
export function missingTool (harvestTools, carriedTypes, nameOf) {
  if (!harvestTools) return null
  if (carriedTypes.some(type => harvestTools[type])) return null
  return nameOf(Number(Object.keys(harvestTools)[0]))
}

// Standing on a partial block, the pathfinder plans from the cell ABOVE it. Right for farmland and slabs, wrong for a bed: you wake up on
// it, and in a small hut with a low roof the cell above has no way out (no headroom, the door is one level down). Plan from the bed's cell
export const plansFromOwnCell = blockName => /_bed$/.test(blockName)

// standing on a bed under a low roof, the pathfinder plans from the cell above the bed, finds the roof there and no move at all.
// One plain step onto free floor next to me cures it: which side? cells: [{feet, head, ground}] bounding boxes of the four sides
// Second choice, when there is no free floor beside me (a bed along a 1-wide room): another low block (`low`: the bed's other half, a
// slab) that I can walk onto level, and step off again from there
export function stepOffChoice (cells) {
  const floor = cells.findIndex(c => c.feet === 'empty' && c.head === 'empty' && c.ground === 'block')
  if (floor >= 0) return floor
  // a wooden door is a way out (the reflex opens it): a bed whose only free neighbour was the door sent the body up and down the bed for ever
  const door = cells.findIndex(c => c.door)
  return door >= 0 ? door : cells.findIndex(c => c.low && c.head === 'empty')
}

// the cell the pathfinder counts me in: standing on a block that is not a full one high (farmland, a slab, a dirt path) my feet are
// inside that block's cell, and the pathfinder plans from the cell above. Judging arrival from the floored cell fails by one block
export function feetCell (position, onGround) {
  const sunk = onGround && position.y - Math.floor(position.y) > 0.001
  return { x: Math.floor(position.x), y: Math.floor(position.y) + (sunk ? 1 : 0), z: Math.floor(position.z) }
}

// mineflayer-pathfinder's goto resolves as a success when the search comes back with an empty path (boxed in, in a shaft):
// the caller must check the goal itself
export const arrivalError = reached => reached ? null : 'no path to the goal: the search found nothing to walk from here'

export function explainNoPath (error, dig) {
  if (dig || !/no path to the goal|took to long to decide/i.test(error)) return error
  return 'no walkable path (walks don\'t dig or bridge): look for a way round, go in shorter legs, or pass dig=true if breaking and placing blocks on the way is fine'
}

// the search found nothing (no or a partial path with not one step in it) and the legs stand still, but the pathfinder keeps the goal
// and whoever awaits the walk waits for ever: 13 of 61 stalls on 09-19, each killing a whole harvest or collect after 12 s.
// End just that walk, early: the task sees an ordinary "no path" and goes on to its next block or drop
export const deadWalk = ({ hasGoal, moved, digging, seconds, path }) =>
  hasGoal && !digging && moved < 0.2 && seconds >= 4 && Boolean(path) && path.status !== 'success' && path.nodes.length === 0

// a task that wants to walk somewhere but has neither moved nor dug for a while is hung (seen: walks started right beside a door)
// mc without MC_HOME knows no body. It used to fall back to the first one (Claude's): whoever ran bot/mc from a drifted shell drove somebody else's body
export const stackTop = stack => String(stack ?? '').split('\n').filter(l => /^\s+at /.test(l)).slice(0, 3)
  .map(l => l.trim().replace(/^at /, '').replace(/\(?file:\/\/\S*\/([^/)]+)\)?$/, '$1').replace(/[()]/g, '')).join(' < ')
export const isBaby = metadata => metadata?.[16] === true
export const noHomeError = (home, action) => home || ['clock', 'dawn'].includes(action) ? null : 'no agent chosen: this is the shared bot/ folder, and its mc drives nobody. Run YOUR OWN wrapper with its full path: /home/dan/minecraft/claude/bot/agents/<YourName>/mc <action> ... (your shell has probably drifted out of your folder: cd back into it)'
export const progressed = (from, here) => Math.hypot(here.x - from.x, here.z - from.z) >= 0.2 || Math.abs(here.y - from.y) >= 1.5
export const isStalled = ({ hasGoal, moved, digging, seconds }) => hasGoal && !digging && moved < 0.2 && seconds >= 12

// how a hunt (attack mob=) stands: null = keep fighting. A target that runs or falls away is let go past the leash
export function chaseVerdict (c) {
  if (!c.targetValid) return { killed: true }
  if (!c.hunting) return { killed: false, gaveUp: 'the fight was broken off (the body fled, ate or was interrupted): it is still alive. Look around, then attack again or keep away' }
  if (c.strayed <= c.leash) return null
  return { killed: false, gaveUp: `it led me ${Math.round(c.strayed)} blocks away (leash=${c.leash}): let it go, or attack again from here` }
}

// #105: the fight REFLEX had no leash of its own. mineflayer-pvp walks the body after its target, and the target stays
// beside the body, so any distance measured mob-to-body stays small however far the pair travels: a spider walked the
// body into a cave and it died down there. This leash is measured from where the fight STARTED, and the drop is checked
// before the walk, because falling out of the daylight is what kills, not the distance.
export const CHASE_LEASH = 8
export const CHASE_DROP = 3
// a fight that starts by crossing ground (rangedThreat 'charge' runs at a skeleton that shot from 20 blocks) is owed
// that ground on top of its leash: measured from where the body stood, a flat 8 aborts the charge and walks it back
// into the arrows.
export const chargeLeash = (start, mob, { leash = CHASE_LEASH } = {}) =>
  leash + Math.round(Math.hypot(mob.x - start.x, mob.z - start.z))

// a break-off that was a DROP has to dig and bridge its way back: the body dug its way down and the same ground is in
// the way going up. Walking it instead left the body standing in the hole while a zombie killed it (2026-09-23).
export const breakOffDigs = (start, here) => start.y - here.y > 0

export const chaseBroken = (start, here, { leash = CHASE_LEASH, drop = CHASE_DROP } = {}) => {
  if (!start) return null
  const fell = start.y - here.y
  if (fell > drop) return `the fight pulled me ${Math.round(fell)} blocks down (from y=${Math.round(start.y)}): broken off before it becomes a cave, and I am walking back`
  const away = Math.hypot(here.x - start.x, here.z - start.z)
  if (away > leash) return `the fight pulled me ${Math.round(away)} blocks from where it started (leash=${leash}): broken off, and I am walking back`
  return null
}

// #97: an enderman killed Ganesha's body at its own cabin in five seconds, because the fight reflex treated it as one
// more mob to beat. Nothing this body carries wins that fight, and aiming at its head is what starts it: these are never
// attacked, never chased, and one that comes within arm's reach is backed away from the way a creeper is.
export const NEVER_FIGHT = new Set(['enderman', 'warden'])
export const ENDERMAN_RANGE = 5
export const attackRefusal = name => NEVER_FIGHT.has(name)
  ? `${name}: not a fight this body can win (one killed Ganesha's body in five seconds at its own door, #97). Aiming at its head is what provokes it, so I will not aim at one either. Break the line of sight - a block, a door, deep water - and walk away`
  : null
// a neutral enderman keeps its distance and teleports about; one standing next to the body has almost always been
// provoked already, and by then the body has about five seconds
export const fleeUnwinnable = (mobs, range = ENDERMAN_RANGE) =>
  mobs.filter(m => NEVER_FIGHT.has(m.name) && m.dist <= range).sort((a, b) => a.dist - b.dist)[0] ?? null

// tools: [{name (null = bare hand), time, harvests}]. The fastest that is not a weapon; undefined when only a weapon can harvest the block
const isWeapon = name => /_sword$|^trident$|^mace$/.test(name ?? '')
export const peacefulTool = tools => tools.filter(t => !isWeapon(t.name) && t.harvests).sort((a, b) => a.time - b.time)[0]

// what a deposit puts in: the named items, or everything only when asked for outright
export function depositWanted (a, carried) {
  if (itemsArg(a)) return itemsArg(a)
  if (a.all) return carried
  return { error: `deposit needs items='{"dirt":4}' (or all=true for everything you carry, tools included)` }
}

// wraps an action's arguments and remembers which were read, so a reply can name the ones that were not (a misspelt name, mostly)
export function trackReads (given) {
  const read = new Set()
  const args = new Proxy(given, { get: (target, key) => { read.add(key); return target[key] } })
  return { args, given, unread: () => Object.keys(given).filter(key => !read.has(key)) }
}
// after a failure only keys the action's source never mentions are blamed: it may have failed before it got to read the others
export function ignoredParams (unread, ok, source) {
  const ignored = unread.filter(key => ok || !new RegExp(`\\b${key}\\b`).test(source))
  return ignored.length ? `${ignored.join(' ')} (not a parameter here, or not used this time: check the name in the guide)` : null
}

// what furnace_take says about what is left inside: a furnace with input and no fire never finishes, and used to look just like one that cooks
export function furnaceReport ({ input, fuel, burning }) {
  const stillCooking = input?.count ?? 0
  if (!stillCooking || burning) return { stillCooking }
  if (!fuel) return { stillCooking, stuck: `${input.name} is waiting but the fire is out and the fuel slot is empty: smelt fuel=coal count=${stillCooking} x= y= z= adds fuel only (no item= needed; charcoal, planks or logs work too)` }
  return { stillCooking, stuck: `${input.name} is waiting and ${fuel.name} is in the fuel slot, but nothing burns: that input cannot be smelted here, or the output slot holds something else. chest_contents shows the slots` }
}

// running out of air. 'start' takes the body away from its task (a task that keeps steering drowned Jizo), 'hold' keeps swimming up.
// oxygen alone misfires on dry land through ViaBackwards, so starting also needs the head under water
export function airReflex (s) {
  if (!s.surfacing) return s.headInWater && s.oxygen <= 8 ? 'start' : null
  return s.oxygen >= 18 || !s.inWater ? 'stop' : 'hold'
}

// swimming is faster than any walk, so the reflex only looks for open water sideways when the column over the head is
// roofed. `names` are the blocks from just over the head upwards; water plants and a bubble column are still water.
const WATERY = new Set(['water', 'bubble_column', 'kelp', 'kelp_plant', 'seagrass', 'tall_seagrass'])
const BREATHABLE = new Set(['air', 'cave_air', 'void_air'])
export function openAbove (names) {
  const blocker = names.find(name => !WATERY.has(name))
  return blocker === undefined || BREATHABLE.has(blocker)
}

// Chani drowned at 126,53,-52 walking to a surface three blocks away: the pathfinder said it was moving, so the reflex
// never pressed jump. Give the walk two seconds to lift the body, then drop the path and swim straight up.
export function surfacingStalled ({ startedAt, now, startY, y, swimming }) {
  if (swimming) return false
  return now - startedAt >= 2000 && y < startY + 0.5
}

// a step on any of these destroys it, so the pathfinder walks round planted ground. Farmland itself stays walkable:
// an empty bed is the only way across some fields, and trampling is off on this server.
const CROPS_UNDERFOOT = new Set([
  'wheat', 'carrots', 'potatoes', 'beetroots', 'melon_stem', 'pumpkin_stem',
  'attached_melon_stem', 'attached_pumpkin_stem', 'sweet_berry_bush', 'torchflower_crop', 'pitcher_crop'
])
export const breaksUnderfoot = name => CROPS_UNDERFOOT.has(name)

// the cells a standing body's hitbox (0.6 wide) lies flush against, at feet and head level: the candidates for what wedges it
export function flushCells (pos) {
  const span = v => [...new Set([Math.floor(v - 0.299), Math.floor(v + 0.299)])]
  const side = v => { const f = v - Math.floor(v); return f <= 0.305 ? Math.floor(v) - 1 : f >= 0.695 ? Math.floor(v) + 1 : null }
  const levels = [Math.floor(pos.y), Math.floor(pos.y) + 1]
  const [sx, sz] = [side(pos.x), side(pos.z)]
  return [
    ...(sx === null ? [] : span(pos.z).flatMap(z => levels.map(y => ({ x: sx, y, z })))),
    ...(sz === null ? [] : span(pos.x).flatMap(x => levels.map(y => ({ x, y, z: sz }))))
  ]
}

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

// farm.find_spot scores a patch of ground the way someone choosing where to farm would: flat first, because every cell
// off the common level is a block to dig or fill, then water (farmland dries without a source within 4), then open sky
// (crops need light), then how far you had to walk. Ground a zone or a saved plan already claims is never a candidate,
// and neither is a patch with a hole in it: tops are the surface of every column, and a null is a lake or a drop.
const commonest = xs => Number(Object.entries(xs.reduce((n, x) => ({ ...n, [x]: (n[x] ?? 0) + 1 }), {}))
  .sort((a, b) => b[1] - a[1] || Number(a[0]) - Number(b[0]))[0][0])

export const spotScore = ({ tops, taken, water, sky, away = 0 }) => {
  if (taken || tops.some(t => t === null || t === undefined)) return null
  const y = commonest(tops)
  const work = tops.reduce((n, t) => n + Math.abs(t - y), 0)
  const level = Math.round(100 * tops.filter(t => t === y).length / tops.length)
  const score = level + (water ? 25 : 0) + (sky ? 15 : 0) - Math.min(40, work) - Math.min(30, Math.round(away / 4))
  return { y, level, work, water: Boolean(water), sky: Boolean(sky), away: Math.round(away), score }
}

export const bestSpots = (scored, limit = 3) => scored.filter(Boolean)
  .sort((a, b) => b.score - a.score || a.away - b.away).slice(0, limit)

export const apiaryGoods = items => Object.fromEntries(Object.entries(items)
  .filter(([name, count]) => count > 0 && ['honeycomb', 'honey_bottle'].includes(name)))

// a position 2 cm clear of whatever the hitbox lies flush against (see flushCells): out of the pinned state without digging
export function nudgeAway (pos) {
  const clear = v => { const f = v - Math.floor(v); return f <= 0.305 ? Math.floor(v) + 0.32 : f >= 0.695 ? Math.floor(v) + 0.68 : v }
  return { x: clear(pos.x), y: pos.y, z: clear(pos.z) }
}

// prismarine-physics stops a move only at a box that lies strictly ahead, and counts any overlap on the other axes. A hitbox edge that rounds
// 1e-14 past a block face (-128.3001 + 0.3001 = -127.99999999999999) then counts as "already inside": the body walks into the block and the
// server puts it back every tick (the step-up wedge). Same comparison, with a tolerance. Boxes are { min: [x,y,z], max: [x,y,z] }
const EDGE = 1e-7
export function clampedOffset (block, player, axis, offset) {
  const beside = [0, 1, 2].filter(a => a !== axis).some(a => player.max[a] <= block.min[a] + EDGE || player.min[a] >= block.max[a] - EDGE)
  if (beside) return offset
  if (offset > 0 && player.max[axis] <= block.min[axis] + EDGE) return Math.min(Math.max(block.min[axis] - player.max[axis], 0), offset)
  if (offset < 0 && player.min[axis] >= block.max[axis] - EDGE) return Math.max(Math.min(block.max[axis] - player.min[axis], 0), offset)
  return offset
}

// leading animals with food in hand: they follow from up to 10 blocks and are slower than I am. distances = how far each one still with me is
// heldFor: seconds I have stood waiting. An animal that does not come (a fence between us) would keep me waiting for ever: go and get it,
// and after 3 fetches that brought me no nearer the goal, give up and tell the driver
export function leadVerdict ({ distances, holding, heldFor = 0, fetchesSinceProgress = 0, noPath = false }) {
  if (!distances.length) return 'lost'
  const farthest = Math.max(...distances)
  const fetch = farthest > 9 || (holding && heldFor >= 12 && farthest > 4)
  if (fetch) return fetchesSinceProgress >= 3 ? 'giveup' : 'fetch'
  if (farthest > (holding ? 4 : 6)) return 'hold'
  return noPath ? 'noway' : 'go'
}

// a body loads the shared code once, at start: which files have changed since then (null: none), so the driver knows a restart brings fixes
// While any file is under 3 minutes old the change may be half made (a body started then might not even load): say nothing yet
export function staleCode (started, mtimes, now) {
  const newer = Object.entries(mtimes).filter(([, mtime]) => mtime > started)
  // quiet for 15 minutes, or for 2 when this body is already 45 minutes behind: waiting for a lull starved the long-running bodies of every fix
  const quiet = now - started >= 2700000 ? 120000 : 900000
  if (newer.some(([, mtime]) => now - mtime < quiet)) return null
  return newer.length ? newer.map(([file]) => file) : null
}

// where a harvested crop is planted again, relative to the crop: the block to place against and the face of it. Field crops stand on the
// farmland below; a cocoa pod hangs on the side of a jungle log, and its `facing` points at that log
const FACING = { north: [0, 0, -1], south: [0, 0, 1], west: [-1, 0, 0], east: [1, 0, 0] }
export function replantSpot (name, props) {
  if (name !== 'cocoa') return { against: [0, -1, 0], face: [0, 1, 0] }
  const against = FACING[props.facing]
  // `|| 0`: no -0 in the answer
  return { against, face: against.map(v => -v || 0) }
}

// bamboo and sugar cane: cutting the second segment brings down everything above it, and the base grows back
export const STALKS = ['bamboo', 'sugar_cane']
export const isStalkCut = (block, below, belowThat) => STALKS.includes(block) && below === block && belowThat !== block

// when others skip the night the server's "leave bed" sometimes never reaches the body, and it lies in bed all day. In bed in broad
// daylight (past the first 15 s of the day, no thunderstorm) means: get up myself
export const oversleeping = s => s.asleep && !s.thundering && s.timeOfDay > 300 && s.timeOfDay < 12000

// shut a door or gate I opened: once I am past it, or as soon as I have stopped beside it (but never on myself in the doorway)
// doors are mine to open on a walk; gates are the pathfinder's, except the one whose cell I already stand in: pressed against its closed panel the body floors into the gate's cell, the path starts there and never includes opening it
export const openNow = ({ near, open, door, moving, inDoorway }) => near && !open && moving && (door || inDoorway)
export const shutNow = ({ near, open, mine, leading, moving, inDoorway }) => open && mine && !leading && (!near || (!moving && !inDoorway))

// The actions that changed name when the library was namespaced. Journals, habits and old notes still say the left-hand
// side, so every "unknown action" names its successor rather than leaving the driver to guess. No aliases: the old name stays dead.
export const RENAMED = {
  harvest: 'farm.harvest',
  maintain_farm: 'farm.maintain',
  compost: 'farm.compost',
  get_seeds: 'farm.get_seeds',
  plan: 'farm.plan',
  fields: 'farm.fields',
  mine: 'mine.get',
  collect_items: 'collect',
  lead: 'flock.lead',
  breed: 'flock.breed',
  pen_check: 'pen.check'
}
// where to read about the successor: its section is the half before the dot, and a top-level one is its own topic
export const renamedTo = typed => RENAMED[typed] ? `it is now ${RENAMED[typed]} (./mc help ${RENAMED[typed].split('.')[0]})` : null
export const renamedList = () => `renamed: ${Object.entries(RENAMED).map(([was, now]) => `${was} -> ${now}`).join(', ')}`

// an action name nobody knows: the real ones that share a word with it
// words drivers reach for that share nothing with the real name
const OTHER_WORDS = { cancel: 'stop', abort: 'stop', halt: 'stop', nearby: 'look_around', entities: 'look_around', mobs: 'look_around', say: 'chat', walk: 'goto', move: 'goto', eat: 'consume', attack: 'attack', kill: 'attack', bed: 'sleep', open: 'toggle', close: 'toggle', store: 'deposit', take: 'withdraw', drop: 'toss', throw: 'toss' }
export function didYouMean (typed, actions) {
  if (RENAMED[typed]) return `unknown action ${typed}: ${renamedTo(typed)}`
  const words = typed.toLowerCase().split(/[^a-z]+/).filter(w => w.length >= 3)
  const meant = words.map(w => OTHER_WORDS[w]).filter(a => actions.includes(a))
  const like = [...new Set([...meant, ...actions.filter(a => words.some(w => a.split(/[_.]/).some(part => part.startsWith(w) || w.startsWith(part))))])]
  return `unknown action ${typed}: ${like.length ? `did you mean ${like.slice(0, 4).join(', ')}?` : './mc help lists them all'}`
}
// how many cells a scan may cover: a map is read cell by cell, where= comes back as one line of coordinates
export const scanCap = where => where ? 60000 : 1500

// auto-eat starts below this food level: health only comes back at food 18 and up, so a hurt body eats sooner than a whole one
export const eatBelow = health => health < 20 ? 18 : 15

// place blocks=[...] item=<default>: entries that name no item of their own take the default
export const dryCells = (tilled, waters) => tilled.filter(([x, y, z]) => !waters.some(([wx, wy, wz]) => Math.abs(wx - x) <= 4 && Math.abs(wz - z) <= 4 && (wy === y || wy === y + 1))).map(c => c.join(','))
export const withDefaultItem = (blocks, item) => blocks.map(b => b.item || item === undefined ? b : { ...b, item })

// food in my hand draws every animal that sees it after me, and out through any gate I open: put it away at a gate unless that is the plan
// luring: a lead is on, from its first step TOWARDS the animal (not only once it follows: started beside a gate, lead lost its wheat and gave up with=0)
export const foodAway = ({ held, luring, feeding, gateNear }) => gateNear && !luring && !feeding && Object.values(BREEDING_FOOD).some(foods => foods.includes(held))

// clock.json as read from disk; null when it was caught mid-write (the next look, a few seconds on, finds it whole)
export function parseClock (text) {
  try { return JSON.parse(text) } catch { return null }
}

// a smelt that waits by the furnace: 'night' = stop watching it, everybody else is waiting for me to go to bed
export const smeltWait = ({ got, wanted, night, timedOut }) => got >= wanted || timedOut ? 'done' : night ? 'night' : 'wait'

// lying: where what I tossed still lies 5 s later ('x,y,z'); a thrown item can be picked up after 2 s
export const giveReport = (player, lying, cameBack = 0) => cameBack > 0
  ? { cameBack: `${cameBack} came back to you: NOT given. Something stands between you (a fence, a wall, a gate): go and stand on the same side as ${player}, within 2 blocks, and give again` }
  : lying.length
  ? { lying: `${lying.join(' ')}: ${player} has not picked it up (full inventory, walked off, or it fell out of their reach). Tell them where it lies, or take it back with collect` }
  : { taken: 'yes' }

// what the wedge reflex may break by itself to get the body free: it grows back and nobody built it
// blocks the pathfinder takes for a full cube but that carry nobody: it must neither walk through them nor plan to stand on them (like a fence)
export const noFooting = name => name === 'bamboo'
// why an animal that was shown food stayed put. rises: for its four neighbour cells, how far up the first free standing room is (Infinity: none within reach)
export function pitAdvice (mob, at, rises) {
  if (rises.some(r => r <= 1)) return null
  if (rises.every(r => r === Infinity)) return `the ${mob} at ${at} is walled in on all four sides: open a side (dig), then lead again`
  return `the ${mob} at ${at} stands in a pit (every way out is ${Math.min(...rises)}+ blocks up, it jumps 1): give it a step (place a block beside it, or dig the rim down), then lead again`
}
// what chat and whisper say: {text} | {error}. String(undefined) went out to the whole server as "undefined"
export function chatText (args, max) {
  const text = ['string', 'number'].includes(typeof args.message) ? String(args.message).trim() : ''
  if (text) return { text: text.slice(0, max) }
  const given = Object.keys(args).filter(k => k !== 'player')
  return { error: `nothing said: the text goes in message=${given.length ? ` (you gave ${given.map(k => `${k}=`).join(' ')})` : ''}. Quote it: ./mc chat message="hello all"` }
}
export const wedgeBreakable = name => /_leaves$|^bamboo$/.test(name)
// what the wedge reflex dug [{name, below, at:[x,y,z]}] -> the bamboo BASES among it, to plant again: an upper segment regrows, a base never does
export const wedgeReplant = dug => dug.filter(d => d.name === 'bamboo' && d.below !== 'bamboo').map(d => ({ x: d.at[0], y: d.at[1], z: d.at[2], item: 'bamboo' }))
// bamboo's hitbox sits elsewhere on the server than in the client, so a walk brushing past a stalk gets position resets: a cell beside one costs extra
// my centre lies inside a fence's (wall's, shut gate's) cell: the free neighbour cell whose edge I am nearest to is where I really stand. free(x, y, z)
// One line for gates.log when a fence gate at `at` changes between open and shut: who stood nearest (players: [{name, dist}], myself included)
export function gateChange (at, before, after, players) {
  const isGate = b => b?.name?.endsWith('_fence_gate')
  if (!isGate(before) || !isGate(after) || before.open === after.open) return null
  // a hand reaches about 5 blocks: anyone further off did not do it (the one who did is out of my sight; their own body logs it)
  const nearest = players.filter(p => p.dist <= 6).sort((a, b) => a.dist - b.dist)[0]
  return { gate: `${at.x},${at.y},${at.z}`, now: after.open ? 'open' : 'shut', nearest: nearest?.name ?? null, dist: nearest ? Math.round(nearest.dist) : null }
}

// Walking me out of a fence's cell (see realCell): `pressed` is the cell I am being walked into, or null. Start when the idle nudge finds me in a fence's cell; from then
// on keep going whatever the nudge says, until I stand within 0.3 of that cell's middle: only there does a new plan start from the right side of the fence
export function fencePush (pressed, inFence, nudge, pos) {
  const target = pressed ?? (nudge && inFence ? inFence : null)
  if (!target) return { target: null, replan: false }
  const there = Math.abs(pos.x - target.x - 0.5) <= 0.3 && Math.abs(pos.z - target.z - 0.5) <= 0.3
  return there ? { target: null, replan: true } : { target, replan: false }
}
export function realCell (position, free) {
  const [x, y, z] = [Math.floor(position.x), Math.floor(position.y), Math.floor(position.z)]
  const [fx, fz] = [position.x - x, position.z - z]
  // how far my centre is from each of the eight cells around; more than my half width (0.3) away and no part of me stands there
  const gapTo = { '-1': f => f, 0: () => 0, 1: f => 1 - f }
  const sides = [-1, 0, 1].flatMap(dx => [-1, 0, 1].map(dz => ({ d: Math.hypot(gapTo[dx](fx), gapTo[dz](fz)), x: x + dx, z: z + dz }))).filter(s => s.x !== x || s.z !== z)
  const side = sides.sort((a, b) => a.d - b.d).find(s => s.d <= 0.3 && free(s.x, y, s.z))
  return side ? { x: side.x, y, z: side.z } : null
}
// the names of the four cells beside a cell; none for the position-less block the pathfinder makes up for an unloaded cell
export const besideNames = (position, nameAt) => position ? [[1, 0], [-1, 0], [0, 1], [0, -1]].map(([dx, dz]) => nameAt(position.x + dx, position.y, position.z + dz)) : []
export const thicketCost = neighbours => neighbours.includes('bamboo') ? 25 : 0

// in bed in broad daylight: 'ask' the server to let me up; when that changes nothing for 6 s the server has me up already and only my own flag is stale: 'declare' myself awake
export const wakeStep = ({ oversleeping, forMs }) => !oversleeping ? null : forMs >= 6000 ? 'declare' : 'ask'

// gates (keyed by String(Vec3)) that I walked through and that still stand open, as [x, y, z]: the gate reflex only reaches 5 blocks, so after leading animals in, the gate behind us needs shutting by hand
export const gatesLeftOpen = (opened, held, isOpen) => [...opened].filter(key => !held.has(key) && isOpen(key)).map(key => key.match(/-?\d+/g).map(Number))

// chest transfers go wrong through ViaBackwards: one is lost, or a whole stack comes along (asked 8 wheat, got 24). Compare what arrived
// with the plan: `back` is what to return, `more` what to ask for once again
export function transferFix (plan, before, after) {
  const rows = plan.map(t => ({ name: t.name, off: (after[t.name] ?? 0) - (before[t.name] ?? 0) - t.count }))
  return {
    back: rows.filter(r => r.off > 0).map(r => ({ name: r.name, count: r.off })),
    more: rows.filter(r => r.off < 0).map(r => ({ name: r.name, count: -r.off }))
  }
}

// Can an animal walk out of this pen? Flood-fill the way a cow moves from `start` [x, y, z]: to a neighbouring column whose surface is at
// most 1 higher (a fence or wall top counts 1.5 above its foot, so it holds from level ground but not from a block beside it) or any
// amount lower. topsAt(x, z) gives the heights it could stand at in that column. Leaving `radius` columns from the start = out (24: at 12 a 17-long pen read LEAKS from its far end)
// rimsAt(x, z): the tops of blocks that carry a fence or wall: a post leaves a ledge beside it. An animal gets onto one only by walking (at most half a step up), and
// from a rim it goes on along the wall only: up onto a fence or wall top (a lower fence next along is half a step) or level to the next rim, never through its own
// fence onto plain ground, be that lower, level or a hillside higher
export function penLeak ({ start, topsAt, rimsAt = () => [], radius = 24, withFloor = false }) {
  const key = c => c.join(',')
  const from = new Map([[key(start), null]])
  const queue = [start]
  while (queue.length) {
    const cell = queue.shift()
    const [x, y, z] = cell
    if (Math.max(Math.abs(x - start[0]), Math.abs(z - start[2])) > radius) {
      const path = []
      for (let c = cell; c; c = from.get(key(c))) path.unshift(c)
      // the telling part is the first climb (the step, the barrier top, the far side) or, on level ground, the first gap with a barrier on both sides
      const open = (c, dx, dz) => topsAt(c[0] + dx, c[2] + dz).some(top => top - c[1] <= 1)
      const gap = (c, prev) => prev[0] !== c[0] ? !open(c, 0, 1) && !open(c, 0, -1) : !open(c, 1, 0) && !open(c, -1, 0)
      const told = path.findIndex((c, i) => i > 0 && (c[1] !== path[i - 1][1] || gap(c, path[i - 1])))
      // ...or, through a gap too wide for that on level ground, the last cell beside the fence ring (a fence or wall top ends on .5) before the way leads off
      // into the open: without it the "first climb" was a slope 15-20 blocks from Ganesha's pen
      const fenced = (x, z) => topsAt(x, z).some(top => top % 1 !== 0)
      const byRing = c => fenced(c[0], c[2]) || [[1, 0], [-1, 0], [0, 1], [0, -1]].some(([dx, dz]) => fenced(c[0] + dx, c[2] + dz))
      const leaves = path.findIndex((c, i) => i > 0 && byRing(c) && !path.slice(i + 1, i + 3).some(byRing))
      const found = [told, leaves].filter(i => i > 0)
      const at = found.length ? Math.min(...found) : -1
      const climbs = at > 0 && path[at][1] !== path[at - 1][1]
      return { enclosed: false, via: path.slice(Math.max(at, 0), climbs ? at + 3 : at + 1).map(key).join(' ') }
    }
    // Where a step into the next column can put it. Up to a half step it climbs; otherwise it steps off the edge and
    // falls to the FIRST surface below, never through one: a pen on a skin of ground over a cave used to read as LEAKS
    // by a path under its own floor (claude-test-pen, 113,67,-125), and the block a fence stands on is no way down either.
    const landingIn = (x, z, from) => {
      const under = [...topsAt(x, z), ...rimsAt(x, z)].filter(top => top <= from)
      if (!under.length) return []
      const first = Math.max(...under)
      return topsAt(x, z).includes(first) ? [first] : []
    }
    const waysInto = (x, z, from, onRim) => [
      ...topsAt(x, z).filter(top => top > from && top - from <= 1 && (!onRim || top % 1 !== 0)),
      ...(onRim ? [] : landingIn(x, z, from))
    ]
    // round a corner too, when at least one side of it is open: between two posts that only touch at the corner nothing squeezes
    const passable = (dx, dz) => waysInto(x + dx, z + dz, y, false).length > 0
    const steps = [[1, 0], [-1, 0], [0, 1], [0, -1], ...[[1, 1], [1, -1], [-1, 1], [-1, -1]].filter(([dx, dz]) => passable(dx, 0) || passable(0, dz))]
    for (const [dx, dz] of steps) {
      const onRim = rimsAt(x, z).includes(y)
      const ways = [...waysInto(x + dx, z + dz, y, onRim), ...rimsAt(x + dx, z + dz).filter(rim => rim - y <= 0.5 && (!onRim || rim >= y))]
      for (const top of ways) {
        const next = [x + dx, top, z + dz]
        if (from.has(key(next))) continue
        from.set(key(next), cell)
        queue.push(next)
      }
    }
  }
  const floor = [...from.keys()].filter(k => { const [x, y, z] = k.split(',').map(Number); return !rimsAt(x, z).includes(y) })
  return { enclosed: true, cells: floor.length, ...(withFloor ? { floor } : {}) }
}

export function enchantNames (raw, nameOf) {
  const list = Array.isArray(raw) ? raw.map(e => ({ name: e.name, level: e.lvl })) : raw?.enchantments?.map(e => ({ name: nameOf(e.id) ?? `enchantment#${e.id}`, level: e.level }))
  return list?.length ? list.map(e => `${e.name} ${e.level}`).join(', ') : undefined
}

// Which of an enchanting table's three offers to take: offers[n].level is the xp level slot n+1 asks for (-1: none), and it also needs n+1 lapis.
// `wanted` is the slot number 1-3; without it the dearest I can pay for
export function enchantChoice (offers, xp, lapis, wanted) {
  const listed = offers.map((o, n) => `${n + 1}=level ${o.level}`).join(', ')
  if (offers.every(o => !(o.level > 0))) return { error: 'the table offers nothing for this item: it cannot be enchanted here (already enchanted, or not enchantable)' }
  const affordable = n => offers[n]?.level > 0 && offers[n].level <= xp && lapis >= n + 1
  if (wanted !== undefined) return affordable(wanted - 1) ? { choice: wanted - 1 } : { error: `slot ${wanted} needs xp level ${offers[wanted - 1]?.level} and ${wanted} lapis_lazuli: you have level ${xp} and ${lapis} lapis. Offers: ${listed}` }
  const best = [2, 1, 0].find(affordable)
  return best === undefined ? { error: `nothing affordable: you have xp level ${xp} and ${lapis} lapis_lazuli. Offers: ${listed} (slot n also needs n lapis). Gain xp by mining ore, smelting, breeding or fighting` } : { choice: best }
}

// the collect plugin's way of saying "inventory full" (it wants a chest to empty into, and I give it none)
export const mineFailure = message => /no defined chest locations/i.test(message) ? 'your inventory is full: deposit or drop something, then mine again' : message

// a lead must end on a spot to stand on: a marker set on the fence line ends the walk OUTSIDE the pen
export const leadTargetError = (to, block) => block?.solid ? `flock.lead: ${to.x},${to.y},${to.z} is inside a ${block.name}, not a spot to stand on: give a free floor cell INSIDE the pen (or mark the place again there)` : null

// Gates of a pen that nothing can walk through ("x,y,z"): a way in has pen floor on one side and, straight across, ground that is neither floor nor fence.
// A gate set in the corner of the ring has fence on both far sides (Ganesha's pen: a day of leads that ended outside)
export const blindGates = (gates, inFloor, barrier) => gates
  .filter(g => ![[1, 0], [0, 1]].some(([dx, dz]) => [1, -1].some(s => inFloor(g.x + s * dx, g.z + s * dz) && !inFloor(g.x - s * dx, g.z - s * dz) && !barrier(g.x - s * dx, g.z - s * dz))))
  .map(g => `${g.x},${g.y},${g.z}`)

// Is this enclosure (penLeak's floor: "x,y,z" keys) a PEN, or just a sealed pocket of rock? A pen has a fence or wall beside its floor: a top ending on .5, more than a step up
export const fencedIn = (floor, topsAt) => floor.some(k => {
  const [x, y, z] = k.split(',').map(Number)
  return [[1, 0], [-1, 0], [0, 1], [0, -1]].some(([dx, dz]) => topsAt(x + dx, z + dz).some(top => top % 1 === 0.5 && top - y > 1))
})

// `events` after a restart showed "nothing yet" although events.jsonl was full, and sent agents to the raw log: a body starts from the
// tail of its own file. The read may cut the first line in half, and the last may be half written
export function parseEventTail (text, limit) {
  const parse = line => { try { return JSON.parse(line) } catch { return null } }
  return text.split('\n').map(parse).filter(e => e && typeof e === 'object').slice(-limit)
}

// `fill` answered ok with an empty bucket (standing in the source, the click goes elsewhere)
export const fillOutcome = held => /_bucket$/.test(held ?? '')
  ? { holding: held }
  : { error: 'the bucket is still empty: stand on the shore 1-2 blocks from the source with a clear view of it, not in the water, and fill again' }

// `place` blamed reach ("the server refused it") when the spot itself was the trouble: a seed on dried-out farmland, a carrot on a
// tile that had one. Three agents lost many retries to that. null = nothing in the way that I know of
const GIVES_WAY = /^(air|cave_air|void_air|water|lava|short_grass|tall_grass|fern|large_fern|dead_bush|snow|fire|vine|seagrass|tall_seagrass|leaf_litter|glow_lichen|hanging_roots|nether_sprouts|crimson_roots|warped_roots|light|bubble_column|structure_void)$/
const ON_FARMLAND = /^(wheat_seeds|beetroot_seeds|melon_seeds|pumpkin_seeds|carrot|potato|torchflower_seeds|pitcher_pod)$/
const SOIL = /^(dirt|grass_block|coarse_dirt|podzol|rooted_dirt|moss_block|mud|farmland|mycelium)$/
export function placeObstacle (item, existing, below) {
  if (!GIVES_WAY.test(existing)) return `${existing} is already there (not a block, but in the way): harvest or dig it first`
  if (ON_FARMLAND.test(item) && below !== 'farmland') return `${item} needs farmland under it, and there is ${below}: till that block first`
  if (/_sapling$/.test(item) && !SOIL.test(below)) return `${item} needs dirt or grass under it, and there is ${below}`
  return null
}

// what a craft is really short of. mineflayer names one arbitrary member of an item tag: "needs cherry_planks:2 stick:1" to someone
// holding 5 oak planks and no stick (four agents chased that). Take the recipe nearest to what is carried, tell only what is missing,
// and call an ingredient that other recipes swap for a cousin by its family name
export function craftShortfall (recipes, have) {
  const missing = r => Object.entries(r).map(([name, n]) => [name, n - (have[name] ?? 0)]).filter(([, n]) => n > 0)
  const total = r => missing(r).reduce((sum, [, n]) => sum + n, 0)
  const best = recipes.reduce((a, b) => total(b) < total(a) ? b : a)
  const swapsFor = name => [...new Set(recipes.filter(r => !(name in r)).flatMap(r => Object.keys(r).filter(n => !(n in best))))]
  const label = (name, n) => {
    const cousins = swapsFor(name)
    const family = name.split('_').pop()
    if (!cousins.length) return `${name}:${n}`
    return name.includes('_') && cousins.every(c => c.split('_').pop() === family) ? `any ${family}:${n}` : `${name}:${n} (or ${cousins.slice(0, 2).join(', ')})`
  }
  return missing(best).map(([name, n]) => label(name, n)).join(' ')
}

// which of the blocks found `mine` goes for. Never inside a protected zone; and not in or beside water unless asked: the walk to a
// submerged block ends in a pocket under water, the air reflex cancels the task, and three bodies drowned on 09-19 fetching sand
export function mineTargets ({ nearby, wanted, inZone, wet, allowWet, what }) {
  if (!nearby.length) return { error: `no more ${what}` }
  const free = nearby.filter(p => !inZone(p))
  if (!free.length) return { error: `the only ${what} is inside protected zones (someone's build): go further away and retry` }
  const dry = free.filter(p => allowWet || !wet(p))
  if (!dry.length) return { error: `the only ${what} lies in or next to water, where mining bodies drown: dig it block by block from the shore (dig x= y= z=), or pass wet=true if you take the risk` }
  const skippedWet = free.length - dry.length
  return skippedWet ? { found: dry.slice(0, wanted), skippedWet } : { found: dry.slice(0, wanted) }
}

// blocks that somebody put there: a walk with dig=true must go round them, zone or no zone (Aviendha's goto dig=true tunnelled through
// her own cobble pen wall day after day, and the cows walked out of the hole). Logs and leaves stay diggable: forests are in the way a lot
const BUILT = /(^|_)(cobblestone|planks|fence|gate|wall|door|trapdoor|bed|stairs|slab|glass|pane|wool|carpet|torch|lantern|chest|barrel|furnace|smoker|table|farmland|bricks|ladder|sign|banner|rail|hopper|composter|campfire|anvil|bookshelf|concrete|terracotta)$|^(wheat|carrots|potatoes|beetroots|melon_stem|pumpkin_stem|cocoa|sugar_cane|bamboo|hay_block)$/
export const looksBuilt = name => BUILT.test(name)

// weeds on top of a block keep a hoe or shovel from working it: till and path clear these by themselves (not flowers or crops: someone may want those)
export const isGroundCover = name => /^(short_grass|tall_grass|fern|large_fern|dead_bush|snow|leaf_litter)$/.test(name)

// the pathfinder previews each step with a physics simulation, and when that says "can't" it presses no key at all: it then throws the
// path away as 'stuck' every 3.5 s and finds the same one again, for ever. After 1.5 s of that, walk at the next node by hand
export function idleNudge ({ hasGoal, busy, idleTicks, node, pos, collided }) {
  if (!hasGoal || busy || idleTicks < 30 || !node) return null
  const [dx, dz] = [node.x - pos.x, node.z - pos.z]
  if (Math.abs(dx) <= 0.35 && Math.abs(dz) <= 0.35) return null
  return { dx, dz, jump: collided || node.y - pos.y > 0.5 }
}

// on: the block I stand in, above: the bounding box two cells up. A bed is 0.56 high, so a ceiling 2 above the floor leaves 1.44: nobody fits
export const bedTrap = (on, above) => /_bed$/.test(on ?? '') && above === 'block'
  ? 'you are standing ON a bed under a low ceiling (1.4 blocks of headroom, nobody fits, so no walk can start): dig the bed, walk out, place it back. For good: leave one free floor cell beside the bed, or raise the ceiling over it by one'
  : null

// beds: nearest first. A bed in another agent's zone is theirs (zones are named owner-something; starter-* is shared) and a bed holds one sleeper
export function bedChoice (beds, zones, me, any = false, occupied = new Set()) {
  if (!beds.length) return { error: 'no bed within 32 blocks' }
  const theirs = bed => zones.find(z => inAnyZone([z], bed) && !new RegExp(`^(${me.toLowerCase()}|starter)-`).test(z.name))
  const free = beds.filter(b => !occupied.has(`${b.x},${b.y},${b.z}`))
  const bed = any ? free[0] : free.find(b => !theirs(b))
  if (bed) return { bed }
  const spare = 'Place your own (spare beds are in the starter chest at 112,70,-138)'
  if (any || beds.some(b => !theirs(b))) return { error: `every bed you may use within 32 blocks is occupied: a bed holds one sleeper. ${spare}` }
  return { error: `the only bed within 32 blocks is in ${theirs(beds[0]).name}: that is their bed, and a bed holds one sleeper. ${spare}, or sleep any=true if they invited you` }
}
// the bedtime reflex failed with this error: what to tell the driver (null: nothing, the driver's own order took over)
export const bedtimeReport = error => /^cancelled: superseded/.test(error)
  ? null
  : /monsters nearby/.test(error) ? `${error}: the server lets nobody sleep with a monster within 8 blocks of the bed. Kill it (attack mob=<its name>) and sleep again, or wait it out indoors; walls and light around the bed keep them off` : error

// a fluid is not a block: the server never sends a break for one, so bot.dig on water sat at doing=dig for 167 seconds (#110)
export const FLUIDS = new Set(['water', 'lava', 'flowing_water', 'flowing_lava', 'bubble_column'])
const fluidCure = 'Scoop the source with fill x= y= z= (an empty bucket), or fill the cell in with place item=dirt'

// target: the block to dig. above: the names of the 3 blocks over it. Water beside it is fine (a trench by a pond); water over it means a dive
export const digRefusal = (target, above, allowWet) => {
  if (FLUIDS.has(target)) {
    const burns = target === 'lava' || target === 'flowing_lava'
    return `${target} is a fluid, not a block: digging it never finishes (one such dig ran 167 seconds before it was cancelled)${burns ? ', and it burns whatever reaches into it' : ''}. ${fluidCure}`
  }
  return !allowWet && above.includes('water')
    ? 'that block is under water: the body would dive for it and run out of air. Work from the shore (dig down beside it, or drain it with sand or dirt first), or pass wet=true if it is shallow and you watch your air'
    : null
}

// The pathfinder climbs and bridges with any placeable block it carries, and spent Chani's cobblestone twice without a word.
// cells: what a walk built, {x,y,z,name}. feet: where the body stands now. Which of them it can take back without dropping itself
export const scaffoldTakeBack = (cells, feet) => cells.filter(c =>
  !(c.x === Math.floor(feet.x) && c.z === Math.floor(feet.z)) &&
  Math.abs(c.y - Math.floor(feet.y)) <= 3 &&
  Math.hypot(c.x + 0.5 - feet.x, c.z + 0.5 - feet.z) <= 4.5)

// tried: every cell the pathfinder aimed a placement at (it retries one cell several times a tick, and its own place call
// rejects over blocks the server did put down). What it really built is whatever now stands in those cells
export const scaffoldBuilt = (tried, nameAt) => {
  const seen = new Set()
  return tried.filter(c => {
    const key = `${c.x},${c.y},${c.z}`
    if (seen.has(key)) return false
    seen.add(key)
    return true
  }).map(c => ({ ...c, name: nameAt(c) })).filter(c => c.name && !isAir(c.name))
}

const countLine = counts => Object.entries(counts).map(([name, n]) => `${name}:${n}`).join(' ')

// spent/taken: {cobblestone: 4}. left: the cells still standing. Reported beside the drops, so vanishing stone has a reason
export const scaffoldNote = (spent, taken, left) => {
  if (!Object.keys(spent).length) return null
  const back = Object.keys(taken).length ? `dug back ${countLine(taken)} (a drop that falls off a height is left below)` : 'dug none back'
  const where = left.length
    ? `; ${left.length} still ${left.length > 1 ? 'stand' : 'stands'} at ${left.slice(0, 6).map(c => `${c.x},${c.y},${c.z}`).join(' ')}${left.length > 6 ? ' ...' : ''}: dig ${left.length > 1 ? 'them' : 'it'} when you pass`
    : ''
  return `${countLine(spent)} went into the towers and bridges the walk built (the pathfinder climbs with whatever placeable block you carry); ${back}${where}`
}

// clear sweeps a box: one fluid cell in it would hang the whole sweep, so they are skipped and named. counts: {water: 3}
export const fluidsLeft = counts => {
  const named = Object.entries(counts).sort((a, b) => b[1] - a[1]).map(([name, n]) => `${name} x${n}`)
  return named.length ? `${named.join(', ')} left in the box: a fluid cannot be dug. ${fluidCure}` : null
}

// drops: {id, dist, deep}, deep = water with water under it, a swim. Each drop gets one try
export const nextDrop = (drops, tried, allowWet) =>
  drops.filter(d => !tried.has(d.id) && (allowWet || !d.deep)).sort((a, b) => a.dist - b.dist)[0]

export function coordsError (a, withY = true) {
  const axes = withY ? ['x', 'y', 'z'] : ['x', 'z']
  if (!axes.every(k => typeof a[k] === 'number' && Number.isFinite(a[k]))) return `${withY ? 'x, y and z' : 'x and z'} must be numbers (got ${axes.map(k => `${k}=${a[k]}`).join(' ')})`
  return withY && (a.y < -64 || a.y > 319) ? `y=${a.y} is outside the world (-64 to 319): x and y swapped?` : null
}

// stacks: the sizes of the stacks of this item I already carry; batch: how many one craft makes
// made/count: asked again before every batch, because the stack that had room at the start fills up on the way
export const craftRoom = ({ freeSlots, stacks, stackSize, batch, item, made = 0, count }) => freeSlots > 0 || stacks.some(n => n + batch <= stackSize)
  ? null
  : `${made ? `${made}/${count} made, then ` : ''}your inventory is full and ${item} has nowhere to go (the craft would eat the ingredients and drop or lose the result): toss or deposit something first`

// scan where=<name, * wildcards>: the cells themselves, x then z then y ascending, so nobody has to count columns in the picture
export function scanWhere (nameAt, { x1, y1, z1, x2, y2, z2 }, where, limit = 20) {
  const wanted = new RegExp('^' + String(where).replace(/\*/g, '.*') + '$')
  const range = (a, b) => Array.from({ length: Math.abs(b - a) + 1 }, (_, i) => Math.min(a, b) + i)
  const hits = range(y1, y2).flatMap(y => range(z1, z2).flatMap(z => range(x1, x2).filter(x => wanted.test(nameAt(x, y, z))).map(x => `${String(where).includes('*') ? nameAt(x, y, z) + '@' : ''}${x},${y},${z}`)))
  if (!hits.length) return `${where} 0`
  return `${where} ${hits.length}: ${hits.slice(0, limit).join(' ')}${hits.length > limit ? ` (+${hits.length - limit} more)` : ''}`
}

// what a hurt event cannot show by naming who is nearby. fell: blocks dropped just before; sinceCreeperMs: since a creeper was last seen close
export function hurtCause ({ lost, nearby, sinceCreeperMs, fell, food, oxygen, fledFrom, sinceFledMs }) {
  if (nearby.length) return null
  if (oxygen <= 0) return 'drowning: get to air'
  if (fell >= 4) return `a fall of ${fell} blocks`
  if (lost >= 5 && sinceCreeperMs < 5000) return 'a creeper blew up (it is gone now)'
  if (food <= 0) return 'starving: eat'
  return fledFrom && sinceFledMs < 30000 ? `hit while the body fled from a ${fledFrom} by itself: that run is why you have moved` : null
}

// Item 14 (Perrin, BUGS.md 09-23). A block that comes back null is not air and not stone: it is a chunk this body has
// never been sent, which is every chunk more than a view away. `pen.check` on a pen 200 blocks off answered "not a spot
// to stand on", blaming his coordinates for a world his client had never seen, and the role's own case (fetch from the
// shared stock to your own pen) starts exactly there. Nothing may be guessed from an unloaded chunk: say so, or go.
export const outOfSight = (block, at, from) => block
  ? null
  : `${at.x},${at.y},${at.z} is too far to see: ${from ? `it is ${Math.round(Math.hypot(at.x - from.x, at.y - from.y, at.z - from.z))} blocks off and ` : ''}that chunk is not loaded, so nothing there can be read. goto it first, then ask again`

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

// floor: the "x,y,z" cells pen.check walked; animals: {name,x,y,z}. In = standing in a column of the pen, whatever the height
const onFloor = floor => {
  const columns = new Set(floor.map(k => k.split(',').filter((_, i) => i !== 1).join(',')))
  return p => columns.has(`${Math.floor(p.x)},${Math.floor(p.z)}`)
}
// the animals that are not already standing in the pen (floor = penLeak's floor cells, null when the goal is no pen)
export const unpenned = (floor, animals, posOf) => floor ? animals.filter(a => !onFloor(floor)(posOf(a))) : animals
// a pen that leaks (penLeak's via) through nothing but an open gate: where that gate is, so lead can shut it and see the pen
export function gateLeak (via, blockAt) {
  const spots = via.split(' ')
  if (spots.length !== 1) return null
  const [x, y, z] = spots[0].split(',').map(n => Math.floor(Number(n)))
  const block = blockAt(x, y, z)
  return block?.open && /_fence_gate$/.test(block.name) ? [x, y, z] : null
}
// where to stand in a pen so that animals following 2.5 blocks behind end up inside it: the floor cell furthest from them
export function deepestCell (floor, from) {
  const far = ([x, , z]) => (x + 0.5 - from.x) ** 2 + (z + 0.5 - from.z) ** 2
  return floor.map(k => k.split(',').map(Number)).sort((a, b) => far(b) - far(a))[0]
}
// farm animals that are NOT on the pen floor but within a few blocks of its gate: the ones that slipped out with whoever just walked through
export function strays (floor, animals, [gx, , gz], within = 6) {
  const out = animals.filter(a => !onFloor(floor)(a) && Math.hypot(a.x - gx - 0.5, a.z - gz - 0.5) <= within)
  return out.length ? out.map(a => `${a.name}@${Math.floor(a.x)},${Math.floor(a.y)},${Math.floor(a.z)}`).join(' ') : null
}
export function penCensus (floor, animals) {
  const isIn = onFloor(floor)
  const counts = animals.filter(isIn).reduce((n, a) => ({ ...n, [a.name]: (n[a.name] ?? 0) + 1 }), {})
  const inside = Object.entries(counts).map(([name, n]) => `${name}:${n}`).join(' ')
  const outside = animals.filter(a => !isIn(a)).map(a => `${a.name}@${Math.floor(a.x)},${Math.floor(a.y)},${Math.floor(a.z)}`).join(' ')
  return { ...(inside ? { inside } : {}), ...(outside ? { outside } : {}) }
}

// mineflayer-pathfinder 2.4.5, monitorMovement: after opening a gate it takes the next thing to place, and when there is none "placing" stays true:
// the next tick reads placingBlock.y of undefined, every tick, until the path is reset. Only bites with a scaffolding block in the inventory
const GATE_SHIFT = "          placingBlock = nextPoint.toPlace.shift()\n        }, err => {\n"
const GATE_GUARD = "          placingBlock = nextPoint.toPlace.shift()\n          if (!placingBlock) placing = false // patched by bot/patch-deps.mjs\n        }, err => {\n"
export const patchPathfinder = source => source.includes(GATE_GUARD)
  ? { status: 'already', source }
  : source.includes(GATE_SHIFT) ? { status: 'patched', source: source.replace(GATE_SHIFT, GATE_GUARD) } : { status: 'anchor missing', source }

// prismarine-item, `get enchants`: with item components it returns the raw data ({enchantments: [{id, level}]}) instead of the [{name, lvl}] list that mineflayer's
// digTime (`enchantments.concat`) and everyone else expect: an enchanted tool in hand broke harvest and made digs crawl (Kettricken, right after the first ./mc enchant)
const ENCHANTS_RAW = "        return this.componentMap.get('enchantments').data\n"
const ENCHANTS_LIST = "        const raw = this.componentMap.get('enchantments').data // patched by patch-deps.mjs\n        return (Array.isArray(raw) ? raw : raw?.enchantments ?? []).map(e => ({ lvl: e.level ?? e.lvl, name: e.name ?? registry.enchantments[e.id]?.name ?? `unknown_${e.id}` }))\n"
export const patchItemEnchants = source => source.includes(ENCHANTS_LIST)
  ? { status: 'already', source }
  : source.includes(ENCHANTS_RAW) ? { status: 'patched', source: source.replace(ENCHANTS_RAW, ENCHANTS_LIST) } : { status: 'anchor missing', source }

// mineflayer-auto-eat marks itself eating BEFORE it equips the food and only unmarks after the meal: an equip that throws leaves it "eating" for
// ever and the body starves with bread in its pockets (Jizo). A meal takes 1.6 s
export const eatJammed = eatingForMs => eatingForMs >= 15000

// One fact, said once. After the 09-22 20:53 server restart Perrin's and Mariel's bodies wrote the same uncaught error
// into their events files every few seconds until the file was unreadable, and the one line that mattered (the restart)
// was buried under thousands of copies of itself. So the first of a message is said, the repeats are counted silently,
// and the count is said when the message changes or the window runs out. `seen` is opaque state: keep it, pass it back.
export function errorRepeat (seen, message, now, window = 60000) {
  if (seen?.message !== message) return { say: message, seen: { message, said: now, suppressed: 0 } }
  const suppressed = seen.suppressed + 1
  if (now - seen.said < window) return { say: null, seen: { ...seen, suppressed } }
  return { say: `${message} (${suppressed} more in the last ${Math.round((now - seen.said) / 1000)}s)`, seen: { message, said: now, suppressed: 0 } }
}
// what water costs a walk: a digging one tunnelled into an underground lake and half drowned in its own shaft (Aviendha)
export const waterWary = dig => dig ? { liquidCost: 40, infiniteLiquidDropdownDistance: false } : { liquidCost: 1, infiniteLiquidDropdownDistance: true }

// what stepping into this block costs a walk, in steps: pen gates are doors for those with business in the pen, not shortcuts (a cow left Dan's pen with my mine)
// 8, not 30: every extra point widens the search (30 made a 9-step walk into my paddock visit 1308 nodes, and Ganesha's walks back to their pen over
// hilly ground ran into the 5 s limit: `lead` arrived with=0 three times). Two gates = 16: a pen is still no shortcut unless the way round is longer than that
export const gateStepCost = name => name?.endsWith('_fence_gate') ? 8 : 0

// will this bed let me go in the morning? The server wakes a sleeper ON the bed when it finds no better spot, 0.56 above the floor, and from there
// a cell with a block 2 above its floor cannot be entered (Aviendha, every morning). exits: the cells around the bed, { at: 'x,y,z', free, lintel }
export function bedExit (exits) {
  if (!exits.length || exits.some(e => e.free && !e.lintel)) return null
  const low = exits.find(e => e.free)
  if (!low) return 'this bed has no free cell beside it: you will wake up standing ON it with nowhere to step. Clear one cell next to it (feet and head, and the block above those)'
  const [x, y, z] = low.at.split(',').map(Number)
  return `this bed is a trap: you wake up standing ON it (0.56 high) and the only free cell beside it (${low.at}) has a block 2 above its floor, which leaves 1.44: nobody fits and no walk will start. Dig the block at ${x},${y + 2},${z} (or move the bed next to a cell with 3 of headroom). In the morning, if stuck: dig the bed, walk out, place it back`
}

// what a code_updated notice was about: the same files edited AGAIN are news again (a body told once was never told of the later batches)
export const staleKey = (stale, mtimes) => `${stale.join()}@${Math.max(...stale.map(f => mtimes[f]))}`

// mineflayer says "destination full" for a full chest AND for full pockets: say which side
export const fullSide = (message, way) => !/destination full|inventory is full/i.test(message)
  ? message
  : way === 'withdraw'
    ? 'YOUR INVENTORY is full: what fitted was taken (the + above). deposit or toss something, then withdraw the rest'
    : 'the CHEST is full: what fitted went in (the - above). Put the rest in another chest, or take out what does not belong here'

// mine started inside a stocked pen: it digs down from where the body stands and the shaft stays (mine in Dan's starter pen, 09-19)
export const penShaftRefusal = (inside, force) => inside && !force
  ? `you stand in a pen with animals in it (${inside}): mine digs its way down from where you stand and would leave a shaft for them to fall into. Walk out through the gate first, then mine (force=true if you really mean it, and fill the hole after)`
  : null

// the ground mine broke open around its start and left open: what to put back, with what. solid(name) says whether a block was ground at all
const FILLERS = ['dirt', 'cobblestone', 'cobbled_deepslate', 'stone', 'andesite', 'diorite', 'granite', 'netherrack']
export function holesLeft (before, after, carried, solid) {
  const item = FILLERS.find(f => carried.includes(f))
  if (!item) return []
  return Object.keys(before).filter(k => solid(before[k]) && !solid(after[k])).map(k => { const [x, y, z] = k.split(',').map(Number); return { x, y, z, item } })
}

// leading: has the herd come through this gate? Every follower must be nearer to me than the gate is, with a block to spare. Until then the gate stays open;
// after that it is shut AT ONCE, not at the end of the lead (Ganesha's body walked 170 blocks back to shut two)
const gap = (a, b) => Math.hypot(a[0] - b[0], a[1] - b[1], a[2] - b[2])
export const herdPassed = (me, gate, animals) => animals.every(a => gap(a, me) < gap(gate, me) - 1)
// gates still open when a task ends: the ones within reach get shut, the far ones are only named (no silent cross-country walk)
export const gatesByReach = (gates, me, reach = 32) => ({ near: gates.filter(g => gap(g, me) <= reach), far: gates.filter(g => gap(g, me) > reach).map(g => g.join(',')).join(' ') })

// which animal a lead goes for: nearest first, but one standing in a pen belongs to somebody (my lead went for Aviendha's cow, 60 blocks off)
export function leadPick (candidates, allowPenned) {
  const pick = candidates.find(c => allowPenned || !c.penned)
  if (pick) return { id: pick.id }
  return { error: candidates.length ? `the only ones in range stand in a pen (nearest at ${candidates[0].at}): they are somebody's. penned=true takes one anyway: only from the starter pen or a pen of your own` : 'none in range' }
}

// after cutting stalks: the bases that are gone all the same, and which of them I can plant again from my pockets (the cut itself never takes a base: isStalkCut)
export function stalkReplant (cut, carried) {
  const gone = cut.filter(c => c.baseNow !== c.stalk)
  return { plant: gone.filter(c => carried.includes(c.stalk)).map(c => ({ x: c.base[0], y: c.base[1], z: c.base[2], item: c.stalk })), lost: gone.length }
}

// ---------------------------------------------------------------- composite actions: plans
// A plan is an ASCII map of a farm or pen, one character per block, anchored at its NORTH-WEST corner: rows run south (z),
// columns east (x). `y` is the crop/floor level, so the ground under a cell is y-1: a crop stands at y on farmland at y-1,
// water lies AT y-1, a fence stands at y on dirt. The plan is the truth of what SHOULD be there; the world is what is.
// `ground` is what must lie at y-1; `seed` is the item a crop is planted from; `item` the block a structure is placed from.
export const PLAN_LEGEND = {
  w: { kind: 'crop', crop: 'wheat', seed: 'wheat_seeds', ground: 'farmland' },
  c: { kind: 'crop', crop: 'carrots', seed: 'carrot', ground: 'farmland' },
  p: { kind: 'crop', crop: 'potatoes', seed: 'potato', ground: 'farmland' },
  b: { kind: 'crop', crop: 'beetroots', seed: 'beetroot_seeds', ground: 'farmland' },
  s: { kind: 'crop', crop: 'sugar_cane', seed: 'sugar_cane', ground: 'sand' },
  m: { kind: 'crop', crop: 'melon_stem', seed: 'melon_seeds', ground: 'farmland' },
  k: { kind: 'crop', crop: 'pumpkin_stem', seed: 'pumpkin_seeds', ground: 'farmland' },
  B: { kind: 'crop', crop: 'bamboo', seed: 'bamboo', ground: 'dirt' },
  // a channel is built COVERED: a slab laid in the source keeps the water (and the farmland wet) but leaves a floor to
  // walk on. Open water in a field is a trap - the body wades in, `dig` refuses every block beside it, and the
  // pathfinder will not cross it, which is how Chani ended up walled into her own plan
  '~': { kind: 'water', ground: 'water', cover: 'oak_slab' },
  '.': { kind: 'path', ground: 'dirt' },
  '#': { kind: 'fence', item: 'oak_fence', ground: 'dirt' },
  G: { kind: 'gate', item: 'oak_fence_gate', ground: 'dirt' },
  T: { kind: 'torch', item: 'oak_fence', ground: 'dirt' },
  C: { kind: 'chest', item: 'chest', ground: 'dirt' },
  K: { kind: 'composter', item: 'composter', ground: 'dirt' },
  F: { kind: 'flower', item: 'dandelion', ground: 'grass_block' },
  t: { kind: 'sapling', item: 'oak_sapling', ground: 'dirt' },
  A: { kind: 'table', item: 'crafting_table', ground: 'dirt' }
}
const PLAN_MAX = 64

// the ASCII map as rows and cells; a space is a hole in the plan, not a cell
export function parsePlan (map) {
  const rows = String(map ?? '').replace(/\t/g, ' ').split('\n')
  const first = rows.findIndex(r => r.trim())
  if (first < 0) return { error: 'the map has no cells: give rows of legend characters, one character per block' }
  const last = rows.length - [...rows].reverse().findIndex(r => r.trim())
  const kept = rows.slice(first, last).map(r => r.replace(/\s+$/, ''))
  const width = Math.max(...kept.map(r => r.length))
  if (width > PLAN_MAX || kept.length > PLAN_MAX) return { error: `a plan is at most ${PLAN_MAX}x${PLAN_MAX} blocks (got ${width}x${kept.length})` }
  return { rows: kept, width, height: kept.length, cells: kept.flatMap((row, dz) => [...row].flatMap((ch, dx) => ch === ' ' ? [] : [{ dx, dz, ch }])) }
}

// Every cell in world coordinates: x east of the anchor, z south of it, y the GROUND block — the farmland, pen floor
// or path the plan describes, the level `till` asks for. What the plan puts on it (crop, fence, gate, torch, chest,
// composter, flower, sapling) stands at y+1; a water source lies AT y, with its cover at y+1.
export const planCells = place => (parsePlan(place.plan).cells ?? []).map(c => ({ ...c, x: place.x + c.dx, y: place.y, z: place.z + c.dz }))

// farmland stays wet within 4 blocks of a water source, level with it or one above it: a plan that breaks that rule
// turns back into dirt within minutes of being built. A gate in a corner is one nothing can ever walk through (see blindGates)
const BARRIER_KINDS = new Set(['fence', 'gate'])
export function planErrors (parsed) {
  if (parsed.error) return [parsed.error]
  const unknown = parsed.cells.filter(c => !PLAN_LEGEND[c.ch])
  if (unknown.length) return unknown.map(c => `${c.ch} at ${c.dx},${c.dz} is not in the legend (${Object.keys(PLAN_LEGEND).join(' ')})`)
  const waters = parsed.cells.filter(c => PLAN_LEGEND[c.ch].kind === 'water')
  const dry = parsed.cells.filter(c => PLAN_LEGEND[c.ch].ground === 'farmland' &&
    !waters.some(w => Math.abs(w.dx - c.dx) <= 4 && Math.abs(w.dz - c.dz) <= 4))
  const at = (dx, dz) => parsed.cells.find(c => c.dx === dx && c.dz === dz)
  const kindAt = (dx, dz) => PLAN_LEGEND[at(dx, dz)?.ch]?.kind ?? null
  const walkable = kind => kind && !BARRIER_KINDS.has(kind)
  const blind = parsed.cells.filter(c => PLAN_LEGEND[c.ch].kind === 'gate').filter(g =>
    ![[1, 0], [-1, 0], [0, 1], [0, -1]].some(([dx, dz]) =>
      walkable(kindAt(g.dx + dx, g.dz + dz)) && !BARRIER_KINDS.has(kindAt(g.dx - dx, g.dz - dz))))
  // one complaint for the whole dry patch: a plan that forgot its channel used to answer with a line per cell
  const dryLine = dry.length
    ? [`${dry.length} cell${dry.length === 1 ? ' is' : 's are'} farmland with no water within 4 blocks (${dry.slice(0, 4).map(c => `${c.dx},${c.dz}`).join(' ')}${dry.length > 4 ? ` and ${dry.length - 4} more` : ''}): move the channel or shorten the row`]
    : []
  return [
    ...dryLine,
    ...blind.map(g => `the gate at ${g.dx},${g.dz} is in a corner: nothing can walk through it. Put it in the middle of a wall`)
  ]
}

// what it takes to build this plan from nothing: one water bucket does the whole field, a torch cell needs its post too
export function planBill (parsed) {
  const bill = {}
  const add = (item, n = 1) => { bill[item] = (bill[item] ?? 0) + n }
  for (const { ch } of parsed.cells ?? []) {
    const cell = PLAN_LEGEND[ch]
    if (!cell) continue
    if (cell.kind === 'crop') add(cell.seed)
    if (cell.cover) add(cell.cover)
    if (cell.kind === 'torch') add('torch')
    if (cell.item) add(cell.item)
  }
  if ((parsed.cells ?? []).some(c => PLAN_LEGEND[c.ch]?.kind === 'water')) add('water_bucket')
  return bill
}

// one line: how big it is and what is in it
export function planSummary (parsed) {
  if (parsed.error) return parsed.error
  const counts = {}
  for (const { ch } of parsed.cells) {
    const cell = PLAN_LEGEND[ch]
    const label = cell?.kind === 'crop' ? cell.crop.replace(/s$/, '') : cell?.kind ?? ch
    counts[label] = (counts[label] ?? 0) + 1
  }
  return `${parsed.width}x${parsed.height} ${compact(counts)}`
}

// ---------------------------------------------------------------- composite actions: what a farm needs
// worldAt(x,y,z) answers { name, properties } or null (not loaded). Both of these are read-only judgements: no walking, no digging.
// what stands on a farm right now, judged against its plan
export function fieldCensus (cells, worldAt) {
  const out = { crops: {}, cells: cells.length, ripe: 0, growing: 0, empty: 0, untilled: 0, dry: 0 }
  for (const cell of cells) {
    const spec = PLAN_LEGEND[cell.ch]
    if (!spec) continue
    const ground = worldAt(cell.x, cell.y, cell.z)
    const here = worldAt(cell.x, cell.y + 1, cell.z)
    if (spec.kind === 'water') {
      if (ground && !holdsWater(ground)) out.dry++
      // open water in a field is a hole: the body wades in, `dig` refuses the blocks beside it and no walk will cross it
      else if (ground?.name === 'water') out.open = (out.open ?? 0) + 1
      continue
    }
    if (spec.kind !== 'crop') continue
    if (spec.ground === 'farmland' && ground && ground.name !== 'farmland') out.untilled++
    if (here?.name !== spec.crop) { out.empty++; continue }
    out.crops[spec.crop] = (out.crops[spec.crop] ?? 0) + 1
    if (ripeCrop(here.name, here.properties?.age)) out.ripe++
    else out.growing++
  }
  return out
}

// A plan whose y is one off reads as a field of empty, untilled beds (Chani's wheat field: 28 wheat stood one block
// above where the code looked) or has a build dig the turf out and lay its floor one lower (her sheep pen). A plan's y
// is the GROUND block, and this answers `off`: what to add to that y to reach the level the world is really at.
// Two ways to tell, in order:
//  - the world's own copy of the plan, standing one block up or one down. Water is no evidence of a shift on its own
//    (a channel reads as water either way), and one matching block is a coincidence, so it takes two.
//  - nothing built there yet, but every cell the plan names is open air (or grass) over solid ground: that is the level
//    you STAND on, one above the ground block a plan wants.
const anchorHit = (spec, here) => {
  if (!here) return false
  if (spec.kind === 'crop') return here.name === spec.crop
  if (spec.kind === 'gate') return here.name.endsWith('_fence_gate')
  return Boolean(spec.item) && here.name === spec.item
}
const CONVENTION = "a plan's y is the GROUND block (the farmland, pen floor or path itself; crops, fences, gates, chests and a water cover stand at y+1)"
export const isAir = name => /^(air|cave_air|void_air)$/.test(String(name))
// water still stands in a cell whose block was waterlogged (a slab or stairs laid into the source): the farmland beside
// it stays wet, so a covered channel is a full channel
export const holdsWater = block => Boolean(block) && (block.name === 'water' || String(block.properties?.waterlogged) === 'true')
// what you can stand in: air, or the grass and flowers that grow on open ground
const isOpenCell = name => isAir(name) || isGroundCover(name) || WEEDS.has(name)
// what you can stand on
const isFooting = name => Boolean(name) && !isOpenCell(name) && name !== 'water' && name !== 'lava'
function freshGround (cells, worldAt, y) {
  const seen = cells.filter(c => PLAN_LEGEND[c.ch] && worldAt(c.x, c.y, c.z))
  const standing = seen.filter(c => isOpenCell(worldAt(c.x, c.y, c.z).name) && isFooting(worldAt(c.x, c.y - 1, c.z)?.name))
  if (seen.length < 2 || standing.length !== seen.length) return { off: 0 }
  return {
    off: -1,
    fresh: standing.length,
    note: `the plan says y=${y}, but all ${standing.length} of its cells are open air over solid ground at y=${y - 1}: you gave the level you stand on. ${CONVENTION}, so re-save it with y=${y - 1}`
  }
}
// planAnchor looks one block up and one block down, and nowhere to the side: a pen ring standing a few cells ACROSS
// from its plan is bare ground as far as it can tell. Chani's pen was marked 2 east and 3 south of the ring it
// describes, so pen.build read an empty field, laid half a second ring through the middle of the first and let 4
// sheep out (2026-09-23 02:55Z). This is the sideways half of the same question: where does the thing the plan
// describes actually stand? Answers null unless a shift within `reach` explains more than half the plan's solid
// cells and more of them than the plan's own spot does - a neighbour's wall brushing the plan is not its own ring.
const compassOf = (dx, dz) => [dx && `${Math.abs(dx)} ${dx < 0 ? 'west' : 'east'}`, dz && `${Math.abs(dz)} ${dz < 0 ? 'north' : 'south'}`]
  .filter(Boolean).join(' and ')
export function planBeside (cells, worldAt, { name = 'the place', reach = 3 } = {}) {
  const solidCells = cells.filter(c => PLAN_LEGEND[c.ch] && PLAN_LEGEND[c.ch].kind !== 'path' && PLAN_LEGEND[c.ch].kind !== 'water')
  if (solidCells.length < 6) return null
  const score = (dx, dy, dz) => solidCells.filter(c => anchorHit(PLAN_LEGEND[c.ch], worldAt(c.x + dx, c.y + 1 + dy, c.z + dz))).length
  // the plan's own spot, judged the way planAnchor judges it: a build that is simply unfinished is not a plan in the
  // wrong place, and a plan that already stands where it says is not searched for at all (the whole of a big farm, every shift)
  const here = Math.max(...[0, -1, 1].map(dy => score(0, dy, 0)))
  if (here === solidCells.length) return null
  const shifts = range(-reach, reach).flatMap(dx => range(-reach, reach).flatMap(dz =>
    dx === 0 && dz === 0 ? [] : [0, -1, 1].map(dy => ({ dx, dy, dz, found: score(dx, dy, dz) }))))
  const away = s => Math.abs(s.dx) + Math.abs(s.dz) + Math.abs(s.dy)
  const best = [...shifts].sort((a, b) => b.found - a.found || away(a) - away(b))[0]
  if (!best || best.found < Math.max(6, solidCells.length / 2) || best.found <= here) return null
  const at = { x: Math.min(...cells.map(c => c.x)) + best.dx, y: cells[0].y + best.dy, z: Math.min(...cells.map(c => c.z)) + best.dz }
  return {
    ...best,
    at,
    note: `${best.found} of the ${solidCells.length} blocks it describes stand ${compassOf(best.dx, best.dz)} of where the plan puts them, on ground at y=${at.y}: what the plan describes is already built, just not where the place is marked. A place's x,z is the NORTH-WEST corner cell of its plan (its lowest x and lowest z) and its y is the GROUND block, so mark it where the thing itself stands and build again: ./mc mark name=${name} x=${at.x} y=${at.y} z=${at.z} (marking again keeps the plan). If you meant to build a SECOND one here, move the plan further off: this one would run through the middle of what stands`
  }
}
export function planAnchor (cells, worldAt) {
  const solidCells = cells.filter(c => PLAN_LEGEND[c.ch] && PLAN_LEGEND[c.ch].kind !== 'path' && PLAN_LEGEND[c.ch].kind !== 'water')
  const score = dy => solidCells.filter(c => anchorHit(PLAN_LEGEND[c.ch], worldAt(c.x, c.y + 1 + dy, c.z))).length
  const here = score(0)
  const best = [{ dy: 1, n: score(1) }, { dy: -1, n: score(-1) }].sort((a, b) => b.n - a.n)[0]
  const y = cells[0]?.y
  if (!best || best.n < 2 || best.n <= here) return cells.length ? freshGround(cells, worldAt, y) : { off: 0 }
  return {
    off: best.dy,
    found: best.n,
    note: `the plan says y=${y}, but ${best.n} of the blocks it describes stand at y=${y + best.dy + 1}: ${CONVENTION}, so re-save it with y=${y + best.dy}`
  }
}

// the work that would put a farm back the way its plan says, in the order it has to happen: clear the bed, till it,
// refill the channel, plant, then build what is missing. A cell with somebody's block on it is left alone.
// what may be broken to clear a crop bed: weeds and the flowers that spring up around bone meal. A crop cell holding anything
// else (cobblestone, someone's torch) is left alone and shows up as `empty` in the census instead.
const WEEDS = new Set(['short_grass', 'tall_grass', 'fern', 'large_fern', 'dead_bush', 'snow', 'dandelion', 'poppy', 'cornflower',
  'oxeye_daisy', 'azure_bluet', 'blue_orchid', 'allium', 'lily_of_the_valley', 'red_tulip', 'orange_tulip', 'white_tulip', 'pink_tulip',
  'bush', 'firefly_bush', 'leaf_litter', 'wildflowers', 'short_dry_grass', 'tall_dry_grass'])
const JOB_ORDER = ['skip', 'clear', 'till', 'pour', 'cover', 'plant', 'place']
// A bed tilled and left bare goes back to dirt: dry within minutes, and any of it the moment something jumps on it.
// A field tilled in one pass and sown in the next loses the beds the body walked back over (15 of 28, round 2 item 3),
// so every till is followed at once by the planting of its own cell, and the walk does each bed once.
const sowAsTilled = jobs => {
  const plants = new Map(jobs.filter(j => j.do === 'plant').map(j => [`${j.x},${j.y},${j.z}`, j]))
  const sown = new Set()
  const out = []
  for (const job of jobs) {
    if (job.do === 'plant' && sown.has(job)) continue
    out.push(job)
    const after = job.do === 'till' ? plants.get(`${job.x},${job.y + 1},${job.z}`) : null
    if (!after) continue
    out.push(after)
    sown.add(after)
  }
  return out
}

// what a hand-tilled bed has to be told, whether or not it has water: nothing keeps bare farmland
export const tillWarning = (dry, total) => dry
  ? `${dry} of ${total} have no water within 4 blocks (level with them or one up): plant them AT ONCE or they turn back to dirt within minutes; to keep a field, pour water beside it`
  : 'plant them now: bare farmland turns back to dirt the moment anything jumps on it, and a field tilled in one pass and sown in the next loses the beds it walked back over'
export function farmJobs ({ cells, worldAt, items = {} }) {
  const jobs = []
  const push = (job, item) => jobs.push({ ...job, ...(item ? { item, have: (items[item] ?? 0) > 0 } : {}) })
  // A farm's own channel drowns the work beside it: `dig` refuses a block with water in the three above it (the body
  // would dive for it and run out of air), and Chani's farm.maintain gave that up as "twice in a row" on the block
  // under her own channel. A cell like that is never dug: it is listed as skipped, with what would have to happen first.
  const drowned = (x, y, z) => [1, 2, 3].some(dy => worldAt(x, y + dy, z)?.name === 'water')
  const clear = (job, item) => drowned(job.x, job.y, job.z)
    ? (jobs.push({ ...job, do: 'skip', why: `${job.why}, and water stands over it: drain it or dig it from the shore first` }), true)
    : (push(job, item), false)
  for (const cell of cells) {
    const spec = PLAN_LEGEND[cell.ch]
    if (!spec) continue
    // the plan's y is the ground the cell is made of; what the plan puts on it stands one above
    const ground = worldAt(cell.x, cell.y, cell.z)
    const here = worldAt(cell.x, cell.y + 1, cell.z)
    const standing = here && here.name !== 'air' ? here.name : null
    if (spec.kind === 'water') {
      // a cell nobody can see is nobody's job; a slab already laid in the source is a finished channel, water and floor both
      if (!ground || (holdsWater(ground) && ground.name !== 'water')) continue
      // nothing of a dry channel is worth starting without the water: the dig leaves a pit and the cover lays a slab
      // on bare ground. One bucket does the whole field, so this asks only whether any water is carried at all
      const dry = !holdsWater(ground)
      if (dry && !((items.water_bucket ?? 0) > 0)) {
        // and a cell already dug out (the bucket emptied between the dig and the pour) is filled back in rather than
        // left as the pit Chani could not path past. What is missing is still the water, so that is what is reported
        const hole = isAir(ground.name)
        if (hole) push({ do: 'fill', x: cell.x, y: cell.y, z: cell.z, why: `the channel at ${cell.x},${cell.y},${cell.z} is an open hole I carry no water to fill` }, 'dirt')
        jobs.push({ do: 'skip', x: cell.x, y: cell.y, z: cell.z, item: 'water_bucket', have: false, why: `the channel at ${cell.x},${cell.y},${cell.z} is dry and I carry no water: ${hole ? 'filled back in rather than left as a hole' : 'fill a bucket first'}` })
        continue
      }
      if (dry) {
        // a channel somebody walked over and filled in: dig the cell out before pouring, or the water lands on the ground beside it
        // (and pouring onto a bed that could not be dug out would only put the water one block too high)
        // the dig and the pour are one job in two halves: a body with no water in hand must not open the hole it
        // cannot fill. Carrying water_bucket on the DIG stops it at the bill and again when the job runs, which is
        // the case that left a pit in Chani's field (one bucket bills for a whole field but empties on first use)
        if (ground.name !== 'air' && clear({ do: 'clear', x: cell.x, y: cell.y, z: cell.z, why: `${ground.name} where the channel should be` }, 'water_bucket')) continue
        // `pour` names the solid block to pour ONTO and the water lands one above it: the source belongs at y, so pour onto y-1
        push({ do: 'pour', x: cell.x, y: cell.y - 1, z: cell.z, why: `the channel at ${cell.x},${cell.y},${cell.z} is dry` }, 'water_bucket')
      }
      // and cover it: open water in a field is a hole to fall into and a wall to the pathfinder
      push({ do: 'cover', x: cell.x, y: cell.y, z: cell.z, why: `the channel at ${cell.x},${cell.y},${cell.z} is open water` }, spec.cover)
      continue
    }
    if (spec.kind === 'path') continue
    if (spec.kind === 'crop') {
      if (standing === spec.crop) continue
      if (standing && !WEEDS.has(standing)) continue
      if (standing && clear({ do: 'clear', x: cell.x, y: cell.y + 1, z: cell.z, why: `${standing} grew on the bed` })) continue
      if (spec.ground === 'farmland' && ground && ground.name !== 'farmland') push({ do: 'till', x: cell.x, y: cell.y, z: cell.z, why: `${ground.name} where farmland should be` })
      push({ do: 'plant', x: cell.x, y: cell.y + 1, z: cell.z, why: 'an empty bed' }, spec.seed)
      continue
    }
    if (!spec.item || standing === spec.item || (spec.kind === 'gate' && standing?.endsWith('_fence_gate'))) continue
    // a plant in the way of a wall is weeding, not somebody's block: a bush grew into claude-test-pen's west wall and
    // the build walked past it, leaving a pen that looked finished and leaked
    if (standing && !WEEDS.has(standing)) continue
    if (standing && clear({ do: 'clear', x: cell.x, y: cell.y + 1, z: cell.z, why: `${standing} grew where the ${spec.item} goes` })) continue
    push({ do: 'place', x: cell.x, y: cell.y + 1, z: cell.z, why: `no ${spec.item} there` }, spec.item)
  }
  return sowAsTilled(jobs.sort((a, b) => JOB_ORDER.indexOf(a.do) - JOB_ORDER.indexOf(b.do)))
}

// each job a plan asks for, as the primitive that does it. Shared by farm.maintain and farm.build: the same list of
// jobs builds a farm from bare ground and puts a tired one back the way its plan says.
const JOB_ACTION = { clear: 'dig', till: 'till', pour: 'pour', plant: 'place', place: 'place', fill: 'place', cover: 'place' }
export const jobCall = job => {
  const action = JOB_ACTION[job.do]
  if (!action) return null
  const at = { x: job.x, y: job.y, z: job.z }
  // a cover is a BOTTOM slab laid into the water: any other half would sit above the source and leave the hole open
  return [action, action === 'place' ? { item: job.item, ...at, ...(job.do === 'cover' ? { half: 'bottom' } : {}) } : at]
}

// what a list of jobs will use up. Counted from the jobs, not from the plan, so what already stands is not asked for
// twice: a finished farm needs nothing. One bucket does a whole field, however many channel cells are dry.
export function jobsBill (jobs) {
  const bill = {}
  for (const { item } of jobs) {
    if (!item) continue
    bill[item] = item === 'water_bucket' ? 1 : (bill[item] ?? 0) + 1
  }
  return bill
}

// one line of "what I am short of", the way every farm composite says it: wheat_seeds:12 oak_fence:4
export const shortLine = short => Object.entries(short).map(([item, n]) => `${item}:${n}`).join(' ')

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

// place=<name> or x= y= z=: the cell an action was pointed at, floored, or why it is not one
export function placeTarget (places, a, who) {
  const place = a.place ? places.find(p => p.name === a.place) : null
  if (a.place && !place) return { error: `no place called ${a.place}: places lists them` }
  const to = place ?? a
  if (to.x === undefined || to.y === undefined || to.z === undefined) return { error: `${who} needs place=<name> or x= y= z=` }
  return { at: { x: Math.floor(to.x), y: Math.floor(to.y), z: Math.floor(to.z) } }
}

// Where to ask pen.check whether a pen already stands around a plan: over the floor cells the plan marks inside its
// walls, nearest the middle first, and one level lower as well - a pen whose floor is sunk below its plan is the case
// this guards, and its feet stand at the plan's own y. A wall cell is never a spot to stand on, so only `.` cells count.
export function penProbes (cells, limit = 2) {
  const floor = cells.filter(c => PLAN_LEGEND[c.ch]?.kind === 'path')
  if (!floor.length) return []
  const mid = { x: (Math.min(...floor.map(c => c.x)) + Math.max(...floor.map(c => c.x))) / 2, z: (Math.min(...floor.map(c => c.z)) + Math.max(...floor.map(c => c.z))) / 2 }
  const near = (a, b) => (Math.abs(a.x - mid.x) + Math.abs(a.z - mid.z)) - (Math.abs(b.x - mid.x) + Math.abs(b.z - mid.z))
  return [...floor].sort(near).slice(0, limit).flatMap(({ x, y, z }) => [{ x, y: y + 1, z }, { x, y, z }])
}

// Where to stand to ask whether a pen holds: over the floor cell the plan marks inside its walls, nearest the middle,
// because penAround starts from where my feet are and a wall cell is not a spot to stand on. The plan's y is the floor
// block itself, so feet go one above it.
export const penInside = cells => penProbes(cells, 1)[0] ?? null

// A build fills and digs before it places anything, and every one of those jobs takes a floor or a wall apart for as
// long as the list runs: a floor raised beside a standing fence leaves half a block, and an animal steps over half a
// block. Chani ran pen.build over a pen holding 4 sheep and all four were 20 blocks away by the time it failed
// (BUGS.md 2026-09-23 02:55Z). Placing only ever adds, so a plan with nothing to fill or clear may be built over a
// pen that is full. `census` is what pen.check answered for the pen the plan's cells lie in (null: no pen there).
const OPENS = new Set(['fill', 'clear'])
export const openingJobs = jobs => jobs.filter(j => OPENS.has(j.do))
export function penOpenRefusal (name, jobs, census) {
  const opens = openingJobs(jobs)
  if (!opens.length || !census?.inside) return null
  const counts = ['fill', 'clear'].map(verb => [verb, opens.filter(j => j.do === verb).length]).filter(([, n]) => n > 0)
  const what = counts.map(([verb, n], i) => i === 0 ? `${n} ${n === 1 ? 'cell' : 'cells'} to ${verb}` : `${n} to ${verb}`).join(' and ')
  return `${name} holds ${census.inside} and the build would open it (${what}): a floor raised or a block dug out beside a standing fence leaves half a block, and an animal steps over half a block. Lead them out first (flock.lead), or mark a plan that matches the pen as it stands - then build`
}

// Where the block a plan marks with one character really stands: the plan's y is the ground it sits on, so a chest,
// composter or torch is at y+1. Every composite that walks to one asks for it this way.
export function planStructure (cells, ch) {
  const cell = cells.find(c => c.ch === ch)
  return cell ? { x: cell.x, y: cell.y + 1, z: cell.z } : null
}

// what a plan needs that I do not carry. Counted before a build starts: half a farm is worse than none.
export const billShortfall = (bill, items = {}) =>
  Object.fromEntries(Object.entries(bill).map(([item, n]) => [item, n - (items[item] ?? 0)]).filter(([, n]) => n > 0))

// what to build a floor out of, by the ground the plan's legend asks for
const FLOOR_ITEM = { sand: 'sand' }
// a cell that already holds what the plan wants there is never dug out (a chest full of seed, a crop halfway grown)
const planHas = (spec, name) => name === spec.item || name === spec.crop || (spec.kind === 'gate' && name.endsWith('_fence_gate'))
// The levelling a plan needs before any of its jobs can be done: a floor under every cell and open air in the cell and
// over it. Read-only judgement; `solid(name)` answers whether a block stands in the way (grass and flowers do not).
export function groundJobs ({ cells, worldAt, solid }) {
  const jobs = []
  const fills = []
  for (const cell of cells) {
    const spec = PLAN_LEGEND[cell.ch]
    if (!spec) continue
    // the plan's y IS the floor of a cell, so it is never dug out; a channel holds its source at that level, and the
    // block the water is poured onto is the one below it
    const floorY = spec.kind === 'water' ? cell.y - 1 : cell.y
    const floor = worldAt(cell.x, floorY, cell.z)
    if (floor && !solid(floor.name)) fills.push({ do: 'fill', x: cell.x, y: floorY, z: cell.z, why: `${floor.name} where the floor should be`, item: FLOOR_ITEM[spec.ground] ?? 'dirt' })
    for (const y of [cell.y + 1, cell.y + 2]) {
      const here = worldAt(cell.x, y, cell.z)
      if (!here || !solid(here.name) || planHas(spec, here.name)) continue
      jobs.push({ do: 'clear', x: cell.x, y, z: cell.z, why: y === cell.y + 1 ? `${here.name} stands in the cell` : `${here.name} stands where the plan wants open air` })
    }
  }
  // dig first, fill afterwards: the boulder often stands over the hole
  return [...jobs, ...fills]
}

// ---------------------------------------------------------------- composite actions: the runner's rules
// A composite declares the key=values it takes, so a typo is refused before the body walks anywhere.
// These belong to the runner and every composite accepts them.
const RUNNER_ARGS = { timeout: 'number', force: 'boolean', dig: 'boolean' }
export function checkArgs (name, spec, given) {
  const full = { ...RUNNER_ARGS, ...spec }
  const wanted = Object.entries(spec).map(([k, t]) => String(t).endsWith('!') ? k : `${k}?`).join(', ')
  const missing = Object.entries(spec).find(([k, t]) => String(t).endsWith('!') && given[k] === undefined)
  if (missing) return `${name} needs ${missing[0]}=`
  // a composite forwards its own optional arguments unset (range: a.range): that is no argument at all, not a wrong one
  for (const [key, value] of Object.entries(given).filter(([, v]) => v !== undefined)) {
    const type = full[key]
    if (!type) return `${name}: ${key}= is not an argument here (${wanted})`
    const want = String(type).replace('!', '')
    if (want !== 'any' && typeof value !== want) return `${name}: ${key}= wants a ${want}, got ${JSON.stringify(value)}`
  }
  return null
}

// A composite cannot opt out of these: the runner checks them between steps and hands control back to the driver, ending
// the task with stopped=<reason>. Night WITH a bed is not here: the runner sleeps and the composite never sees it.
export function handBackReason (s) {
  if (s.spoken) return `spoken to (${s.spoken})`
  if (s.health <= 8) return `health ${Math.round(s.health)}`
  if (s.food <= 6 && !s.edible) return `food ${s.food} and nothing edible carried`
  if (s.failedTwice) return `twice in a row: ${s.failedTwice}`
  if (s.invFull && !s.canDeposit) return 'inventory full and no chest to deposit in'
  if (s.night && !s.bedNear) return 'night and no bed within 32 blocks'
  if (s.days !== undefined && s.elapsedDays >= s.days) return 'days'
  if (s.count !== undefined && s.done >= s.count) return 'count'
  if (s.until !== undefined && s.now >= s.until) return 'until'
  return null
}

// a library module must declare itself before the body will run it: a bad one is caught at body start, not mid-errand
const ARG_TYPES = ['string', 'number', 'boolean', 'any']
export function compositeError (name, mod) {
  if (typeof mod?.run !== 'function') return `library/${name}.mjs: needs \`run\``
  if (typeof mod.doc !== 'string' || !mod.doc) return `library/${name}.mjs: needs \`doc\` (its one line in the guide)`
  if (!mod.args || typeof mod.args !== 'object') return `library/${name}.mjs: needs \`args\` ({} when it takes none)`
  const bad = Object.entries(mod.args).find(([, t]) => !ARG_TYPES.includes(String(t).replace('!', '')))
  return bad ? `library/${name}.mjs: args.${bad[0]} is ${JSON.stringify(bad[1])}; use ${ARG_TYPES.join(', ')} (with ! for required)` : null
}

// ---------------------------------------------------------------- composite actions: the farm's produce
// the seed a field is sown from: kept back from the chest, so a farm always carries enough to sow itself again
export const SEED_ITEMS = new Set(['wheat_seeds', 'beetroot_seeds', 'melon_seeds', 'pumpkin_seeds', 'carrot', 'potato', 'sugar_cane', 'bamboo'])
// what a farm makes. Anything else I carry (tools, armour, cobblestone, the bread I live on) is mine, not the chest's.
const FARM_GOODS = new Set([...SEED_ITEMS, 'wheat', 'beetroot', 'melon_slice', 'melon', 'pumpkin', 'poisonous_potato', 'hay_block', 'cocoa_beans'])
// what belongs in the farm's chest: its produce, above the reserve the plan needs to sow itself again
export function farmSurplus (items, reserve = {}) {
  const out = {}
  for (const [name, count] of Object.entries(items)) {
    if (!FARM_GOODS.has(name)) continue
    const spare = count - (reserve[name] ?? 0)
    if (spare > 0) out[name] = spare
  }
  return out
}

// ---------------------------------------------------------------- composite actions: composting
// The game's own odds that one item raises a composter by a level; 7 raises fill it and it yields 1 bone meal.
// Anything not here a composter refuses.
const COMPOST_GROUPS = [
  [0.3, ['wheat_seeds', 'beetroot_seeds', 'melon_seeds', 'pumpkin_seeds', 'torchflower_seeds', 'pitcher_pod', 'short_grass', 'tall_grass', 'fern', 'large_fern', 'seagrass', 'kelp', 'dried_kelp', 'oak_leaves', 'birch_leaves', 'spruce_leaves', 'jungle_leaves', 'acacia_leaves', 'dark_oak_leaves', 'cherry_leaves', 'mangrove_leaves', 'azalea_leaves', 'oak_sapling', 'birch_sapling', 'spruce_sapling', 'jungle_sapling', 'acacia_sapling', 'dark_oak_sapling', 'cherry_sapling', 'mangrove_propagule', 'sweet_berries', 'glow_berries', 'hanging_roots', 'moss_carpet', 'small_dripleaf', 'dead_bush']],
  [0.5, ['sugar_cane', 'cactus', 'vine', 'melon_slice', 'pumpkin', 'carved_pumpkin', 'nether_wart', 'glow_lichen', 'tall_seagrass', 'big_dripleaf', 'lily_pad']],
  [0.65, ['apple', 'beetroot', 'carrot', 'potato', 'wheat', 'cocoa_beans', 'melon', 'brown_mushroom', 'red_mushroom', 'sea_pickle', 'moss_block', 'pink_petals', 'bamboo', 'nether_sprouts']],
  [0.85, ['bread', 'cookie', 'baked_potato', 'hay_block', 'brown_mushroom_block', 'red_mushroom_block', 'nether_wart_block', 'warped_wart_block', 'waterlily']],
  [1, ['cake', 'pumpkin_pie']]
]
export const COMPOST_CHANCE = Object.fromEntries(COMPOST_GROUPS.flatMap(([chance, items]) => items.map(name => [name, chance])))
// a composter eats bread as happily as a seed: my own food is never fed to it unless it is asked for by name
// (a sweep of the farm put all 7 loaves in the composter, 22:1)
const COMPOST_FOOD = new Set(['bread', 'cookie', 'baked_potato', 'apple', 'sweet_berries', 'glow_berries', 'melon_slice', 'cake', 'pumpkin_pie', 'beetroot', 'carrot', 'potato'])

// items= as an object, a list or a bare name; a count of 'all' or none means whatever I carry
const wantedCounts = want => {
  if (!want) return null
  if (typeof want === 'string') return { [want]: Infinity }
  const rows = Array.isArray(want) ? want.map(w => [w.name, w.count]) : Object.entries(want)
  return Object.fromEntries(rows.map(([name, count]) => [name, count === undefined || count === 'all' ? Infinity : count]))
}

// what to feed a composter: what I asked for, or everything compostable I carry that is not seed I need to sow again
export function compostPlan (items, { want, keep = {} } = {}) {
  const asked = wantedCounts(want)
  const feed = []
  const skipped = []
  for (const [name, wanted] of Object.entries(asked ?? items)) {
    const chance = COMPOST_CHANCE[name]
    // with no items= every slot is offered, so saying 'a composter will not take it' about each tool is noise: only answer for what was asked for
    if (!chance) {
      if (asked) skipped.push({ name, why: 'a composter will not take it' })
      continue
    }
    const held = items[name] ?? 0
    const heldBack = SEED_ITEMS.has(name) ? 'kept to sow the fields again' : COMPOST_FOOD.has(name) ? 'food, not compost' : null
    const reserve = keep[name] ?? (!asked && heldBack ? held : 0)
    const count = Math.min(asked ? wanted : held, held - reserve)
    if (count <= 0) { skipped.push({ name, why: asked ? `you carry ${held}` : heldBack ?? 'none to spare' }); continue }
    feed.push({ name, count, chance })
  }
  return { feed, skipped }
}

// ---------------------------------------------------------------- routines
// A routine is a plain list of {action, ...args}: handed over as steps=, or shipped by a role as roles/<role>/<name>.json
// so that "the farmer's day" is a file anyone can read and edit rather than something baked into an action.
const parseSteps = text => {
  try { return { list: JSON.parse(text) } } catch (e) { return { error: e.message } }
}

// a routine a role ships cannot know which farm it will be run on, so it writes $place and routine place= fills it in
const fillPlace = (step, place) => Object.fromEntries(Object.entries(step).map(([k, v]) => [k, v === '$place' ? place : v]))
const wantsPlace = steps => steps.some(step => Object.values(step).includes('$place'))

export function routineSteps ({ steps, name, place }, readRole) {
  if (!steps && !name) return { error: 'routine needs steps= or name= (a routine shipped in roles/<role>/<name>.json)' }
  const where = steps ? 'steps= must be a list of {"action":...} objects' : `roles/${name}.json is not a list of steps`
  const raw = steps ?? readRole(name)
  if (raw === null || raw === undefined) return { error: `no routine called ${name} (roles/${name}.json)` }
  const read = typeof raw === 'string' ? parseSteps(raw) : { list: raw }
  if (read.error) return { error: `${where}: ${read.error}` }
  if (!Array.isArray(read.list)) return { error: where }
  const bad = read.list.findIndex(step => !step || !step.action)
  if (bad >= 0) return { error: `step ${bad + 1} has no action=` }
  if (wantsPlace(read.list) && !place) return { error: `routine name=${name} needs place=<the name of a marked farm> to work on` }
  return { steps: read.list.map(step => fillPlace(step, place)) }
}

// ---------------------------------------------------------------- where seed comes from
// Every crop has one renewable source. Wheat seed falls out of grass; cane and bamboo are cut above the base so the
// stand lives; melons and pumpkins are cut off the stem; roots are never found wild, so they come out of a farm chest.
const SEED_SOURCES = {
  wheat: { from: 'grass', block: 'short_grass', item: 'wheat_seeds', advice: 'break grass until the seed adds up; tall grass drops it too' },
  carrot: { from: 'chest', block: 'carrots', item: 'carrot', advice: 'carrots never drop from grass: take some from a farm chest, a village plot or a trade' },
  potato: { from: 'chest', block: 'potatoes', item: 'potato', advice: 'potatoes never drop from grass: take some from a farm chest, a village plot or a trade' },
  beetroot: { from: 'chest', block: 'beetroots', item: 'beetroot_seeds', advice: 'beetroot seed never drops from grass: take some from a farm chest or a village plot' },
  sugar_cane: { from: 'stalk', block: 'sugar_cane', item: 'sugar_cane', advice: 'cut the stand above its base so it grows back' },
  bamboo: { from: 'stalk', block: 'bamboo', item: 'bamboo', advice: 'cut the stand above its base so it grows back' },
  melon: { from: 'wild', block: 'melon', item: 'melon_slice', advice: 'cut the fruit and leave the stem: it grows another' },
  pumpkin: { from: 'wild', block: 'pumpkin', item: 'pumpkin', advice: 'cut the fruit and leave the stem: it grows another' }
}
const CROP_ALIASES = {
  potatoes: 'potato', carrots: 'carrot', beetroots: 'beetroot', beets: 'beetroot', melons: 'melon', pumpkins: 'pumpkin',
  wheat_seeds: 'wheat', beetroot_seeds: 'beetroot', melon_seeds: 'melon', pumpkin_seeds: 'pumpkin',
  sugarcane: 'sugar_cane', cane: 'sugar_cane'
}
export const seedSource = crop => SEED_SOURCES[CROP_ALIASES[crop] ?? crop] ?? null

// ---------------------------------------------------------------- the catalogue behind ./mc help
// A primitive is ONE game operation that needs the body's innards (a window, the pathfinder, a reflex, entity
// tracking). Anything that loops over primitives or decides between them is a composite in library/<folder>/<file>.mjs
// and is called folder.file. Sections below are for primitives; composites are grouped by their folder.
export const SECTIONS = {
  sense: 'what I can see from here',
  map: 'the shared map everyone reads',
  move: 'getting about',
  block: 'blocks and the ground',
  item: 'the things I carry',
  creature: 'animals and monsters',
  self: 'my own body',
  control: 'driving the body',
  farm: 'crops, fields and what comes off them',
  pen: 'fences and gates',
  flock: 'animals as a herd',
  mine: 'digging for stone and ore',
  work: 'whole jobs that run themselves'
}

// a composite declares its arguments as {name: 'type'}, 'type!' for one it cannot do without
export const argsUsage = args => Object.entries(args ?? {})
  .map(([name, type]) => (String(type).endsWith('!') ? `${name}=` : `[${name}=]`))
  .sort((a, b) => Number(a.startsWith('[')) - Number(b.startsWith('[')))
  .join(' ')
  // a point is one argument to whoever types it
  .replace('[x=] [y=] [z=]', '[x= y= z=]')

// a composite's doc reads 'name args=: what it does'; the catalogue prints the usage from the args declaration instead
// ./mc <action> key=value ...: a bare word (./mc help farm.maintain) is the topic, -v asks for the raw answer
export const parseCliArgs = argv => Object.fromEntries(argv.filter(kv => kv !== '-v').map(kv => {
  const at = kv.indexOf('=')
  const parse = v => { try { return JSON.parse(v) } catch { return v } }
  return at < 0 ? ['topic', parse(kv)] : [kv.slice(0, at), parse(kv.slice(at + 1))]
}))

export const docText = doc => String(doc).includes(': ') ? String(doc).slice(String(doc).indexOf(': ') + 2) : String(doc)

// farm.maintain belongs to farm; a composite with no folder (routine, hunt) is a job in its own right
const sectionOf = entry => entry.section ?? (entry.name.includes('.') ? entry.name.split('.')[0] : 'work')
const usageOf = entry => `${entry.name}${entry.args ? ` ${entry.args}` : ''}`

// ./mc help: every action and its arguments; ./mc help <section|action>: what it does, and what makes it hand back
export function helpText (topic, entries) {
  const groups = new Map()
  for (const entry of entries) groups.set(sectionOf(entry), [...(groups.get(sectionOf(entry)) ?? []), entry])
  const order = [...Object.keys(SECTIONS).filter(s => groups.has(s)), ...[...groups.keys()].filter(s => !SECTIONS[s]).sort()]
  const header = section => `${section}: ${SECTIONS[section] ?? 'its own corner of the world'}`
  const brief = entry => `  ${usageOf(entry)}${entry.stops ? ` | stops: ${entry.stops}` : ''}`
  const detail = entry => [`  ${usageOf(entry)} - ${entry.doc}`, ...(entry.stops ? [`    stops: ${entry.stops}`] : [])]
  if (!topic) {
    return [...order.flatMap(s => [header(s), ...groups.get(s).map(brief)]),
      './mc help <section> or ./mc help <action> for what one does',
      renamedList()].join('\n')
  }
  if (groups.has(topic)) return [header(topic), ...groups.get(topic).flatMap(detail)].join('\n')
  const one = entries.find(entry => entry.name === topic)
  if (!one) return didYouMean(topic, entries.map(entry => entry.name))
  return [usageOf(one), one.doc, ...(one.stops ? [`stops: ${one.stops}`] : [])].join('\n')
}

// Every primitive, its section, what it reads and one line of what it does: this IS ./mc help, so an action missing
// from here is invisible to whoever drives the body. bot.mjs says so at start-up if a dispatch entry has no line here.
export const PRIMITIVES = {
  // ---- sense
  state: { section: 'sense', args: '', doc: 'health, food, xp, the time, where I stand, what I hold and who else is about' },
  look: { section: 'sense', args: '[pano=] [dir=north] [x= y= z=]', doc: 'a picture of what I see, rendered to a PNG file' },
  look_at: { section: 'sense', args: 'x= y= z=', doc: 'turn my head to face a point' },
  look_around: { section: 'sense', args: '[range=16] [blocks=] [blockRange=] [mob=] [limit=]', doc: 'what stands around me: players, mobs, dropped items and any blocks you name' },
  entity: { section: 'sense', args: 'name= [count=2]', doc: 'the raw server metadata of the nearest entities with that name (a debugging aid)' },
  animals: { section: 'sense', args: '[mob=] [within=24] [x= y= z=]', doc: 'the farm animals near me: kind, id, where, grown or a baby, and whether it is in a pen (the one around me, or around x= y= z=)' },
  find_blocks: { section: 'sense', args: 'block= [maxDistance=64] [count=10]', doc: 'where the nearest blocks of a kind are; * wildcards work (*_log)' },
  block_at: { section: 'sense', args: 'x= y= z=', doc: 'the name and properties of one block' },
  scan: { section: 'sense', args: 'x1= y1= z1= x2= y2= z2= [where=]', doc: 'an ASCII map of a box of the world, or with where= just the coordinates of one kind of block' },
  path_to: { section: 'sense', args: 'x= y= z= [range=] [dig=] [stroll=]', doc: 'what the pathfinder makes of a walk from here, without walking it' },
  inventory: { section: 'sense', args: '', doc: 'what I carry, what I wear and how many slots are free' },
  chest_contents: { section: 'sense', args: '[x= y= z=]', doc: 'what is in a chest' },
  events: { section: 'sense', args: '[type=] [last=]', doc: 'my own event log: what happened while you were not looking' },
  // ---- map
  places: { section: 'map', args: '[name=] [q=] [by=] [kind=] [within=] [limit=]', doc: 'search the shared map: bases, farms, mines, villages, dangers. name= gives one place whole; never read places.json yourself' },
  mark: { section: 'map', args: 'name= [kind=] [note=] [x= y= z=]', doc: 'put a place on the shared map, here or at a point' },
  unmark: { section: 'map', args: 'name=', doc: 'take a place off the shared map' },
  zones: { section: 'map', args: '', doc: 'the protected areas: what nobody may dig through' },
  protect: { section: 'map', args: 'name= x1= y1= z1= x2= y2= z2=', doc: 'protect a box of the world, mine or shared' },
  unprotect: { section: 'map', args: 'name=', doc: 'drop a protected area' },
  // ---- move
  goto: { section: 'move', args: 'place= | player= | x= z= [y=] [range=] [dig=]', doc: 'walk there, opening doors and swimming; it does not dig or bridge unless dig=true' },
  follow: { section: 'move', args: 'player=', doc: 'keep walking after someone until stop' },
  // ---- block
  dig: { section: 'block', args: 'x= y= z= [wet=] [dig=]', doc: 'break one block and pick up what it drops (dig=true: the walk to it may tunnel). Not water or lava: use fill or place' },
  place: { section: 'block', args: 'item= x= y= z= [facing=] [half=] | blocks=', doc: 'build: one block, or a whole list of them in the order given' },
  clear: { section: 'block', args: 'x1= y1= z1= x2= y2= z2= [keep=]', doc: 'dig out a whole box top-down, up to 400 blocks; beds, containers and fluids are kept' },
  till: { section: 'block', args: 'x= y= z= | blocks=', doc: 'hoe dirt or grass into farmland (give the ground block, not the air above it)' },
  path: { section: 'block', args: 'x= y= z= | blocks=', doc: 'shovel grass into a walking path' },
  fertilize: { section: 'block', args: 'x= y= z= | blocks=', doc: 'bone meal on a crop, a sapling or a grass block' },
  fill: { section: 'block', args: 'x= y= z=', doc: 'scoop a water or lava source block into a bucket' },
  pour: { section: 'block', args: 'x= y= z=', doc: 'empty the bucket onto the solid block you name; the water lands one above it' },
  toggle: { section: 'block', args: 'x= y= z= [open=]', doc: 'work a gate, door, trapdoor, lever or button by hand' },
  use: { section: 'block', args: 'x= y= z= [item=] [ticks=]', doc: 'right-click a block with what I hold: a composter, a lectern, anything toggle refuses' },
  // ---- item
  craft: { section: 'item', args: 'item= [count=1]', doc: 'craft, using a crafting table within 32 blocks when the recipe needs one' },
  smelt: { section: 'item', args: 'item= [count=] [fuel=] [fuelCount=] [wait=] [x= y= z=]', doc: 'cook or melt in the nearest furnace and wait for it, by day' },
  furnace_take: { section: 'item', args: '[x= y= z=]', doc: 'take what is done out of a furnace' },
  deposit: { section: 'item', args: 'items= | item= [count=] | all=true [x= y= z=]', doc: 'put things into a chest, then open it again to check they really went in' },
  withdraw: { section: 'item', args: 'items= | item= [count=] [x= y= z=]', doc: 'take things out of a chest, checking the same way' },
  equip: { section: 'item', args: 'item= [destination=]', doc: 'hold it, or wear it: armour finds its own slot' },
  toss: { section: 'item', args: 'item= [count=]', doc: 'drop something on the ground' },
  give: { section: 'item', args: 'player= item= [count=] [dig=]', doc: 'hand something to a player and watch that it was taken' },
  enchant: { section: 'item', args: 'item= [slot=] [x= y= z=]', doc: 'enchant one item I carry at an enchanting table, paying lapis and levels' },
  // ---- creature
  attack: { section: 'creature', args: 'mob= [id=] [leash=24]', doc: 'hunt one animal or monster: the nearest of its kind, or the id= that animals gave you; it leaves the drops lying where they fall' },
  shear: { section: 'creature', args: '[count=] [within=40]', doc: 'wool without killing: needs shears' },
  feed: { section: 'creature', args: 'mob= [id=]', doc: 'walk to one animal and hold out the food it breeds on (id= from animals)' },
  escort: { section: 'creature', args: 'mob= x= y= z= [count=] [within=32] [penned=] [range=]', doc: 'the walk itself: fetch the animals and bring them to a spot, stopping for stragglers (flock.lead is the whole job)' },
  'pen.check': { section: 'pen', args: '[x= y= z=] [radius=]', doc: 'walk a fence and find where a pen leaks: gaps, corner gates, rims an animal can hop' },
  // ---- self
  sleep: { section: 'self', args: '[any=]', doc: 'sleep in the nearest free bed within 32 blocks' },
  wake: { section: 'self', args: '', doc: 'get out of bed' },
  quit: { section: 'self', args: '', doc: 'stop my body; ./start in the background brings it back' },
  chat: { section: 'self', args: 'message=', doc: 'say something to everyone' },
  whisper: { section: 'self', args: 'player= message=', doc: 'say something to one player' },
  // ---- control
  run: { section: 'control', args: 'steps=', doc: 'run a list of actions in order, one after another' },
  stop: { section: 'control', args: '', doc: 'cancel whatever the body is doing' },
  watch: { section: 'control', args: 'name= block=|mob=|item= [where=] [count=] [atMost=] [within=] [x= y= z=] [repeat=]', doc: 'tell me when the world comes to look like this' },
  unwatch: { section: 'control', args: 'name=', doc: 'drop a watch' },
  watches: { section: 'control', args: '', doc: 'the watches I have set' },
  reflexes: { section: 'control', args: '[on=]', doc: 'switch the body reflexes (eating, fleeing, bedtime, shutting gates) on or off' },
  control: { section: 'control', args: 'state= [ms=]', doc: 'hold one movement key down by hand (a debugging aid)' },
  wait: { section: 'control', args: '[seconds=100]', doc: 'block until something happens that needs me; reads the event log, so it needs no body' },
  dawn: { section: 'control', args: '', doc: 'block until morning; needs no body, so a bodiless night is spent here' },
  clock: { section: 'control', args: '', doc: 'the world time as last seen by any body; needs no body' },
  help: { section: 'control', args: '[<section or action>]', doc: 'this catalogue, one section of it, or everything about one action' }
}


// ---------------------------------------------------------------- the client jar (textures/ is not checked in)
// The version folders under ~/.minecraft/versions that hold a plain client jar, newest first. A name that is not
// only digits and dots belongs to something else: OptiFine builds, pre-releases, release candidates, snapshots and
// loader folders all sit beside the releases, and none of them is the jar tools/textures.mjs is looking for.
const RELEASE = /^\d+(\.\d+)*$/
const versionOrder = (a, b) => {
  const [x, y] = [a.split('.').map(Number), b.split('.').map(Number)]
  return [...Array(Math.max(x.length, y.length)).keys()].map(i => (y[i] ?? 0) - (x[i] ?? 0)).find(d => d !== 0) ?? 0
}
export const clientVersions = dirs => dirs.filter(d => RELEASE.test(d)).sort(versionOrder)

// The block textures inside a client jar, in the order the jar lists them. Everything else in there is somebody
// else's business: items, entities, the gui, the animation metadata beside a png, and other namespaces.
export const BLOCK_TEXTURES = 'assets/minecraft/textures/block/'
export const blockTextures = names => names.filter(n => n.startsWith(BLOCK_TEXTURES) && n.endsWith('.png'))
