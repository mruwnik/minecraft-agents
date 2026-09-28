// The catalogue behind ./mc help, and the answer to an action name nobody knows.

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
  pen_check: 'pen.check',
  'villager.board': 'boat.board',
  'villager.route': 'boat.route',
  'villager.stage': 'boat.stage',
  'villager.ferry': 'boat.ferry',
  'villager.dock': 'boat.dock',
  'villager.undock': 'boat.undock',
  'villager.receive': 'boat.receive'
}
// where to read about the successor: its section is the half before the dot, and a top-level one is its own topic
export const renamedTo = typed => RENAMED[typed] ? `it is now ${RENAMED[typed]} (./mc help ${RENAMED[typed].split('.')[0]})` : null
export const renamedList = () => `renamed: ${Object.entries(RENAMED).map(([was, now]) => `${was} -> ${now}`).join(', ')}`

// an action name nobody knows: the real ones that share a word with it
// words drivers reach for that share nothing with the real name
const OTHER_WORDS = { cancel: 'stop', abort: 'stop', halt: 'stop', nearby: 'look_around', entities: 'look_around', mobs: 'look_around', say: 'chat', walk: 'goto', move: 'goto', eat: 'eat', attack: 'attack', kill: 'attack', bed: 'sleep', open: 'toggle', close: 'toggle', store: 'deposit', take: 'withdraw', drop: 'toss', throw: 'toss' }
export function didYouMean (typed, actions) {
  if (RENAMED[typed]) return `unknown action ${typed}: ${renamedTo(typed)}`
  const words = typed.toLowerCase().split(/[^a-z]+/).filter(w => w.length >= 3)
  const meant = words.map(w => OTHER_WORDS[w]).filter(a => actions.includes(a))
  const like = [...new Set([...meant, ...actions.filter(a => words.some(w => a.split(/[_.]/).some(part => part.startsWith(w) || w.startsWith(part))))])]
  return `unknown action ${typed}: ${like.length ? `did you mean ${like.slice(0, 4).join(', ')}?` : './mc help lists them all'}`
}

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
  rail_state: { section: 'sense', args: '[id=]', doc: 'read a specified minecart and its passengers, and whether this traveler is mounted' },
  animals: { section: 'sense', args: '[mob=] [within=24] [x= y= z=]', doc: 'the farm animals near me: kind, id, where, grown or a baby, and whether it is in a pen (the one around me, or around x= y= z=)' },
  find_blocks: { section: 'sense', args: 'block= [maxDistance=64] [count=10]', doc: 'where the nearest blocks of a kind are; * wildcards work (*_log)' },
  block_at: { section: 'sense', args: 'x= y= z=', doc: 'the name and properties of one block' },
  scan: { section: 'sense', args: 'x1= y1= z1= x2= y2= z2= [where=]', doc: 'an ASCII map of a box of the world, or with where= just the coordinates of one kind of block' },
  path_to: { section: 'sense', args: 'x= y= z= [range=] [dig=] [stroll=] [route=] [live=] [surface=horse]', doc: 'what the pathfinder makes of a walk from here, without walking it; route=true names the gates it opens and a waypoint every six steps; live=true plans with the movements walks use right now and names what differs from a fresh set. surface=horse checks an explicit checkpoint within16 blocks using horse-width dry surface geometry, including a return path; route=true lists every checked waypoint' },
  inventory: { section: 'sense', args: '', doc: 'what I carry, what I wear and how many slots are free' },
  chest_contents: { section: 'sense', args: '[x= y= z=]', doc: 'what is in a chest, with free= (empty slots) and slots= (its size) so a deposit can pick a chest with room' },
  events: { section: 'sense', args: '[type=] [last=]', doc: 'my own event log: what happened while you were not looking' },
  // ---- map
  places: { section: 'map', args: '[name=] [q=] [by=] [kind=] [within=] [limit=]', doc: 'search the shared map: bases, farms, mines, villages, dangers. name= gives one place whole; never read places.json yourself' },
  mark: { section: 'map', args: 'name= [kind=] [note=] [x= y= z=] [move=]', doc: 'put a place on the shared map, here or at a point. Marking an existing place again keeps where it is unless x= y= z= is given; moving one off what still stands there needs move=true' },
  unmark: { section: 'map', args: 'name=', doc: 'take a place off the shared map' },
  zones: { section: 'map', args: '', doc: 'the protected areas: what nobody may dig through' },
  protect: { section: 'map', args: 'name= x1= y1= z1= x2= y2= z2=', doc: 'protect a box of the world, mine or shared' },
  unprotect: { section: 'map', args: 'name=', doc: 'drop a protected area' },
  // ---- move
  goto: { section: 'move', args: 'place= | player= | x= z= [y=] [range=] [dig=] [surface=horse]', doc: 'walk there, opening doors and swimming; it does not dig or bridge unless dig=true, and crosses planted cells only where there is no other way, at a walking pace. surface=horse requires explicit x/y/z within16 blocks, checks loaded approach and retreat, rejects nearby danger/night, and stops on damage or stalled progress; it never automatically retreats' },
  follow: { section: 'move', args: 'player=', doc: 'keep walking after someone until stop' },
  rail_ride: { section: 'move', args: 'id= track=x:y:z,x:y:z exit=x:y:z', doc: 'ride one explicit empty minecart along a checked straight powered corridor to a braking station; never builds track and remains mounted on interruption' },
  horse_state: { section: 'move', args: '[id=]', doc: 'inspect nearby horses, donkeys and mules: server-confirmed age, tameness, saddle and passenger state' },
  tame: { section: 'move', args: 'id= [attempts=12] [seconds=120]', doc: 'tame an authorized adult horse, donkey or mule by mounting again after bucking; verifies the server tame flag and remains mounted on success' },
  horse_saddle: { section: 'move', args: 'id=', doc: 'fit one carried saddle to a tamed horse, donkey or mule through its checked inventory; remains mounted' },
  ride: { section: 'move', args: 'id= [x= y= z=] [terrain=flat|steps] [plan=true] [approach=surface]', doc: 'mount an authorized tamed equine; optional bounded surface approach follows a nearby moving horse for at most 8s; coordinates ride checked flat legs up to 128m or natural step legs up to 16m, plan=true validates without boarding; remains mounted' },
  horse_dismount: { section: 'move', args: '[id=]', doc: 'leave a stationary horse on inspected dry ground; waits for server passenger removal and actual dismount position before walking resumes' },
  boat_state: { section: 'move', args: '[id=]', doc: 'read nearby boats, their passenger IDs and UUIDs, and the boat I ride' },
  boat_place: { section: 'move', args: 'item= x= y= z=', doc: 'place one carried boat at a checked water or ground cell and report its entity ID' },
  boat_leash: { section: 'move', args: 'id=', doc: 'attach a carried lead to one nearby boat and verify that I hold its leash' },
  boat_unleash: { section: 'move', args: 'id=', doc: 'detach the lead from one boat after its passenger is secured' },
  boat_recover: { section: 'move', args: 'id=', doc: 'break and reclaim a verified empty, unleashed boat for reuse' },
  boat_mount: { section: 'move', args: 'id=', doc: 'board a nearby boat and confirm I am its controlling first passenger' },
  boat_drive: { section: 'move', args: 'id= x= y= z=', doc: 'board an explicitly chosen nearby ordinary boat and steer to an exact boat-feet position through checked loaded level source water; stays aboard at arrival or interruption' },
  boat_land: { section: 'move', args: 'id= x= y= z=', doc: 'leave a stationary boat onto a checked adjacent dry full-block landing; integer coordinates name the player feet cell, and server confirmation is required' },
  boat_dismount: { section: 'move', args: '', doc: 'leave the boat and verify that I am on foot' },
  boat_release: { section: 'move', args: 'id= passengerUuid=', doc: 'break a nearby boat after I dismount and verify the named passenger is safely on foot' },
  boat_swim: { section: 'move', args: 'x= y= z= [ms=700]', doc: 'swim toward a checked water waypoint with forward and jump held together for one bounded stroke' },
  // ---- block
  dig: { section: 'block', args: 'x= y= z= [wet=] [dig=] [batch=] [silk_touch=true place=<owned forest>] [safe_hive=true smoke={x,y,z} place=<owned forest>]', doc: 'break one block and pick up what it drops (dig=true: the walk to it may tunnel; batch=true: one cell of a sweep, no wait for the drop and no chase after it, collect afterwards). silk_touch=true requires and equips an actual Silk Touch tool for a known hive inside the named owned forest plan. safe_hive=true permits destroying that exact claimed hive only while smoke={x,y,z} names a lit campfire 1–5 blocks directly below it with a clear column. Walks only when the cell is out of arm\'s reach. Not water or lava: use fill or place' },
  place: { section: 'block', args: 'item= x= y= z= [facing=] [half=] [against=] | blocks=', doc: 'build: one block, or a whole list of them in the order given' },
  clear: { section: 'block', args: 'x1= y1= z1= x2= y2= z2= [keep=]', doc: 'dig out a whole box top-down, up to 400 blocks; beds, containers and fluids are kept' },
  till: { section: 'block', args: 'x= y= z= | blocks=', doc: 'hoe dirt or grass into farmland (give the ground block, not the air above it)' },
  path: { section: 'block', args: 'x= y= z= | blocks=', doc: 'shovel grass into a walking path' },
  fertilize: { section: 'block', args: 'x= y= z= | blocks=', doc: 'bone meal on a crop, a sapling or a grass block' },
  fill: { section: 'block', args: 'x= y= z=', doc: 'scoop a water or lava source block into a bucket' },
  pour: { section: 'block', args: 'x= y= z=', doc: 'empty the bucket onto the solid block you name; the water lands one above it' },
  toggle: { section: 'block', args: 'x= y= z= [open=]', doc: 'work a gate, door, trapdoor, lever or button by hand' },
  use: { section: 'block', args: 'x= y= z= [item=] [empty_hand=] [ticks=]', doc: 'right-click a block with what I hold, item=, or empty_hand=true: a composter, a lectern, anything toggle refuses' },
  // ---- item
  craft: { section: 'item', args: 'item= [count=1]', doc: 'craft, using a crafting table within 32 blocks when the recipe needs one. Answers made= (a batch can overshoot what you asked for). It counts its result and what went in once the pockets have settled (a crafting window hands its grid back after it closes), so what it says was consumed is what really left. A failure says whether the ingredients were consumed: if they were not, retry, the second call usually works' },
  smelt: { section: 'item', args: 'item= [count=] [fuel=] [fuelCount=] [wait=] [x= y= z=]', doc: 'cook or melt in the nearest furnace and wait for it, by day' },
  furnace_take: { section: 'item', args: '[x= y= z=]', doc: 'take what is done out of a furnace' },
  deposit: { section: 'item', args: 'items= | item= [count=] | all=true [x= y= z=]', doc: 'put things into a chest, then open it again to check they really went in' },
  withdraw: { section: 'item', args: 'items= | item= [count=] [x= y= z=]', doc: 'take things out of a chest, checking the same way' },
  equip: { section: 'item', args: 'item= [destination=]', doc: 'hold it, or wear it: armour finds its own slot' },
  toss: { section: 'item', args: 'item= [count=]', doc: 'drop something on the ground' },
  inventory_compact: { section: 'item', args: 'item= [maxMoves=72]', doc: 'merge compatible stacks of one carried item with ordinary inventory clicks, preserving counts and returning freed slots' },
  villager_food: { section: 'creature', args: 'uuid= item= count= [otherUuid=]', doc: 'drop a bounded breeding-food portion toward one observed on-foot adult and report server-confirmed item pickup by UUID' },
  give: { section: 'item', args: 'player= item= [count=] [dig=]', doc: 'walk to within arm\'s reach of a player, toss every stack until count= is met and watch that it was taken. short= says what the pocket lacked; lying= where a drop still lies and how far from them' },
  enchant: { section: 'item', args: 'item= [slot=] [x= y= z=]', doc: 'enchant one item I carry at an enchanting table, paying lapis and levels' },
  trades: { section: 'item', args: '[x= y= z=] [id=] [uuid=] [place=]', doc: 'read one nearby villager profession and numbered offers; place= explicitly associates it with a saved place' },
  trade: { section: 'item', args: 'offer= [times=1] [x= y= z=] [id=] [uuid=] [place=]', doc: 'buy a numbered offer from one nearby villager, verify the inventory change and optionally set place=' },
  // ---- creature
  attack: { section: 'creature', args: 'mob= [id=] [leash=24]', doc: 'hunt one animal or monster: the nearest of its kind, or the id= that animals gave you; it leaves the drops lying where they fall' },
  shear: { section: 'creature', args: '[count=] [within=40]', doc: 'wool without killing: needs shears' },
  feed: { section: 'creature', args: 'mob= [id=]', doc: 'walk to one animal and hold out the food it breeds on (id= from animals)' },
  escort: { section: 'creature', args: 'mob= x= y= z= [count=] [within=32] [penned=] [range=]', doc: 'the walk itself: fetch the animals and bring them to a spot, stopping for stragglers (flock.lead is the whole job); with leads in your pocket it puts up to count= (2) of them on leads instead and pulls them along, gates included, taking the leads off at the goal' },
  leash: { section: 'creature', args: 'mob=|id= [count=1] [within=16] [penned=]', doc: 'put a lead on the nearest grown such animal (or the one id= names), one lead each, and hold it: it is pulled after me from then on. Answers leashed= (mob#id@x,y,z) and leads= left in the pocket' },
  unleash: { section: 'creature', args: '[x= y= z=]', doc: 'take the leads off every animal on my leads: each lead drops and is picked up (leads=). With a fence post or wall at x= y= z=, ties them to a knot there instead and the leads stay on it (tied=)' },
  'pen.check': { section: 'pen', args: '[x= y= z=] [radius=]', doc: 'walk a fence and find where a pen leaks: gaps, corner gates, rims an animal can hop' },
  // ---- self
  eat: { section: 'self', args: '[item=] [anyway=]', doc: 'eat one of the foods I carry now: the reflex should beat you to it, but when it cannot this says what went wrong. At food 6 or less with nothing else edible I eat the never-eat list too (rotten flesh: its hunger cannot take me below where the empty belly already would); anyway=true does that at any hunger, on your say-so. A meal the plugin calls missing is judged again once the pockets have settled (right after a craft they are still moving) and tried once more' },
  sleep: { section: 'self', args: '[any=] [bed=] [bed_range=]', doc: 'sleep in the nearest free bed within 32 blocks; with none, walk to your own bed (bed=<place>, else your nearest kind=bed mark) when it is within bed_range (default 200) and sleep there' },
  wake: { section: 'self', args: '', doc: 'get out of bed' },
  quit: { section: 'self', args: '', doc: 'stop my body; ./start in the background brings it back' },
  chat: { section: 'self', args: 'message=', doc: 'say something to everyone' },
  whisper: { section: 'self', args: 'player= message=', doc: 'say something to one player' },
  // ---- control
  run: { section: 'control', args: 'steps=', doc: 'run one bounded EDN flow form, e.g. (seq (action :goto {:x 4 :y 64 :z 2}) (when (= (read :block_at {:x 4 :y 64 :z 2} [:properties :open]) true) 30 (action :toggle {:x 4 :y 64 :z 2 :open false}))). Compose seq, when, any, action; conditions use read, and, or, not and comparisons. A when body may be any flow node. A wait starts at its sequence position; any polls branches in order and runs the first ready branch (ties go to the earlier branch). Only allowlisted observations and existing commands are available; timeout runs no branch action. Legacy object-list steps remain supported' },
  stop: { section: 'control', args: '', doc: 'cancel the active job after cleanup, clear the pending queue, and stop following' },
  job: { section: 'control', args: 'id=', doc: 'inspect a durable job status and result by ID; body-changing actions return a job ID immediately, or pass sync=true to wait up to waitMs=120000 for its result (wait=true is an alias except for smelt)' },
  jobs: { section: 'control', args: '[after=] [limit=]', doc: 'list durable job status, active owner, pending queue, and any failure hold; actions queue by default. Use verbose=true on a submitted action to receive detailed progress notifications; otherwise progress stays in job status. Use interrupt=true to cancel the owner after cleanup and run urgent work; queue=false is refused' },
  cancel: { section: 'control', args: 'id=', doc: 'cancel one queued or active job; active cleanup finishes before another body job may start' },
  resume: { section: 'control', args: '[recovered=true]', doc: 'release a normal failure/cancellation hold and resume the FIFO queue. If cleanup reported restorationPending or the body restarted with work pending, inspect/repair the named world state first, then pass recovered=true to acknowledge recovery and release the safety hold' },
  discard: { section: 'control', args: '', doc: 'cancel every queued job and clear the queue hold' },
  watch: { section: 'control', args: 'name= block=|mob=|item= [where=] [count=] [atMost=] [within=] [x= y= z=] [repeat=]', doc: 'tell me when the world comes to look like this' },
  unwatch: { section: 'control', args: 'name=', doc: 'drop a watch' },
  watches: { section: 'control', args: '', doc: 'the watches I have set' },
  reflexes: { section: 'control', args: '[on=]', doc: 'switch the body reflexes (eating, fleeing, bedtime, shutting gates) on or off' },
  control: { section: 'control', args: 'state= [ms=]', doc: 'hold one movement key down by hand (a debugging aid)' },
  scaffold_side: { section: 'control', args: 'x= y= z= from_x= from_y= from_z=', doc: 'place one supported horizontal scaffold beside a specified scaffold, distance at most six from vertical support; verifies loaded clear headroom, reach and actual placement; never walks or digs' },
  center_work_stand: { section: 'control', args: 'x= y= z= support=', doc: 'center within the current supported work cell over an owned solid pillar or scaffold; bounded same-cell motion only, with verified support and clear headroom' },
  scaffold_extend: { section: 'control', args: 'x= y= z= base_y=', doc: 'extend a supported scaffold column by one verified block from beside its base; clicks the side, requires clear loaded headroom and carried scaffolding, and never walks or digs' },
  pillar_up: { section: 'control', args: '[steps=1] [item=]', doc: 'climb 1..4 blocks by normal jumping and placing underfoot; requires full support and a clear jump column, never digs, and verifies actual ascent' },
  wait: { section: 'control', args: '[job=] [seconds=100]', doc: 'wait for a job ID to finish, or wait for an event that needs me; job results remain available with ./mc job id=' },
  dawn: { section: 'control', args: '', doc: 'block until morning; needs no body, so a bodiless night is spent here' },
  clock: { section: 'control', args: '', doc: 'the world time as last seen by any body; needs no body' },
  help: { section: 'control', args: '[<section or action>]', doc: 'this catalogue, one section of it, or everything about one action' }
}
