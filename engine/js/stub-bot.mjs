// A minimal stand-in for a mineflayer bot, enough for primitives.mjs tests. Never connects to anything.
// Every method that talks to the world is recorded in `calls`; methods listed in `hang` never settle and
// methods named in `reject` ({name: message}) reject with an Error carrying that message.
import vec3 from 'vec3'
import { EventEmitter } from 'node:events'

const { Vec3 } = vec3

const never = () => new Promise(() => {})
const key = (x, y, z) => `${x},${y},${z}`

export function stubBot ({ oxygen = 20, blocks = {}, items = [], worn = [], entities = {}, hang = [], reject = {}, pos = [0, 64, 0], food = 10, timeOfDay = 15000, containers = {}, props = {}, shapes = {}, effects = [], unloaded = false, sleeping = false, onActivate = () => {}, onUseBlock = () => {}, freeSlot = 9, held = null, moveCap = Infinity, onClick = () => {} } = {}) {
  const calls = []
  const bot = new EventEmitter()
  const hangs = new Set(hang)
  const asleepRejects = new Set(['goto', 'setGoal', 'setControlState', 'dig', 'placeBlock', '_placeBlockWithOptions', 'attack', 'activateItem', 'equip', 'toss', 'tossStack'])
  const act = (name, impl = () => undefined) => (...args) => {
    calls.push({ name, args })
    if (bot.isSleeping && asleepRejects.has(name)) return Promise.reject(new Error('asleep'))
    if (name in reject) return Promise.reject(new Error(reject[name]))
    return hangs.has(name) ? never() : Promise.resolve(impl(...args))
  }
  let notLoaded = unloaded // the chunks have not arrived: blockAt answers null everywhere until loadWorld()
  const blockAt = v => {
    if (notLoaded) return null
    const name = blocks[key(v.x, v.y, v.z)]
    if (name === undefined) return { name: 'air', position: new Vec3(v.x, v.y, v.z), boundingBox: 'empty', diggable: false, getProperties: () => ({}) }
    return { name, position: new Vec3(v.x, v.y, v.z), boundingBox: 'block', shapes: shapes[key(v.x, v.y, v.z)] ?? [[0, 0, 0, 1, 1, 1]], diggable: name !== 'bedrock', getProperties: () => props[key(v.x, v.y, v.z)] ?? {} }
  }
  const ownColumn = v => { const p = bot.entity.position; return v.x === p.x && v.y === p.y && v.z === p.z }
  // A window moves items in the stub's container (at most moveCap of them), then calls onClick(window) so a test can
  // emit updateSlot the way mineflayer's late resyncs do.
  const windowOf = pos => {
    const win = new EventEmitter()
    const move = (name, sign) => (type, meta, count) => {
      const item = (containers[pos] ?? []).find(i => i.name === name)
      if (item) item.count += sign * Math.min(count, moveCap)
      onClick(win)
    }
    const names = { 1: 'bread', 2: 'cobblestone' }
    return Object.assign(win, {
      containerItems: () => containers[pos] ?? [],
      deposit: act('deposit', (type, meta, count) => move(names[type], 1)(type, meta, count)),
      withdraw: act('withdraw', (type, meta, count) => move(names[type], -1)(type, meta, count))
    })
  }
  Object.assign(bot, {
    username: 'Stub',
    entity: { position: new Vec3(...pos), height: 1.62, onGround: true, velocity: new Vec3(0, 0, 0), effects: Object.fromEntries(effects.map(e => [e.id, { id: e.id, amplifier: e.amplifier, duration: e.duration }])) },
    entities,
    health: 20,
    food,
    time: { timeOfDay },
    isSleeping: sleeping,
    heldItem: held,
    inventory: { items: () => items, slots: Object.fromEntries([...items, ...worn].map(i => [i.slot, i])), firstEmptyInventorySlot: () => freeSlot },
    registry: { effects: Object.fromEntries(effects.map(e => [e.id, { id: e.id, name: e.name }])), itemsByName: { bread: { id: 1 }, cobblestone: { id: 2 }, iron_helmet: { id: 3, maxDurability: 165 }, iron_boots: { id: 4, maxDurability: 195 }, shield: { id: 5, maxDurability: 336 } }, foodsByName: { bread: { foodPoints: 5 }, apple: { foodPoints: 4 } } },
    physics: { playerHalfWidth: 0.3 },
    calls,
    loadWorld: () => { notLoaded = false },
    unloadWorld: () => { notLoaded = true },
    // the probe at the body's own position (chunkLoaded, the physics watchdog) is not a world read worth recording
    blockAt: v => { if (!ownColumn(v)) calls.push({ name: 'blockAt', args: [v] }); return blockAt(v) },
    findBlocks: ({ matching, maxDistance, count }) => {
      const found = Object.keys(blocks).map(k => new Vec3(...k.split(',').map(Number)))
        .filter(p => matching(blockAt(p)) && p.distanceTo(bot.entity.position) <= maxDistance)
      return found.slice(0, count)
    },
    pathfinder: { goto: act('goto'), setGoal: act('setGoal'), setMovements: () => {} },
    oxygenLevel: oxygen,
    controlState: {},
    clearControlStates: act('clearControlStates', () => { bot.controlState = {} }),
    setControlState: act('setControlState', (control, state) => { bot.controlState[control] = state }),
    dig: act('dig'),
    stopDigging: act('stopDigging'),
    placeBlock: act('placeBlock'),
    _placeBlockWithOptions: act('_placeBlockWithOptions'),
    equip: act('equip'),
    toss: act('toss'),
    tossStack: act('tossStack'),
    consume: act('consume'),
    deactivateItem: act('deactivateItem'),
    activateItem: act('activateItem', () => onActivate(bot)),
    attack: act('attack'),
    activateBlock: act('activateBlock', (block, face, cursor) => onUseBlock(bot, block, face, cursor)),
    setQuickBarSlot: act('setQuickBarSlot'),
    unequip: act('unequip'),
    sleep: act('sleep', () => { bot.isSleeping = true }),
    wake: act('wake'), // records only: the real bot.wake() sends a wrong id on this protocol
    // the raw client: leave_bed (by name) is what gets the body out of bed
    _client: Object.assign(new EventEmitter(), { write: (name, data) => { calls.push({ name: 'write', args: [name, data] }); if (name === 'entity_action' && data.actionId === 'leave_bed') bot.isSleeping = false } }),
    useOn: act('useOn'),
    lookAt: act('lookAt', point => {
      const delta = point.minus(bot.entity.position.offset(0, bot.entity.height, 0))
      Object.assign(bot.entity, { yaw: Math.atan2(-delta.x, -delta.z), pitch: Math.atan2(delta.y, Math.hypot(delta.x, delta.z)) })
    }),
    look: act('look', (yaw, pitch) => { Object.assign(bot.entity, { yaw, pitch }) }),
    closeWindow: act('closeWindow'),
    openContainer: act('openContainer', block => windowOf(key(block.position.x, block.position.y, block.position.z))),
    openFurnace: act('openFurnace', () => Object.assign(new EventEmitter(), { slots: [], close: act('close'), putInput: act('putInput'), putFuel: act('putFuel'), takeOutput: act('takeOutput') })),
    openEnchantmentTable: act('openEnchantmentTable', () => Object.assign(new EventEmitter(), { slots: [], enchantments: [], close: act('close') })),
    quit: act('quit')
  })
  return bot
}
export const names = bot => bot.calls.map(c => c.name)
export { Vec3 }
