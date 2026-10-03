// A minimal stand-in for a mineflayer bot, enough for primitives.mjs tests. Never connects to anything.
// Every method that talks to the world is recorded in `calls`; methods listed in `hang` never settle and
// methods named in `reject` ({name: message}) reject with an Error carrying that message.
import vec3 from 'vec3'
import { EventEmitter } from 'node:events'

const { Vec3 } = vec3

const never = () => new Promise(() => {})
const key = (x, y, z) => `${x},${y},${z}`

export function stubBot ({ oxygen = 20, blocks = {}, items = [], entities = {}, hang = [], reject = {}, pos = [0, 64, 0], food = 10, timeOfDay = 15000, containers = {}, props = {}, onActivate = () => {} } = {}) {
  const calls = []
  const bot = new EventEmitter()
  const hangs = new Set(hang)
  const act = (name, impl = () => undefined) => (...args) => {
    calls.push({ name, args })
    if (name in reject) return Promise.reject(new Error(reject[name]))
    return hangs.has(name) ? never() : Promise.resolve(impl(...args))
  }
  const blockAt = v => {
    const name = blocks[key(v.x, v.y, v.z)]
    if (name === undefined) return { name: 'air', position: new Vec3(v.x, v.y, v.z), boundingBox: 'empty', diggable: false, getProperties: () => ({}) }
    return { name, position: new Vec3(v.x, v.y, v.z), boundingBox: 'block', diggable: name !== 'bedrock', getProperties: () => props[key(v.x, v.y, v.z)] ?? {} }
  }
  const windowOf = pos => ({
    containerItems: () => containers[pos] ?? [],
    deposit: act('deposit'),
    withdraw: act('withdraw')
  })
  Object.assign(bot, {
    username: 'Stub',
    entity: { position: new Vec3(...pos), height: 1.62, onGround: true },
    entities,
    health: 20,
    food,
    time: { timeOfDay },
    isSleeping: false,
    heldItem: null,
    inventory: { items: () => items },
    registry: { itemsByName: { bread: { id: 1 }, cobblestone: { id: 2 } }, foodsByName: { bread: { foodPoints: 5 }, apple: { foodPoints: 4 } } },
    calls,
    blockAt: v => { calls.push({ name: 'blockAt', args: [v] }); return blockAt(v) },
    findBlocks: ({ matching, maxDistance, count }) => {
      const found = Object.keys(blocks).map(k => new Vec3(...k.split(',').map(Number)))
        .filter(p => matching(blockAt(p)) && p.distanceTo(bot.entity.position) <= maxDistance)
      return found.slice(0, count)
    },
    pathfinder: { goto: act('goto'), setGoal: act('setGoal'), setMovements: () => {} },
    oxygenLevel: oxygen,
    clearControlStates: act('clearControlStates'),
    setControlState: act('setControlState'),
    dig: act('dig'),
    stopDigging: act('stopDigging'),
    placeBlock: act('placeBlock'),
    equip: act('equip'),
    consume: act('consume'),
    deactivateItem: act('deactivateItem'),
    activateItem: act('activateItem', () => onActivate(bot)),
    attack: act('attack'),
    sleep: act('sleep'),
    wake: act('wake'),
    lookAt: act('lookAt'),
    look: act('look'),
    closeWindow: act('closeWindow'),
    openContainer: act('openContainer', block => windowOf(key(block.position.x, block.position.y, block.position.z))),
    quit: act('quit')
  })
  return bot
}
export const names = bot => bot.calls.map(c => c.name)
export { Vec3 }
