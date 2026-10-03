// Sets up live tests of a Minecraft bot against the local server: sends a small allow-list of RCON commands
// (time, weather, tp, give, effects, damage, summon, setblock, fill, ...) aimed at allowed test players only.
//   node tools/rcon-test.mjs <subcommand> [args...]      e.g. node tools/rcon-test.mjs give ClaudeProbe bread 3
// Needs enable-rcon=true in server.properties and the password in ~/.config/minecraft-claude/rcon-password (chmod 600).
// This is a guard rail against accidents, not a security boundary: anything running as the same OS user could read
// the password file and speak RCON itself.
//
// To extend: edit the constant lists below (ITEMS, EFFECTS, ENTITIES, BLOCKS) or add a case to `builders`.
// Extra target players: set RCON_TEST_TARGETS=Name1,Name2 (each must be a valid player name); the default allow-list (ClaudeProbe and the Probe* bodies) is always allowed.
// With broadcast-rcon-to-ops=true, ops see every command sent here in chat.
//
// buildCommand always returns an ARRAY of command strings (summon with a count sends several).
// Limitation: `fire` deals 1 fire damage via `damage ... minecraft:on_fire` but does NOT ignite the player.
import fs from 'node:fs'
import net from 'node:net'
import os from 'node:os'
import path from 'node:path'
import { encodePacket, decodePacket } from './rcon.mjs'

const AUTH = 3
const COMMAND = 2

const DEFAULT_TARGETS = ['ClaudeProbe', 'ProbeWater', 'ProbeFight', 'ProbeNight', 'ProbeStuck']
const ITEMS = ['bread', 'cooked_beef', 'apple', 'water_bucket', 'bucket', 'cobblestone', 'dirt', 'oak_planks', 'iron_sword',
  'diamond_sword', 'iron_axe', 'torch', 'oak_sapling', 'wheat_seeds', 'red_bed', 'shield', 'leather_helmet', 'iron_helmet']
const EFFECTS = ['poison', 'wither', 'hunger', 'instant_damage', 'regeneration', 'saturation', 'fire_resistance',
  'water_breathing', 'slowness']
const ENTITIES = ['zombie', 'skeleton', 'spider', 'creeper', 'cow', 'pig', 'sheep', 'chicken']
const BLOCKS = ['air', 'water', 'lava', 'stone', 'dirt', 'cobblestone', 'oak_planks', 'fire', 'glass', 'torch', 'red_bed', 'chest']

const NAME = /^[A-Za-z0-9_]{3,16}$/
const NUMBER = /^[+-]?\d+(\.\d+)?$/
const INTEGER = /^[+-]?\d+$/
const ID = /^[a-z_]+$/

const MAX_GIVE = 64
const MAX_EFFECT_SECONDS = 120
const MAX_AMPLIFIER = 4
const MAX_DAMAGE = 20
const MAX_SUMMON = 5
const MAX_RADIUS = 32
// kill-mobs targets only these: a selector cannot or-together types, so it is one kill command per type, and
// villagers, pets, item frames and players are never in the list
const HOSTILE_MOBS = ['zombie', 'skeleton', 'creeper', 'spider', 'enderman', 'witch', 'drowned', 'husk', 'stray', 'zombie_villager', 'phantom', 'slime', 'pillager', 'vindicator']
const MAX_FILL_VOLUME = 500

export function targetsFromEnv (env) {
  const extra = (env.RCON_TEST_TARGETS ?? '').split(',').map(s => s.trim()).filter(Boolean)
  extra.forEach(name => name$(name))
  return [...DEFAULT_TARGETS, ...extra]
}

function name$ (name) {
  if (!NAME.test(name)) throw new Error(`not a valid player name: ${JSON.stringify(name)}`)
  return name
}

const usageError = usage => new Error(`usage: ${usage}`)

function target (value, targets) {
  if (!NAME.test(value) || !targets.includes(value)) {
    throw new Error(`not an allowed target: ${JSON.stringify(value)} (allowed: ${targets.join(', ')})`)
  }
  return value
}

function number (value, what) {
  if (!NUMBER.test(value)) throw new Error(`${what} must be a plain number, got ${JSON.stringify(value)}`)
  return value
}

function integer (value, what) {
  if (!INTEGER.test(value)) throw new Error(`${what} must be an integer, got ${JSON.stringify(value)}`)
  return Number(value)
}

function boundedInt (value, what, min, max) {
  const n = integer(value, what)
  if (n < min || n > max) throw new Error(`${what} must be between ${min} and ${max}, got ${n}`)
  return n
}

function oneOf (value, list, what) {
  if (!ID.test(value) || !list.includes(value)) {
    throw new Error(`${what} not allowed: ${JSON.stringify(value)} (allowed: ${list.join(', ')})`)
  }
  return value
}

function coords (values) {
  return values.map(v => number(v, 'coordinate'))
}

// each builder: [arity range, (args, targets) => string[]]; arity is checked before the builder runs
const builders = {
  list: [[0, 0], '', () => ['list']],
  time: [[1, 1], '<day|night|midnight|noon|N>', ([v]) => {
    if (['day', 'night', 'midnight', 'noon'].includes(v)) return [`time set ${v}`]
    if (!/^\d+$/.test(v)) throw new Error(`time must be day, night, midnight, noon or a non-negative integer, got ${JSON.stringify(v)}`)
    return [`time set ${v}`]
  }],
  weather: [[1, 1], '<clear|rain|thunder>', ([v]) => {
    if (!['clear', 'rain', 'thunder'].includes(v)) throw new Error(`weather must be clear, rain or thunder, got ${JSON.stringify(v)}`)
    return [`weather ${v}`]
  }],
  tp: [[4, 4], '<target> <x> <y> <z>', ([t, ...xyz], targets) => [`tp ${target(t, targets)} ${coords(xyz).join(' ')}`]],
  give: [[2, 3], '<target> <item> [count<=64]', ([t, item, count = '1'], targets) =>
    [`give ${target(t, targets)} minecraft:${oneOf(item, ITEMS, 'item')} ${boundedInt(count, 'count', 1, MAX_GIVE)}`]],
  clear: [[1, 1], '<target>', ([t], targets) => [`clear ${target(t, targets)}`]],
  effect: [[2, 4], '<target> <effect> [seconds<=120] [amplifier<=4]', ([t, effect, seconds = '30', amplifier = '0'], targets) =>
    [`effect give ${target(t, targets)} minecraft:${oneOf(effect, EFFECTS, 'effect')} ` +
      `${boundedInt(seconds, 'seconds', 1, MAX_EFFECT_SECONDS)} ${boundedInt(amplifier, 'amplifier', 0, MAX_AMPLIFIER)}`]],
  'effect-clear': [[1, 1], '<target>', ([t], targets) => [`effect clear ${target(t, targets)}`]],
  damage: [[2, 2], '<target> <amount<=20>', ([t, amount], targets) => {
    const a = number(amount, 'amount')
    if (!(Number(a) > 0 && Number(a) <= MAX_DAMAGE)) throw new Error(`amount must be positive and at most ${MAX_DAMAGE}, got ${a}`)
    return [`damage ${target(t, targets)} ${a}`]
  }],
  heal: [[1, 1], '<target>', ([t], targets) => [`effect give ${target(t, targets)} minecraft:instant_health 1 10`]],
  feed: [[1, 1], '<target>', ([t], targets) => [`effect give ${target(t, targets)} minecraft:saturation 1 10`]],
  fire: [[1, 1], '<target>  (1 fire damage only; does not ignite)', ([t], targets) =>
    [`damage ${target(t, targets)} 1 minecraft:on_fire`]],
  summon: [[4, 5], '<entity> <x> <y> <z> [count<=5]', ([entity, ...rest]) => {
    const [x, y, z, count = '1'] = rest
    const id = oneOf(entity, ENTITIES, 'entity')
    const pos = coords([x, y, z]).join(' ')
    return Array.from({ length: boundedInt(count, 'count', 1, MAX_SUMMON) }, () => `summon minecraft:${id} ${pos}`)
  }],
  'kill-mobs': [[4, 4], '<x> <y> <z> <radius<=32>', ([x, y, z, radius]) => {
    const [cx, cy, cz] = coords([x, y, z])
    const r = number(radius, 'radius')
    if (!(Number(r) > 0 && Number(r) <= MAX_RADIUS)) throw new Error(`radius must be positive and at most ${MAX_RADIUS}, got ${r}`)
    return HOSTILE_MOBS.map(type => `kill @e[type=minecraft:${type},x=${cx},y=${cy},z=${cz},distance=..${r}]`)
  }],
  setblock: [[4, 4], '<x> <y> <z> <block>', ([x, y, z, block]) =>
    [`setblock ${coords([x, y, z]).join(' ')} minecraft:${oneOf(block, BLOCKS, 'block')}`]],
  fill: [[7, 7], '<x1> <y1> <z1> <x2> <y2> <z2> <block>', ([...args]) => {
    const block = oneOf(args[6], BLOCKS, 'block')
    const [x1, y1, z1, x2, y2, z2] = args.slice(0, 6).map(v => integer(v, 'fill coordinate'))
    const volume = (Math.abs(x2 - x1) + 1) * (Math.abs(y2 - y1) + 1) * (Math.abs(z2 - z1) + 1)
    if (volume > MAX_FILL_VOLUME) throw new Error(`fill volume ${volume} exceeds ${MAX_FILL_VOLUME}`)
    return [`fill ${x1} ${y1} ${z1} ${x2} ${y2} ${z2} minecraft:${block}`]
  }]
}

// argv -> array of command strings; throws Error on anything not allowed
export function buildCommand (argv, { targets = DEFAULT_TARGETS } = {}) {
  const [sub, ...args] = argv
  if (!Object.hasOwn(builders, sub ?? '')) throw new Error('unknown or forbidden subcommand')
  const [[min, max], usage, build] = builders[sub]
  if (args.length < min || args.length > max) throw usageError(`${sub} ${usage}`.trim())
  return build(args, targets)
}

function exchange (socket, id, type, body) {
  return new Promise((resolve, reject) => {
    let pending = Buffer.alloc(0)
    const onData = chunk => {
      pending = Buffer.concat([pending, chunk])
      const packet = decodePacket(pending)
      if (!packet) return
      socket.off('data', onData)
      resolve(packet)
    }
    socket.on('data', onData)
    socket.once('error', reject)
    socket.write(encodePacket(id, type, body))
  })
}

async function send (commands) {
  const props = fs.readFileSync('/home/dan/minecraft/claude/server.properties', 'utf8')
  const port = Number(/^rcon\.port=(\d+)/m.exec(props)?.[1] ?? 25575)
  const password = fs.readFileSync(path.join(os.homedir(), '.config/minecraft-claude/rcon-password'), 'utf8').trim()
  const socket = net.connect({ host: '127.0.0.1', port })
  await new Promise((resolve, reject) => socket.once('connect', resolve).once('error', reject))
  const auth = await exchange(socket, 1, AUTH, password)
  if (auth.id === -1) throw new Error('RCON refused the password')
  const replies = []
  for (const [i, command] of commands.entries()) replies.push((await exchange(socket, i + 2, COMMAND, command)).body)
  socket.end()
  return replies
}

async function main (argv) {
  const commands = buildCommand(argv, { targets: targetsFromEnv(process.env) })
  const replies = await send(commands)
  replies.filter(Boolean).forEach(r => console.log(r))
}

if (import.meta.filename === process.argv[1]) main(process.argv.slice(2)).catch(e => { console.error(`rcon-test failed: ${e.message}`); process.exit(1) })
