// Captures real light from a live server as oracle fixtures (engine/js/light-fixtures.mjs), acting only as the body ProbeView.
//   node tools/light-capture.mjs --regions regions.json --out engine/js/fixtures/light
// regions.json: [{name, center: [x, y, z], size: [sx, sy, sz]}]. Light is read from column.dumpLight() decoded in vanilla order
// (prismarine-chunk's getSkyLight/getBlockLight are scrambled for this version).
import fs from 'node:fs'
import path from 'node:path'
import { parseArgs } from 'node:util'
import { createRequire } from 'node:module'
import { execFileSync } from 'node:child_process'
import { encodeColumn } from '../engine/js/view.mjs'
import { parseColumnFile, decodeLight } from './view/web/decode.mjs'
import { writeLightFixture } from '../engine/js/light-fixtures.mjs'

const require = createRequire(import.meta.url)
const mineflayer = require('mineflayer')
const BODY = 'ProbeView'
const SETTLE_MS = 5000
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms))
const root = path.resolve(import.meta.dirname, '..')

const rcon = (...command) => execFileSync('node', ['tools/rcon-raw.mjs', ...command], { cwd: root, encoding: 'utf8' }).trim()

const columnLight = (column, cx, cz, version) => {
  const parsed = parseColumnFile(encodeColumn({ column, x: cx, z: cz, t: 0, body: BODY, mcVersion: version }))
  return decodeLight(parsed.light.bytes, parsed.light.meta, column.worldHeight >> 4)
}

export async function extract ({ bot, origin, size }) {
  const [sx, sy, sz] = size
  const states = new Uint16Array(sx * sy * sz)
  const sky = new Uint8Array(sx * sy * sz)
  const block = new Uint8Array(sx * sy * sz)
  const cache = new Map()
  const minY = bot.game.minY
  for (let x = 0; x < sx; x++) {
    for (let z = 0; z < sz; z++) {
      const wx = origin[0] + x
      const wz = origin[2] + z
      const cx = wx >> 4
      const cz = wz >> 4
      const key = `${cx},${cz}`
      if (!cache.has(key)) {
        const column = bot.world.getColumn(cx, cz)
        cache.set(key, { column, light: columnLight(column, cx, cz, bot.version) })
      }
      const { column, light } = cache.get(key)
      for (let y = 0; y < sy; y++) {
        const wy = origin[1] + y
        const i = (y * sz + z) * sx + x
        states[i] = column.getBlockStateId({ x: wx & 15, y: wy, z: wz & 15 })
        const packed = light[((wy - minY) >> 4) * 4096 + (((wy - minY) & 15) << 8 | (wz & 15) << 4 | (wx & 15))]
        sky[i] = packed >> 4
        block[i] = packed & 15
      }
    }
  }
  return { states, sky, block }
}

const columnsLoaded = (bot, origin, size) => {
  for (let cx = origin[0] >> 4; cx <= (origin[0] + size[0] - 1) >> 4; cx++) {
    for (let cz = origin[2] >> 4; cz <= (origin[2] + size[2] - 1) >> 4; cz++) {
      if (!bot.world.getColumn(cx, cz)) return false
    }
  }
  return true
}

const summarize = ({ origin, size, states, sky, block }, bot) => {
  const at = (wx, wy, wz) => (((wy - origin[1]) * size[2] + (wz - origin[2])) * size[0]) + (wx - origin[0])
  return {
    blockLit: block.reduce((n, v) => n + (v > 0 ? 1 : 0), 0),
    skyFull: sky.reduce((n, v) => n + (v === 15 ? 1 : 0), 0),
    states, at, bot
  }
}

const main = async () => {
  const { values } = parseArgs({ options: { regions: { type: 'string' }, out: { type: 'string' } } })
  const regions = JSON.parse(fs.readFileSync(values.regions, 'utf8'))
  const world = JSON.parse(fs.readFileSync(path.join(root, 'state/worlds/claude/world.json'), 'utf8'))
  const bot = mineflayer.createBot({ host: world.host, port: world.port, username: BODY, auth: 'offline', viewDistance: 'far' })
  bot.on('error', e => console.error('bot error', e.message))
  await new Promise((resolve, reject) => { bot.once('spawn', resolve); bot.once('kicked', r => reject(new Error(`kicked ${JSON.stringify(r)}`))) })
  fs.mkdirSync(values.out, { recursive: true })
  const minY = bot.game.minY
  const top = minY + bot.game.height - 1
  try {
    for (const region of regions) {
      const [cx, cy, cz] = region.center
      const [sx, sy0, sz] = region.size ?? [40, 48, 40]
      const lowY = Math.max(minY, cy - (sy0 >> 1))
      const highY = Math.min(top, cy - (sy0 >> 1) + sy0 - 1)
      const origin = [cx - (sx >> 1), lowY, cz - (sz >> 1)]
      const size = [sx, highY - lowY + 1, sz]
      rcon('effect', 'give', BODY, 'minecraft:resistance', '60', '4', 'true')
      rcon('effect', 'give', BODY, 'minecraft:fire_resistance', '60', '0', 'true')
      rcon('tp', BODY, String(cx), String(cy + 2), String(cz))
      const deadline = Date.now() + 60000
      while (!columnsLoaded(bot, origin, size)) {
        if (Date.now() > deadline) throw new Error(`${region.name}: columns did not load`)
        await sleep(250)
      }
      await sleep(SETTLE_MS)
      const data = await extract({ bot, origin, size })
      const info = summarize({ origin, size, ...data }, bot)
      console.log(`${region.name}: origin ${origin} size ${size} block light>0 ${info.blockLit}, sky=15 ${info.skyFull}`)
      const file = path.join(values.out, `${region.name}.bin`)
      writeLightFixture(file, { name: region.name, version: bot.version, origin, size, ...data, capturedAt: new Date().toISOString() })
      console.log(`  wrote ${file} ${(fs.statSync(file).size / 1024).toFixed(1)} KB`)
    }
  } finally {
    rcon('effect', 'clear', BODY)
    bot.end()
  }
}

if (import.meta.filename === process.argv[1]) {
  await main()
  process.exit(0)
}
