// Glue between the bot and the renderer: copies nearby chunk data into a grid, has render-worker.mjs draw it with the
// block textures in ./textures (extracted from a client jar, see README) and writes what the bot sees to ./snapshots/*.png
import fs from 'node:fs'
import path from 'node:path'
import { Worker } from 'node:worker_threads'
import { makeGrid } from './renderer.mjs'

const MARGIN = 8
export const YAWS = { north: 0, west: 90, south: 180, east: 270 }
const rad = deg => deg * Math.PI / 180

export function makeEyes (bot, { textureDir, snapshotDir }) {
  let shot = 0

  // started on the first look and again after a crash; a crash fails the looks it was drawing, not the body.
  // It holds the process open only while it draws, so a body (or a test) that is done can exit.
  let worker = null
  let asked = 0
  const pending = new Map()
  const painter = () => {
    if (worker) return worker
    worker = new Worker(new URL('./render-worker.mjs', import.meta.url), { workerData: { version: bot.registry.version.minecraftVersion, textureDir } })
    worker.on('message', ({ id, error, ...drawn }) => {
      const asker = pending.get(id)
      pending.delete(id)
      if (!pending.size) worker.unref()
      if (error) asker.reject(new Error(error))
      else asker.resolve(drawn)
    })
    worker.on('error', e => console.error('[eyes] the render worker failed:', e.message))
    worker.on('exit', () => {
      pending.forEach(asker => asker.reject(new Error('the render worker stopped')))
      pending.clear()
      worker = null
    })
    return worker
  }
  const draw = scene => new Promise((resolve, reject) => {
    const id = ++asked
    pending.set(id, { resolve, reject })
    painter().ref()
    worker.postMessage({ id, scene })
  })

  // Copies the chunk data in a box round a cell, `across` blocks out each way and `up` blocks above and below. The box
  // stays centred past the world's floor and ceiling, as a ray starts from inside it; there it is air.
  function snapshotWorld (centre, across, up) {
    const minY = bot.game.minY ?? -64
    const maxY = minY + (bot.game.height ?? 384) - 1
    const origin = { x: centre.x - across, y: centre.y - up, z: centre.z - across }
    const size = { x: across * 2 + 1, y: up * 2 + 1, z: across * 2 + 1 }
    const grid = makeGrid(origin, size)
    let top = -Infinity
    const local = { x: 0, y: 0, z: 0 }
    for (let cx = origin.x >> 4; cx <= (origin.x + size.x - 1) >> 4; cx++) {
      for (let cz = origin.z >> 4; cz <= (origin.z + size.z - 1) >> 4; cz++) {
        const column = bot.world.getColumn(cx, cz)
        if (!column) continue
        for (local.x = 0; local.x < 16; local.x++) {
          for (local.z = 0; local.z < 16; local.z++) {
            for (local.y = Math.max(minY, origin.y); local.y <= Math.min(maxY, origin.y + size.y - 1); local.y++) {
              const id = column.getBlockStateId(local)
              if (!id) continue
              grid.set(cx * 16 + local.x, local.y, cz * 16 + local.z, id)
              if (local.y > top) top = local.y
            }
          }
        }
      }
    }
    return Object.assign(grid, { top })
  }

  // Copying the world is the part of a look the body's own thread pays, so the copy is kept between looks and kept
  // true by block updates. It reaches MARGIN past the view, so the body can step about before it is copied again.
  let copy = null
  const forget = () => { copy = null }
  bot.on('chunkColumnLoad', forget)
  bot.on('chunkColumnUnload', forget)
  // the top only rises: a block taken from under it leaves rays walking a little further, never a wrong picture
  bot.on('blockUpdate', (old, now) => {
    if (!copy) return
    copy.grid.set(now.position.x, now.position.y, now.position.z, now.stateId)
    if (now.stateId && now.position.y > copy.grid.top) copy.grid.top = now.position.y
  })
  const worldAround = (eye, radius) => {
    const at = { x: Math.floor(eye.x), y: Math.floor(eye.y), z: Math.floor(eye.z) }
    const fits = copy && copy.world === bot.world && copy.radius === radius && ['x', 'y', 'z'].every(a => Math.abs(at[a] - copy.centre[a]) <= MARGIN)
    if (!fits) copy = { grid: snapshotWorld(at, radius + MARGIN, Math.min(radius, 48) + MARGIN), world: bot.world, radius, centre: at }
    return copy.grid
  }

  const visibleEntities = () => Object.values(bot.entities)
    .filter(e => e !== bot.entity && e.position)
    .map(e => ({
      name: e.name ?? e.type,
      label: e.username ?? e.name,
      kind: e.type === 'player' ? 'player' : e.type === 'hostile' || e.kind === 'Hostile mobs' ? 'hostile' : e.type,
      x: e.position.x,
      y: e.position.y,
      z: e.position.z,
      width: e.name === 'item' ? 0.35 : e.width || 0.6,
      height: e.name === 'item' ? 0.35 : e.height || 1.8
    }))

  // look {pano} | {dir: north|south|east|west} | {yaw, pitch in degrees} | {x,y,z to look towards}; default: where the bot is facing
  return async function look (a = {}) {
    const eye = bot.entity.position.offset(0, bot.entity.eyeHeight ?? 1.62, 0)
    const towards = a.x === undefined ? null : { dx: a.x + 0.5 - eye.x, dy: (a.y ?? eye.y) + 0.5 - eye.y, dz: a.z + 0.5 - eye.z }
    const yaw = towards ? Math.atan2(-towards.dx, -towards.dz) : a.dir ? rad(YAWS[a.dir]) : a.yaw !== undefined ? rad(a.yaw) : bot.entity.yaw
    const pitch = towards ? Math.atan2(towards.dy, Math.hypot(towards.dx, towards.dz)) : a.pitch !== undefined ? rad(a.pitch) : a.dir || a.yaw !== undefined ? 0 : bot.entity.pitch
    if (Number.isNaN(yaw)) throw new Error('dir must be north, south, east or west')
    const panorama = Boolean(a.pano)
    // an image costs the driver about width*height/750 tokens: ~170 for a view, ~250 for a panorama
    const width = a.width ?? (panorama ? 864 : 480)
    const height = a.height ?? (panorama ? 216 : 270)
    const maxDist = Math.min(a.dist ?? 64, 96)
    const { origin, size, data, top } = worldAround(eye, maxDist)
    const out = await draw({
      grid: { origin, size, data, top }, eye: { x: eye.x, y: eye.y, z: eye.z }, entities: visibleEntities(), timeOfDay: bot.time.timeOfDay,
      width, height, maxDist, panorama, yaw, pitch, fov: a.fov ?? 100
    })
    fs.mkdirSync(snapshotDir, { recursive: true })
    const file = path.join(snapshotDir, a.file ?? `look-${String(++shot).padStart(3, '0')}.png`)
    fs.writeFileSync(file, out.png)
    const facing = Object.keys(YAWS).reduce((best, k) => Math.cos(rad(YAWS[k]) - yaw) > Math.cos(rad(YAWS[best]) - yaw) ? k : best)
    return {
      file: path.relative(process.cwd(), file),
      view: panorama ? 'pano N=centre W=left E=right S=edges' : `${facing} pitch ${Math.round(pitch * 180 / Math.PI)}`,
      blocked: out.near >= 0.4 ? `${Math.round(out.near * 100)}% of the view is a wall under 2m away: move or look another way before reading the picture` : null,
      seen: out.seen.map(e => `${e.name} ${e.dist}m @px${e.px},${e.py}`)
    }
  }
}
