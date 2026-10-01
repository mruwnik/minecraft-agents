// Glue between the bot and the renderer: copies nearby chunk data into a grid, has render-worker.mjs draw it with the
// block textures in ./textures (extracted from a client jar, see README) and writes what the bot sees to ./snapshots/*.png
import fs from 'node:fs'
import path from 'node:path'
import { Worker } from 'node:worker_threads'
import { makeGrid } from './renderer.mjs'

const MARGIN = 8
export const YAWS = { north: 0, west: 90, south: 180, east: 270 }
const rad = deg => deg * Math.PI / 180

// Everything a picture depends on, coarse enough that a body standing still answers the same key: the eye to a
// sixteenth of a block, the direction to half a degree, the day to a hundred ticks, entities to a quarter block and
// a sixteenth of a turn, and the world copy's identity and edit count
export const lookKey = ({ eye, yaw, pitch, timeOfDay, entities, world, width, height, maxDist, panorama, fov }) => [
  Math.round(eye.x * 16), Math.round(eye.y * 16), Math.round(eye.z * 16),
  Math.round(yaw * 360 / Math.PI), Math.round(pitch * 360 / Math.PI),
  Math.floor((timeOfDay ?? 0) / 100),
  world, width, height, maxDist, panorama ? 'pano' : 'view', fov,
  ...entities.map(e => `${e.name}@${Math.round(e.x * 4)},${Math.round(e.y * 4)},${Math.round(e.z * 4)}/${Math.round((e.yaw ?? 0) * 8 / Math.PI)}`)
].join('|')

export function makeEyes (bot, { textureDir, snapshotDir }) {
  let shot = 0
  let last = null

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
  let copies = 0
  const forget = () => { copy = null }
  bot.on('chunkColumnLoad', forget)
  bot.on('chunkColumnUnload', forget)
  // the top only rises: a block taken from under it leaves rays walking a little further, never a wrong picture
  bot.on('blockUpdate', (old, now) => {
    if (!copy) return
    copy.grid.set(now.position.x, now.position.y, now.position.z, now.stateId)
    if (now.stateId && now.position.y > copy.grid.top) copy.grid.top = now.position.y
    copy.edits++
  })
  const worldAround = (eye, radius) => {
    const at = { x: Math.floor(eye.x), y: Math.floor(eye.y), z: Math.floor(eye.z) }
    const fits = copy && copy.world === bot.world && copy.radius === radius && ['x', 'y', 'z'].every(a => Math.abs(at[a] - copy.centre[a]) <= MARGIN)
    if (!fits) copy = { grid: snapshotWorld(at, radius + MARGIN, Math.min(radius, 48) + MARGIN), world: bot.world, radius, centre: at, id: ++copies, edits: 0 }
    return copy
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
      width: e.name === 'item' ? 0.35 : e.width || bot.registry.entitiesByName[e.name]?.width || 0.6,
      height: e.name === 'item' ? 0.35 : e.height || bot.registry.entitiesByName[e.name]?.height || 1.8,
      yaw: e.yaw ?? 0
    }))

  // look {pano} | {dir: north|south|east|west} | {yaw, pitch in degrees} | {x,y,z to look towards}; default: where the bot is facing.
  // {marks: true} adds each seen entity's outline
  const look = async function look (a = {}) {
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
    const entities = visibleEntities()
    const world = worldAround(eye, maxDist)
    const key = lookKey({ eye, yaw, pitch, timeOfDay: bot.time.timeOfDay, entities, world: `${world.id}:${world.edits}`, width, height, maxDist, panorama, fov: a.fov ?? 100 })
    // a body standing still is asked for the same picture ten times a second: the worker draws it once
    const cached = last?.key === key
    if (!cached) look.draws++
    const out = cached ? last.out : await draw({
      grid: { origin: world.grid.origin, size: world.grid.size, data: world.grid.data, top: world.grid.top }, eye: { x: eye.x, y: eye.y, z: eye.z }, entities, timeOfDay: bot.time.timeOfDay,
      width, height, maxDist, panorama, yaw, pitch, fov: a.fov ?? 100
    })
    last = { key, out }
    fs.mkdirSync(snapshotDir, { recursive: true })
    const file = path.join(snapshotDir, a.file ?? `look-${String(++shot).padStart(3, '0')}.png`)
    fs.writeFileSync(file, out.png)
    const facing = Object.keys(YAWS).reduce((best, k) => Math.cos(rad(YAWS[k]) - yaw) > Math.cos(rad(YAWS[best]) - yaw) ? k : best)
    return {
      file: path.relative(process.cwd(), file),
      at: { x: Math.floor(bot.entity.position.x), y: Math.floor(bot.entity.position.y), z: Math.floor(bot.entity.position.z) },
      view: panorama ? 'pano N=centre W=left E=right S=edges' : `${facing} pitch ${Math.round(pitch * 180 / Math.PI)}`,
      blocked: out.near >= 0.4 ? `${Math.round(out.near * 100)}% of the view is a wall under 2m away: move or look another way before reading the picture` : null,
      seen: out.seen.map(e => `${e.name} ${e.dist}m @px${e.px},${e.py}`),
      // for the dashboard's outlines, in fractions of the picture so a scaled image needs no size; the driver never asks
      ...(a.marks ? { marks: out.seen.map(e => ({ name: e.name, kind: e.kind, dist: e.dist, box: [e.box[0] / width, e.box[1] / height, (e.box[2] + 1) / width, (e.box[3] + 1) / height].map(v => Math.round(v * 1000) / 1000) })) } : {})
    }
  }
  // tests only: how many looks actually asked the worker to draw, versus answering from the cache
  look.draws = 0
  return look
}
