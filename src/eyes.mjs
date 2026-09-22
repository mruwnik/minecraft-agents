// Glue between the bot and vision.mjs: copies nearby chunk data into a grid, loads block textures from
// ./textures (extracted from a client jar, see README) and writes what the bot sees to ./snapshots/*.png
import fs from 'node:fs'
import path from 'node:path'
import prismarineBlock from 'prismarine-block'
import { decodePng, encodePng, makeGrid, render, textureCandidates } from './vision.mjs'

const GRASS = [124, 189, 107]
const FOLIAGE = [89, 174, 48]
const TINTS = [
  [/^(grass_block_top|short_grass|tall_grass_(top|bottom)|fern|large_fern_(top|bottom))$/, GRASS],
  [/^birch_leaves$/, [128, 167, 85]],
  [/^spruce_leaves$/, [97, 153, 97]],
  [/^(oak|jungle|acacia|dark_oak|mangrove)_leaves$|^vine$|^lily_pad$/, FOLIAGE],
  [/^water_still$/, [63, 118, 228]]
]
const AIR = new Set(['air', 'cave_air', 'void_air', 'light', 'barrier', 'structure_void'])
const FULL_CUBE = JSON.stringify([[0, 0, 0, 1, 1, 1]])
export const YAWS = { north: 0, west: 90, south: 180, east: 270 }
const rad = deg => deg * Math.PI / 180

export function makeEyes (bot, { textureDir, snapshotDir }) {
  const Block = prismarineBlock(bot.registry)
  const images = new Map()
  const blockInfo = new Map()
  const faceTextures = new Map()
  let shot = 0

  const image = name => {
    if (images.has(name)) return images.get(name)
    const file = path.join(textureDir, `${name}.png`)
    const decoded = fs.existsSync(file) ? { ...decodePng(fs.readFileSync(file)), tint: TINTS.find(([re]) => re.test(name))?.[1] } : null
    images.set(name, decoded)
    return decoded
  }

  const texture = (block, face, props) => {
    const key = `${block}|${face}|${props?.half ?? ''}|${props?.age ?? ''}`
    if (!faceTextures.has(key)) faceTextures.set(key, textureCandidates(block, face, props).map(image).find(Boolean) ?? null)
    return faceTextures.get(key)
  }

  const describe = stateId => {
    const b = Block.fromStateId(stateId, 0)
    if (AIR.has(b.name)) return null
    const base = { name: b.name, props: b.getProperties() }
    if (b.name === 'water' || b.name === 'lava') return { ...base, kind: 'cube' }
    if (!b.shapes.length) return { ...base, kind: 'cross' }
    return JSON.stringify(b.shapes) === FULL_CUBE ? { ...base, kind: 'cube' } : { ...base, kind: 'boxes', boxes: b.shapes }
  }
  const info = stateId => {
    if (!blockInfo.has(stateId)) blockInfo.set(stateId, describe(stateId))
    return blockInfo.get(stateId)
  }

  function snapshotWorld (eye, radius) {
    const minY = bot.game.minY ?? -64
    const maxY = minY + (bot.game.height ?? 384) - 1
    const vertical = Math.min(radius, 48)
    const origin = { x: Math.floor(eye.x) - radius, y: Math.max(minY, Math.floor(eye.y) - vertical), z: Math.floor(eye.z) - radius }
    const size = { x: radius * 2 + 1, y: Math.min(maxY, Math.floor(eye.y) + vertical) - origin.y + 1, z: radius * 2 + 1 }
    const grid = makeGrid(origin, size)
    const local = { x: 0, y: 0, z: 0 }
    for (let cx = origin.x >> 4; cx <= (origin.x + size.x - 1) >> 4; cx++) {
      for (let cz = origin.z >> 4; cz <= (origin.z + size.z - 1) >> 4; cz++) {
        const column = bot.world.getColumn(cx, cz)
        if (!column) continue
        for (local.x = 0; local.x < 16; local.x++) {
          for (local.z = 0; local.z < 16; local.z++) {
            for (local.y = origin.y; local.y < origin.y + size.y; local.y++) {
              const id = column.getBlockStateId(local)
              if (id) grid.set(cx * 16 + local.x, local.y, cz * 16 + local.z, id)
            }
          }
        }
      }
    }
    return grid
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
  return function look (a = {}) {
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
    const out = render({
      grid: snapshotWorld(eye, maxDist), info, texture, eye, entities: visibleEntities(), timeOfDay: bot.time.timeOfDay,
      width, height, maxDist, panorama, yaw, pitch, fov: a.fov ?? 100
    })
    fs.mkdirSync(snapshotDir, { recursive: true })
    const file = path.join(snapshotDir, a.file ?? `look-${String(++shot).padStart(3, '0')}.png`)
    fs.writeFileSync(file, encodePng(width, height, out.rgba))
    const facing = Object.keys(YAWS).reduce((best, k) => Math.cos(rad(YAWS[k]) - yaw) > Math.cos(rad(YAWS[best]) - yaw) ? k : best)
    return {
      file: path.relative(process.cwd(), file),
      view: panorama ? 'pano N=centre W=left E=right S=edges' : `${facing} pitch ${Math.round(pitch * 180 / Math.PI)}`,
      blocked: out.near >= 0.4 ? `${Math.round(out.near * 100)}% of the view is a wall under 2m away: move or look another way before reading the picture` : null,
      seen: out.seen.map(e => `${e.name} ${e.dist}m @px${e.px},${e.py}`)
    }
  }
}
