// Why JavaScript: graphics/binary; draws what a body sees from its dumped chunk files.
// What a body sees, drawn from the files it dumps: worlds/<world>/chunks/<cx>.<cz>.bin and
// worlds/<world>/agents/<name>/view/pose.json. Nothing here touches a body or a server.
import fs from 'node:fs'
import path from 'node:path'
import { render, encodePng, castRay, directionFor, hitLight, lightTable, lightReport } from './renderer.mjs'
import { columnCache, makeChunkClass } from './columns.mjs'
import { buildGrid } from './grid.mjs'
import { cameraFromPose, entitiesFromPose } from './camera.mjs'
import { makeBlockSource } from './blocks.mjs'
import { bodyDir, worldsDir } from '../../engine/js/bodies.mjs'

const ROOT = path.join(import.meta.dirname, '../..')
export const DEFAULT_STATE_DIR = { worldsDir: path.resolve(ROOT, 'worlds') }
const TEXTURE_DIR = path.join(ROOT, 'textures')
const UP = 48

// per game version: the chunk class, the columns loaded so far, the block descriptions
const perVersion = new Map()
const forVersion = version => {
  if (perVersion.has(version)) return perVersion.get(version)
  const Chunk = makeChunkClass(version)
  const made = { Chunk, columns: columnCache(Chunk), blocks: makeBlockSource(Chunk.registry, TEXTURE_DIR) }
  perVersion.set(version, made)
  return made
}

export const viewDir = (world, agentName, stateDir) => path.join(bodyDir(stateDir, world, agentName), 'view')

export class PoseError extends Error {
  name = 'PoseError'
}

const finite = v => typeof v === 'number' && Number.isFinite(v)
const hasPosition = pose => ['x', 'y', 'z'].every(k => finite(pose.eye?.[k])) && finite(pose.yaw) && finite(pose.pitch)

export function readPose (world, agentName, stateDir = DEFAULT_STATE_DIR) {
  const file = path.join(viewDir(world, agentName, stateDir), 'pose.json')
  if (!fs.existsSync(file)) throw new Error(`no pose.json for ${agentName} (looked at ${file})`)
  const pose = JSON.parse(fs.readFileSync(file, 'utf8'))
  if (!hasPosition(pose)) throw new PoseError('pose has no position; body never fully started')
  return pose
}

// `pose`: an already read pose (else read from the body's view/pose.json). `aim`: also return `center`, the block the
// view's centre ray hits first, as the crosshair targets it ({name, x, y, z, t, face, light}, t the distance, light
// {sky, block, seeing} on the face hit), or null. Entities in `seen` carry their light too.
export function renderView ({ world, agentName, width = 320, height = 180, fov = 70, maxDist = 64, radius = 8, stateDir = DEFAULT_STATE_DIR, override = {}, noPng = false, pose: given = null, aim = false }) {
  const started = performance.now()
  const pose = given ?? readPose(world, agentName, stateDir)
  const poseDone = performance.now()
  const { columns, blocks } = forVersion(pose.mcVersion)
  const camera = { ...cameraFromPose(pose), ...override }
  const chunkDir = path.join(worldsDir(stateDir), pose.world, 'chunks')
  let loaded = 0
  const grid = buildGrid({
    column: (cx, cz) => {
      const column = columns.get(path.join(chunkDir, `${cx}.${cz}.bin`))
      if (column) loaded++
      return column
    },
    eye: camera.eye,
    across: Math.min(radius * 16, Math.ceil(maxDist) + 1),
    up: UP
  })
  const gridDone = performance.now()
  const gridMs = gridDone - started
  const out = render({ grid, info: blocks.info, texture: blocks.texture, eye: camera.eye, entities: entitiesFromPose(pose), timeOfDay: pose.timeOfDay, rain: pose.rain ?? 0, width, height, maxDist, yaw: camera.yaw, pitch: camera.pitch, fov })
  const rayDone = performance.now()
  const png = noPng ? null : encodePng(width, height, out.rgba)
  const done = performance.now()
  const timings = { pose: poseDone - started, grid: gridDone - poseDone, raycast: rayDone - gridDone, png: done - rayDone, total: done - started }
  const hit = aim ? castRay(grid, blocks.info, camera.eye, directionFor(camera.yaw, camera.pitch), maxDist) : null
  const center = hit && { name: hit.block.name ?? String(hit.id), x: hit.x, y: hit.y, z: hit.z, t: hit.t, face: hit.face, light: lightReport(lightTable(pose.timeOfDay, pose.rain ?? 0), hitLight(grid, hit)) }
  return { png, ms: done - started, gridMs, timings, columns: loaded, pose, seen: out.seen, textured: blocks.textured, center }
}

// columns held and reloaded so far, over every game version seen
export const columnStats = () => [...perVersion.values()].map(v => v.columns.stats()).reduce((a, b) => ({ loaded: a.loaded + b.loaded, reloads: a.reloads + b.reloads }), { loaded: 0, reloads: 0 })
