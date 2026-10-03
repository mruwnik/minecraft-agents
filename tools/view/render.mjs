// What a body sees, drawn from the files it dumps: state/worlds/<world>/chunks/<cx>.<cz>.bin and
// state/agents/<name>/view/pose.json. Nothing here touches a body or a server.
import fs from 'node:fs'
import path from 'node:path'
import { render, encodePng } from '../../src/vision/renderer.mjs'
import { columnCache, makeChunkClass } from './columns.mjs'
import { buildGrid } from './grid.mjs'
import { cameraFromPose, entitiesFromPose } from './camera.mjs'
import { makeBlockSource } from './blocks.mjs'

const ROOT = path.join(import.meta.dirname, '../..')
export const DEFAULT_STATE_DIR = path.join(ROOT, 'state')
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

export const viewDir = (agentName, stateDir) => path.join(stateDir, 'agents', agentName, 'view')

export function readPose (agentName, stateDir = DEFAULT_STATE_DIR) {
  const file = path.join(viewDir(agentName, stateDir), 'pose.json')
  if (!fs.existsSync(file)) throw new Error(`no pose.json for ${agentName} (looked at ${file})`)
  return JSON.parse(fs.readFileSync(file, 'utf8'))
}

export function renderView ({ agentName, width = 320, height = 180, fov = 70, maxDist = 64, radius = 8, stateDir = DEFAULT_STATE_DIR, override = {} }) {
  const started = performance.now()
  const pose = readPose(agentName, stateDir)
  const { columns, blocks } = forVersion(pose.mcVersion)
  const camera = { ...cameraFromPose(pose), ...override }
  const chunkDir = path.join(stateDir, 'worlds', pose.world, 'chunks')
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
  const gridMs = performance.now() - started
  const out = render({ grid, info: blocks.info, texture: blocks.texture, eye: camera.eye, entities: entitiesFromPose(pose), timeOfDay: pose.timeOfDay, width, height, maxDist, yaw: camera.yaw, pitch: camera.pitch, fov })
  const png = encodePng(width, height, out.rgba)
  return { png, ms: performance.now() - started, gridMs, columns: loaded, pose, seen: out.seen, textured: blocks.textured }
}
