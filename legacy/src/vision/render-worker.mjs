// Draws a look off the body's thread (eyes.mjs posts it a scene): a picture is ~100 ms of raycasting, long enough to
// make the body miss its physics ticks. It keeps the block descriptions and textures, which only it needs.
import fs from 'node:fs'
import path from 'node:path'
import { parentPort, workerData } from 'node:worker_threads'
import prismarineBlock from 'prismarine-block'
import prismarineRegistry from 'prismarine-registry'
import { decodePng, encodePng, render, textureCandidates, tintOf } from './renderer.mjs'

const AIR = new Set(['air', 'cave_air', 'void_air', 'light', 'barrier', 'structure_void'])
const FULL_CUBE = JSON.stringify([[0, 0, 0, 1, 1, 1]])

const { version, textureDir } = workerData
const Block = prismarineBlock(prismarineRegistry(version))
const images = new Map()
const blockInfo = new Map()
const faceTextures = new Map()

const image = name => {
  if (images.has(name)) return images.get(name)
  const file = path.join(textureDir, `${name}.png`)
  const decoded = fs.existsSync(file) ? { ...decodePng(fs.readFileSync(file)), tint: tintOf(name) } : null
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

const draw = scene => {
  const out = render({ ...scene, info, texture })
  return { png: encodePng(scene.width, scene.height, out.rgba), seen: out.seen, near: out.near }
}

// a scene that cannot be drawn fails that one look, not every look after it
parentPort.on('message', ({ id, scene }) => {
  let answer
  try {
    answer = { id, ...draw(scene) }
  } catch (e) {
    answer = { id, error: e.message }
  }
  parentPort.postMessage(answer)
})
