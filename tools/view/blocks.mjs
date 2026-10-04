// Why JavaScript: graphics; block descriptions and textures for the software renderer.
// Block descriptions and textures for the renderer, as src/vision/render-worker.mjs builds them (copied: that file
// is a worker that reads workerData on load, so it cannot be imported). No textures dir means every block is a
// flat colour hashed from its name, which the renderer does by itself.
import fs from 'node:fs'
import path from 'node:path'
import prismarineBlock from 'prismarine-block'
import { decodePng, textureCandidates, tintOf } from './renderer.mjs'

const AIR = new Set(['air', 'cave_air', 'void_air', 'light', 'barrier', 'structure_void'])
const FULL_CUBE = JSON.stringify([[0, 0, 0, 1, 1, 1]])

export function makeBlockSource (registry, textureDir) {
  const Block = prismarineBlock(registry)
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
  return { info, texture, textured: fs.existsSync(textureDir) }
}
