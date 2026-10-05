// Why JavaScript: graphics; block descriptions and textures for the software renderer.
// Block descriptions and textures for the renderer. No textures dir means every block is a flat colour hashed from
// its name, which the renderer does by itself.
import fs from 'node:fs'
import path from 'node:path'
import prismarineBlock from 'prismarine-block'
import { decodePng, textureCandidates, tintOf } from './renderer.mjs'

const AIR = new Set(['air', 'cave_air', 'void_air', 'light', 'barrier', 'structure_void'])
const FULL_CUBE = JSON.stringify([[0, 0, 0, 1, 1, 1]])

// An open fence gate has no collision shape (the prismarine shapes are empty), which the renderer would draw as a plant. It is
// drawn as the game does: its two posts with the leaves swung open along the gate's facing axis, as boxes in the cell.
const openGateBoxes = facing => {
  const [a, b] = [[0, 0.125], [0.875, 1]]
  const acrossX = facing === 'north' || facing === 'south'
  return [a, b].map(([lo, hi]) => acrossX ? [lo, 0.3125, 0.375, hi, 1, 1] : [0.375, 0.3125, lo, 1, 1, hi])
}

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
    if (b.name.endsWith('_fence_gate') && base.props.open) return { ...base, kind: 'boxes', boxes: openGateBoxes(base.props.facing) }
    if (!b.shapes.length) return { ...base, kind: 'cross' }
    return JSON.stringify(b.shapes) === FULL_CUBE ? { ...base, kind: 'cube' } : { ...base, kind: 'boxes', boxes: b.shapes }
  }
  const info = stateId => {
    if (!blockInfo.has(stateId)) blockInfo.set(stateId, describe(stateId))
    return blockInfo.get(stateId)
  }
  return { info, texture, textured: fs.existsSync(textureDir) }
}
