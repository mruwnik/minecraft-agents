// Why JavaScript: binary/graphics; the view dump's column, light and pose encoding.
// View dump light: masks, nibbles, the local relight overlay and its box merging.
import prismarineRegistry from 'prismarine-registry'
import { lightTable } from './light.mjs'

export const LIGHT_SECTION_BYTES = 2048
export const RELIGHT_BUDGET_MS = 10
export const RELIGHT_REACH = 16
const SECTION_VOLUME = 4096

// ---- light: masks, nibbles, and the local relight overlay ----

// bit i of a long-array mask of [hi, lo] int32 pairs
const maskBit = (mask, i) => ((mask?.[i >> 6]?.[(i & 63) >= 32 ? 0 : 1] ?? 0) >>> (i & 31)) & 1
const copyMask = mask => (mask ?? []).map(pair => [...pair])
function setMaskBit (mask, i, on) {
  while (mask.length <= (i >> 6)) mask.push([0, 0])
  const at = (i & 63) >= 32 ? 0 : 1
  const bit = 1 << (i & 31)
  mask[i >> 6][at] = on ? mask[i >> 6][at] | bit : mask[i >> 6][at] & ~bit
}

// light section index -> its nibble buffer, for the set bits of the mask (the dump lists buffers in section order)
const lightBuffers = (buffers, mask, numSections) => {
  const out = new Map()
  let next = 0
  for (let l = 0; l < numSections + 2; l++) {
    if (maskBit(mask, l) && next < buffers.length) out.set(l, buffers[next++])
  }
  return out
}

// one byte per cell (vanilla cell order y<<8|z<<4|x) -> 2048 bytes, even cell in the low nibble
export function packNibbles (cells) {
  const out = new Uint8Array(LIGHT_SECTION_BYTES)
  for (let i = 0; i < SECTION_VOLUME; i += 2) out[i >> 1] = cells[i] | cells[i + 1] << 4
  return out
}

const unpackNibbles = (buffer, into, offset) => {
  for (let i = 0; i < SECTION_VOLUME; i++) into[offset + i] = (buffer[i >> 1] >> ((i & 1) * 4)) & 15
}

// a column's dumped light as {sky, block}: one byte per cell for every world section, section s at s * 4096.
// Mirrors tools/view/web/decodeLight: sky with no data and not flagged empty is open (15), block with no data is 0.
export function decodeColumnLight (light, numSections) {
  const sky = new Uint8Array(numSections * SECTION_VOLUME)
  const block = new Uint8Array(numSections * SECTION_VOLUME)
  const skyBuffers = lightBuffers(light.skyLight, light.skyLightMask, numSections)
  const blockBuffers = lightBuffers(light.blockLight, light.blockLightMask, numSections)
  for (let s = 0; s < numSections; s++) {
    const skyBuffer = skyBuffers.get(s + 1)
    if (skyBuffer) unpackNibbles(skyBuffer, sky, s * SECTION_VOLUME)
    else if (!maskBit(light.emptySkyLightMask, s + 1)) sky.fill(15, s * SECTION_VOLUME, (s + 1) * SECTION_VOLUME)
    const blockBuffer = blockBuffers.get(s + 1)
    if (blockBuffer) unpackNibbles(blockBuffer, block, s * SECTION_VOLUME)
  }
  return { sky, block }
}

// One light section of the column as {sky, block}, one byte per cell in vanilla order, read straight from prismarine's
// BitArray (what dumpLight serialises, without serialising): each pair of Uint32 words holds 8 vanilla bytes, the
// second word of the pair first, both big-endian. Same defaults as decodeColumnLight for sections with no data.
const unpackWords = (words, into) => {
  for (let p = 0, at = 0; p < words.length; p += 2) {
    for (let w = 1; w >= 0; w--) {
      const word = words[p + w]
      for (let shift = 24; shift >= 0; shift -= 8) {
        const byte = (word >>> shift) & 255
        into[at++] = byte & 15
        into[at++] = byte >> 4
      }
    }
  }
}

// Dimensions with no sky send no sky light at all (no data and no empty flag), which must not read as open sky.
export const hasSkyLight = dimension => !/^(minecraft:)?(the_nether|the_end)$/.test(dimension ?? '')

export function columnLightSection (column, s, hasSky = true) {
  const l = s + 1
  const sky = new Uint8Array(SECTION_VOLUME)
  const block = new Uint8Array(SECTION_VOLUME)
  const skyData = column.skyLightSections[l]
  if (skyData && column.skyLightMask.get(l)) unpackWords(skyData.data, sky)
  else if (hasSky && !column.emptySkyLightMask.get(l)) sky.fill(15)
  const blockData = column.blockLightSections[l]
  if (blockData && column.blockLightMask.get(l)) unpackWords(blockData.data, block)
  return { sky, block }
}

// one section's block state ids as a Uint16Array, read once from the palette container
export function columnStateSection (column, s) {
  const out = new Uint16Array(SECTION_VOLUME)
  const container = column.sections[s]?.data
  if (!container) return out
  if (container.data === undefined) return out.fill(container.value)
  const { data, palette } = container
  if (palette) for (let i = 0; i < SECTION_VOLUME; i++) out[i] = palette[data.get(i)]
  else for (let i = 0; i < SECTION_VOLUME; i++) out[i] = data.get(i)
  return out
}

// the dump with the overlay's sections in place of the column's: Map<world section, {sky, block}> of one byte per cell
export function overlayLight (light, overlay, numSections) {
  const skyBuffers = lightBuffers(light.skyLight, light.skyLightMask, numSections)
  const blockBuffers = lightBuffers(light.blockLight, light.blockLightMask, numSections)
  const skyLightMask = copyMask(light.skyLightMask)
  const blockLightMask = copyMask(light.blockLightMask)
  const emptySkyLightMask = copyMask(light.emptySkyLightMask)
  const emptyBlockLightMask = copyMask(light.emptyBlockLightMask)
  for (const [s, cells] of overlay) {
    const l = s + 1
    skyBuffers.set(l, packNibbles(cells.sky))
    blockBuffers.set(l, packNibbles(cells.block))
    setMaskBit(skyLightMask, l, 1)
    setMaskBit(blockLightMask, l, 1)
    setMaskBit(emptySkyLightMask, l, 0)
    setMaskBit(emptyBlockLightMask, l, 0)
  }
  const inOrder = buffers => [...buffers.keys()].sort((a, b) => a - b).map(l => buffers.get(l))
  return { skyLight: inOrder(skyBuffers), blockLight: inOrder(blockBuffers), skyLightMask, blockLightMask, emptySkyLightMask, emptyBlockLightMask }
}

// the light part: sky buffers then block buffers, each LIGHT_SECTION_BYTES; the meta says how to restore them
export const encodeLight = (column, overlay) => {
  if (!column.dumpLight) return { buffer: Buffer.alloc(0), meta: {} }
  const dumped = column.dumpLight()
  const light = overlay?.size ? overlayLight(dumped, overlay, column.numSections ?? column.worldHeight >> 4) : dumped
  return {
    buffer: Buffer.concat([...light.skyLight, ...light.blockLight].map(b => Buffer.from(b))),
    meta: {
      skyCount: light.skyLight.length,
      blockCount: light.blockLight.length,
      sectionBytes: LIGHT_SECTION_BYTES,
      skyLightMask: light.skyLightMask,
      blockLightMask: light.blockLightMask,
      emptySkyLightMask: light.emptySkyLightMask,
      emptyBlockLightMask: light.emptyBlockLightMask
    }
  }
}


// ---- local relight ----

const lightTables = new Map()
export const tableFor = target => {
  const cached = lightTables.get(target.version)
  if (cached) return cached
  const registry = target.registry?.blocksArray ? target.registry : prismarineRegistry(target.version)
  const table = { ...lightTable(registry), stone: registry.blocksByName.stone.defaultState, air: registry.blocksByName.air.defaultState }
  lightTables.set(target.version, table)
  return table
}

export const MERGE_MAX_XZ = 48
export const MERGE_MAX_Y = 64
export const MERGE_MAX_GROWTH = 1.5

const overlaps = (a, b) => a.x0 <= b.x1 && b.x0 <= a.x1 && a.y0 <= b.y1 && b.y0 <= a.y1 && a.z0 <= b.z1 && b.z0 <= a.z1
const volume = b => (b.x1 - b.x0 + 1) * (b.y1 - b.y0 + 1) * (b.z1 - b.z0 + 1)
// yc0: the low end of the box without the downward sky extension (which does not count towards the height cap)
const unionBounds = (a, b, top) => {
  const y1 = Math.max(a.y1, b.y1)
  return {
    x0: Math.min(a.x0, b.x0), x1: Math.max(a.x1, b.x1), y0: Math.min(a.y0, b.y0), yc0: Math.min(a.yc0 ?? a.y0, b.yc0 ?? b.y0), y1,
    z0: Math.min(a.z0, b.z0), z1: Math.max(a.z1, b.z1), virtualTop: y1 === top - 1
  }
}
const mergeable = (a, b, top) => {
  if (!overlaps(a, b)) return false
  const u = unionBounds(a, b, top)
  if (u.x1 - u.x0 + 1 > MERGE_MAX_XZ || u.z1 - u.z0 + 1 > MERGE_MAX_XZ || u.y1 - u.yc0 + 1 > MERGE_MAX_Y) return false
  return volume(u) <= MERGE_MAX_GROWTH * (volume(a) + volume(b))
}

// Merges overlapping boxes while the union stays within the caps (a fill of hundreds of blocks would otherwise chain
// into one huge box). Boxes that overlap but stay apart are still exact when run one after another: a change only
// alters light within 15 cells of itself, inside its own box's interior, and the box shell (16 away) is never touched.
// Block states are read live, so every change is already in them when any box runs, and each change's box recomputes
// its whole neighbourhood from the final states. A cell left stale near another box's shell is inside some other
// change's box and is fixed when that one runs.
// Each box joins the first kept box it can merge with, repeated until nothing merges. The input boxes are never mutated.
export function mergeOverlapping (boxes, top) {
  const pass = list => {
    const out = []
    for (const box of list) {
      const at = out.findIndex(kept => mergeable(kept, box, top))
      if (at < 0) out.push({ ...box, changes: [...box.changes] })
      else {
        const kept = out[at]
        Object.assign(kept, unionBounds(kept, box, top))
        for (const change of box.changes) kept.changes.push(change)
      }
    }
    return out
  }
  let out = pass(boxes)
  for (let again = pass(out); again.length < out.length; again = pass(out)) out = again
  return out
}

// One writer per body. `attach(bot)` hooks a bot (call again for each new bot after a reconnect), `detach()` writes the
// offline pose. `onEvent(event)` receives view.stats and view.error. BODY_VIEW=0 turns it all off.
