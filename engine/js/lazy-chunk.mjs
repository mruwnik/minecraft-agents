// Why JavaScript: patches prismarine-chunk's column class at the Mineflayer boundary; raw packet buffers.
// A body reads few of the chunk sections it receives, so a column's block and biome sections stay in the packet
// buffer until first touched (only their headers are scanned to find where each starts), and light nibbles are
// unpacked on first read. Reads see the same states and light as an eager load. Also memoises the fluid test of
// ChunkSection.set, which built a Block per call.
import { createRequire } from 'node:module'
import prismarineBlock from 'prismarine-block'
import prismarineRegistry from 'prismarine-registry'

const require = createRequire(import.meta.url)
const chunkModule = createRequire(require.resolve('prismarine-chunk'))
const { SmartBuffer } = chunkModule('smart-buffer')

const MAX_BITS_PER_BLOCK = 8 // palette sizes above it are direct (prismarine-chunk constants)
const MAX_BITS_PER_BIOME = 3
const SECTION_VOLUME = 4096
const BIOME_VOLUME = 64
const LIGHT_BYTES = 2048

const pending = Symbol('lazy-chunk')

function skipVarInt (buf, off) {
  while (true) {
    if (off >= buf.length) throw new RangeError('chunk data ends inside a varint')
    if ((buf[off++] & 0x80) === 0) return off
  }
}

function readVarInt (buf, off) {
  let result = 0
  let shift = 0
  while (true) {
    if (off >= buf.length) throw new RangeError('chunk data ends inside a varint')
    const b = buf[off++]
    result |= (b & 0x7F) << shift
    if ((b & 0x80) === 0) return [result >>> 0, off]
    shift += 7
  }
}

// offset after the packed longs of a palette container
function skipLongs (buf, off, bits, capacity, noSizePrefix) {
  if (noSizePrefix) return off + Math.ceil(capacity / Math.floor(64 / bits)) * 8
  const [longs, next] = readVarInt(buf, off)
  return next + longs * 8
}

function skipPalette (buf, off) {
  const [length, next] = readVarInt(buf, off)
  off = next
  for (let i = 0; i < length; i++) off = skipVarInt(buf, off)
  return off
}

// offset after one block section (solid count, fluid count if sent, then the container), as ChunkSection.read walks it
function skipBlockSection (buf, off, { noSizePrefix, hasFluidCount }) {
  off += hasFluidCount ? 4 : 2
  const bits = buf[off++]
  if (bits > 16) throw new Error(`Bits per block is too big: ${bits}`)
  if (bits === 0) {
    off = skipVarInt(buf, off)
    return noSizePrefix ? off : off + 1
  }
  if (bits > MAX_BITS_PER_BLOCK) return skipLongs(buf, off, bits, SECTION_VOLUME, noSizePrefix)
  return skipLongs(buf, skipPalette(buf, off), bits, SECTION_VOLUME, noSizePrefix)
}

function skipBiomeSection (buf, off, { noSizePrefix }) {
  const bits = buf[off++]
  if (bits > 8) throw new Error(`Bits per biome is too big: ${bits}`)
  if (bits === 0) {
    off = skipVarInt(buf, off)
    return noSizePrefix ? off : off + 1
  }
  if (bits > MAX_BITS_PER_BIOME) return skipLongs(buf, off, bits, BIOME_VOLUME, noSizePrefix)
  return skipLongs(buf, skipPalette(buf, off), bits, BIOME_VOLUME, noSizePrefix)
}

// An object that looks like a Real instance: its fields are accessors on the prototype until first read, which
// builds a real instance and copies its fields onto the object. The object stays in the column's array for good,
// so there is only ever one holder of the counts that set() keeps.
function lazyProto (Real, fields) {
  const proto = Object.create(Real.prototype)
  const force = self => {
    const state = self[pending]
    if (!state) return
    self[pending] = null
    const real = state.make()
    for (const key of Object.keys(real)) Object.defineProperty(self, key, { value: real[key], writable: true, enumerable: true, configurable: true })
  }
  for (const key of fields) {
    Object.defineProperty(proto, key, {
      get () { force(this); return this[key] },
      set (value) { force(this); this[key] = value },
      configurable: true
    })
  }
  return proto
}

const placeholder = (proto, make) => {
  const self = Object.create(proto)
  self[pending] = { make }
  return self
}

// the class has the layout this patch knows (1.18+ columns with light arrays and a static section class)
const patchable = C => {
  const sample = new C()
  return typeof C.section?.read === 'function' && Array.isArray(sample.sections) && Array.isArray(sample.biomes) &&
    Array.isArray(sample.skyLightSections) && typeof sample.loadParsedLight === 'function' && sample.sections[0].data !== undefined
}

function fluidTester (registry) {
  const Block = prismarineBlock(registry)
  const known = new Map()
  return stateId => {
    let yes = known.get(stateId)
    if (yes !== undefined) return yes
    const block = Block.fromStateId(stateId)
    yes = block.name === 'water' || block.name === 'lava' || Boolean(block.isWaterlogged)
    known.set(stateId, yes)
    return yes
  }
}

function patchClass (C, registry) {
  if (!patchable(C)) return C
  const Section = C.section
  const sample = new C()
  const Biome = sample.biomes[0].constructor
  const BitArray = sample.skyLightMask.constructor
  const { noSizePrefix, hasFluidCount } = sample.sections[0]
  const flags = { noSizePrefix, hasFluidCount }
  const sectionProto = lazyProto(Section, ['noSizePrefix', 'hasFluidCount', 'data', 'palette', 'solidBlockCount', 'fluidCount'])
  const biomeProto = lazyProto(Biome, ['noSizePrefix', 'data'])
  const lightProto = lazyProto(BitArray, ['data', 'capacity', 'bitsPerValue', 'valuesPerLong', 'valueMask'])
  const isFluid = hasFluidCount ? fluidTester(registry) : null

  C.prototype.load = function (data) {
    const buf = Buffer.isBuffer(data) ? data : Buffer.from(data)
    const sectionAt = []
    const biomeAt = []
    let off = 0
    for (let i = 0; i < this.numSections; i++) {
      sectionAt.push(off)
      off = skipBlockSection(buf, off, flags)
      biomeAt.push(off)
      off = skipBiomeSection(buf, off, flags)
    }
    if (off > buf.length) throw new RangeError('chunk data ends inside a section')
    const reader = at => { const r = SmartBuffer.fromBuffer(buf); r.readOffset = at; return r }
    for (let i = 0; i < this.numSections; i++) {
      this.sections[i] = placeholder(sectionProto, () => Section.read(reader(sectionAt[i]), this.maxBitsPerBlock, noSizePrefix, hasFluidCount))
      this.biomes[i] = placeholder(biomeProto, () => Biome.read(reader(biomeAt[i]), this.maxBitsPerBiome, noSizePrefix))
    }
  }

  C.prototype.loadParsedLight = function (skyLight, blockLight, skyLightMask, blockLightMask, emptySkyLightMask, emptyBlockLightMask) {
    const readSection = (sections, data, lightMask, pLightMask, emptyMask, pEmptyMask) => {
      let current = 0
      const incomingLightMask = BitArray.fromLongArray(pLightMask, 1)
      const incomingEmptyMask = BitArray.fromLongArray(pEmptyMask, 1)
      for (let y = 0; y < sections.length; y++) {
        const isEmpty = incomingEmptyMask.get(y)
        if (!incomingLightMask.get(y) && !isEmpty) continue
        emptyMask.set(y, isEmpty)
        lightMask.set(y, 1 - isEmpty)
        const bytes = isEmpty ? null : Buffer.from(data[current++])
        sections[y] = placeholder(lightProto, () => unpackLight(BitArray, bytes))
      }
    }
    readSection(this.skyLightSections, skyLight, this.skyLightMask, skyLightMask, this.emptySkyLightMask, emptySkyLightMask)
    readSection(this.blockLightSections, blockLight, this.blockLightMask, blockLightMask, this.emptyBlockLightMask, emptyBlockLightMask)
  }

  if (isFluid) {
    Section.prototype.set = function (pos, stateId) {
      const blockIndex = (pos.y << 8) | (pos.z << 4) | pos.x
      const oldBlock = this.data.get(blockIndex)
      if (stateId === 0 && oldBlock !== 0) this.solidBlockCount -= 1
      else if (stateId !== 0 && oldBlock === 0) this.solidBlockCount += 1
      if (this.hasFluidCount) {
        const now = isFluid(stateId)
        const before = isFluid(oldBlock)
        if (!now && before) this.fluidCount -= 1
        else if (now && !before) this.fluidCount += 1
      }
      this.data = this.data.set(blockIndex, stateId)
      this.palette = this.data.palette
    }
  }
  return C
}

// 4096 light nibbles as BitArray's pairs of 32-bit words (high word second), like BitArray.readBuffer
function unpackLight (BitArray, bytes) {
  const array = new BitArray({ bitsPerValue: 4, capacity: SECTION_VOLUME })
  if (!bytes) return array
  if (bytes.length < LIGHT_BYTES) throw new RangeError(`light section holds ${bytes.length} bytes`)
  const words = array.data
  for (let i = 0, at = 0; i < words.length; i += 2, at += 8) {
    words[i + 1] = bytes.readUInt32BE(at)
    words[i] = bytes.readUInt32BE(at + 4)
  }
  return array
}

let installed = false

// Makes every prismarine-chunk column class made after this call lazy; Mineflayer makes its class when a bot is created.
export function installLazyChunks () {
  if (installed) return
  installed = true
  const path = require.resolve('prismarine-chunk')
  const loader = require('prismarine-chunk')
  const patched = new WeakSet()
  const wrapped = registryOrVersion => {
    const registry = typeof registryOrVersion === 'string' ? prismarineRegistry(registryOrVersion) : registryOrVersion
    const C = loader(registry)
    if (patched.has(C)) return C
    patched.add(C)
    return patchClass(C, registry)
  }
  require.cache[path].exports = Object.assign(wrapped, loader)
}
