// Light fixtures: a captured box of block states plus the server's sky and block light, for oracle tests of the light model.
// File: zlib deflate of uint32le header length, JSON header, states (uint16le per cell), sky (u8), block (u8).
// Cells are indexed (y * sz + z) * sx + x, relative to the box's low corner `origin`.
import fs from 'node:fs'
import zlib from 'node:zlib'

export function writeLightFixture (file, { name, version, origin, size, states, sky, block, capturedAt }) {
  const header = Buffer.from(JSON.stringify({ name, version, origin, size, capturedAt }), 'utf8')
  const length = Buffer.alloc(4)
  length.writeUInt32LE(header.length)
  const statesBytes = Buffer.from(states.buffer, states.byteOffset, states.byteLength)
  fs.writeFileSync(file, zlib.deflateSync(Buffer.concat([length, header, statesBytes, Buffer.from(sky), Buffer.from(block)]), { level: 9 }))
}

export function readLightFixture (file) {
  const raw = zlib.inflateSync(fs.readFileSync(file))
  const n = raw.readUInt32LE(0)
  const header = JSON.parse(raw.subarray(4, 4 + n).toString('utf8'))
  const cells = header.size[0] * header.size[1] * header.size[2]
  const at = 4 + n
  const states = new Uint16Array(cells)
  for (let i = 0; i < cells; i++) states[i] = raw.readUInt16LE(at + i * 2)
  const sky = Uint8Array.from(raw.subarray(at + cells * 2, at + cells * 3))
  const block = Uint8Array.from(raw.subarray(at + cells * 3, at + cells * 4))
  return { ...header, states, sky, block }
}

// mismatch breakdown between captured and computed light over cells where `inside(x, y, z)` (box-relative) holds.
// Returns {skyTotal, blockTotal, cells, classes: [{state, channel, sign, count, example: [x, y, z] (world), captured, computed}]} by count.
export function lightBreakdown ({ fixture, computed, registry, inside = () => true, top = 10 }) {
  const [sx, sy, sz] = fixture.size
  const classes = new Map()
  const totals = { sky: 0, block: 0 }
  let cells = 0
  for (let y = 0; y < sy; y++) {
    for (let z = 0; z < sz; z++) {
      for (let x = 0; x < sx; x++) {
        if (!inside(x, y, z)) continue
        cells++
        const i = (y * sz + z) * sx + x
        for (const channel of ['sky', 'block']) {
          const diff = fixture[channel][i] - computed[channel][i]
          if (diff === 0) continue
          totals[channel]++
          const state = registry.blockStates?.[fixture.states[i]]?.name ?? registry.blocksByStateId?.[fixture.states[i]]?.name ?? `state${fixture.states[i]}`
          const sign = diff > 0 ? 'captured>computed' : 'captured<computed'
          const key = `${state}|${channel}|${sign}`
          const entry = classes.get(key) ?? { state, channel, sign, count: 0, example: [fixture.origin[0] + x, fixture.origin[1] + y, fixture.origin[2] + z], captured: fixture[channel][i], computed: computed[channel][i] }
          entry.count++
          classes.set(key, entry)
        }
      }
    }
  }
  return { skyTotal: totals.sky, blockTotal: totals.block, cells, classes: [...classes.values()].sort((a, b) => b.count - a.count).slice(0, top) }
}

export function formatBreakdown (name, b) {
  const lines = [`${name}: ${b.cells} cells compared, sky mismatches ${b.skyTotal}, block mismatches ${b.blockTotal}`]
  for (const c of b.classes) lines.push(`  ${String(c.count).padStart(6)}  ${c.state} ${c.channel} ${c.sign} e.g. ${c.example.join(',')} captured ${c.captured} computed ${c.computed}`)
  return lines.join('\n')
}
