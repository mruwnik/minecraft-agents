// Rendering a scan/render window and the fallback symbols it falls back to when a block has no glyph.

import { range } from './world.mjs'
const FALLBACK_SYMBOLS = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ#%&*+='

// ASCII slices of a box of the world, top layer first; air is '.', everything else gets a letter and a legend.
export function renderScan (nameAt, { x1, y1, z1, x2, y2, z2 }) {
  const symbols = new Map()
  const symbolFor = name => {
    if (name === 'air') return '.'
    if (symbols.has(name)) return symbols.get(name)
    const taken = new Set(symbols.values())
    const symbol = [...name.replaceAll('_', ''), ...FALLBACK_SYMBOLS].find(c => !taken.has(c)) ?? '?'
    symbols.set(name, symbol)
    return symbol
  }
  const xs = range(x1, x2)
  const zs = range(z1, z2)
  // labels padded to one width, or rows like -9 and -10 shift against each other and columns get misread
  const labelWidth = Math.max(...zs.map(z => String(z).length))
  const layers = range(y1, y2).reverse().flatMap(y => {
    const rows = zs.map(z => `${String(z).padStart(labelWidth)} ${xs.map(x => symbolFor(nameAt(x, y, z))).join('')}`)
    return rows.every(r => /^ *-?\d+ \.+$/.test(r)) ? [`y=${y} all air`] : [`y=${y}`, ...rows]
  })
  const ruler = `${' '.repeat(labelWidth)} ${xs.map(x => Math.abs(x) % 10).join('')}`
  const legend = [...symbols].map(([name, s]) => `${s}=${name}`).join(' ')
  return [`x ${xs[0]}..${xs.at(-1)} across (ruler: last digit of x), z down`, ruler, ...layers, legend].join('\n')
}
// how many cells a scan may cover: a map is read cell by cell, where= comes back as one line of coordinates
export const scanCap = where => where ? 60000 : 1500

// scan where=<name, * wildcards>: the cells themselves, x then z then y ascending, so nobody has to count columns in the picture
export function scanWhere (nameAt, { x1, y1, z1, x2, y2, z2 }, where, limit = 20) {
  const wanted = new RegExp('^' + String(where).replace(/\*/g, '.*') + '$')
  const range = (a, b) => Array.from({ length: Math.abs(b - a) + 1 }, (_, i) => Math.min(a, b) + i)
  const hits = range(y1, y2).flatMap(y => range(z1, z2).flatMap(z => range(x1, x2).filter(x => wanted.test(nameAt(x, y, z))).map(x => `${String(where).includes('*') ? nameAt(x, y, z) + '@' : ''}${x},${y},${z}`)))
  if (!hits.length) return `${where} 0`
  return `${where} ${hits.length}: ${hits.slice(0, limit).join(' ')}${hits.length > limit ? ` (+${hits.length - limit} more)` : ''}`
}
