// Why JavaScript: graphics; writes name labels into the software renderers' pixel buffers (tools/view/renderer.mjs, raycaster.mjs).
import { LABEL_MARGIN as MARGIN, LABEL_CELLS_HIGH, labelWidth } from './web/mobs.mjs'

// Name labels over mobs in a software-rendered picture: a dark plate with white 5x7 capitals, scaled to the label's height, drawn
// only where the terrain is farther than the label (depth buffer). Where labels go and what they say is web/mobs.mjs placeLabels.
const GLYPH_ROWS = {
  A: '01110 10001 10001 11111 10001 10001 10001', B: '11110 10001 10001 11110 10001 10001 11110',
  C: '01110 10001 10000 10000 10000 10001 01110', D: '11110 10001 10001 10001 10001 10001 11110',
  E: '11111 10000 10000 11110 10000 10000 11111', F: '11111 10000 10000 11110 10000 10000 10000',
  G: '01110 10001 10000 10111 10001 10001 01111', H: '10001 10001 10001 11111 10001 10001 10001',
  I: '01110 00100 00100 00100 00100 00100 01110', J: '00111 00010 00010 00010 00010 10010 01100',
  K: '10001 10010 10100 11000 10100 10010 10001', L: '10000 10000 10000 10000 10000 10000 11111',
  M: '10001 11011 10101 10101 10001 10001 10001', N: '10001 11001 10101 10011 10001 10001 10001',
  O: '01110 10001 10001 10001 10001 10001 01110', P: '11110 10001 10001 11110 10000 10000 10000',
  Q: '01110 10001 10001 10001 10101 10010 01101', R: '11110 10001 10001 11110 10100 10010 10001',
  S: '01111 10000 10000 01110 00001 00001 11110', T: '11111 00100 00100 00100 00100 00100 00100',
  U: '10001 10001 10001 10001 10001 10001 01110', V: '10001 10001 10001 10001 10001 01010 00100',
  W: '10001 10001 10001 10101 10101 11011 10001', X: '10001 10001 01010 00100 01010 10001 10001',
  Y: '10001 10001 01010 00100 00100 00100 00100', Z: '11111 00001 00010 00100 01000 10000 11111',
  0: '01110 10001 10011 10101 11001 10001 01110', 1: '00100 01100 00100 00100 00100 00100 01110',
  2: '01110 10001 00001 00010 00100 01000 11111', 3: '11110 00001 00001 01110 00001 00001 11110',
  4: '00010 00110 01010 10010 11111 00010 00010', 5: '11111 10000 11110 00001 00001 10001 01110',
  6: '00110 01000 10000 11110 10001 10001 01110', 7: '11111 00001 00010 00100 01000 01000 01000',
  8: '01110 10001 10001 01110 10001 10001 01110', 9: '01110 10001 10001 01111 00001 00010 01100',
  ' ': '00000 00000 00000 00000 00000 00000 00000', _: '00000 00000 00000 00000 00000 00000 11111',
  '-': '00000 00000 00000 11111 00000 00000 00000', '.': '00000 00000 00000 00000 00000 01100 01100',
  "'": '00100 00100 01000 00000 00000 00000 00000', ':': '00000 01100 01100 00000 01100 01100 00000',
  '?': '01110 10001 00001 00010 00100 00000 00100'
}
const GLYPHS = Object.fromEntries(Object.entries(GLYPH_ROWS).map(([ch, rows]) => [ch, rows.split(' ').map(r => [...r].map(c => c === '1'))]))
const GLYPH_W = 5
const GLYPH_H = 7
const ADVANCE = 6 // a glyph and one column of gap

const glyphFor = ch => GLYPHS[ch.toUpperCase()] ?? GLYPHS['?']

const PLATE = [0, 0, 0]
const PLATE_ALPHA = 0.6

// labels: placeLabels' {text, px, py, h, depth}. depth: terrain distance per pixel (a label shows where it is nearer).
export function drawLabels ({ rgba, width, height, depth, labels }) {
  for (const label of labels) {
    const cell = label.h / LABEL_CELLS_HIGH // pixels per glyph cell
    const margin = MARGIN * cell
    const plateW = labelWidth(label.text, label.h)
    const x0 = Math.round(label.px - plateW / 2)
    const y0 = Math.round(label.py - label.h)
    const x1 = Math.round(x0 + plateW)
    const y1 = Math.round(y0 + label.h)
    for (let y = Math.max(0, y0); y < Math.min(height, y1); y++) {
      for (let x = Math.max(0, x0); x < Math.min(width, x1); x++) {
        const p = y * width + x
        if (label.depth >= depth[p]) continue
        const gx = Math.floor((x - x0 - margin) / cell)
        const gy = Math.floor((y - y0 - margin) / cell)
        const column = gx % ADVANCE
        const ch = gx >= 0 ? label.text[Math.floor(gx / ADVANCE)] : undefined
        const ink = ch !== undefined && gy >= 0 && gy < GLYPH_H && column < GLYPH_W && glyphFor(ch)[gy][column]
        const at = p * 4
        if (ink) {
          rgba[at] = 255
          rgba[at + 1] = 255
          rgba[at + 2] = 255
        } else {
          rgba[at] = rgba[at] * (1 - PLATE_ALPHA) + PLATE[0] * PLATE_ALPHA
          rgba[at + 1] = rgba[at + 1] * (1 - PLATE_ALPHA) + PLATE[1] * PLATE_ALPHA
          rgba[at + 2] = rgba[at + 2] * (1 - PLATE_ALPHA) + PLATE[2] * PLATE_ALPHA
        }
      }
    }
  }
}
