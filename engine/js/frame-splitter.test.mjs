// Why JavaScript: tests the Node loader-boundary module frame-splitter.mjs.
import test from 'node:test'
import assert from 'node:assert/strict'
import { createRequire } from 'node:module'

const require = createRequire(import.meta.url)
const framing = require('minecraft-protocol/src/transforms/framing.js')
const stock = framing.createSplitter
const { FrameSplitter } = await import('./frame-splitter.mjs')
const [, writeVarInt, sizeOfVarInt] = require('protodef').types.varint

function frame (payload) {
  const size = sizeOfVarInt(payload.length)
  const out = Buffer.alloc(size + payload.length)
  writeVarInt(payload.length, out, 0)
  payload.copy(out, size)
  return out
}

function feed (splitter, chunks) {
  const out = []
  splitter.on('data', d => out.push(Buffer.from(d)))
  for (const c of chunks) splitter.write(c)
  return out.map(b => b.toString('hex'))
}

function cut (buffer, seed) {
  let s = seed
  const next = () => (s = (s * 1103515245 + 12345) % 2147483648) / 2147483648
  const chunks = []
  for (let at = 0; at < buffer.length;) {
    const n = 1 + Math.floor(next() * 400)
    chunks.push(buffer.subarray(at, at + n))
    at += n
  }
  return chunks
}

const sizes = [0, 1, 5, 127, 128, 300, 16383, 16384, 70000]
const stream = Buffer.concat(sizes.map((n, i) => frame(Buffer.alloc(n, i + 1))))

test('frames come out the same as the stock splitter for any cut of the byte stream', () => {
  for (let seed = 1; seed <= 40; seed++) {
    const chunks = cut(stream, seed)
    assert.deepEqual(feed(new FrameSplitter(), chunks), feed(stock(), chunks), `seed ${seed}`)
  }
})

test('a partial last frame is held until the rest arrives', () => {
  const head = [stream.subarray(0, stream.length - 10)]
  const s = new FrameSplitter()
  assert.deepEqual(feed(s, head), feed(stock(), head))
  s.write(stream.subarray(stream.length - 10))
  assert.equal(s.length, 0)
})

test('the legacy ping is recognised only while the flag is set', () => {
  const ping = Buffer.from([0xfe])
  const a = new FrameSplitter(); a.recognizeLegacyPing = true
  const b = stock(); b.recognizeLegacyPing = true
  assert.deepEqual(feed(a, [ping]), feed(b, [ping]))
})

test('createSplitter of minecraft-protocol builds this splitter', () => {
  assert.ok(framing.createSplitter() instanceof FrameSplitter)
})
