#!/usr/bin/env node
import path from 'node:path'
import fs from 'node:fs'
import http from 'node:http'
import crypto from 'node:crypto'
import { fileURLToPath } from 'node:url'
import { parseArgs } from 'node:util'
import edn from 'edn-data'
import { defaultStateDir } from './drive-lib.mjs'
import { NAME, bodyDir, missingWorldError } from '../js/bodies.mjs'
import { get } from './observe.mjs'
import { readEDN, writeEDN, keyword } from './observe-lib.mjs'

export const usage = `usage: jobs.mjs <body> --world <world> list [--limit 8 --offset 0] | show <jID> | submit <EDN-spec> | interrupt <EDN-spec> | cancel <jID> | retry <jID> [--state DIR] [--request-id ID]
  jobs.mjs Bob --world claude submit '(jobs.movement.go-to {:pos {:x 10 :y 64 :z 20}})'
  jobs.mjs Bob --world claude interrupt '(jobs.movement.look-around {:every-ms 2000})'
Mutations return immediately; observe.mjs Bob --world claude --wait --watch jID tracks completion.`
export function specFor (text) {
  if (typeof text !== 'string' || Buffer.byteLength(text) > 12000) throw new Error('spec must be EDN text, at most 12000 bytes')
  const outer = edn.parseEDNString(`(${text})`)
  if (outer?.list?.length !== 1 || !outer.list[0]?.list?.[0]?.sym) throw new Error('spec must be one native EDN job expression list')
  return outer.list[0]
}
export function requestFor (argv) {
  try {
    const p = parseArgs({ args: argv, allowPositionals: true, options: {
      state: { type: 'string', default: defaultStateDir }, world: { type: 'string' }, 'request-id': { type: 'string' },
      limit: { type: 'string' }, offset: { type: 'string' }
    } })
    const [body, op = 'list', arg, ...extra] = p.positionals
    if (!body || !/^[A-Za-z0-9_-]{1,40}$/.test(body)) throw new Error('body must be a valid name')
    if (p.values.world === undefined) throw new Error(missingWorldError('--world'))
    if (!NAME.test(p.values.world)) throw new Error('the world must be a name of letters, digits, _ and -')
    if (!['list', 'show', 'submit', 'interrupt', 'cancel', 'retry'].includes(op)) throw new Error('unknown operation')
    if (extra.length || (op === 'list' ? arg !== undefined : arg === undefined)) throw new Error(op === 'list' ? 'list takes no argument' : `${op} needs exactly one argument`)
    const mutating = ['submit', 'interrupt', 'cancel', 'retry'].includes(op)
    if (!mutating && p.values['request-id'] !== undefined) throw new Error('--request-id requires a mutation')
    if (op !== 'list' && (p.values.limit !== undefined || p.values.offset !== undefined)) throw new Error('--limit and --offset require list')
    const state = path.resolve(p.values.state)
    const socketPath = path.join(bodyDir(state, p.values.world, body), 'engine', 'events.sock')
    if (op === 'list') {
      const limit = Number(p.values.limit ?? 8), offset = Number(p.values.offset ?? 0)
      if (!Number.isInteger(limit) || limit < 1 || limit > 32 || !Number.isInteger(offset) || offset < 0 || offset > 10000) throw new Error('list limit must be1..32 and offset0..10000')
      return { body, state, socketPath, path: `/jobs?limit=${limit}&offset=${offset}`, mutating: false }
    }
    if (['show', 'cancel', 'retry'].includes(op) && !/^j[0-9]+$/.test(arg)) throw new Error('job ID must be j<number>')
    if (op === 'show') return { body, state, socketPath, path: `/job?id=${arg}`, mutating: false }
    const id = p.values['request-id'] ?? crypto.randomUUID()
    if (!/^[A-Za-z0-9_.:-]{1,80}$/.test(id)) throw new Error('--request-id must be a short identifier')
    return { body, state, socketPath, path: '/jobs', mutating: true, request: {
      op: keyword(op), 'request-id': id, ...(['submit', 'interrupt'].includes(op) ? { spec: specFor(arg) } : { id: arg })
    } }
  } catch (error) { return { error: error.message } }
}
export function post (socketPath, body, { timeoutMs = 3000, requestImpl = http.request } = {}) {
  return new Promise((resolve, reject) => {
    const payload = writeEDN(body)
    let done = false, timer
    const finish = (fn, value) => { if (!done) { done = true; clearTimeout(timer); fn(value) } }
    const req = requestImpl({ socketPath, path: '/jobs', method: 'POST', headers: { 'content-type': 'application/edn', 'content-length': Buffer.byteLength(payload) } }, res => {
      let bytes = 0, text = ''
      res.setEncoding('utf8')
      res.on('data', chunk => { bytes += Buffer.byteLength(chunk); if (bytes > 65536) req.destroy(Object.assign(new Error('response too large'), { code: 'ERESPONSETOOLARGE' })); else text += chunk })
      res.on('error', error => finish(reject, error))
      res.on('aborted', () => finish(reject, Object.assign(new Error('aborted'), { code: 'ECONNRESET' })))
      res.on('end', () => finish(resolve, { status: res.statusCode, contentType: res.headers['content-type'], text }))
    })
    req.on('error', error => finish(reject, error))
    timer = setTimeout(() => req.destroy(Object.assign(new Error('timeout'), { code: 'ETIMEDOUT' })), timeoutMs)
    req.end(payload)
  })
}
export async function main (argv = process.argv.slice(2)) {
  const r = requestFor(argv)
  if (r.error) { console.error(`${r.error}\n${usage}`); return 2 }
  try {
    let response
    if (r.mutating) {
      const snapshot = await get(r.socketPath, '/snapshot')
      if (snapshot.status !== 200 || !/^application\/edn(?:;|$)/i.test(snapshot.contentType ?? '')) throw new Error('snapshot unavailable')
      const metaDir = path.join(r.state, 'commands', r.body, 'jobs')
      const metaFile = path.join(metaDir, `${r.request['request-id']}.edn`)
      fs.mkdirSync(metaDir, { recursive: true, mode: 0o700 })
      const cached = fs.existsSync(metaFile) ? readEDN(fs.readFileSync(metaFile, 'utf8')) : null
      let generation = cached?.['generation-id'] ?? readEDN(snapshot.text)['generation-id']
      if (typeof generation !== 'string') throw new Error('generation unavailable')
      if (!cached) {
        try { fs.writeFileSync(metaFile, writeEDN({ 'generation-id': generation }), { mode: 0o600, flag: 'wx' }) }
        catch (e) { if (e.code !== 'EEXIST') throw e; generation = readEDN(fs.readFileSync(metaFile, 'utf8'))['generation-id'] }
        const files = fs.readdirSync(metaDir).filter(f => f.endsWith('.edn')).map(f => ({ file: path.join(metaDir, f), at: fs.statSync(path.join(metaDir, f)).mtimeMs })).sort((a, b) => b.at - a.at)
        for (const f of files.slice(128)) fs.rmSync(f.file, { force: true })
      }
      response = await post(r.socketPath, { ...r.request, 'generation-id': generation })
    } else response = await get(r.socketPath, r.path)
    if (!/^application\/edn(?:;|$)/i.test(response.contentType ?? '')) throw new Error('unexpected response format')
    const value = readEDN(response.text)
    if (response.status === 404 && value.reason?.key === 'not-found') {
      process.stdout.write(writeEDN({ ok: false, reason: keyword('jobs-unavailable'), action: keyword('restart-with-current-build') }) + '\n')
      return 2
    }
    if (!r.mutating && r.path.startsWith('/job?') && response.status === 200) {
      const detail = Object.fromEntries(['id', 'name', 'status', 'round', 'spec'].filter(k => value[k] != null).map(k => [k, value[k]]))
      if (value.failure) detail.failure = value.failure
      if (value.attention?.total) detail.attention = value.attention
      process.stdout.write(writeEDN(detail) + '\n')
      return 0
    }
    if (r.mutating && value.reason?.key === 'request-uncertain') {
      process.stdout.write(writeEDN({ ...value, 'request-id': r.request['request-id'] }) + '\n')
      return 1
    }
    process.stdout.write(response.text.endsWith('\n') ? response.text : response.text + '\n')
    return response.status === 200 ? 0 : 1
  } catch (error) {
    process.stdout.write(writeEDN({ ok: false, reason: keyword(['ENOENT', 'ECONNREFUSED'].includes(error.code) ? 'no-running-body' : 'transport-error'),
      ...(r.mutating ? { 'request-id': r.request['request-id'], confirmation: keyword('unknown'), message: 'Query/retry with the same request ID; do not submit a new ID.' } : {}) }) + '\n')
    return 2
  }
}
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) process.exitCode = await main()
