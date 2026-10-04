#!/usr/bin/env node
import path from 'node:path'
import fs from 'node:fs'
import http from 'node:http'
import { fileURLToPath } from 'node:url'
import tools from './agent-tools-loader.mjs'
import { get } from './observe.mjs'
import { readEDN, writeEDN, keyword } from './observe-lib.mjs'

// Validation and native request construction live in compiled ClojureScript.
// Existing HTTP/EDN transport remains at this JavaScript library boundary.
export const usage = tools.jobsUsage
export const specFor = tools.jobsSpecFor
export const requestFor = argv => tools.jobsRequestFor(argv)
export function post (socketPath, body, { path: requestPath = '/jobs', timeoutMs = 3000, requestImpl = http.request } = {}) {
  return new Promise((resolve, reject) => {
    const payload = writeEDN(body)
    let done = false, timer
    const finish = (fn, value) => { if (!done) { done = true; clearTimeout(timer); fn(value) } }
    const req = requestImpl({ socketPath, path: requestPath, method: 'POST', headers: { 'content-type': 'application/edn', 'content-length': Buffer.byteLength(payload) } }, res => {
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
    if (r.resolve) {
      response = await post(r.socketPath, r.request, { path: r.path })
    } else if (r.mutating) {
      const snapshot = await get(r.socketPath, '/snapshot')
      if (snapshot.status !== 200 || !/^application\/edn(?:;|$)/i.test(snapshot.contentType ?? '')) throw new Error('snapshot unavailable')
      const metaDir = path.join(path.dirname(path.dirname(r.socketPath)), '.commands', 'jobs')
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
      response = await post(r.socketPath, { ...r.request, 'generation-id': generation }, { path: r.path })
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
      ...(r.resolve ? { 'request-id': r.request['request-id'], confirmation: keyword('unknown'), message: 'Resolve confirmation is unknown; inspect outstanding attention before another request.' }
        : r.mutating ? { 'request-id': r.request['request-id'], confirmation: keyword('unknown'), message: 'Query/retry with the same request ID; do not submit a new ID.' } : {}) }) + '\n')
    return 2
  }
}
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) process.exitCode = await main()
