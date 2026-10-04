#!/usr/bin/env node
// Stable Node transport for the ahead-of-time ClojureScript entity query.
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import tools from './agent-tools-loader.mjs'
import { get } from './observe.mjs'
import { keyword, readEDN, writeEDN } from './observe-lib.mjs'

export const usage = tools.entitiesUsage
export const options = tools.entitiesOptions
export const project = tools.entitiesProject

const MAX_RESPONSE_BYTES = 4 * 1024 * 1024 + 4096
const MAX_OUTPUT_BYTES = 65536
const socketFor = request => path.join(request.ctx['world-dir'], 'agents', request.body, 'engine', 'events.sock')
const failure = (reason, message) => ({ ok: false, reason: keyword(reason), message })

export async function execute (request, getImpl = get) {
  const response = await getImpl(socketFor(request), '/entities', { maxBytes: MAX_RESPONSE_BYTES })
  if (!/^application\/edn(?:;|$)/i.test(response.contentType ?? '')) {
    return failure('bad-response', 'the body returned a non-EDN entity response')
  }
  let snapshot
  try { snapshot = readEDN(response.text) } catch {
    return failure('bad-response', 'the body returned invalid EDN for its entity cache')
  }
  if (response.status === 404 && snapshot.reason?.key === 'not-found') {
    return failure('entities-unavailable', 'this body build has no /entities endpoint; restart it with the current engine')
  }
  if (response.status !== 200 && snapshot.ok !== false) {
    return failure('entities-unavailable', `the body entity endpoint returned HTTP ${response.status}`)
  }
  try { return project(request, snapshot) } catch (error) { return errorResult(error) }
}

function errorResult (error) {
  const reason = error?.reason ?? error?.data?.reason ?? 'invalid-request'
  const message = String(error?.message ?? error).slice(0, 500)
  return failure(typeof reason === 'string' ? reason : 'invalid-request', message)
}

export async function main (argv = process.argv.slice(2), output = text => process.stdout.write(`${text}\n`), getImpl = get) {
  if (argv.includes('--help') || argv.includes('-h')) { output(usage); return 0 }
  let request
  try { request = options(argv) } catch (error) {
    output(writeEDN(errorResult(error)))
    return 2
  }
  try {
    const result = await execute(request, getImpl)
    const text = writeEDN(result)
    if (Buffer.byteLength(text, 'utf8') > MAX_OUTPUT_BYTES) {
      output(writeEDN(failure('output-too-large', 'entity output exceeds 65536 bytes; lower --limit or omit --raw')))
      return 1
    }
    output(text)
    return result.ok ? 0 : 1
  } catch (error) {
    const reason = error?.reason ?? error?.data?.reason ?? (['ECONNREFUSED', 'ENOENT'].includes(error?.code) ? 'no-running-body'
      : error?.code === 'ETIMEDOUT' ? 'timeout'
        : error?.code === 'ERESPONSETOOLARGE' ? 'response-too-large'
          : error?.code === 'EACCES' ? 'socket-access-denied' : 'transport-error')
    output(writeEDN(failure(reason, String(error?.message ?? error).slice(0, 240))))
    return 1
  }
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  process.exitCode = await main()
}
