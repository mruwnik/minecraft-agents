#!/usr/bin/env node
import http from 'node:http'
import path from 'node:path'
import { parseArgs } from 'node:util'
import { fileURLToPath } from 'node:url'
import { readEDN, writeEDN, compactStatus, waitObserve } from './observe-lib.mjs'
import { defaultStateDir } from './drive-lib.mjs'

export const REQUEST_TIMEOUT_MS = 3000
export const MAX_RESPONSE_BYTES = 262144
export const usage = `usage: observe.mjs <agent> [status [--raw|--verbose] [--wait --timeout 60s --chatter addressed --observer agent --watch j12 --watch-action move-home] | job <id> | catalog <job|trigger> <name> | catalog <jobs|triggers> [prefix]] [--limit <n>] [--offset <n>] [--state <dir>]`

export function unsupportedObserveRoute (response) {
  return response.status === 404 && /^application\/edn(?:;|$)/i.test(response.contentType ?? '') &&
    /^\s*\{\s*:ok\s+false\s*,?\s*:reason\s+:not-found\s*\}\s*$/.test(response.text)
}

export function legacyEngineNotice (request) {
  const endpoint = request.path.split('?')[0]
  return `{:ok false :reason :observe-unavailable :body ${JSON.stringify(request.agent)} :endpoint ${JSON.stringify(endpoint)} :action :restart-with-current-build :fallback {:op :status :raw true :state ${JSON.stringify(request.state)}}}`
}

export function requestFor (argv) {
  let parsed
  try {
    parsed = parseArgs({
      args: argv,
      options: {
        state: { type: 'string', default: defaultStateDir },
        limit: { type: 'string' },
        offset: { type: 'string' },
        raw: { type: 'boolean', default: false },
        verbose: { type: 'boolean', default: false },
        wait: { type: 'boolean', default: false },
        timeout: { type: 'string' },
        chatter: { type: 'string' },
        observer: { type: 'string' },
        watch: { type: 'string', multiple: true },
        'watch-action': { type: 'string', multiple: true },
        from: { type: 'string' },
        'poll-ms': { type: 'string' },
        danger: { type: 'boolean', default: false },
        disconnect: { type: 'boolean', default: false }
      },
      allowPositionals: true
    })
  } catch (error) {
    return { error: error.message }
  }
  const [agent, requestedOp, kindOrId, ...rest] = parsed.positionals
  const op = requestedOp ?? 'status'
  if (!agent || !/^[A-Za-z0-9_-]{1,40}$/.test(agent)) return { error: 'agent must be a body name' }
  const state = path.resolve(parsed.values.state)
  const params = new URLSearchParams()
  let endpoint
  if (op === 'status') {
    if (kindOrId !== undefined || rest.length) return { error: 'status takes no positional arguments' }
    if (parsed.values.offset !== undefined) return { error: '--offset is only valid for catalog lists' }
    endpoint = parsed.values.raw ? '/snapshot' : '/status'
    if (parsed.values.raw && parsed.values.limit !== undefined) return { error: '--limit cannot be combined with --raw' }
    if (parsed.values.limit !== undefined) {
      const limit = Number(parsed.values.limit)
      if (!Number.isInteger(limit) || limit < 1 || limit > 32) return { error: '--limit must be an integer from 1 to 32' }
      params.set('limit', String(limit))
    }
  } else if (op === 'job') {
    if (!kindOrId || rest.length) return { error: 'job needs one job ID, such as j12' }
    if (parsed.values.raw || parsed.values.offset !== undefined) return { error: '--raw and --offset are only valid for status and catalog lists respectively' }
    endpoint = '/job'
    params.set('id', kindOrId)
    if (parsed.values.limit !== undefined) {
      const limit = Number(parsed.values.limit)
      if (!Number.isInteger(limit) || limit < 1 || limit > 32) return { error: '--limit must be an integer from 1 to 32' }
      params.set('limit', String(limit))
    }
  } else if (op === 'catalog') {
    endpoint = '/catalog'
    if (['job', 'trigger'].includes(kindOrId)) {
      if (parsed.values.raw || parsed.values.limit !== undefined || parsed.values.offset !== undefined) return { error: 'catalog detail does not accept --raw, --limit, or --offset' }
      const [name] = rest
      if (!name || rest.length !== 1) return { error: 'catalog needs job <jobs.namespace.name> or trigger <trigger-name>' }
      if (kindOrId === 'job' && !/^jobs(?:\.[a-z][a-z0-9-]*)+$/.test(name)) return { error: 'job name must be an exact jobs namespace' }
      if (kindOrId === 'trigger' && !/^[a-z][a-z0-9-]*$/.test(name)) return { error: 'trigger name must be a lowercase identifier' }
      params.set('kind', kindOrId)
      params.set('name', name)
    } else if (['jobs', 'triggers'].includes(kindOrId)) {
      if (parsed.values.raw) return { error: '--raw is only valid for status' }
      if (rest.length > 1) return { error: 'catalog list accepts at most one prefix' }
      const prefix = rest[0] ?? ''
      if (kindOrId === 'jobs' && prefix && !/^jobs(?:\.[a-z][a-z0-9-]*)*(?:\.)?$/.test(prefix)) return { error: 'job prefix must start with jobs.' }
      if (kindOrId === 'triggers' && prefix && !/^[a-z][a-z0-9-]*$/.test(prefix)) return { error: 'trigger prefix must be a lowercase identifier prefix' }
      const limit = parsed.values.limit === undefined ? 20 : Number(parsed.values.limit)
      const offset = parsed.values.offset === undefined ? 0 : Number(parsed.values.offset)
      if (!Number.isInteger(limit) || limit < 1 || limit > 64) return { error: '--limit must be an integer from 1 to 64' }
      if (!Number.isInteger(offset) || offset < 0 || offset > 10000) return { error: '--offset must be an integer from 0 to 10000' }
      params.set('kind', kindOrId)
      params.set('prefix', prefix)
      params.set('limit', String(limit))
      params.set('offset', String(offset))
    } else {
      return { error: 'catalog needs job|trigger <name> or jobs|triggers [prefix]' }
    }
  } else {
    return { error: `unknown operation ${op}` }
  }
  let waitOptions
  const v = parsed.values
  if (v.wait) {
    if (op !== 'status' || v.raw || v.verbose || v.limit) return { error: '--wait is only valid with compact status' }
    const match = /^(\d+(?:\.\d+)?)(ms|s|m)?$/.exec(v.timeout ?? '60s')
    const timeoutMs = match ? Number(match[1]) * ({ ms: 1, s: 1000, m: 60000 }[match[2] ?? 's']) : NaN
    if (!Number.isFinite(timeoutMs) || timeoutMs < 10 || timeoutMs > 3600000) return { error: '--timeout must be between 10ms and 60m' }
    const observer = v.observer ?? 'agent'
    if (!/^[A-Za-z0-9_-]{1,40}$/.test(observer)) return { error: '--observer must be 1-40 letters, digits, underscores or hyphens' }
    const chatter = v.chatter ?? 'addressed'
    if (!['none', 'addressed', 'all'].includes(chatter)) return { error: '--chatter must be none, addressed, or all' }
    const watch = (v.watch ?? []).flatMap(ids => ids.split(','))
    if (watch.some(id => !/^j[0-9]+$/.test(id)) || watch.length > 32) return { error: '--watch needs up to 32 comma-separated job IDs' }
    const watchActions = (v['watch-action'] ?? []).flatMap(ids => ids.split(','))
    if (watchActions.some(id => !/^[A-Za-z0-9_.:-]{1,80}$/.test(id)) || watchActions.length > 32) return { error: '--watch-action needs up to 32 comma-separated action request IDs' }
    if (v.from && !/^[A-Za-z0-9_-]{1,40}$/.test(v.from)) return { error: '--from must be a player name' }
    const pollMs = Number(v['poll-ms'] ?? 250)
    if (!Number.isInteger(pollMs) || pollMs < 50 || pollMs > 5000) return { error: '--poll-ms must be 50-5000' }
    waitOptions = { timeoutMs, observer, chatter, watch, watchActions, pollMs, from: v.from, danger: v.danger, disconnect: v.disconnect }
  } else if (['timeout', 'chatter', 'observer', 'watch', 'watch-action', 'from', 'poll-ms'].some(k => v[k] !== undefined) || v.danger || v.disconnect) return { error: 'wait options require --wait' }
  if (v.verbose && (op !== 'status' || v.raw)) return { error: '--verbose is only valid for status without --raw' }
  const query = params.toString()
  return {
    agent,
    state,
    ...(waitOptions ? { waitOptions } : {}),
    ...(v.verbose ? { verbose: true } : {}),
    socketPath: path.join(state, 'agents', agent, 'engine', 'events.sock'),
    path: query ? `${endpoint}?${query}` : endpoint
  }
}

export function get (socketPath, requestPath, { timeoutMs = REQUEST_TIMEOUT_MS, maxBytes = MAX_RESPONSE_BYTES, requestImpl = http.request, signal } = {}) {
  return new Promise((resolve, reject) => {
    let settled = false
    let timer
    let responseBytes = 0
    const finish = (fn, value) => {
      if (settled) return
      settled = true
      clearTimeout(timer)
      signal?.removeEventListener('abort', abort)
      fn(value)
    }
    const abort = () => { const error = Object.assign(new Error('observe cancelled'), { code: 'ABORT_ERR' }); finish(reject, error); req.destroy(error) }
    const req = requestImpl({ socketPath, method: 'GET', path: requestPath }, (res) => {
      let text = ''
      res.setEncoding('utf8')
      res.on('data', chunk => {
        const chunkBytes = Buffer.byteLength(chunk)
        if (responseBytes + chunkBytes > maxBytes) {
          const error = Object.assign(new Error(`observe response exceeded ${maxBytes} bytes`), { code: 'ERESPONSETOOLARGE' })
          finish(reject, error)
          res.destroy(error)
          req.destroy(error)
          return
        }
        responseBytes += chunkBytes
        text += chunk
      })
      res.on('aborted', () => finish(reject, Object.assign(new Error('observe response was aborted'), { code: 'ECONNRESET' })))
      res.on('error', error => finish(reject, error))
      res.on('end', () => finish(resolve, {
        status: res.statusCode,
        contentType: res.headers?.['content-type'] ?? res.getHeader?.('content-type'),
        text
      }))
    })
    req.on('error', error => finish(reject, error))
    timer = setTimeout(() => {
      const error = Object.assign(new Error(`observe request exceeded ${timeoutMs} ms`), { code: 'ETIMEDOUT' })
      finish(reject, error)
      req.destroy(error)
    }, timeoutMs)
    signal?.addEventListener('abort', abort, { once: true })
    if (signal?.aborted) abort()
    else req.end()
  })
}

export async function main (argv = process.argv.slice(2)) {
  const request = requestFor(argv)
  if (request.error) {
    console.error(`${request.error}\n${usage}`)
    return 2
  }
  try {
    if (request.waitOptions) {
      const controller = new AbortController()
      const cancel = () => controller.abort()
      process.once('SIGINT', cancel)
      process.once('SIGTERM', cancel)
      try {
        await waitObserve(request, get, controller.signal, result => new Promise((resolve, reject) => {
          process.stdout.write(`${writeEDN(result)}\n`, error => error ? reject(error) : resolve())
        }))
        return 0
      } finally { process.removeListener('SIGINT', cancel); process.removeListener('SIGTERM', cancel) }
    }
    const response = await get(request.socketPath, request.path)
    if (!/^application\/edn(?:;|$)/i.test(response.contentType ?? '')) {
      process.stdout.write(`{:ok false :reason :bad-response :detail :unexpected-content-type}\n`)
      return 1
    }
    if (request.path.startsWith('/status') || request.path.startsWith('/job') || request.path.startsWith('/catalog')) {
      if (unsupportedObserveRoute(response)) {
        process.stdout.write(`${legacyEngineNotice(request)}\n`)
        return 2
      }
    }
    if (request.path.startsWith('/status') && !request.verbose && response.status === 200) {
      process.stdout.write(`${writeEDN(compactStatus(readEDN(response.text)))}\n`)
      return 0
    }
    process.stdout.write(response.text.endsWith('\n') ? response.text : `${response.text}\n`)
    return response.status >= 200 && response.status < 300 ? 0 : 1
  } catch (error) {
    const reason = error?.code === 'ABORT_ERR' ? ':cancelled'
      : error?.code === 'EOBSERVERBUSY' ? ':observer-busy'
      : error?.code === 'EOBSERVERLIMIT' ? ':observer-limit'
      : error?.code === 'EATTENTIONLIMIT' ? ':attention-limit'
      : error?.code === 'EOBSERVEUNAVAILABLE' ? ':observe-unavailable'
      : error?.code === 'ETIMEDOUT' ? ':timeout'
      : error?.code === 'ERESPONSETOOLARGE' ? ':response-too-large'
        : ['ECONNREFUSED', 'ENOENT'].includes(error?.code) ? ':no-running-body'
          : error?.code === 'EACCES' ? ':socket-access-denied' : ':transport-error'
    process.stdout.write(`{:ok false :reason ${reason} :body ${JSON.stringify(request.agent)}}\n`)
    return 2
  }
}

if (process.argv[1] && path.resolve(process.argv[1]) === path.resolve(fileURLToPath(import.meta.url))) {
  process.exitCode = await main()
}
