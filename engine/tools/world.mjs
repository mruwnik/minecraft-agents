#!/usr/bin/env node
import crypto from 'node:crypto'
import fs from 'node:fs'
import http from 'node:http'
import path from 'node:path'
import { parseArgs } from 'node:util'
import { defaultStateDir, socketPathFor } from './drive-lib.mjs'
import { NAME, missingWorldError, storageRoot } from '../js/bodies.mjs'

const kw = name => ({ __keyword: name })
const edn = value => {
  if (value?.__keyword) return `:${value.__keyword}`
  if (value === null || value === undefined) return 'nil'
  if (typeof value === 'string') return JSON.stringify(value)
  if (typeof value === 'boolean' || typeof value === 'number') {
    if (typeof value === 'number' && !Number.isFinite(value)) throw new Error('EDN numbers must be finite')
    return String(value)
  }
  if (Array.isArray(value)) return `[${value.map(edn).join(' ')}]`
  if (typeof value === 'object') return `{${Object.entries(value).map(([k, v]) => `:${k} ${edn(v)}`).join(' ')}}`
  throw new Error('unsupported EDN value')
}

const usage = `usage: world.mjs <agent> <command> [args] --world <world> [--who claude] [--worlds <dir>] [--state <legacy-parent>]
  submit move-to <x> <y> <z> [--range <n>] [--timeout-s <1..10>] [--max-distance <1..64>]
  submit dig <x> <y> <z> | submit place <x> <y> <z> <item>
  submit use-on <x> <y> <z> [--item <item>] [--face up|down|north|south|east|west]
  submit interact <entity-id> [--item <item>] [--request-id <id>]
  status <request-id> | cancel <request-id> | inventory
Acquire the body first with drive.mjs <agent> --world <world> take --who <same-name> --idle-s <seconds>.
Submit returns immediately with a request-id; poll status while continuing to observe/chat.`

const opts = { who: { type: 'string', default: 'claude' }, state: { type: 'string' }, worlds: { type: 'string' }, world: { type: 'string' },
  range: { type: 'string' }, 'timeout-s': { type: 'string' }, 'max-distance': { type: 'string' },
  item: { type: 'string' }, face: { type: 'string' }, 'request-id': { type: 'string' } }
const num = (s, name) => {
  if (s === undefined || !Number.isFinite(Number(s))) throw new Error(`${name} must be a finite number`)
  return Number(s)
}

function requestForUnsafe (argv) {
  let parsed
  const neg = /^-\d+(\.\d+)?$/
  const hide = value => neg.test(value) ? `\0${value}` : value
  const unhide = value => value.startsWith('\0') ? value.slice(1) : value
  try { parsed = parseArgs({ args: argv.map(hide), options: opts, allowPositionals: true, strict: true }) }
  catch (e) { return { error: e.message } }
  const [agent, command, action, ...args] = parsed.positionals.map(unhide)
  if (!agent || !/^[A-Za-z0-9_-]{1,40}$/.test(agent)) return { error: 'agent must be a body name (letters, digits, _ or -, max 40)' }
  if (!command) return { error: 'need <agent> and <command>' }
  const { who, state, world, range, 'timeout-s': timeoutS, 'max-distance': maxDistance, item, face } = parsed.values
  if (world === undefined) return { error: missingWorldError('--world') }
  if (!NAME.test(world)) return { error: 'the world must be a name of letters, digits, _ and -' }
  const requestIdOption = parsed.values['request-id']
  if (typeof who !== 'string' || who.length < 1 || who.length > 80) return { error: '--who must be 1..80 characters' }
  if (requestIdOption !== undefined && command !== 'submit') return { error: '--request-id is only valid for submit' }
  const supplied = key => parsed.values[key] !== undefined
  if (command !== 'submit' && ['range', 'timeout-s', 'max-distance', 'item', 'face'].some(supplied)) return { error: 'action options are only valid with submit' }
  let body
  if (command === 'inventory') body = { op: kw('inventory'), who }
  else if (command === 'status' || command === 'cancel') {
    if (!action || args.length) return { error: `${command} needs exactly one request-id` }
    body = { op: kw(command), who, 'request-id': action }
  } else if (command === 'submit') {
    const allowedOptions = action === 'move-to' ? ['range', 'timeout-s', 'max-distance'] :
      action === 'use-on' ? ['item', 'face'] : action === 'interact' ? ['item'] : []
    const unsupported = ['range', 'timeout-s', 'max-distance', 'item', 'face'].find(key => supplied(key) && !allowedOptions.includes(key))
    if (unsupported) return { error: `--${unsupported} is not valid for ${action}` }
    const posArgs = expected => {
      if (args.length !== expected) throw new Error(`${action} needs ${expected === 4 ? 'x y z item' : 'x y z'}`)
      return { pos: { x: num(args[0], 'x'), y: num(args[1], 'y'), z: num(args[2], 'z') } }
    }
    let argsMap
    if (action === 'move-to') argsMap = { ...posArgs(3), ...(range !== undefined ? { range: num(range, '--range') } : {}),
      ...(timeoutS !== undefined ? { timeoutS: num(timeoutS, '--timeout-s') } : {}),
      ...(maxDistance !== undefined ? { maxDistance: num(maxDistance, '--max-distance') } : {}) }
    else if (action === 'dig') argsMap = posArgs(3)
    else if (action === 'place') {
      if (args.length !== 4 || item !== undefined) throw new Error('place needs x y z item')
      argsMap = { ...posArgs(4), item: args[3] }
    } else if (action === 'use-on') argsMap = { ...posArgs(3), ...(item ? { item } : {}), ...(face ? { face } : {}) }
    else if (action === 'interact') {
      if (args.length !== 1) throw new Error('interact needs one entity-id')
      argsMap = { id: num(args[0], 'entity-id'), ...(item ? { item } : {}) }
    } else throw new Error(`unknown action ${action}`)
    const actionNames = { 'move-to': 'move-to', dig: 'dig', place: 'place', 'use-on': 'use-on', interact: 'interact' }
    const requestId = requestIdOption ?? crypto.randomUUID()
    if (!/^[A-Za-z0-9._-]{1,80}$/.test(requestId)) throw new Error('--request-id must be 1..80 letters, digits, dot, _ or -')
    body = { op: kw('submit'), who, 'request-id': requestId, action: kw(actionNames[action]), args: argsMap }
  } else return { error: `unknown command ${command}` }
  return { agent, world, state: storageRoot(parsed.values, defaultStateDir), who, body }
}

export function requestFor (argv) {
  try { return requestForUnsafe(argv) } catch (error) { return { error: error.message } }
}

export const REQUEST_TIMEOUT_MS = 3000
export const MAX_RESPONSE_BYTES = 65536

const send = (socketPath, body) => new Promise((resolve, reject) => {
  const payload = `${edn(body)}\n`
  let settled = false
  let timer
  let responseBytes = 0
  const finish = (fn, value) => {
    if (settled) return
    settled = true
    clearTimeout(timer)
    fn(value)
  }
  const req = http.request({ socketPath, method: 'POST', path: '/world',
    headers: { 'content-type': 'application/edn', 'content-length': Buffer.byteLength(payload) } }, res => {
    const chunks = []
    res.setEncoding('utf8')
    res.on('data', chunk => {
      const bytes = Buffer.byteLength(chunk)
      if (responseBytes + bytes > MAX_RESPONSE_BYTES) {
        const error = Object.assign(new Error('world response exceeded 65536 bytes'), { code: 'ERESPONSETOOLARGE' })
        finish(reject, error)
        res.destroy(error)
        req.destroy(error)
        return
      }
      responseBytes += bytes
      chunks.push(chunk)
    })
    res.on('end', () => finish(resolve, { status: res.statusCode, contentType: res.headers['content-type'] ?? '', text: chunks.join('') }))
    res.on('aborted', () => finish(reject, Object.assign(new Error('world response was aborted'), { code: 'ECONNRESET' })))
    res.on('error', error => finish(reject, error))
  })
  timer = setTimeout(() => {
    const error = Object.assign(new Error(`world request timed out after ${REQUEST_TIMEOUT_MS} ms`), { code: 'ETIMEDOUT' })
    finish(reject, error)
    req.destroy(error)
  }, REQUEST_TIMEOUT_MS)
  req.on('error', error => finish(reject, error))
  req.end(payload)
})

const main = async () => {
  const parsed = requestFor(process.argv.slice(2))
  if (parsed.error) { console.error(`${parsed.error}\n${usage}`); return 2 }
  const socketPath = socketPathFor(parsed)
  try {
    const response = await send(socketPath, parsed.body)
    if (!/^application\/edn(?:;|$)/i.test(response.contentType)) {
      process.stdout.write(`{:ok false :reason :world-unavailable :http-status ${response.status} :hint "restart body with the current engine build"}\n`)
      return 1
    }
    process.stdout.write(response.text)
    return response.status >= 200 && response.status < 300 ? 0 : 1
  } catch (error) {
    const why = error.code === 'ETIMEDOUT' ? 'request timed out after 3s' :
      error.code === 'ERESPONSETOOLARGE' ? 'engine response exceeded 64 KB' :
        fs.existsSync(socketPath) ? `connection failed (${error.code ?? error.message})` : `no running body (no socket at ${socketPath})`
    const requestId = parsed.body?.['request-id']
    console.error(`${why}; command was not confirmed${requestId ? `; retry/query with --request-id ${requestId}` : ''}`)
    return 2
  }
}

if (process.argv[1] === new URL(import.meta.url).pathname) process.exitCode = await main()
