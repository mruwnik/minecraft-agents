import edn from 'edn-data'
import fs from 'node:fs'
import { createHash } from 'node:crypto'
import path from 'node:path'
import { setTimeout as delay } from 'node:timers/promises'

export const keyword = key => ({ key })
export const kind = value => value?.key
function unpack (v) {
  if (Array.isArray(v)) return v.map(unpack)
  if (v?.map) return Object.fromEntries(v.map.map(([k, value]) => [k.key ?? k, unpack(value)]))
  return v
}
function pack (v) {
  if (Array.isArray(v)) return v.map(pack)
  if (v && typeof v === 'object') {
    if (Object.keys(v).length === 1 && ['key', 'sym', 'set', 'list', 'tag'].some(k => k in v)) return v
    return { map: Object.entries(v).filter(([, value]) => value !== undefined).map(([k, value]) => [keyword(k), pack(value)]) }
  }
  return v
}
export const readEDN = text => unpack(edn.parseEDNString(text))
export const writeEDN = value => edn.toEDNString(pack(value))
const short = (s, max = 240) => typeof s === 'string' ? s.slice(0, max) : s
const clean = obj => Object.fromEntries(Object.entries(obj).filter(([, v]) => v !== null && v !== undefined))
const job = item => clean({ id: item.id, name: short(item.name, 120), status: item.status, reflex: item.reflex })
export function compactStatus (s) {
  if (s.ok === false) return s
  const pos = s.position
  const queued = (s.jobs?.items ?? []).filter(item => item.id !== s.current?.id)
  const queuedTotal = (s.jobs?.total ?? 0) - ((s.jobs?.items ?? []).some(item => item.id === s.current?.id) ? 1 : 0)
  return clean({ mode: s.mode, idle: s.current ? undefined : true, pos: pos && [pos.x, pos.y, pos.z].map(n => Math.round(n * 10) / 10),
    health: s.health, food: s.food, current: s.current && job(s.current),
    manual: s.manual && clean({ who: short(s.manual.who, 80), why: short(s.manual.why, 160) }),
    jobs: queuedTotal ? { total: queuedTotal, items: queued.map(job), ...(s.jobs['more?'] ? { 'more?': true } : {}) } : undefined,
    failed: s.failed?.total ? { total: s.failed.total, items: s.failed.items.map(x => ({ id: x.id, error: short(x.error) })) } : undefined,
    attention: s.outstanding?.total ? { total: s.outstanding.total, items: s.outstanding.items.map(attentionItem) } : undefined })
}
function attentionItem (a) { return clean({ id: a['request-id'], job: a['job-id'], reason: a.reason, message: short(a.message) }) }
function attentionSignature (r) {
  const e = r.event ?? {}
  return createHash('sha256').update(writeEDN({ job: r['job-id'] ?? null, reason: r.reason ?? null, kind: e.kind ?? null, message: short(e.message) ?? null, data: { ...e.data, pos: undefined, 'time-ms': undefined } })).digest('hex')
}
export function attentionChanges (outstanding, seen) {
  if (Object.keys(outstanding ?? {}).length > 4096) throw Object.assign(new Error('too many attention requests'), { code: 'EATTENTIONLIMIT' })
  const next = {}
  const changed = []
  for (const [id, r] of Object.entries(outstanding ?? {})) {
    const signature = attentionSignature(r)
    next[id] = seen[id]
    if (seen[id] !== signature && changed.length < 4) { next[id] = signature; changed.push(attentionItem({ ...r, 'request-id': id, message: r.event?.message })); }
  }
  return { seen: next, changed: changed.slice(0, 4), more: Object.entries(outstanding ?? {}).some(([id, r]) => next[id] !== attentionSignature(r)) }
}
export function classify (e, opts, body) {
  const k = kind(e.kind)
  const d = e.data ?? {}
  if (kind(e.source) === 'body' && ['chat', 'whisper'].includes(k)) {
    if (opts.from && d.from?.toLowerCase() !== opts.from.toLowerCase()) return null
    const addressed = k === 'whisper' || d.whisper === true || d.to?.toLowerCase() === body.toLowerCase() ||
      new RegExp(`(^|[^A-Za-z0-9_])${body}([^A-Za-z0-9_]|$)`, 'i').test(e.message ?? '')
    if (opts.chatter === 'none' || (opts.chatter === 'addressed' && !addressed)) return null
    return { wake: keyword('chat'), from: short(d.from, 40), message: short(e.message), ...(k === 'whisper' ? { whisper: true } : {}) }
  }
  if (kind(e.source) === 'body' && k === 'reconnect-failed') return { wake: keyword('reconnect-failed'), reason: short(d.reason) }
  if (kind(e.source) === 'body' && opts.danger && ['hurt', 'died'].includes(k)) return { wake: keyword('danger'), event: e.kind, health: d.health }
  if (kind(e.source) === 'body' && opts.disconnect && k === 'disconnected') return { wake: keyword('disconnected'), reason: short(d.reason) }
  if (kind(e.source) === 'action' && k === 'done' && opts.watchActions?.includes(e.context?.['action-id'])) {
    const r = d.result ?? {}
    const pos = r.pos
    return { wake: keyword('action-finished'), action: e.context['action-id'], result: clean({
      status: r.status ?? d.status, reason: short(r.reason ?? d.reason ?? d.error),
      block: short(r.block, 80), consumed: r.consumed, hurt: r.hurt, health: r.health,
      ...(pos ? { pos: [pos.x, pos.y, pos.z].map(n => Math.round(n * 10) / 10) } : {}),
      ...(typeof r.distance === 'number' ? { distance: Math.round(r.distance * 10) / 10 } : {})
    }) }
  }
  if (kind(e.source) === 'job' && ['completed', 'failed'].includes(k)) {
    const id = e.context?.['job-id']
    if (opts.watch.includes(id)) return { wake: keyword('job-finished'), job: id, result: e.kind, message: short(e.message ?? d.error) }
  }
  return null
}
export function collect (summary, e) {
  const k = kind(e.kind)
  const d = e.data ?? {}
  const category = kind(e.source) === 'job' && ['completed', 'failed'].includes(k) ? k
    : ['picked-up', 'hurt', 'died', 'disconnected', 'online', 'reconnect-failed'].includes(k) ? k
      : kind(e.attention) === 'notice' ? 'notices' : null
  if (!category) return
  summary.counts[category] = Math.min(1000000, (summary.counts[category] ?? 0) + 1)
  if (summary.items.length < 4) summary.items.push(clean({ event: e.kind, job: e.context?.['job-id'],
    item: short(d.item, 80), count: d.count, message: short(e.message ?? d.error ?? d.reason) }))
  else summary.more = true
}
function summaryResult (summary, result) {
  return Object.keys(summary.counts).length ? { ...clean(result), summary: { counts: summary.counts, items: summary.items, ...(summary.more ? { 'more?': true } : {}) } }
    : clean(result)
}
function acquire (dir, observer) {
  fs.mkdirSync(dir, { recursive: true, mode: 0o700 })
  const lock = path.join(dir, `${observer}.lock`)
  try { fs.mkdirSync(lock, { mode: 0o700 }) } catch (error) {
    if (error.code !== 'EEXIST') throw error
    let pid
    try { pid = Number(fs.readFileSync(path.join(lock, 'pid'), 'utf8')) } catch {}
    if (pid && Number.isInteger(pid)) {
      try { process.kill(pid, 0); throw Object.assign(new Error('observer busy'), { code: 'EOBSERVERBUSY' }) }
      catch (e) { if (e.code !== 'ESRCH') throw e }
      fs.rmSync(lock, { recursive: true })
      fs.mkdirSync(lock, { mode: 0o700 })
    } else throw Object.assign(new Error('observer busy'), { code: 'EOBSERVERBUSY' })
  }
  fs.writeFileSync(path.join(lock, 'pid'), String(process.pid), { mode: 0o600 })
  return () => fs.rmSync(lock, { recursive: true, force: true })
}
function checkpoint (file, state) {
  const temp = `${file}.${process.pid}.tmp`
  fs.writeFileSync(temp, `${writeEDN(state)}\n`, { mode: 0o600 })
  fs.renameSync(temp, file)
}
/** Tool-owned cursor and attention state. Caller writes the returned compact EDN. */
export async function waitObserve (request, get, signal, deliver = async () => {}) {
  const opts = request.waitOptions
  // per body, and a name is unique only within a world
  const dir = path.join(request.state, 'worlds', request.world, 'observers', request.agent)
  const release = acquire(dir, opts.observer)
  const file = path.join(dir, `${opts.observer}.edn`)
  let saved
  let timeoutFinish
  let deadline
  try {
    if (fs.existsSync(file)) saved = readEDN(fs.readFileSync(file, 'utf8'))
    if (new Set(fs.readdirSync(dir).filter(f => f.endsWith('.edn') || f.endsWith('.lock')).map(f => f.replace(/\.(edn|lock)$/, ''))).size > 64) throw Object.assign(new Error('observer limit'), { code: 'EOBSERVERLIMIT' })
    deadline = Date.now() + opts.timeoutMs
    const read = async endpoint => {
      const response = await get(request.socketPath, endpoint, { signal, timeoutMs: Math.max(1, Math.min(3000, deadline - Date.now())) })
      if (response.status !== 200 || !/^application\/edn(?:;|$)/i.test(response.contentType ?? '')) throw Object.assign(new Error(response.text), { code: response.status === 404 ? 'EOBSERVEUNAVAILABLE' : 'EBADRESPONSE' })
      return readEDN(response.text)
    }
    let snap = await read('/snapshot')
    const generation = snap['generation-id']
    let cursor = saved?.cursor ?? snap.cursor
    let seen = saved?.seen ?? {}
    let lookup = !saved || saved.lookup === true
    let pending = saved?.pending ?? []
    if (!saved) checkpoint(file, { cursor, generation, seen, lookup: true })
    const summary = { counts: {}, items: [], more: false }
    const finish = async result => {
      if (signal?.aborted) throw Object.assign(new Error('cancelled'), { code: 'ABORT_ERR' })
      const output = summaryResult(summary, result)
      await deliver(output)
      checkpoint(file, { cursor, generation, seen, pending, ...(lookup ? { lookup: true } : {}) })
      return output
    }
    timeoutFinish = () => finish({ wake: keyword('timeout'), ...(Object.keys(summary.counts).length ? {} : { changed: false }) })
    if (saved && saved.generation !== generation) {
      cursor = snap.cursor
      seen = {}
      pending = []; lookup = false
      return await finish({ wake: keyword('reset'), reason: keyword('engine-restarted'), status: compactStatus(await read('/status')) })
    }
    const body = snap.body ?? request.agent
    for (;;) {
      const changes = attentionChanges(snap.outstanding, seen)
      seen = changes.seen
      if (changes.changed.length) return await finish({ wake: keyword('attention'), requests: changes.changed, ...(changes.more ? { 'more?': true } : {}) })
      if (lookup && ((opts.watchActions?.length ?? 0) || opts.watch.length)) {
        const query = after => `/events?stream-id=${encodeURIComponent(cursor['stream-id'])}&after=${after}&limit=1000`
        let history = await read(query(Math.max(0, cursor.seq - 1000)))
        if (history['gap?'] && typeof history['oldest-seq'] === 'number') history = await read(query(Math.max(0, cursor.seq - 1000, history['oldest-seq'] - 1)))
        if (history['gap?']) { lookup = false; return await finish({ wake: keyword('reset'), reason: keyword('history-unavailable') }) }
        const latest = new Map()
        for (const e of history.events ?? []) {
          if (e.seq > cursor.seq || e['generation-id'] !== generation) continue
          const action = kind(e.source) === 'action' && opts.watchActions?.includes(e.context?.['action-id']) && ['started', 'done'].includes(kind(e.kind))
          const job = kind(e.source) === 'job' && opts.watch.includes(e.context?.['job-id']) && ['queued', 'round_started', 'completed', 'failed'].includes(kind(e.kind))
          if (action || job) latest.set(`${action ? 'action' : 'job'}:${e.context[action ? 'action-id' : 'job-id']}`, e)
        }
        pending = [...latest.values()].sort((a, b) => a.seq - b.seq).map(e => classify(e, opts, body)).filter(Boolean)
      }
      lookup = false
      pending = pending.filter(e => kind(e.wake) === 'action-finished' ? opts.watchActions?.includes(e.action) : opts.watch.includes(e.job))
      if (pending.length) return await finish(pending.shift())

      if (Date.now() >= deadline) return await finish({ wake: keyword('timeout'), ...(Object.keys(summary.counts).length ? {} : { changed: false }) })
      const page = await read(`/events?stream-id=${encodeURIComponent(cursor['stream-id'])}&after=${cursor.seq}&limit=256`)
      if (page['gap?']) {
        cursor = page.cursor
        seen = {}
        return await finish({ wake: keyword('reset'), reason: keyword('event-gap'), status: compactStatus(await read('/status')) })
      }
      for (const event of page.events ?? []) {
        cursor = { 'stream-id': page['stream-id'], seq: event.seq }
        if (kind(event.attention) === 'required') {
          snap = await read('/snapshot')
          const update = attentionChanges(snap.outstanding, seen)
          seen = update.seen
          if (update.changed.length) return await finish({ wake: keyword('attention'), requests: update.changed, ...(update.more ? { 'more?': true } : {}) })
        }
        if (kind(event.source) === 'attention' && kind(event.kind) === 'resolved') { delete seen[event['request-id']]; delete snap.outstanding[event['request-id']] }
        if (kind(event.source) === 'system' && ['started', 'restored'].includes(kind(event.kind))) { snap = await read('/snapshot'); cursor = snap.cursor; seen = {}; return await finish({ wake: keyword('reset'), reason: keyword('engine-restarted') }) }
        const immediate = classify(event, opts, body)
        if (immediate) return await finish(immediate)
        collect(summary, event)
      }
      if (Date.now() >= deadline) return await finish({ wake: keyword('timeout'), ...(Object.keys(summary.counts).length ? {} : { changed: false }) })
      if (cursor.seq < page['latest-seq']) continue
      await delay(Math.min(opts.pollMs, Math.max(1, deadline - Date.now())), undefined, { signal })
    }
  } catch (error) {
    if (error.code === 'ETIMEDOUT' && Date.now() >= deadline && timeoutFinish) return await timeoutFinish()
    throw error
  } finally { release() }
}
