import { withMapLock, atomicMapWrite } from '../map-store.mjs'
// The body's own record: events.jsonl and the tail of it that `events` serves, one-line-per-fault error reporting, and
// the files every body shares (state/zones.json, state/places.json, state/gates.log).
import fs from 'node:fs'
import path from 'node:path'
import { parseEventTail, repeatByType } from '../lib.mjs'
import { HOME, ROOT } from './home.mjs'

// ---------------------------------------------------------------- events
const EVENTS_FILE = path.join(HOME, 'events.jsonl')
// Boxes the pathfinder must not dig through or scaffold in (it happily tunnels through walls otherwise).
const ZONES_FILE = path.join(ROOT, 'state', 'zones.json')
const readZones = () => fs.existsSync(ZONES_FILE) ? JSON.parse(fs.readFileSync(ZONES_FILE, 'utf8')) : []
export const zones = readZones()
// another bot may protect something while we run
fs.watchFile(ZONES_FILE, { interval: 2000 }, () => zones.splice(0, zones.length, ...readZones()))
export const saveZones = () => fs.writeFileSync(ZONES_FILE, JSON.stringify(zones, null, 1))
// Points of interest shared by every agent (./mc mark / places / unmark, and goto place=<name>).
const PLACES_FILE = path.join(ROOT, 'state', 'places.json')
export const GATES_FILE = path.join(ROOT, 'state', 'gates.log')
export const readPlaces = () => fs.existsSync(PLACES_FILE) ? JSON.parse(fs.readFileSync(PLACES_FILE, 'utf8')) : []
// every body shares this file: write beside it and rename, so a reader never catches it half written
export const savePlaces = places => withMapLock(PLACES_FILE, () => atomicMapWrite(PLACES_FILE, places))
// what `events` shows: starts from the tail of the file, so a restart does not wipe the history
function readEventTail () {
  if (!fs.existsSync(EVENTS_FILE)) return []
  const size = fs.statSync(EVENTS_FILE).size
  const tail = Buffer.alloc(Math.min(size, 200000))
  const file = fs.openSync(EVENTS_FILE, 'r')
  fs.readSync(file, tail, 0, tail.length, size - tail.length)
  fs.closeSync(file)
  return parseEventTail(tail.toString('utf8'), 500)
}
export const recent = readEventTail()
let seq = 0
export function emit (type, data = {}) {
  const ev = { seq: ++seq, t: new Date().toISOString(), type, ...data }
  recent.push(ev)
  if (recent.length > 500) recent.shift()
  fs.appendFileSync(EVENTS_FILE, JSON.stringify(ev) + '\n')
  console.log(`[${type}]`, JSON.stringify(data))
}
// Errors go through here rather than straight to emit: a fault that repeats (a timer left running over a reconnect,
// above all) writes the same line every few seconds until the events file is a wall. See errorRepeat.
// one window per KIND of message: a single shared slot let two faults taking turns reset each other's window, so
// neither was ever suppressed (#119)
let errorSeen = null
export function sayOnce (type, message) {
  const { say, seen } = repeatByType(errorSeen, type, message, Date.now())
  errorSeen = seen
  return say
}
export function sayError (message, extra = {}, type = 'error') {
  const say = sayOnce(type, message)
  if (say) emit(type, { ...extra, message: say })
}
