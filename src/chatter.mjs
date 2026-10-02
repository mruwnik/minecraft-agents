// Chattiness: how much a chat/whisper line asks an agent for an answer, and whether it is loud enough to end
// ./mc wait. The rules-based half of this (messageWeight, hears, skippedLine) has to live in src/cli.mjs itself:
// tools/mc.mjs and src/cli.mjs are both restricted (#148) to importing none of our other files (mc.mjs: none but
// src/config.mjs), so they cannot reach a separate module. This file re-exports that same code (one function, not
// two copies) and adds what only needs to run OUTSIDE that restricted path: the outbound chat guard in
// src/bot.mjs, and the optional haiku grader.
import { execFileSync } from 'node:child_process'
import fs from 'node:fs'
import path from 'node:path'
import { messageWeight, hears, skippedLine, isGreeting, isQuestion, isDanger, namesMe } from './cli.mjs'

export { messageWeight, hears, skippedLine, isGreeting, isQuestion, isDanger, namesMe }

const ROOT = import.meta.url.replace(/^file:\/\//, '').replace(/\/src\/chatter\.mjs$/, '')
export const CACHE_FILE = path.join(ROOT, 'state', 'chat-weights.jsonl')

const HAIKU_MODEL = 'claude-haiku-4-5-20251001'
export const HAIKU_TIMEOUT_MS = 5000

// One key per line so every agent grades it once: who said it, when, and what (t is the event's own timestamp, not
// now, so two agents reading the same events.jsonl line agree on the key)
export const cacheKey = ({ from, t, message }) => `${from}\u0000${t}\u0000${message}`

export const readCache = (file = CACHE_FILE) => {
  if (!fs.existsSync(file)) return new Map()
  const lines = fs.readFileSync(file, 'utf8').split('\n').filter(Boolean)
  return new Map(lines.map(line => { const { key, weight } = JSON.parse(line); return [key, weight] }))
}

export const appendCache = (key, weight, file = CACHE_FILE) => fs.appendFileSync(file, JSON.stringify({ key, weight }) + '\n')

export const haikuPrompt = (event, me) =>
  `On a scale from 0 to 1, how much does this message ask ${me} to answer?\n${event.from}: "${event.message}"\nReply with only the number, nothing else.`

// The shell-out, isolated so a test can stub `run`. A reply that is not a plain number in [0,1], or a run that
// throws (including a timeout), is null: the caller falls back to the rules-based weight rather than block ./mc.
export const haikuGrade = (prompt, { run = execFileSync, timeout = HAIKU_TIMEOUT_MS } = {}) => {
  try {
    const out = run('claude', ['-p', '--model', HAIKU_MODEL, prompt], { encoding: 'utf8', timeout })
    const n = Number(String(out).trim())
    return Number.isFinite(n) && n >= 0 && n <= 1 ? n : null
  } catch {
    return null
  }
}

// The weight to use for one event, honouring config.chat.grader: "rules" (or anything but "haiku") is messageWeight
// alone; "haiku" asks a cheap model once per line, caches the answer (state/chat-weights.jsonl, shared by every
// agent reading the same log), and falls back to the rules-based weight on a bad reply, an error, or a slow run.
export function gradeWeight ({ event, chat = {}, me, humans = [], run, cacheFile = CACHE_FILE } = {}) {
  const to = event.type === 'whisper' ? me : event.to
  const rulesWeight = messageWeight({ from: event.from, message: event.message, to, me, humans })
  if (chat.grader !== 'haiku') return rulesWeight
  const key = cacheKey(event)
  const cached = readCache(cacheFile)
  if (cached.has(key)) return cached.get(key)
  const graded = haikuGrade(haikuPrompt(event, me), { run })
  const weight = graded ?? rulesWeight
  appendCache(key, weight, cacheFile)
  return weight
}
