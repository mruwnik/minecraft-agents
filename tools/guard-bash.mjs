#!/usr/bin/env node
// Why JavaScript: a Claude Code PreToolUse hook is a small launcher boundary; it must start fast with no build step.
// Reads the hook JSON on stdin; exit 0 allows, exit 2 blocks with a one-line reason on stderr.
// Any malformed input or unknown tool is allowed: a broken hook must never lock agents out.
import { fileURLToPath } from 'node:url'

const REASON = 'Tests run only via mcp__live-tests__run_tests; a refusal means wait and retry. Kill by PID only.'
const SUITE_REASON = 'use suite engine / world (live-tests runs different args of one suite in parallel)'
const DEPRECATED_SUITES = new Set(['engine-2', 'engine-3', 'world-2', 'world-3', 'world-4'])

// Split a command line into segments of unquoted words. Separators (; && || | newline, $( and
// backtick) only count outside quotes, so quoted text such as grep 'a; pkill' stays one word.
export function segments(cmd) {
  const segs = []
  let words = [], word = '', has = false, quote = null
  const endWord = () => { if (has) words.push(word); word = ''; has = false }
  const endSeg = () => { endWord(); if (words.length) segs.push(words); words = [] }
  for (let i = 0; i < cmd.length; i++) {
    const c = cmd[i]
    if (quote) {
      if (c === quote) quote = null
      else if (c === '\\' && quote === '"' && i + 1 < cmd.length) word += cmd[++i]
      else word += c
      continue
    }
    if (c === "'" || c === '"') { quote = c; has = true; continue }
    if (c === '\\' && i + 1 < cmd.length) { word += cmd[++i]; has = true; continue }
    if (c === '<' && cmd[i + 1] === '<' && cmd[i + 2] !== '<') {
      // heredoc: the body is data, skip to the terminator line
      const m = /^<<-?\s*(['"]?)([A-Za-z_][\w]*)\1/.exec(cmd.slice(i))
      if (m) {
        const nl = cmd.indexOf('\n', i)
        if (nl < 0) break
        const re = new RegExp(`^\\s*${m[2]}\\s*$`, 'm')
        const rest = cmd.slice(nl + 1)
        const t = re.exec(rest)
        endSeg()
        // keep the rest of the line after the marker as its own segment text
        const lineRest = cmd.slice(i + m[0].length, nl)
        if (lineRest.trim()) segs.push(...segments(lineRest))
        i = t ? nl + 1 + t.index + t[0].length - 1 : cmd.length
        continue
      }
    }
    if (c === ';' || c === '\n' || c === '`' || c === '(' || c === ')') { endSeg(); continue }
    if (c === '&' || c === '|') { endSeg(); continue }
    if (c === ' ' || c === '\t') { endWord(); continue }
    word += c; has = true
  }
  endSeg()
  return segs
}

const base = (w) => w.split('/').pop()
const ASSIGN = /^[A-Za-z_]\w*=/
const WRAPPERS = new Set(['env', 'nohup', 'sudo', 'time', 'exec', 'command', 'nice'])

// Drop leading VAR=x, env, timeout N, nohup etc. so the command word comes first.
function strip(words) {
  let i = 0
  while (i < words.length) {
    const w = words[i]
    if (ASSIGN.test(w) || WRAPPERS.has(base(w))) { i++; continue }
    if (base(w) === 'timeout') {
      i++
      while (i < words.length && words[i].startsWith('-')) i++
      i++ // duration
      continue
    }
    break
  }
  return words.slice(i)
}

function blockedWords(words) {
  const w = strip(words)
  if (!w.length) return false
  const cmd = base(w[0]), args = w.slice(1)
  if (cmd === 'pkill' || cmd === 'killall') return true
  if (cmd === 'test-engine' || cmd === 'shadow-cljs' || cmd === 'world-test.mjs') return true
  if (cmd.endsWith('.test.sh')) return true
  if (cmd === 'res-slot') {
    const i = args.indexOf('--')
    return i >= 0 && blockedWords(args.slice(i + 1))
  }
  if (cmd === 'node') {
    const si = args.findIndex((a) => !a.startsWith('-'))
    const flags = si < 0 ? args : args.slice(0, si) // later words are the script's own args
    if (flags.some((a) => a.startsWith('--test'))) return true
    const script = args[si]
    return !!script && base(script) === 'world-test.mjs'
  }
  if (['npx', 'pnpx', 'bunx', 'yarn', 'pnpm', 'npm'].includes(cmd)) {
    if (args.includes('shadow-cljs')) return true
    if (cmd === 'npx' || cmd === 'pnpx' || cmd === 'bunx') return blockedWords(args.filter((a) => !a.startsWith('-')))
    const i = args.findIndex((a) => a === 'test' || a === 't' || a.startsWith('test:') || a === 'run' || a === 'run-script')
    if (i < 0) return false
    if (args[i].startsWith('run')) return /^test/.test(args[i + 1] ?? '')
    return true
  }
  if (['bash', 'sh', 'zsh'].includes(cmd)) {
    const ci = args.indexOf('-c')
    if (ci >= 0 && args[ci + 1] !== undefined) return blockedCommand(args[ci + 1])
    const script = args.find((a) => !a.startsWith('-'))
    return !!script && script.endsWith('.test.sh')
  }
  return false
}

function blockedCommand(command) {
  if (command.includes('LIVE_TESTS_NONCE=')) return false
  return segments(command).some(blockedWords)
}

export function decide(input) {
  const allow = { block: false }
  try {
    if (!input || typeof input !== 'object') return allow
    const ti = input.tool_input
    if (!ti || typeof ti !== 'object') return allow
    if (input.tool_name === 'Bash') {
      return typeof ti.command === 'string' && blockedCommand(ti.command) ? { block: true, reason: REASON } : allow
    }
    if (input.tool_name === 'mcp__live-tests__run_tests') {
      return DEPRECATED_SUITES.has(ti.suite) ? { block: true, reason: `${SUITE_REASON}. ${REASON}` } : allow
    }
  } catch { /* a broken hook must not lock agents out */ }
  return allow
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  let raw = ''
  process.stdin.setEncoding('utf8')
  process.stdin.on('data', (d) => { raw += d })
  process.stdin.on('end', () => {
    let input = null
    try { input = JSON.parse(raw) } catch { process.exit(0) }
    const d = decide(input)
    if (!d.block) process.exit(0)
    process.stderr.write(`${d.reason}\n`)
    process.exit(2)
  })
}
