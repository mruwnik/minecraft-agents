#!/usr/bin/env node
// Why JavaScript: a Claude Code PreToolUse hook is a small launcher boundary; it must start fast with no build step.
// Reads the hook JSON on stdin; exit 0 allows, exit 2 blocks with a one-line reason on stderr.
// Any malformed input or unknown tool is allowed: a broken hook must never lock agents out.
import { fileURLToPath } from 'node:url'

const REASON = 'Tests run only via mcp__live-tests__run_tests; a refusal means wait and retry. Kill by PID only.'
const SUITE_REASON = 'use suite engine / world (live-tests runs different args of one suite in parallel)'
const DEPRECATED_SUITES = new Set(['engine-2', 'engine-3', 'world-2', 'world-3', 'world-4'])

// Index of the closing ` or ) of the substitution starting at cmd[i] (cmd.length when unclosed).
function substitutionEnd(cmd, i) {
  if (cmd[i] === '`') {
    for (let j = i + 1; j < cmd.length; j++) {
      if (cmd[j] === '\\') j++
      else if (cmd[j] === '`') return j
    }
    return cmd.length
  }
  for (let j = i + 2, depth = 1; j < cmd.length; j++) {
    if (cmd[j] === '(') depth++
    else if (cmd[j] === ')' && --depth === 0) return j
  }
  return cmd.length
}

// Split a command line into segments of unquoted words. Separators (; && || | newline, $( and
// backtick) only count outside quotes, so quoted text such as grep 'a; pkill' stays one word.
export function segments(cmd) {
  const segs = []
  let words = [], word = '', has = false, quote = null
  const endWord = () => { if (has) words.push(word); word = ''; has = false }
  const endSeg = () => { endWord(); if (words.length) segs.push(words); words = [] }
  for (let i = 0; i < cmd.length; i++) {
    const c = cmd[i]
    if (quote === '"' && (c === '`' || (c === '$' && cmd[i + 1] === '('))) {
      // substitution inside double quotes runs a command: judge its text as its own segments
      const end = substitutionEnd(cmd, i)
      segs.push(...segments(cmd.slice(i + (c === '`' ? 1 : 2), end)))
      i = end
      continue
    }
    if (quote) {
      if (c === quote) quote = null
      else if (c === '\\' && quote === '"' && i + 1 < cmd.length) word += cmd[++i]
      else word += c
      continue
    }
    if (c === '$' && cmd[i + 1] === "'") continue // $'..' is quoting, not a variable
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
const WRAPPERS = new Set(['env', 'nohup', 'sudo', 'time', 'exec', 'command', 'nice', 'timeout', 'xargs', 'setsid', 'stdbuf', 'ionice', 'doas', 'unbuffer', 'builtin', 'busybox', 'toybox'])
// Options of each wrapper that take a separate argument word.
const ARG_OPTS = {
  exec: ['-a'], ionice: ['-c', '-n', '-p', '-P', '-u'], stdbuf: ['-i', '-o', '-e'], doas: ['-u', '-C'], setsid: [],
  sudo: ['-u', '-g', '-h', '-p', '-C', '-r', '-t', '-U', '-D', '-R'], env: ['-u', '-C', '-S'],
  timeout: ['-s', '-k'], nice: ['-n'], xargs: ['-n', '-I', '-P', '-L', '-d', '-E', '-s', '-a', '-l', '-i'],
}
const KEYWORDS = new Set(['if', 'then', 'else', 'elif', 'do', 'while', 'until', '!', '{', '}', 'fi', 'done'])
const RAW_RUNNERS = new Set(['test-engine', 'shadow-cljs', 'world-test.mjs', 'test-run.mjs', 'test-shards.mjs'])

// Drop leading VAR=x, shell keywords and wrappers (with their options) so the command word comes first.
function strip(words) {
  let i = 0
  while (i < words.length) {
    const w = words[i], b = base(w)
    if (ASSIGN.test(w) || KEYWORDS.has(w)) { i++; continue }
    if (!WRAPPERS.has(b)) break
    i++
    // `command -v/-V` only looks a name up; it runs nothing
    if (b === 'command' && /^-[a-zA-Z]*[vV]/.test(words[i] ?? '')) return []
    while (i < words.length && words[i].startsWith('-')) {
      if (words[i] === '--') { i++; break }
      i += (ARG_OPTS[b] ?? []).includes(words[i]) ? 2 : 1
    }
    if (b === 'timeout') i++ // duration
  }
  return words.slice(i)
}

// relax: judge only pkill/killall (the raw-runner rules are off), at every nesting level.
function blockedWords(words, relax = false) {
  const w = strip(words)
  if (!w.length) return false
  const cmd = base(w[0]), args = w.slice(1)
  if (cmd === 'pkill' || cmd === 'killall') return true
  if (cmd === 'eval') return blockedCommand(args.join(' '), relax)
  if (RAW_RUNNERS.has(cmd)) return !relax
  if (cmd === 'find') {
    const i = args.findIndex((a) => ['-exec', '-execdir', '-ok', '-okdir'].includes(a))
    return i >= 0 && blockedWords(args.slice(i + 1), relax)
  }
  if (cmd.endsWith('.test.sh')) return !relax
  if (cmd === 'res-slot') {
    const i = args.indexOf('--')
    return i >= 0 && blockedWords(args.slice(i + 1), relax)
  }
  if (cmd === 'node') {
    const si = args.findIndex((a) => !a.startsWith('-'))
    const flags = si < 0 ? args : args.slice(0, si) // later words are the script's own args
    if (flags.some((a) => a.startsWith('--test'))) return !relax
    const script = args[si]
    return !relax && !!script && RAW_RUNNERS.has(base(script))
  }
  if (['npx', 'pnpx', 'bunx', 'yarn', 'pnpm', 'npm'].includes(cmd)) {
    if (args.includes('shadow-cljs')) return !relax
    if (cmd === 'npx' || cmd === 'pnpx' || cmd === 'bunx') return blockedWords(args.filter((a) => !a.startsWith('-')), relax)
    const i = args.findIndex((a) => a === 'test' || a === 't' || a.startsWith('test:') || a === 'run' || a === 'run-script')
    if (i < 0) return false
    if (args[i].startsWith('run')) return !relax && /^test/.test(args[i + 1] ?? '')
    return !relax
  }
  if (['bash', 'sh', 'zsh'].includes(cmd)) {
    const ci = args.findIndex((a) => /^-[A-Za-z]*c[A-Za-z]*$/.test(a))
    if (ci >= 0 && args[ci + 1] !== undefined) return blockedCommand(args[ci + 1], relax)
    const script = args.find((a) => !a.startsWith('-'))
    return !!script && blockedWords([script], relax)
  }
  return false
}

// run_tests launches each step as `{ (cd <dir> && export LIVE_TESTS_NONCE=<x> ... && <argv> 2>&1 | tee ...`.
// Only the segment right after such an export (after a lone `{` and a cd) is exempt from the raw-runner
// rules, never from pkill/killall (also nested in bash -c, find -exec, ...); a command that merely
// mentions the nonce is judged like any other.
const nonceExport = (w) => w[0] === 'export' && w.some((x) => /^LIVE_TESTS_NONCE=[A-Za-z0-9]+$/.test(x))

function blockedCommand(command, relax = false) {
  const segs = segments(command)
  return segs.some((w, i) => {
    const wrapped = i >= 3 && nonceExport(segs[i - 1]) && segs[i - 2][0] === 'cd' && segs[i - 3].length === 1 && segs[i - 3][0] === '{'
    return blockedWords(w, relax || wrapped)
  })
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
