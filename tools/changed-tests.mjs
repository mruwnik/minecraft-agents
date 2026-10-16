#!/usr/bin/env node
// Why JavaScript: a Node tool run by tools/test-engine --changed (a shell wrapper); reads source text and git output only, no engine behaviour.
// Usage: node tools/changed-tests.mjs [<rev>|<a>..<b>|--since-green] [--suites]; --record-green [<rev>] writes .claude/integration-last-green (rev + time) after a green run, --since-green diffs <that rev>..HEAD (FULL when absent or not an ancestor)   -> prints "FULL <reason>" or lines "cljs <ns>" / "js <path>" / "tools <path>" (paths relative to the repo root); --suites prints "<run_tests suite> <args...>" lines instead.
// Changed = <rev> vs the working tree (+ untracked); <a>..<b> = committed files only; no arg = HEAD, or the cut of a fix/* branch. Over 60 namespaces it warns on stderr with the widest files. The require graph is computed from the ns forms at run time.
import fs from 'node:fs'
import path from 'node:path'
import { execFileSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'

// jobs, triggers and hooks are loaded by directory, not required: these namespaces see all of them
const DISCOVERED = /^engine\/src\/(?:jobs|triggers)\//
const SEES_ALL = /^engine\.(?:registry|job-fuzz|job-arg-types|settings|settings-triggers|scenario-register)-test$|-contract-test$/
const FULL_FILES = ['dashboard/shadow-cljs.edn', 'engine/package.json', 'engine/package-lock.json', 'engine/deps.edn']
const CLJ = /\.clj[sc]?$/
const isTestNs = (n) => /-test$/.test(n)

const stripNs = (src) => src.replace(/;[^\n]*/g, '').replace(/"(?:[^"\\]|\\.)*"/g, '""')
const nsForm = (src) => {
  const i = src.indexOf('(ns ')
  if (i < 0) return null
  let depth = 0
  for (let j = i; j < src.length; j++) {
    if (src[j] === '(') depth++
    else if (src[j] === ')' && --depth === 0) return stripNs(src.slice(i, j + 1))
  }
  return null
}
const nsName = (form) => form.match(/\(ns\s+(?:\^\{[^}]*\}\s+|\^:\S+\s+)*([^\s()\[\]]+)/)?.[1]
const symbols = (text) => text.match(/(?<![:\w.\-/])[a-z][\w\-]*(?:\.[\w\-]+)+/g) ?? []
const nsFromPath = (p) => p.replace(/^engine\/(?:src|test)\//, '').replace(CLJ, '').replace(/\//g, '.').replace(/_/g, '-')

const reverseClosure = (edges, seeds) => { // edges: Map from -> Set of deps; returns every node that reaches a seed
  const rev = new Map()
  for (const [from, deps] of edges) for (const d of deps) (rev.get(d) ?? rev.set(d, new Set()).get(d)).add(from)
  const seen = new Set(seeds), todo = [...seeds]
  while (todo.length) for (const f of rev.get(todo.pop()) ?? []) if (!seen.has(f)) { seen.add(f); todo.push(f) }
  return seen
}

// -> {tests: [ns], golden: whether a planner golden namespace is reached}
const cljsReach = (files, changed) => {
  const paths = Object.keys(files).filter((p) => CLJ.test(p) && p.startsWith('engine/'))
  const ofPath = new Map(), forms = new Map()
  for (const p of paths) {
    const form = nsForm(files[p])
    const name = form && nsName(form)
    if (!name) continue
    ofPath.set(p, name); forms.set(name, [...(forms.get(name) ?? []), form])
  }
  const known = new Set(forms.keys())
  for (const c of changed) if (CLJ.test(c) && c.startsWith('engine/') && !ofPath.has(c)) known.add(nsFromPath(c)) // deleted or renamed away: still a dependency name
  const edges = new Map()
  for (const [name, fs_] of forms) edges.set(name, new Set(fs_.flatMap((f) => symbols(f.replace(/^\(ns\s+\S+/, ''))).filter((s) => known.has(s) && s !== name)))
  // tests also name jobs by quoted symbol without requiring them: count any mention in a test file as a dependency
  for (const p of paths) {
    const name = ofPath.get(p)
    if (!name || !p.startsWith('engine/test/')) continue
    for (const s of symbols(files[p])) if (known.has(s) && s !== name) edges.get(name).add(s)
  }
  const seeds = new Set()
  for (const c of changed) {
    if (CLJ.test(c) && c.startsWith('engine/')) { seeds.add(ofPath.get(c) ?? nsFromPath(c)); continue }
    if (!c.startsWith('engine/') || /\.(mjs|js|cjs|json|md)$/.test(c) || /^engine\/(?:out|node_modules)\//.test(c)) continue
    const stem = path.basename(c).replace(/\.[^.]*$/, '')
    const re = new RegExp(`(?<![\\w-])${stem.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}(?![\\w-])`)
    for (const p of paths) if (ofPath.has(p) && p.startsWith('engine/test/') && re.test(files[p])) seeds.add(ofPath.get(p))
  }
  const reached = [...reverseClosure(edges, seeds)].filter((n) => forms.has(n))
  const always = changed.some((c) => DISCOVERED.test(c)) ? [...forms.keys()].filter((n) => SEES_ALL.test(n)) : []
  return { tests: [...new Set([...reached.filter(isTestNs), ...always])].sort(), golden: reached.some((n) => /-golden$/.test(n)) }
}
const cljsSelect = (files, changed) => cljsReach(files, changed).tests

const jsSelect = (files, changed, prefix) => {
  const js = Object.keys(files).filter((p) => /\.mjs$/.test(p) && p.startsWith(prefix))
  const edges = new Map()
  for (const p of js) {
    const deps = new Set()
    for (const m of files[p].matchAll(/(?:from\s*|import\s*\(?\s*)['"](\.[^'"]+)['"]/g)) deps.add(path.posix.join(path.posix.dirname(p), m[1]))
    edges.set(p, deps)
  }
  // extension-less tools (tools/commit-mine) are covered by tools/<name>.test.mjs
  const seeds = changed.map((c) => c.startsWith(prefix) && !/\.\w+$/.test(c) ? `${c}.mjs` : c).filter((c) => /\.mjs$/.test(c) && c.startsWith(prefix))
  return [...reverseClosure(edges, seeds)].filter((p) => /\.test\.mjs$/.test(p) && p in files).sort()
}

// files: {repo-relative path: text}; changed: repo-relative paths. -> {full: reason|null, cljs: [ns], js: [path]}
export const selectTests = (files, changed) => {
  const fullHit = changed.find((c) => FULL_FILES.includes(c))
  if (fullHit) return { full: `${fullHit} changed`, cljs: [], js: [], tools: [], suites: [], fixtures: [] }
  const { tests, golden } = cljsReach(files, changed)
  const suites = [
    ...(changed.some((c) => DISCOVERED.test(c)) ? ['fuzz'] : []),
    ...(changed.some((c) => /^dashboard\//.test(c) && !/^dashboard\/(?:node_modules|out)\//.test(c)) ? ['dashboard'] : []),
    ...(golden ? ['golden'] : []),
  ]
  const fixtures = changed.filter((c) => /^engine\/fixtures\/world\/.*\.edn$/.test(c))
  return { full: null, fixtures, cljs: tests, js: jsSelect(files, changed, 'engine/'), tools: jsSelect(files, changed, 'tools/'), suites }
}

// each changed file with the number of engine test namespaces it selects alone, widest first
export const fanout = (files, changed) =>
  changed.map((file) => ({ file, tests: cljsSelect(files, [file]).length })).filter((f) => f.tests > 0).sort((a, b) => b.tests - a.tests || a.file.localeCompare(b.file))

// run_tests suite -> args
export const suiteArgs = (r) => ({
  ...Object.fromEntries(Object.entries({ engine: r.cljs, 'engine-js': r.js, tools: r.tools }).filter(([, v]) => v.length)),
  ...(r.fixtures.length ? { fixtures: r.fixtures } : {}),
  ...Object.fromEntries(r.suites.map((s) => [s, []])), // whole suites take no args
})

// "a..b": committed files between two revs; "a": a vs the working tree
export const rangeOf = (arg) => {
  const [from, to] = arg.split('..')
  return { from, to: to ?? null }
}


export const formatGreen = (rev, date) => `${rev} ${date.toISOString()}\n`
export const parseGreen = (text) => {
  const m = text.match(/^([0-9a-f]{7,40}) (\S+)\s*$/)
  return m ? { rev: m[1], time: m[2] } : null
}

const git = (repo, ...args) => execFileSync('git', args, { cwd: repo, encoding: 'utf8', maxBuffer: 1 << 28 }).split('\n').filter(Boolean)
const walk = (dir) => fs.readdirSync(dir, { withFileTypes: true }).flatMap((e) => {
  if (e.name === 'node_modules' || e.name === 'out' || e.name === '.shadow-cljs') return []
  const p = path.join(dir, e.name)
  return e.isDirectory() ? walk(p) : /\.(clj[sc]?|mjs)$/.test(e.name) ? [p] : []
})

export const changedFiles = (repo, arg = 'HEAD') => {
  const { from, to } = rangeOf(arg)
  if (to) return git(repo, 'diff', '--name-only', from, to, '--').sort()
  return [...new Set([...git(repo, 'diff', '--name-only', from, '--'), ...git(repo, 'ls-files', '--others', '--exclude-standard')])].sort()
}

// A fixer branch (fix/*) is cut from job-contract or engine-mvp: default to everything since the cut, committed or not.
const defaultBase = (repo) => {
  const branch = git(repo, 'rev-parse', '--abbrev-ref', 'HEAD')[0]
  if (!branch?.startsWith('fix/')) return 'HEAD'
  for (const b of ['job-contract', 'engine-mvp']) {
    try { return git(repo, 'merge-base', 'HEAD', b)[0] } catch { /* branch absent */ }
  }
  return 'HEAD'
}

const WIDE = 60

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const repo = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..')
  const args = process.argv.slice(2)
  const asSuites = args.includes('--suites')
  const greenFile = path.join(repo, '.claude/integration-last-green')
  if (args.includes('--record-green')) { // after a green integration run of <rev> (default HEAD)
    const rev = git(repo, 'rev-parse', args.find((a) => !a.startsWith('--')) ?? 'HEAD')[0]
    fs.writeFileSync(greenFile, formatGreen(rev, new Date()))
    console.log(`recorded ${rev}`)
    process.exit(0)
  }
  let rev = args.find((a) => !a.startsWith('--')) ?? defaultBase(repo)
  if (args.includes('--since-green')) { // <last-green>..HEAD, or FULL when there is no usable record
    const green = fs.existsSync(greenFile) ? parseGreen(fs.readFileSync(greenFile, 'utf8')) : null
    const ok = green && (() => { try { git(repo, 'merge-base', '--is-ancestor', green.rev, 'HEAD'); return true } catch { return false } })()
    if (!ok) { console.log('FULL no usable .claude/integration-last-green'); process.exit(0) }
    rev = `${green.rev}..HEAD`
  }
  const changed = changedFiles(repo, rev).filter((c) => /^(engine|tools|dashboard)\//.test(c))
  const files = {}
  for (const d of ['engine', 'tools']) for (const f of walk(path.join(repo, d))) files[path.relative(repo, f)] = fs.readFileSync(f, 'utf8')
  const r = selectTests(files, changed)
  if (r.full) console.log(`FULL ${r.full}`)
  else if (asSuites) for (const [suite, a] of Object.entries(suiteArgs(r))) console.log(`${suite} ${a.join(' ')}`)
  else {
    for (const n of r.cljs) console.log(`cljs ${n}`)
    for (const f of r.js) console.log(`js ${f}`)
    for (const f of r.tools) console.log(`tools ${f}`)
    for (const s of r.suites) console.log(`suite ${s}`)
  }
  if (r.cljs.length > WIDE) {
    const top = fanout(files, changed).slice(0, 5).map((f) => `${f.file} (${f.tests})`).join(', ')
    console.error(`WARNING: ${r.cljs.length} test namespaces selected (over ${WIDE}); pick a subset. Widest changed files: ${top}`)
  }
}
