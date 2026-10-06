#!/usr/bin/env node
// Why JavaScript: a Node tool run by tools/test-engine --changed (a shell wrapper); reads source text and git output only, no engine behaviour.
// Usage: node tools/changed-tests.mjs [<git-rev>]   -> prints "FULL <reason>" or lines "cljs <ns>" / "js <path>" (paths relative to the repo root).
// Changed files = working tree + staged + untracked vs HEAD (or vs <git-rev>). The require graph is computed from the ns forms at run time.
import fs from 'node:fs'
import path from 'node:path'
import { execFileSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'

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

const cljsSelect = (files, changed) => {
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
    for (const p of paths) if (ofPath.has(p) && re.test(files[p])) seeds.add(ofPath.get(p))
  }
  return [...reverseClosure(edges, seeds)].filter((n) => isTestNs(n) && forms.has(n)).sort()
}

const jsSelect = (files, changed) => {
  const js = Object.keys(files).filter((p) => /\.mjs$/.test(p) && p.startsWith('engine/'))
  const edges = new Map()
  for (const p of js) {
    const deps = new Set()
    for (const m of files[p].matchAll(/(?:from\s*|import\s*\(?\s*)['"](\.[^'"]+)['"]/g)) deps.add(path.posix.join(path.posix.dirname(p), m[1]))
    edges.set(p, deps)
  }
  const seeds = changed.filter((c) => /\.mjs$/.test(c) && c.startsWith('engine/'))
  return [...reverseClosure(edges, seeds)].filter((p) => /\.test\.mjs$/.test(p) && p in files).sort()
}

// files: {repo-relative path: text}; changed: repo-relative paths. -> {full: reason|null, cljs: [ns], js: [path]}
export const selectTests = (files, changed) => {
  const fullHit = changed.find((c) => FULL_FILES.includes(c))
  if (fullHit) return { full: `${fullHit} changed`, cljs: [], js: [] }
  return { full: null, cljs: cljsSelect(files, changed), js: jsSelect(files, changed) }
}

const git = (repo, ...args) => execFileSync('git', args, { cwd: repo, encoding: 'utf8', maxBuffer: 1 << 28 }).split('\n').filter(Boolean)
const walk = (dir) => fs.readdirSync(dir, { withFileTypes: true }).flatMap((e) => {
  if (e.name === 'node_modules' || e.name === 'out' || e.name === '.shadow-cljs') return []
  const p = path.join(dir, e.name)
  return e.isDirectory() ? walk(p) : /\.(clj[sc]?|mjs)$/.test(e.name) ? [p] : []
})

export const changedFiles = (repo, rev = 'HEAD') =>
  [...new Set([...git(repo, 'diff', '--name-only', rev, '--'), ...git(repo, 'ls-files', '--others', '--exclude-standard')])].sort()

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const repo = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..')
  const changed = changedFiles(repo, process.argv[2] ?? 'HEAD').filter((c) => c.startsWith('engine/'))
  const files = {}
  for (const f of walk(path.join(repo, 'engine'))) files[path.relative(repo, f)] = fs.readFileSync(f, 'utf8')
  const r = selectTests(files, changed)
  if (r.full) console.log(`FULL ${r.full}`)
  else {
    for (const n of r.cljs) console.log(`cljs ${n}`)
    for (const f of r.js) console.log(`js ${f}`)
  }
}
