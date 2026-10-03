// The language benchmark: the JS planner and its ClojureScript ports plan the same courses in one node process, the
// versions interleaved plan by plan (their order rotates with every course and round) so machine noise spreads evenly.
// Each plan is timed from outside, setup and result building included. Warm-up rounds are discarded.
//   cd engine && PLANNER_BENCH_DIR=<frozen dir> node bench-lang/bench.mjs [--rounds 30] [--warmup 5] [--out results.json]
//     [--versions js,cljs-tuned-dev] [--max-load 8]
// --max-load waits for the 1-minute load average to drop below it before starting.
import fs from 'node:fs'
import os from 'node:os'
import { performance } from 'node:perf_hooks'
import { setTimeout as sleep } from 'node:timers/promises'
import { pathToFileURL } from 'node:url'
import { loadCourses } from './courses.mjs'
import { loadVersions } from './versions.mjs'

const GROUPS = ['all', 'world', 'course']
const LOAD_POLL_MS = 15000
const LOAD_WAIT_MS = 30 * 60 * 1000

// linear interpolation between the two nearest ranks (the usual median for q = 0.5)
export const quantile = (xs, q) => {
  const sorted = [...xs].sort((a, b) => a - b)
  const at = q * (sorted.length - 1)
  const low = Math.floor(at)
  const high = Math.min(sorted.length - 1, low + 1)
  return sorted[low] + (sorted[high] - sorted[low]) * (at - low)
}

export const rotated = (items, k) => items.map((_, i) => items[(i + k) % items.length])

const sum = xs => xs.reduce((a, b) => a + b, 0)
const geomean = xs => Math.exp(sum(xs.map(Math.log)) / xs.length)

// { all, world, course }: per group one row per version. samples[version][course] holds the ms of each timed round.
//   medianMs, p95Ms: over every timed plan of the group; sumMs: the sum of the per-course medians (one pass over the set);
//   nodesPerSec: nodes expanded in one pass / sumMs; ratio: sumMs / JS's; geomeanRatio: over courses, of median / JS's median
export function summarize ({ versions, courses, samples }) {
  const rows = group => {
    const picked = courses.map((c, k) => k).filter(k => group === 'all' || courses[k].group === group)
    const expanded = sum(picked.map(k => courses[k].expanded))
    const medians = versions.map((_, v) => picked.map(k => quantile(samples[v][k], 0.5)))
    return versions.map((name, v) => {
      const pooled = picked.flatMap(k => [...samples[v][k]])
      const sumMs = sum(medians[v])
      return {
        name,
        plans: pooled.length,
        medianMs: quantile(pooled, 0.5),
        p95Ms: quantile(pooled, 0.95),
        sumMs,
        nodesPerSec: expanded / (sumMs / 1000),
        ratio: sumMs / sum(medians[0]),
        geomeanRatio: geomean(medians[v].map((m, i) => m / medians[0][i]).filter(r => r > 0 && Number.isFinite(r)))
      }
    })
  }
  return Object.fromEntries(GROUPS.map(group => [group, rows(group)]))
}

const flagValue = (argv, flag) => argv.includes(flag) ? argv[argv.indexOf(flag) + 1] : undefined

export const parseArgs = argv => ({
  rounds: Number(flagValue(argv, '--rounds') ?? 30),
  warmup: Number(flagValue(argv, '--warmup') ?? 5),
  out: flagValue(argv, '--out'),
  versions: flagValue(argv, '--versions')?.split(','),
  maxLoad: Number(flagValue(argv, '--max-load') ?? Infinity)
})

const formatTable = (group, rows) => [
  `${group} (${rows[0].plans} timed plans per version)`,
  '  version              median ms    p95 ms   set ms   nodes/s   ratio to JS   geomean ratio',
  ...rows.map(r => `  ${r.name.padEnd(20)} ${r.medianMs.toFixed(3).padStart(9)} ${r.p95Ms.toFixed(2).padStart(9)} ${r.sumMs.toFixed(0).padStart(8)} ${Math.round(r.nodesPerSec).toString().padStart(9)} ${r.ratio.toFixed(3).padStart(13)} ${r.geomeanRatio.toFixed(3).padStart(15)}`)
].join('\n')

const load = () => os.loadavg().map(l => Number(l.toFixed(2)))

async function waitForLoad (maxLoad, log) {
  const t0 = Date.now()
  while (os.loadavg()[0] >= maxLoad && Date.now() - t0 < LOAD_WAIT_MS) {
    log(`load ${load().join(' ')} is not below ${maxLoad}: waiting`)
    await sleep(LOAD_POLL_MS)
  }
}

// times every version on every course, `warmup + rounds` times over; returns the timed samples and the loads seen
export function measure ({ versions, courses, rounds, warmup, log = () => {} }) {
  const samples = versions.map(() => courses.map(() => new Array(rounds).fill(0)))
  const order = versions.map((_, v) => v)
  const loads = []
  for (let round = 0; round < warmup + rounds; round++) {
    const t0 = performance.now()
    courses.forEach((course, k) => {
      for (const v of rotated(order, round + k)) {
        const start = performance.now()
        versions[v].plan(course.snapshot, course.query)
        const ms = performance.now() - start
        if (round >= warmup) samples[v][k][round - warmup] = ms
      }
    })
    loads.push(load())
    log(`round ${round + 1}/${warmup + rounds}${round < warmup ? ' (warm-up)' : ''}: ${((performance.now() - t0) / 1000).toFixed(1)} s, load ${load().join(' ')}`)
  }
  return { samples, loads }
}

async function main (argv) {
  const { rounds, warmup, out, versions: wanted, maxLoad } = parseArgs(argv)
  const log = line => console.error(line)
  const all = loadVersions()
  const versions = wanted ? all.filter(v => wanted.includes(v.name)) : all
  if (versions[0]?.name !== 'js') throw new Error('the JS planner must be among the versions: ratios are to it')
  const loaded = loadCourses()
  const courses = loaded.map(c => ({ id: c.id, group: c.group, expanded: versions[0].plan(c.snapshot, c.query).expanded }))
  await waitForLoad(maxLoad, log)
  const startedAt = new Date().toISOString()
  const loadStart = load()
  const { samples, loads } = measure({ versions, courses: loaded, rounds, warmup, log })
  const names = versions.map(v => v.name)
  const summary = summarize({ versions: names, courses, samples })
  const header = {
    node: process.version, cpus: os.cpus().length, cpu: os.cpus()[0].model, rounds, warmup, courses: courses.length,
    startedAt, endedAt: new Date().toISOString(), loadStart, loadEnd: load(),
    loadMax1m: Math.max(...loads.map(l => l[0])), loadMin1m: Math.min(...loads.map(l => l[0]))
  }
  console.log(JSON.stringify(header))
  GROUPS.forEach(group => console.log(formatTable(group, summary[group])))
  if (out) fs.writeFileSync(out, JSON.stringify({ header, summary, versions: names, courses, loads, samples }))
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) await main(process.argv.slice(2))
