// Why JavaScript: dev script comparing two bench result files from bench.mjs; no engine behaviour.
// Compare two bench result files per query set: status counts, timing, disagreements and path lengths.
//   node engine/js/path/bench-compare.mjs a-results.json b-results.json
import fs from 'node:fs'
import { pathToFileURL } from 'node:url'

const percentile = (xs, p) => [...xs].sort((a, b) => a - b)[Math.min(xs.length - 1, Math.floor(p * xs.length))]
const median = xs => percentile(xs, 0.5)
const read = file => JSON.parse(fs.readFileSync(file, 'utf8'))
const counts = rs => rs.reduce((c, r) => ({ ...c, [r.status]: (c[r.status] ?? 0) + 1 }), {})
const timing = rs => {
  const ms = rs.map(r => r.ms)
  return `ms p50=${percentile(ms, 0.5).toFixed(1)} p90=${percentile(ms, 0.9).toFixed(1)} max=${Math.max(...ms).toFixed(1)}`
}
const label = r => `${r.status}${r.reason ? `(${r.reason})` : ''}`

export function compare (a, b) {
  const byId = new Map(b.map(r => [r.id, r]))
  const pairs = a.filter(r => byId.has(r.id)).map(r => [r, byId.get(r.id)])
  const sets = Map.groupBy(pairs, ([r]) => r.set)
  const lines = [...sets].flatMap(([name, ps]) => [
    `${name} (n=${ps.length})`,
    `  A ${JSON.stringify(counts(ps.map(p => p[0])))} ${timing(ps.map(p => p[0]))}`,
    `  B ${JSON.stringify(counts(ps.map(p => p[1])))} ${timing(ps.map(p => p[1]))}`
  ])
  const differ = pairs.filter(([x, y]) => (x.status === 'success') !== (y.status === 'success'))
  lines.push(`success differs on ${differ.length} queries:`)
  differ.forEach(([x, y]) => lines.push(`  ${x.id}: A ${label(x)} len=${x.pathLength}, B ${label(y)} len=${y.pathLength}`))
  const both = pairs.filter(([x, y]) => x.status === 'success' && y.status === 'success' && x.pathLength > 0)
  lines.push(`both solve ${both.length}; median path steps B/A = ${both.length ? median(both.map(([x, y]) => y.pathLength / x.pathLength)).toFixed(3) : 'n/a'}`)
  return lines.join('\n')
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  const [fileA, fileB] = process.argv.slice(2)
  console.log(compare(read(fileA), read(fileB)))
}
