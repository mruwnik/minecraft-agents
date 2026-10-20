// Why JavaScript: times one full sight pass of kernels alone over the bench worlds, interleaved, min of many passes (node engine/js/sight-kernel-bench.mjs [kernel paths...]; give two paths, e.g. a saved copy of an older kernel, for an A/B).
import { gridDirs, makeRig, makeRaw, groundAt } from './sight-kernel-rig.mjs'

const paths = process.argv.length > 2 ? process.argv.slice(2) : ['./sight-kernel.mjs']
const kernels = await Promise.all(paths.map(async p => (await import(p)).castRays))
const reps = Number(process.env.REPS ?? 40)
for (const kind of ['hills', 'cave', 'pen']) {
  const { raw, bot } = makeRaw(kind)
  const rigs = paths.map(p => makeRig(raw))
  const pos = bot.entity.position
  const views = Array.from({ length: 8 }, (_, i) => {
    const ox = pos.x + i; const oz = pos.z
    return [ox, groundAt(kind, ox, oz) + 2.62, oz, gridDirs(i * 0.8, 0)]
  })
  const best = kernels.map(() => Infinity)
  let cells = 0
  for (let rep = 0; rep < reps; rep++) {
    kernels.forEach((k, j) => {
      let ms = 0
      cells = 0
      for (const [ox, oy, oz, dirs] of views) {
        if (!process.env.WARM || rep === 0) rigs[j].fresh()
        const t0 = process.cpuUsage()
        cells += k(rigs[j].ctx, ox, oy, oz, dirs, dirs.length / 3)
        const u = process.cpuUsage(t0)
        ms += (u.user + u.system) / 1000
      }
      if (rep >= (process.env.WARM ? 1 : 3)) best[j] = Math.min(best[j], ms / views.length)
    })
  }
  console.log(kind.padEnd(6), 'cells/pass', Math.round(cells / views.length), 'records', rigs.map(r => r.records.length / 4).join('/'), 'ms/pass', best.map(b => b.toFixed(2)).join(' '))
}
