// Headless pixel regression check of the browser view against a synthetic world (tools/view/fixture.mjs).
//   node tools/view-web-check.mjs [--out dir] [--keep] [--lighting]
// Prints PASS|FAIL per check, saves screenshots to --out, exits 1 if a check fails.
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { parseArgs } from 'node:util'
import { fileURLToPath } from 'node:url'
import { createViewServer } from './view/serve.mjs'
import { FIXTURE, writeFixture, AGENT } from './view/fixture.mjs'
import { withPage, freePort } from './view/headless.mjs'
import { cameraBasis } from './view/web/camera.mjs'
import { faceRegion } from './view/project.mjs'
import { regionStats, luminance } from './view/stats.mjs'
import { decodePng } from '../src/vision/renderer.mjs'

const repo = path.join(path.dirname(fileURLToPath(import.meta.url)), '..')
const { values } = parseArgs({ options: { out: { type: 'string' }, keep: { type: 'boolean', default: false }, lighting: { type: 'boolean', default: false } } })

const WIDTH = 640
const HEIGHT = 360
const FOV = 70
const SETTLE_MS = 1000
const BASE_QUERY = `agent=${AGENT}&radius=1&w=${WIDTH}&h=${HEIGHT}&fov=${FOV}&dist=64`
// the page's overlay and buttons would cover the top of the wall regions
const HIDE_CHROME = "for (const id of ['overlay', 'agents', 'free']) document.getElementById(id)?.style.setProperty('display', 'none', 'important'); true"
const Z_FACE_SHADE = 0.8

const fmt = list => `[${list.map(v => Math.round(v)).join(',')}]`
const lum = stats => luminance(stats.mean)

// alpha-weighted average colour of a texture
const textureAverage = name => {
  const { rgba } = decodePng(fs.readFileSync(path.join(repo, 'textures', `${name}.png`)))
  const sum = [0, 0, 0, 0]
  for (let i = 0; i < rgba.length; i += 4) {
    const a = rgba[i + 3]
    sum[0] += rgba[i] * a; sum[1] += rgba[i + 1] * a; sum[2] += rgba[i + 2] * a; sum[3] += a
  }
  return sum.slice(0, 3).map(v => v / sum[3])
}
const diamondExpected = textureAverage('diamond_ore').map(v => v * Z_FACE_SHADE)

// A check: {name, region, test(stats, all), describe(stats, all)}; `all` = {stats: this run's regions, runs: {run name: regions}}
const TEXTURES = [
  { name: 'diamond textured', region: 'diamond', test: s => s.std > 12, describe: s => `std ${s.std.toFixed(1)} vs > 12` },
  {
    name: 'diamond colour',
    region: 'diamond',
    test: s => s.mean.every((v, i) => Math.abs(v - diamondExpected[i]) <= 35),
    describe: s => `mean ${fmt(s.mean)} vs ${fmt(diamondExpected)} +-35`
  },
  {
    name: 'leaves have holes',
    region: 'leaves',
    test: s => s.fraction(([r, g, b]) => r > 120 && g < 80 && b < 80) > 0.05 && s.fraction(([r, g, b]) => r > 120 && g < 80 && b < 80) < 0.8 && s.fraction(([r, g]) => g > r + 10) > 0.1,
    describe: s => `reddish ${s.fraction(([r, g, b]) => r > 120 && g < 80 && b < 80).toFixed(2)} vs 0.05..0.8, greenish ${s.fraction(([r, g]) => g > r + 10).toFixed(2)} vs > 0.1`
  },
  { name: 'lit stone textured', region: 'lit', test: s => s.std > 8, describe: s => `std ${s.std.toFixed(1)} vs > 8` }
]

// the lighting stage: only run with --lighting
const LIGHTING = [
  {
    name: 'dark stripe darker',
    region: 'dark',
    test: (s, all) => lum(s) < 0.35 * lum(all.stats.lit),
    describe: (s, all) => `luminance ${lum(s).toFixed(1)} vs < ${(0.35 * lum(all.stats.lit)).toFixed(1)} (0.35 * lit)`
  }
]
const NIGHT = [
  {
    name: 'night darker',
    region: 'lit',
    test: (s, all) => lum(s) < 0.5 * lum(all.runs.day.lit),
    describe: (s, all) => `luminance ${lum(s).toFixed(1)} vs < ${(0.5 * lum(all.runs.day.lit)).toFixed(1)} (0.5 * day)`
  },
  { name: 'torch patch warm at night', region: 'torch', test: s => s.mean[0] > s.mean[2] + 10, describe: s => `r ${s.mean[0].toFixed(0)} vs b ${s.mean[2].toFixed(0)} + 10` }
]

// data-driven runs: {name, query, checks}
const RUNS = [
  { name: 'day', query: BASE_QUERY, checks: [...TEXTURES, ...(values.lighting ? LIGHTING : [])] },
  ...(values.lighting ? [{ name: 'night', query: `${BASE_QUERY}&time=18000`, checks: NIGHT }] : [])
]

const startServer = async stateDir => {
  const server = createViewServer({ stateDir, textureDir: path.join(repo, 'textures'), webDir: path.join(repo, 'tools', 'view', 'web') })
  const port = await freePort()
  await new Promise(resolve => server.listen(port, '127.0.0.1', resolve))
  return { server, port }
}

const regionsOf = () => {
  const basis = cameraBasis({ yaw: FIXTURE.yaw, pitch: FIXTURE.pitch, fov: FOV })
  return Object.fromEntries(FIXTURE.regions.map(({ name, face }) => [name, faceRegion(basis, FIXTURE.eye, face, WIDTH, HEIGHT)]))
}

const shoot = ({ port, run, outDir }) => withPage({ url: `http://127.0.0.1:${port}/?${run.query}`, width: WIDTH, height: HEIGHT }, async page => {
  await page.waitUntil('window.__view?.ready === true', 'window.__view.ready')
  await page.evaluate(HIDE_CHROME)
  await page.sleep(SETTLE_MS)
  const png = await page.screenshot()
  fs.writeFileSync(path.join(outDir, `${run.name}.png`), png)
  return { image: decodePng(png), logs: page.logs }
})

const main = async () => {
  const outDir = values.out ? path.resolve(values.out) : fs.mkdtempSync(path.join(os.tmpdir(), 'view-check-'))
  fs.mkdirSync(outDir, { recursive: true })
  const stateDir = path.join(outDir, 'state')
  fs.rmSync(stateDir, { recursive: true, force: true })
  writeFixture(stateDir)
  const { server, port } = await startServer(stateDir)
  const regions = regionsOf()
  const runs = {}
  let failures = 0
  try {
    for (const run of RUNS) {
      const { image, logs } = await shoot({ port, run, outDir })
      const stats = Object.fromEntries(Object.entries(regions).map(([name, region]) => [name, regionStats(image, region)]))
      runs[run.name] = stats
      for (const check of run.checks) {
        const ok = check.test(stats[check.region], { stats, runs })
        if (!ok) failures++
        console.log(`${ok ? 'PASS' : 'FAIL'} ${run.name}/${check.name}: ${check.describe(stats[check.region], { stats, runs })}`)
      }
      for (const line of logs.slice(0, 5)) console.log(`  console (${run.name}): ${line}`)
    }
  } finally {
    server.close()
    server.closeAllConnections?.()
    if (!values.keep && !values.out) fs.rmSync(outDir, { recursive: true, force: true })
  }
  console.log(values.keep || values.out ? `screenshots: ${outDir}` : '(use --out dir or --keep to keep screenshots)')
  process.exit(failures ? 1 : 0)
}

main().catch(error => {
  console.error(error)
  process.exit(1)
})
