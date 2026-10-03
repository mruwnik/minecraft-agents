// Headless pixel regression check of the browser view against a synthetic world (tools/view/fixture.mjs).
//   node tools/view-web-check.mjs [--out dir] [--keep] [--lighting] [--ghost]
// --ghost: the no-ghost check (tools/view/ghost-fixture.mjs): jump between two places that share window slots and compare with fresh loads.
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
import { PLACES, AGENT as GHOST_AGENT, writeGhostWorld, writePose } from './view/ghost-fixture.mjs'
import { decodePng, encodePng } from '../src/vision/renderer.mjs'

const repo = path.join(path.dirname(fileURLToPath(import.meta.url)), '..')
const { values } = parseArgs({ options: { out: { type: 'string' }, keep: { type: 'boolean', default: false }, lighting: { type: 'boolean', default: false }, ghost: { type: 'boolean', default: false } } })

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

const isRed = ([r, g, b]) => r > 70 && g < 50 && b < 50 // the shaded, AO-darkened wool
const isWhite = ([r, g, b]) => r > 100 && g > 100 && b > 100 && Math.abs(r - b) < 25
const isGold = ([r, g, b]) => r > 110 && g > 90 && b < 80

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
  { name: 'lit stone textured', region: 'lit', test: s => s.std > 4, describe: s => `std ${s.std.toFixed(1)} vs > 4` },
  // partial blocks are boxes of the right size: the rays above a bottom slab / a two-layer snow reach the block behind
  { name: 'slab lower half is planks', region: 'slab lower', test: s => s.mean[0] > s.mean[2] + 30, describe: s => `r ${s.mean[0].toFixed(0)} vs b ${s.mean[2].toFixed(0)} + 30` },
  { name: 'above the slab shows what is behind', region: 'slab upper', test: s => s.fraction(isRed) > 0.6, describe: s => `red ${s.fraction(isRed).toFixed(2)} vs > 0.6 (mean ${fmt(s.mean)})` },
  { name: 'snow lower quarter is white', region: 'snow lower', test: s => s.fraction(isWhite) > 0.6, describe: s => `white ${s.fraction(isWhite).toFixed(2)} vs > 0.6 (mean ${fmt(s.mean)})` },
  { name: 'snow is low', region: 'snow upper', test: s => s.fraction(isGold) > 0.6, describe: s => `gold ${s.fraction(isGold).toFixed(2)} vs > 0.6 (mean ${fmt(s.mean)})` }
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

// ---- no-ghost check ----

const GHOST_MEAN_MAX = 0.5
const GHOST_DIFF_LEVEL = 8
const GHOST_FRACTION_MAX = 0.001
const GHOST_QUERY = `agent=${GHOST_AGENT}&radius=1&w=${WIDTH}&h=${HEIGHT}&fov=${FOV}&dist=64&interp=0&time=6000`

// the page has drawn the pose at `place` with every wanted column loaded
const arrivedAt = place => `(() => { const t = window.__view?.camTrace.at(-1); return !!t && Math.abs(t.x - ${place.cx * 16 + 8}) < 1 && Math.abs(t.z - ${place.cz * 16 + 8}) < 1 && window.__view.ready === true })()`

const snapshot = async (page, name, outDir) => {
  await page.evaluate(HIDE_CHROME)
  await page.sleep(SETTLE_MS)
  const png = await page.screenshot()
  fs.writeFileSync(path.join(outDir, `ghost-${name}.png`), png)
  return decodePng(png)
}

const fresh = (port, stateDir, place, name, outDir) => {
  writePose(stateDir, place)
  return withPage({ url: `http://127.0.0.1:${port}/?${GHOST_QUERY}`, width: WIDTH, height: HEIGHT }, async page => {
    await page.waitUntil(arrivedAt(place), `a fresh load at ${name}`)
    return snapshot(page, name, outDir)
  })
}

const compareImages = (a, b) => {
  const pixels = a.width * a.height
  let sum = 0
  let over = 0
  const diff = new Uint8Array(pixels * 4)
  for (let i = 0; i < pixels; i++) {
    const d = Math.max(...[0, 1, 2].map(c => Math.abs(a.rgba[i * 4 + c] - b.rgba[i * 4 + c])))
    sum += d
    over += d > GHOST_DIFF_LEVEL ? 1 : 0
    diff.set([Math.min(255, d * 8), Math.min(255, d * 8), Math.min(255, d * 8), 255], i * 4)
  }
  return { mean: sum / pixels, fraction: over / pixels, diff }
}

const ghostCheck = async outDir => {
  const stateDir = path.join(outDir, 'ghost-state')
  fs.rmSync(stateDir, { recursive: true, force: true })
  writeGhostWorld(stateDir, PLACES.A)
  const { server, port } = await startServer(stateDir)
  let failures = 0
  try {
    const jumps = await withPage({ url: `http://127.0.0.1:${port}/?${GHOST_QUERY}`, width: WIDTH, height: HEIGHT }, async page => {
      await page.waitUntil(arrivedAt(PLACES.A), 'the first load at A')
      writePose(stateDir, PLACES.B)
      await page.waitUntil(arrivedAt(PLACES.B), 'the jump to B')
      const b1 = await snapshot(page, 'B1', outDir)
      writePose(stateDir, PLACES.A)
      await page.waitUntil(arrivedAt(PLACES.A), 'the jump back to A')
      return { B1: b1, A2: await snapshot(page, 'A2', outDir), logs: page.logs }
    })
    const references = { B1: await fresh(port, stateDir, PLACES.B, 'B0', outDir), A2: await fresh(port, stateDir, PLACES.A, 'A0', outDir) }
    for (const [name, reference] of Object.entries(references)) {
      const { mean, fraction, diff } = compareImages(jumps[name], reference)
      fs.writeFileSync(path.join(outDir, `ghost-diff-${name}.png`), encodePng(WIDTH, HEIGHT, diff))
      const ok = mean <= GHOST_MEAN_MAX && fraction < GHOST_FRACTION_MAX
      if (!ok) failures++
      console.log(`${ok ? 'PASS' : 'FAIL'} ghost/${name} equals a fresh load: mean abs diff ${mean.toFixed(3)} vs <= ${GHOST_MEAN_MAX}, ${(fraction * 100).toFixed(3)}% pixels over ${GHOST_DIFF_LEVEL} vs < ${GHOST_FRACTION_MAX * 100}%`)
    }
    for (const line of jumps.logs.slice(0, 5)) console.log(`  console (ghost): ${line}`)
  } finally {
    server.close()
    server.closeAllConnections?.()
  }
  return failures
}

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
    if (values.ghost) failures += await ghostCheck(outDir)
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
