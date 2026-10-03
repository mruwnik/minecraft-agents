// Headless pixel regression check of the browser view against a synthetic world (tools/view/fixture.mjs).
//   node tools/view-web-check.mjs [--out dir] [--keep] [--lighting] [--ghost] [--models] [--entities] [--web dir]
// --web: serve the page from this directory instead of tools/view/web (e.g. a copy with the shader changed, to prove a check can fail)
// --ghost: the no-ghost check (tools/view/ghost-fixture.mjs): jump between two places that share window slots and compare with fresh loads.
// --models: the jar-modelled blocks (tools/view/model-fixture.mjs). Each check must pass with the client jar and FAIL on the old renderer (the
//   same page served with no jar: `counter` lines); a cubes-only world must render the same either way (the fast path is untouched).
// --entities: the block-entity models (tools/view/entity-fixture.mjs): per type, regions that land in the wrong colour if a part is a sixteenth or two
//   off; each type must pass with the jar and FAIL on the old renderer (a `counter` line per type).
// Prints PASS|FAIL per check, saves screenshots to --out, exits 1 if a check fails.
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { parseArgs } from 'node:util'
import { fileURLToPath } from 'node:url'
import { createViewServer } from './view/serve.mjs'
import { biomeTable } from './view/biome-colors.mjs'
import { FIXTURE, writeFixture, AGENT } from './view/fixture.mjs'
import { withPage, freePort } from './view/headless.mjs'
import { cameraBasis } from './view/web/camera.mjs'
import { faceRegion } from './view/project.mjs'
import { regionStats, luminance } from './view/stats.mjs'
import { PLACES, AGENT as GHOST_AGENT, writeGhostWorld, writePose } from './view/ghost-fixture.mjs'
import { decodePng, encodePng } from '../src/vision/renderer.mjs'
import { ENTITY_REGIONS, VIEWS as ENTITY_VIEWS, WIDTH as ENTITY_WIDTH, HEIGHT as ENTITY_HEIGHT, FOV as ENTITY_FOV, AGENT as ENTITY_AGENT, writeEntityWorld } from './view/entity-fixture.mjs'
import { MODELS, CUBES, EYE as MODEL_EYE, AGENT as MODEL_AGENT, writeModelWorld } from './view/model-fixture.mjs'

const repo = path.join(path.dirname(fileURLToPath(import.meta.url)), '..')
const { values } = parseArgs({ options: { out: { type: 'string' }, keep: { type: 'boolean', default: false }, lighting: { type: 'boolean', default: false }, ghost: { type: 'boolean', default: false }, models: { type: 'boolean', default: false }, entities: { type: 'boolean', default: false }, web: { type: 'string' } } })

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

// grass_block_top is grey and tinted by the biome's grass colour; its top face is unshaded (1.0)
const grassTop = textureAverage('grass_block_top')
const grassColors = biomeTable('26.1', ['plains', 'swamp']).colors
const grassExpected = i => grassTop.map((v, c) => v * grassColors[i * 12 + c] / 255)
const plainsGrassExpected = grassExpected(0)
const swampGrassExpected = grassExpected(1)
const GRASS_TOLERANCE = 35 // wider than the 25 first aimed at: ambient occlusion by the wall and mip averaging make the mean approximate (not measured yet)
const distance = (a, b) => Math.hypot(...a.map((v, i) => v - b[i]))

const limeExpected = textureAverage('lime_wool').map(v => v * Z_FACE_SHADE)
const isLime = rgb => rgb.every((v, i) => Math.abs(v - limeExpected[i]) <= 40) // the block behind the glass, shaded like any z face
const isRed = ([r, g, b]) => r > 70 && g < 50 && b < 50 // the shaded, AO-darkened wool
const isWhite = ([r, g, b]) => r > 100 && g > 100 && b > 100 && Math.abs(r - b) < 25
const isGold = ([r, g, b]) => r > 110 && g > 90 && b < 80

// A check: {name, region, test(stats, all), describe(stats, all)}; `all` = {stats: this run's regions, runs: {run name: regions}}
const TEXTURES = [
  {
    name: 'plains grass colour',
    region: 'plains grass',
    test: s => s.mean.every((v, i) => Math.abs(v - plainsGrassExpected[i]) <= GRASS_TOLERANCE),
    describe: s => `mean ${fmt(s.mean)} vs ${fmt(plainsGrassExpected)} +-${GRASS_TOLERANCE}`
  },
  {
    name: 'swamp grass colour',
    region: 'swamp grass',
    test: s => s.mean.every((v, i) => Math.abs(v - swampGrassExpected[i]) <= GRASS_TOLERANCE),
    describe: s => `mean ${fmt(s.mean)} vs ${fmt(swampGrassExpected)} +-${GRASS_TOLERANCE}`
  },
  {
    name: 'biomes differ',
    region: 'plains grass',
    test: (s, all) => distance(s.mean, all.stats['swamp grass'].mean) > 30,
    describe: (s, all) => `distance ${distance(s.mean, all.stats['swamp grass'].mean).toFixed(1)} vs > 30 (plains ${fmt(s.mean)}, swamp ${fmt(all.stats['swamp grass'].mean)})`
  },
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
  // the inner face between two glass blocks is culled: nothing draws a border line across the lime block seen through them
  { name: 'glass culls the inner face', region: 'glass', test: s => s.fraction(isLime) > 0.85, describe: s => `lime ${s.fraction(isLime).toFixed(3)} vs > 0.85 (mean ${fmt(s.mean)})` },
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

// blockJar: undefined finds the client jar, null serves the old table (no models)
const startServer = async (stateDir, blockJar) => {
  const server = createViewServer({ stateDir, blockJar, textureDir: path.join(repo, 'textures'), webDir: values.web ? path.resolve(values.web) : path.join(repo, 'tools', 'view', 'web') })
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

// ---- jar-modelled blocks ----

const isBrown = ([r, g, b]) => r > g + 8 && g > b + 8 && r > 70
const isGreen = ([r, g, b]) => g > r + 12 && g > b + 12
const isDark = ([r, g, b]) => (r + g + b) / 3 < 45
const MODEL_CHECKS = [
  { name: 'leaf litter: above it shows what is behind', region: 'litter above', test: s => s.fraction(isRed) > 0.6, describe: s => `red ${s.fraction(isRed).toFixed(2)} vs > 0.6` },
  { name: 'leaf litter: the ground quad is litter, tinted brown', region: 'litter top', test: s => s.fraction(isBrown) > 0.15, describe: s => `brown ${s.fraction(isBrown).toFixed(2)} vs > 0.15 (mean ${fmt(s.mean)})` },
  { name: 'stair: the notch shows what is behind', region: 'stair notch', test: s => s.fraction(isRed) > 0.6, describe: s => `red ${s.fraction(isRed).toFixed(2)} vs > 0.6` },
  { name: 'fence: the gap between the rails shows what is behind', region: 'fence gap', test: s => s.fraction(isRed) > 0.6, describe: s => `red ${s.fraction(isRed).toFixed(2)} vs > 0.6` },
  { name: 'fence: the rail is wood', region: 'fence arm', test: s => s.fraction(isRed) < 0.3 && s.mean[1] > 80, describe: s => `red ${s.fraction(isRed).toFixed(2)} vs < 0.3, mean ${fmt(s.mean)}` },
  { name: 'dispenser: the front is on the facing side', legacyDraws: true, region: 'dispenser front', test: s => s.fraction(isDark) > 0.04, describe: s => `dark ${s.fraction(isDark).toFixed(3)} vs > 0.04` },
  { name: 'dispenser: the other side is plain', region: 'dispenser side', test: s => s.fraction(isDark) < 0.01, describe: s => `dark ${s.fraction(isDark).toFixed(3)} vs < 0.01` },
  { name: 'grass block: the side overlay is tinted green', region: 'grass fringe', // the grey overlay times the plains colour is darker than the green already in the base texture, which the old renderer shows
    test: s => s.fraction(isGreen) > 0.4 && s.mean[1] < 100, describe: s => `green ${s.fraction(isGreen).toFixed(2)} vs > 0.4, mean g ${s.mean[1].toFixed(0)} vs < 100 (mean ${fmt(s.mean)})` }
]

const modelQuery = `agent=${MODEL_AGENT}&radius=1&w=${WIDTH}&h=${HEIGHT}&fov=${FOV}&dist=64&interp=0`

const shootWorld = async ({ outDir, which, blockJar, name }) => {
  const stateDir = path.join(outDir, `state-${which}`)
  fs.rmSync(stateDir, { recursive: true, force: true })
  writeModelWorld(stateDir, which)
  const { server, port } = await startServer(stateDir, blockJar)
  try {
    return await shoot({ port, run: { name, query: modelQuery }, outDir })
  } finally {
    server.close()
    server.closeAllConnections?.()
  }
}

const modelRegions = () => {
  const basis = cameraBasis({ yaw: 0, pitch: 0, fov: FOV })
  return Object.fromEntries(MODELS.regions.map(({ name, face }) => [name, faceRegion(basis, MODEL_EYE, face, WIDTH, HEIGHT)]))
}

const modelChecks = async outDir => {
  let failures = 0
  const regions = modelRegions()
  const statsOf = image => Object.fromEntries(Object.entries(regions).map(([name, region]) => [name, regionStats(image, region)]))
  const modelled = statsOf((await shootWorld({ outDir, which: 'models', blockJar: undefined, name: 'models-jar' })).image)
  const legacy = statsOf((await shootWorld({ outDir, which: 'models', blockJar: null, name: 'models-legacy' })).image)
  for (const check of MODEL_CHECKS) {
    const ok = check.test(modelled[check.region])
    const counter = check.legacyDraws || !check.test(legacy[check.region]) // a check marked legacyDraws holds for the old renderer too: its pair is the next one
    if (!ok) failures++
    if (!counter) failures++
    console.log(`${ok ? 'PASS' : 'FAIL'} models/${check.name}: ${check.describe(modelled[check.region])}`)
    if (!check.legacyDraws) console.log(`${counter ? 'PASS' : 'FAIL'} models/counter: ${check.name} fails on the old renderer: ${check.describe(legacy[check.region])}`)
  }
  const cubesJar = (await shootWorld({ outDir, which: 'cubes', blockJar: undefined, name: 'cubes-jar' })).image
  const cubesLegacy = (await shootWorld({ outDir, which: 'cubes', blockJar: null, name: 'cubes-legacy' })).image
  const { mean, fraction, diff } = compareImages(cubesJar, cubesLegacy)
  fs.writeFileSync(path.join(outDir, 'cubes-diff.png'), encodePng(WIDTH, HEIGHT, diff))
  const same = mean <= GHOST_MEAN_MAX && fraction < GHOST_FRACTION_MAX
  if (!same) failures++
  console.log(`${same ? 'PASS' : 'FAIL'} cubes/the fast path looks the same with and without the jar: mean abs diff ${mean.toFixed(3)} vs <= ${GHOST_MEAN_MAX}, ${(fraction * 100).toFixed(3)}% pixels over ${GHOST_DIFF_LEVEL} vs < ${GHOST_FRACTION_MAX * 100}%`)
  return failures
}

// ---- block-entity models ----

const isBackdrop = ([r, g, b]) => g > r + 25 && g > b + 25
const isReddish = ([r, g, b]) => r > g + 40 && r > b + 40
const LIME_MIN = 0.85 // a background rectangle is at least this lime
const SOLID_LIME_MAX = 0.05 // a rectangle on the object has at most this much lime
const SEAM_MIN = 12 // luminance steps between two bands that a seam makes
const LIGHTER_MIN = 30
const SAME_MAX = 8
const DARK_MIN = 0.04

const entityVerdicts = {
  lime: s => ({ ok: s.fraction(isBackdrop) >= LIME_MIN, text: `lime ${s.fraction(isBackdrop).toFixed(2)} vs >= ${LIME_MIN}` }),
  solid: s => ({ ok: s.fraction(isBackdrop) <= SOLID_LIME_MAX, text: `lime ${s.fraction(isBackdrop).toFixed(2)} vs <= ${SOLID_LIME_MAX}` }),
  red: s => ({ ok: s.fraction(isReddish) >= 0.85, text: `reddish ${s.fraction(isReddish).toFixed(2)} vs >= 0.85 (mean ${fmt(s.mean)})` }),
  seam: (s, against) => ({ ok: Math.abs(lum(s) - lum(against)) >= SEAM_MIN, text: `luminance ${lum(s).toFixed(0)} vs ${lum(against).toFixed(0)}, step >= ${SEAM_MIN}` }),
  dark: s => ({ ok: s.fraction(([r, g, b]) => (r + g + b) / 3 < 90) >= DARK_MIN && s.std > 25, text: `dark ${s.fraction(([r, g, b]) => (r + g + b) / 3 < 90).toFixed(3)} vs >= ${DARK_MIN}, std ${s.std.toFixed(1)} vs > 25` }),
  same: (s, against) => ({ ok: Math.abs(lum(s) - lum(against)) < SAME_MAX, text: `luminance ${lum(s).toFixed(0)} vs ${lum(against).toFixed(0)}, step < ${SAME_MAX}` }),
  lighter: (s, against) => ({ ok: lum(s) >= lum(against) + LIGHTER_MIN, text: `luminance ${lum(s).toFixed(0)} vs ${lum(against).toFixed(0)} + ${LIGHTER_MIN}` })
}

const entityQuery = `agent=${ENTITY_AGENT}&radius=1&w=${ENTITY_WIDTH}&h=${ENTITY_HEIGHT}&fov=${ENTITY_FOV}&dist=64&interp=0`

const shootEntities = async ({ outDir, blockJar, name, view }) => {
  const stateDir = path.join(outDir, 'state-entities')
  fs.rmSync(stateDir, { recursive: true, force: true })
  writeEntityWorld(stateDir, view)
  const { server, port } = await startServer(stateDir, blockJar)
  try {
    return await withPage({ url: `http://127.0.0.1:${port}/?${entityQuery}`, width: ENTITY_WIDTH, height: ENTITY_HEIGHT }, async page => {
      await page.waitUntil('window.__view?.ready === true', 'window.__view.ready')
      await page.evaluate(HIDE_CHROME)
      await page.sleep(SETTLE_MS)
      const png = await page.screenshot()
      fs.writeFileSync(path.join(outDir, `${name}.png`), png)
      return decodePng(png)
    })
  } finally {
    server.close()
    server.closeAllConnections?.()
  }
}

// every region measured in the image of its own view
const entityOutcomes = images => {
  return ENTITY_REGIONS.map(region => {
    const { eye, pitch } = ENTITY_VIEWS[region.view]
    const basis = cameraBasis({ yaw: 0, pitch, fov: ENTITY_FOV })
    const stats = face => regionStats(images[region.view], faceRegion(basis, eye, face, ENTITY_WIDTH, ENTITY_HEIGHT, 0.02))
    return { region, ...entityVerdicts[region.expect](stats(region.face), region.against ? stats(region.against) : undefined) }
  })
}

const shootAllViews = async (outDir, blockJar, label) => {
  const images = {}
  for (const view of Object.keys(ENTITY_VIEWS)) images[view] = await shootEntities({ outDir, blockJar, name: `entities-${label}-${view}`, view })
  return images
}

const entityChecks = async outDir => {
  let failures = 0
  const jar = entityOutcomes(await shootAllViews(outDir, undefined, 'jar'))
  const legacy = entityOutcomes(await shootAllViews(outDir, null, 'legacy'))
  for (const [i, { region, ok, text }] of jar.entries()) {
    if (!ok) failures++
    console.log(`${ok ? 'PASS' : 'FAIL'} entities/${region.name}: ${text}${legacy[i].ok ? '' : ' (also fails on the old renderer)'}`)
  }
  for (const type of [...new Set(ENTITY_REGIONS.map(r => r.type))]) {
    const failing = legacy.filter(o => o.region.type === type && !o.ok).length
    const counter = failing > 0
    if (!counter) failures++
    console.log(`${counter ? 'PASS' : 'FAIL'} entities/counter: ${type} fails on the old renderer in ${failing} of ${legacy.filter(o => o.region.type === type).length} regions`)
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
    if (values.models) failures += await modelChecks(outDir)
    if (values.entities) failures += await entityChecks(outDir)
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
