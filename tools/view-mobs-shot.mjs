// Why JavaScript: drives headless Chromium over CDP to look at the WebGL view (browser/GPU).
// A picture of the browser view's mob models: every mob with a model in a grid on a grass floor, seen from the south and above, all turned to --yaw
// (radians, default pi: facing the camera; pi/2 faces west). Prints the file written.
//   node tools/view-mobs-shot.mjs --out file.png [--yaw 3.14159] [--old] [--software] [--width 1400] [--height 800]
// --old: serve the table without the models' layers (blockJar null), so every mob is a flat box. --software: the software renderer instead.
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { parseArgs } from 'node:util'
import { fileURLToPath } from 'node:url'
import { createViewServer } from './view/serve.mjs'
import { writeWorld, poseFor, WORLD, AGENT } from './view/fixture.mjs'
import { withPage, freePort } from './view/headless.mjs'
import { MOBS } from './view/web/mob-models.mjs'
import { renderView } from './view/render.mjs'
import { encodePng } from './view/renderer.mjs'
import { poseFile } from '../engine/js/view.mjs'

const repo = path.join(path.dirname(fileURLToPath(import.meta.url)), '..')
const { values } = parseArgs({ options: { out: { type: 'string' }, yaw: { type: 'string', default: '3.14159' }, old: { type: 'boolean', default: false }, software: { type: 'boolean', default: false }, width: { type: 'string', default: '1400' }, height: { type: 'string', default: '800' } } })
if (!values.out) throw new Error('--out file.png is required')
const [width, height, yaw] = [Number(values.width), Number(values.height), Number(values.yaw)]

const HEIGHTS = { zombie: 1.95, husk: 1.95, drowned: 1.95, zombified_piglin: 1.95, player: 1.8, piglin: 1.95, piglin_brute: 1.95, skeleton: 1.99, stray: 1.99, wither_skeleton: 2.4, bogged: 1.99, creeper: 1.7, cow: 1.4, mooshroom: 1.4, pig: 0.9, sheep: 1.3, spider: 0.9, cave_spider: 0.5, chicken: 0.7, villager: 1.95, wandering_trader: 1.95, witch: 1.95, enderman: 2.9, illusioner: 1.95, zombie_villager: 1.95, horse: 1.6, skeleton_horse: 1.6, zombie_horse: 1.6, donkey: 1.5, mule: 1.6, wolf: 0.85, cat: 0.7, ocelot: 0.7, fox: 0.7, iron_golem: 2.7, snow_golem: 1.9, blaze: 1.8, slime: 0.52 }
const PER_ROW = 7
const FLOOR_Y = 64
const entities = Object.keys(MOBS).map((name, i) => ({
  id: i + 1, name, type: name === 'player' ? 'player' : 'hostile', username: name === 'player' ? 'Steve' : undefined,
  pos: { x: 0.5 + (i % PER_ROW) * 3.4 - 10, y: FLOOR_Y + 1, z: 4 - Math.floor(i / PER_ROW) * 3.8 }, width: 0.6, height: HEIGHTS[name] ?? 1.8, yaw
}))
const eye = { x: 0.5, y: 76, z: 16 }
const camera = { eye, yaw: 0, pitch: -0.62 }

const stateDir = fs.mkdtempSync(path.join(os.tmpdir(), 'view-mobs-'))
try {
  writeWorld({ stateDir, blockAt: (x, y) => y === FLOOR_Y ? 'grass_block' : 'air', light: () => ({ sky: 15, block: 0 }), keys: ['grass_block'], camera })
  fs.writeFileSync(poseFile(stateDir, WORLD, AGENT), JSON.stringify({ ...poseFor(Date.now(), { ...camera, world: WORLD }), entities }))
  if (values.software) {
    const out = renderView({ world: WORLD, agentName: AGENT, width, height, fov: 80, maxDist: 64, stateDir })
    fs.writeFileSync(values.out, out.png)
  } else {
    const server = createViewServer({ stateDir, blockJar: values.old ? null : undefined, textureDir: path.join(repo, 'textures'), webDir: path.join(repo, 'tools', 'view', 'web') })
    const port = await freePort()
    await new Promise(resolve => server.listen(port, '127.0.0.1', resolve))
    try {
      await withPage({ url: `http://127.0.0.1:${port}/?agent=${WORLD}/${AGENT}&radius=2&w=${width}&h=${height}&fov=80&dist=64&interp=0`, width, height }, async page => {
        await page.waitUntil('window.__view?.ready === true', 'window.__view.ready')
        await page.evaluate("for (const id of ['overlay', 'agents', 'free']) document.getElementById(id)?.style.setProperty('display', 'none', 'important'); true")
        await page.sleep(1500)
        fs.writeFileSync(values.out, await page.screenshot())
        for (const line of page.logs.slice(0, 5)) console.log(`console: ${line}`)
      })
    } finally {
      server.close()
      server.closeAllConnections?.()
    }
  }
  console.log(values.out)
} finally {
  fs.rmSync(stateDir, { recursive: true, force: true })
}
