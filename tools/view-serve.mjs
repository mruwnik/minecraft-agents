// Why JavaScript: thin launcher for the JS view server (tools/view/serve.mjs), which serves binary column files and the WebGL page.
// Serves the browser view: node tools/view-serve.mjs [--port 3702] [--state dir] [--host 127.0.0.1] [--push watch|poll] [--poll-ms 50]
import path from 'node:path'
import { storageRoot, worldsDir } from '../engine/js/bodies.mjs'
import { parseArgs } from 'node:util'
import { fileURLToPath } from 'node:url'
import { createViewServer } from './view/serve.mjs'

const repo = path.join(path.dirname(fileURLToPath(import.meta.url)), '..')
const { values } = parseArgs({
  options: {
    port: { type: 'string', default: '3702' },
    state: { type: 'string' }, worlds: { type: 'string' },
    host: { type: 'string', default: '127.0.0.1' },
    push: { type: 'string', default: 'watch' },
    'poll-ms': { type: 'string', default: '50' }
  }
})

const server = createViewServer({
  stateDir: storageRoot(values, repo),
  textureDir: path.join(repo, 'textures'),
  webDir: path.join(repo, 'tools', 'view', 'web'),
  push: values.push,
  pollMs: Number(values['poll-ms'])
})
server.listen(Number(values.port), values.host, () => {
  console.log(`view server: http://${values.host}:${server.address().port}/`)
})
