// Serves the browser view: node tools/view-serve.mjs [--port 3702] [--state dir] [--host 127.0.0.1]
import path from 'node:path'
import { parseArgs } from 'node:util'
import { fileURLToPath } from 'node:url'
import { createViewServer } from './view/serve.mjs'

const repo = path.join(path.dirname(fileURLToPath(import.meta.url)), '..')
const { values } = parseArgs({
  options: {
    port: { type: 'string', default: '3702' },
    state: { type: 'string', default: path.join(repo, 'state') },
    host: { type: 'string', default: '127.0.0.1' }
  }
})

const server = createViewServer({
  stateDir: path.resolve(values.state),
  textureDir: path.join(repo, 'textures'),
  webDir: path.join(repo, 'tools', 'view', 'web')
})
server.listen(Number(values.port), values.host, () => {
  console.log(`view server: http://${values.host}:${server.address().port}/`)
})
