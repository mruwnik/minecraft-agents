import path from 'node:path'
import { createViewServer } from '../../tools/view/serve.mjs'

const prefixes = ['/pose/', '/web/', '/columns/', '/blocks/', '/textures/', '/hud/', '/drive/']
const exact = new Set(['/view', '/view/', '/agents'])

// Serves the live 3D view from another server's origin: the view's http.Server is built but never listened on.
export function mountView ({ repo, stateDir }) {
  const server = createViewServer({
    stateDir,
    textureDir: path.join(repo, 'textures'),
    webDir: path.join(repo, 'tools', 'view', 'web')
  })
  const handles = pathname => exact.has(pathname) || prefixes.some(p => pathname.startsWith(p))
  const handle = (req, res) => {
    const url = new URL(req.url, 'http://localhost')
    if (url.pathname === '/view' || url.pathname === '/view/') req.url = '/' + url.search
    server.emit('request', req, res)
  }
  // ends what the view server started (the block-issues scan worker); the server never listened, so this only emits 'close'
  const close = () => new Promise(resolve => {
    server.once('close', resolve)
    server.close()
  })
  return { handles, handle, close }
}
