// Why JavaScript: browser event wiring only: DOM listeners, pointer lock, setInterval and fetch are the page's own APIs. The
// takeover state and every decision are view.takeover (dashboard/src/view/takeover.cljs, in cljs/viewer.mjs); this forwards events there and applies the view it renders.
// Manual takeover from the view page: a take-over button, a banner while anyone drives the body, and key/mouse
// control while this page does. Talks to the view server's /drive/<agent>; does not depend on app.mjs.
import { createDriver, isAgentKey, whoFrom, withTimeout } from './cljs/viewer.mjs'

const REQUEST_TIMEOUT_MS = 1500
const POLL_MS = 1000
const PING_MS = 500
const LOOK_FLUSH_MS = 50

const params = new URLSearchParams(location.search)
const agent = params.get('agent') // <world>/<name>

const start = () => {
  const bar = document.getElementById('bar')
  const banner = document.getElementById('drive-banner')
  const canvas = document.getElementById('view')
  const button = document.createElement('button')
  button.id = 'drive'
  button.type = 'button'
  bar.appendChild(button)
  const locked = () => document.pointerLockElement === canvas

  const render = (view) => {
    banner.textContent = view.text
    banner.hidden = view.hidden
    document.body.classList.toggle('driving', view.driving)
    button.textContent = view.buttonText
    button.disabled = view.disabled
    button.title = view.title
    if (parent !== window) parent.postMessage({ type: 'drive', ...view.message }, location.origin)
  }

  const driver = createDriver({
    agent,
    me: whoFrom(location.search),
    embed: params.get('embed') === '1',
    fetch,
    timedFetch: withTimeout(fetch, REQUEST_TIMEOUT_MS),
    render,
    exitPointerLock: () => { if (locked()) document.exitPointerLock() },
    requestPointerLock: () => canvas.requestPointerLock?.()
  })

  // a key the driver handles must not reach the page's own handlers
  const swallow = (e, handled) => {
    if (!handled) return
    e.preventDefault()
    e.stopImmediatePropagation()
  }
  addEventListener('keydown', (e) => swallow(e, driver.keydown({ code: e.code, repeat: e.repeat, ctrlKey: e.ctrlKey, metaKey: e.metaKey, altKey: e.altKey, pointerLocked: locked() })), true)
  addEventListener('keyup', (e) => swallow(e, driver.keyup({ code: e.code })), true)
  canvas.addEventListener('click', () => driver.click())
  addEventListener('mousemove', (e) => driver.mousemove(e.movementX, e.movementY, locked()))
  addEventListener('blur', () => driver.leave('blur'))
  document.addEventListener('visibilitychange', () => driver.visibilityChange(document.visibilityState === 'hidden'))
  document.addEventListener('pointerlockchange', () => driver.pointerLockChange(locked()))
  addEventListener('pagehide', () => driver.leave('pagehide'))
  addEventListener('beforeunload', () => driver.leave('pagehide'))
  setInterval(driver.ping, PING_MS)
  setInterval(driver.flushLook, LOOK_FLUSH_MS)
  setInterval(driver.poll, POLL_MS)
  button.addEventListener('click', driver.toggle)
  window.__drive = { state: driver.state, sent: driver.sent, take: driver.take, release: driver.release }
  driver.start()
}

if (isAgentKey(agent)) start()
