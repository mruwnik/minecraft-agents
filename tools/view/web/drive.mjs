// Manual takeover from the view page: a take-over button, a banner while anyone drives the body, and key/mouse
// control while this page does. Talks to the view server's /drive/<agent>; does not depend on app.mjs.
import { controlFor, lookStepFor, mouseLook, mergeLook, bannerText, serialQueue } from './drive-keys.mjs'

const ME = 'view'
const POLL_MS = 1000
const PING_MS = 500
const LOOK_FLUSH_MS = 50
const ERROR_MS = 3000
const SENT_KEEP = 200
const LOST = ['not-taken', 'not-driver']

const agent = new URLSearchParams(location.search).get('agent')

const start = () => {
  const bar = document.getElementById('bar')
  const banner = document.getElementById('drive-banner')
  const canvas = document.getElementById('view')
  const button = document.createElement('button')
  button.id = 'drive'
  button.type = 'button'
  bar.appendChild(button)

  let driving = false
  let manual = null
  let lastReply = null
  let errorText = null
  let errorTimer = null
  let pendingLook = null
  const sent = []
  const enqueue = serialQueue() // requests reach the body in order: a keyup must never overtake its keydown

  const render = () => {
    const text = errorText ?? bannerText(manual, ME)
    banner.textContent = text ?? ''
    banner.hidden = text === null
    const other = manual && manual.who !== ME
    button.textContent = driving ? 'release (G)' : 'take over (G)'
    button.disabled = Boolean(other)
    button.title = other ? `driven by ${manual.who}` : ''
  }

  const showError = (text) => {
    errorText = text
    clearTimeout(errorTimer)
    errorTimer = setTimeout(() => { errorText = null; render() }, ERROR_MS)
    render()
  }

  const dropDriving = () => {
    driving = false
    pendingLook = null
    if (document.pointerLockElement === canvas) document.exitPointerLock()
  }

  const post = async (msg, init = {}) => {
    sent.push({ t: performance.now(), op: msg.op })
    if (sent.length > SENT_KEEP) sent.shift()
    const reply = await fetch('/drive/' + agent, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(msg),
      ...init
    }).then(r => r.json(), () => null)
    if (!reply) return null
    lastReply = reply
    if (reply.manual !== undefined) manual = reply.manual
    if (driving && LOST.includes(reply.reason)) dropDriving()
    render()
    return reply
  }
  const send = (msg) => enqueue(() => post(msg))

  const poll = async () => {
    const reply = await fetch('/drive/' + agent).then(r => r.json(), () => null)
    if (!reply) return
    lastReply = reply
    manual = reply.manual ?? null
    if (driving && manual?.who !== ME) dropDriving()
    render()
  }

  const take = async () => {
    const reply = await send({ op: 'take', who: ME, why: 'driven from the view page' })
    if (!reply) return showError('no running body ' + agent)
    if (!reply.ok) return showError('cannot take over: ' + reply.reason)
    driving = true
    render()
  }
  const release = async () => {
    dropDriving()
    await send({ op: 'release', who: ME })
    render()
  }
  const toggle = () => (driving ? release() : take())

  const handled = (code) => controlFor(code) !== null || lookStepFor(code) !== null || code === 'KeyF'

  addEventListener('keydown', (e) => {
    if (e.code === 'KeyG' && !e.repeat && !e.ctrlKey && !e.metaKey && !e.altKey) {
      if (!manual || manual.who === ME) toggle()
      return
    }
    if (!driving || !handled(e.code)) return
    e.preventDefault()
    e.stopImmediatePropagation()
    const control = controlFor(e.code)
    if (control && !e.repeat) send({ op: 'set', who: ME, controls: { [control]: true } })
    const step = lookStepFor(e.code)
    if (step) send({ op: 'set', who: ME, look: step })
  }, true)

  addEventListener('keyup', (e) => {
    if (!driving || !handled(e.code)) return
    e.preventDefault()
    e.stopImmediatePropagation()
    const control = controlFor(e.code)
    if (control) send({ op: 'set', who: ME, controls: { [control]: false } })
  }, true)

  canvas.addEventListener('click', () => {
    if (driving) canvas.requestPointerLock?.()
  })
  addEventListener('mousemove', (e) => {
    if (!driving || document.pointerLockElement !== canvas) return
    pendingLook = mergeLook(pendingLook ?? {}, mouseLook(e.movementX, e.movementY))
  })

  const stopControls = () => {
    if (driving) send({ op: 'stop', who: ME })
  }
  addEventListener('blur', stopControls)
  document.addEventListener('visibilitychange', () => {
    if (document.visibilityState === 'hidden') stopControls()
  })
  document.addEventListener('pointerlockchange', () => {
    if (document.pointerLockElement !== canvas) stopControls()
  })
  const leave = () => {
    if (!driving) return
    post({ op: 'release', who: ME }, { keepalive: true }) // not queued: the page is going away
  }
  addEventListener('pagehide', leave)
  addEventListener('beforeunload', leave)

  setInterval(() => {
    if (!driving) return
    send({ op: 'ping', who: ME })
  }, PING_MS)
  setInterval(() => {
    if (!driving || !pendingLook) return
    const look = pendingLook
    pendingLook = null
    send({ op: 'set', who: ME, look })
  }, LOOK_FLUSH_MS)
  setInterval(poll, POLL_MS)

  button.addEventListener('click', toggle)
  window.__drive = { state: () => ({ driving, manual, lastReply }), sent }
  render()
  poll()
}

if (agent) start()
