// Manual takeover from the view page: a take-over button, a banner while anyone drives the body, and key/mouse
// control while this page does. Talks to the view server's /drive/<agent>; does not depend on app.mjs.
import { controlFor, lookStepFor, mouseLook, mergeLook, bannerText, serialQueue, whoFrom, shouldTakeOnClick, shouldReleaseOnEscape, leaveAction, withTimeout, isStale, shouldDrop } from './drive-keys.mjs'

const REQUEST_TIMEOUT_MS = 1500
const POLL_MS = 1000
const PING_MS = 500
const LOOK_FLUSH_MS = 50
const ERROR_MS = 3000
const SENT_KEEP = 200

const params = new URLSearchParams(location.search)
const agent = params.get('agent') // <world>/<name>
const ME = whoFrom(location.search)
const EMBED = params.get('embed') === '1'
const timedFetch = withTimeout(fetch, REQUEST_TIMEOUT_MS)

const start = () => {
  const bar = document.getElementById('bar')
  const banner = document.getElementById('drive-banner')
  const canvas = document.getElementById('view')
  const button = document.createElement('button')
  button.id = 'drive'
  button.type = 'button'
  bar.appendChild(button)

  let driving = false
  let takeGen = 0 // bumped on every successful take; replies to requests started earlier are stale
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
    document.body.classList.toggle('driving', driving)
    button.textContent = driving ? 'release (G)' : 'take over (G)'
    button.disabled = Boolean(other)
    button.title = other ? `driven by ${manual.who}` : ''
    if (parent !== window) parent.postMessage({ type: 'drive', driving, manual, expiresAt: manual?.expiresAt ?? null }, location.origin)
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

  // any reply (or a failed request) that shows this page no longer holds the body clears every marker of control
  const checkHold = (reply, startedGen) => {
    if (!shouldDrop({ driving, reply, me: ME, startedGen, currentGen: takeGen })) return
    dropDriving()
    render()
  }

  const post = async (msg, init = {}) => {
    const startedGen = takeGen
    sent.push({ t: performance.now(), op: msg.op })
    if (sent.length > SENT_KEEP) sent.shift()
    const reply = await timedFetch('/drive/' + agent, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(msg),
      ...init
    }).then(r => r.json(), () => null)
    if (isStale({ startedGen, currentGen: takeGen })) return reply
    if (reply) {
      lastReply = reply
      if (reply.manual !== undefined) manual = reply.manual
    }
    checkHold(reply, startedGen)
    if (!reply) return null
    render()
    return reply
  }
  const send = (msg) => enqueue(() => post(msg))

  const poll = async () => {
    const startedGen = takeGen
    const reply = await fetch('/drive/' + agent).then(r => r.json(), () => null)
    if (isStale({ startedGen, currentGen: takeGen })) return
    if (reply) {
      lastReply = reply
      manual = reply.manual ?? null
    }
    checkHold(reply, startedGen)
    if (reply) render()
  }

  const take = async () => {
    const reply = await send({ op: 'take', who: ME, why: 'driven from the view page' })
    if (!reply) return showError('no running body ' + agent)
    if (!reply.ok) return showError('cannot take over: ' + reply.reason)
    takeGen += 1
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
    if (shouldReleaseOnEscape({ code: e.code, driving, pointerLocked: document.pointerLockElement === canvas })) {
      release()
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
    else if (shouldTakeOnClick({ embed: EMBED, driving, manual, me: ME })) take()
  })
  addEventListener('mousemove', (e) => {
    if (!driving || document.pointerLockElement !== canvas) return
    pendingLook = mergeLook(pendingLook ?? {}, mouseLook(e.movementX, e.movementY))
  })

  const onLeave = (event) => {
    if (!driving) return
    const action = leaveAction(event)
    if (action === null) return
    if (event === 'pagehide') {
      post({ op: 'stop', who: ME }, { keepalive: true }) // not queued: the page is going away; no release
      return
    }
    send({ op: 'stop', who: ME })
    if (action === 'stop-release') release()
  }
  addEventListener('blur', () => onLeave('blur'))
  document.addEventListener('visibilitychange', () => {
    if (document.visibilityState === 'hidden') onLeave('hidden')
    else poll()
  })
  let hadLock = false
  document.addEventListener('pointerlockchange', () => {
    if (document.pointerLockElement === canvas) { hadLock = true; return }
    if (!hadLock) return
    hadLock = false
    onLeave('pointerlock-lost')
  })
  addEventListener('pagehide', () => onLeave('pagehide'))
  addEventListener('beforeunload', () => onLeave('pagehide'))

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
  window.__drive = { state: () => ({ driving, manual, lastReply, expiresAt: manual?.expiresAt ?? null }), sent, take, release }
  render()
  poll()
}

if (agent) start()
