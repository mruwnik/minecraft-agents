// Pure key and look mapping for driving a body from the view page (Minecraft degrees: positive dyaw turns right,
// negative dpitch looks up). Ctrl is not a control: Ctrl+W closes the tab.
const CONTROLS = { KeyW: 'forward', KeyS: 'back', KeyA: 'left', KeyD: 'right', Space: 'jump', ShiftLeft: 'sneak', ShiftRight: 'sneak', KeyR: 'sprint' }
const LOOK_STEPS = { ArrowLeft: { dyaw: -15 }, ArrowRight: { dyaw: 15 }, ArrowUp: { dpitch: -10 }, ArrowDown: { dpitch: 10 } }

export const controlFor = (code) => CONTROLS[code] ?? null
export const lookStepFor = (code) => (LOOK_STEPS[code] ? { ...LOOK_STEPS[code] } : null)
export const mouseLook = (movementX, movementY, sensitivity = 0.15) => ({ dyaw: movementX * sensitivity, dpitch: movementY * sensitivity })
export const mergeLook = (a, b) => ({ dyaw: (a.dyaw ?? 0) + (b.dyaw ?? 0), dpitch: (a.dpitch ?? 0) + (b.dpitch ?? 0) })

export const bannerText = (manual, me) => {
  if (!manual) return null
  if (manual.who === me) return 'MANUAL CONTROL (you) — WASD move, space jump, shift sneak, R sprint, arrows/mouse look, G release'
  return `MANUAL CONTROL by ${manual.who}: ${manual.why}`
}

// runs the functions one at a time in call order: each starts after the previous one settled, failed or not
export const serialQueue = () => {
  let tail = Promise.resolve()
  return (fn) => {
    const result = tail.then(() => fn())
    tail = result.catch(() => {})
    return result
  }
}

const WHO_PATTERN = /^[A-Za-z0-9:_-]{1,40}$/
export const whoFrom = (search, fallback = 'view') => {
  const who = new URLSearchParams(search).get('who')
  return who !== null && WHO_PATTERN.test(who) ? who : fallback
}

// embedded in the dashboard a click on the canvas takes over, unless someone else holds the body
export const shouldTakeOnClick = ({ embed, driving, manual, me }) => embed && !driving && (!manual || manual.who === me)

// the first Esc only exits pointer lock (the browser eats it); an Esc with no lock left releases
export const shouldReleaseOnEscape = ({ code, driving, pointerLocked }) => code === 'Escape' && driving && !pointerLocked

// fetch that aborts after ms, so a hung request cannot hold the serial queue
export const withTimeout = (fetchFn, ms) => async (url, init = {}) => {
  const controller = new AbortController()
  const timer = setTimeout(() => controller.abort(), ms)
  try {
    return await fetchFn(url, { ...init, signal: controller.signal })
  } finally {
    clearTimeout(timer)
  }
}
