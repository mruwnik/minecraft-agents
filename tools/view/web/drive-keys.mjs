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
