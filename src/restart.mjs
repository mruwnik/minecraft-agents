// Restarting a body at night. A body started outside after dark dies before its driver can act (13:40Z, a7cd6038):
// the advice a running body gives with code_updated, and the gate ./start runs before anything else, both read the
// shared clock (state/clock.json, written by every running body: {day, timeOfDay, by, at}).
const NIGHT_FROM = 12500
const NIGHT_TO = 23460
// a clock nobody has written for this long means no body is online, and with nobody online the night is not passing:
// a stale clock refuses nothing (the same 90 s ./mc dawn allows before it calls the clock stale)
const CLOCK_FRESH_MS = 90000

const nightTick = tick => typeof tick === 'number' && tick > NIGHT_FROM && tick < NIGHT_TO

// true only when a fresh clock says night: by its day flag, or by a tick past 12500 when it has none
export const nightClock = (clock, now) => {
  if (!clock || typeof clock.at !== 'number' || now - clock.at > CLOCK_FRESH_MS) return false
  if (typeof clock.day === 'boolean') return !clock.day
  return nightTick(clock.timeOfDay)
}

const RESTART = 'restart (./mc quit, then ./start)'
export const restartAdvice = (clock, now = Date.now()) => nightClock(clock, now)
  ? `the shared code has fixes you are not running, but it is night (tick ${clock.timeOfDay}) and a body restarted outside now dies: wait for dawn (./mc dawn) unless you spawn into a bed, then ${RESTART}`
  : `the shared code has fixes you are not running. No hurry: ${RESTART} next time you are idle somewhere safe, or at once if a tool misbehaves`

// what ./start says instead of starting; null when it may go ahead. `now` is the driver's word (./start --now) that the
// body spawns into a bed or under a roof: whether that is so is the driver's call, not the clock's
export const startRefusal = (clock, now, force) => {
  if (force || !nightClock(clock, now)) return null
  const age = Math.round((now - clock.at) / 1000)
  return `it is night (tick ${clock.timeOfDay}, seen ${age}s ago by ${clock.by}): a body started outside now dies. Wait for dawn (./mc dawn) unless you spawn into a bed, then ./start --now to say so`
}
