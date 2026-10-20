// Why JavaScript: wraps Mineflayer's bot.look, the one place every turn of the head (lookAt, steer, drive) goes through.
// Looking at an enderman's eyes angers it (vanilla: dot(view, toEyes) > 1 - 0.025 / distance, with a line of sight).
// A body never does: a look that would meet the eyes of a seen enderman keeps its yaw and moves its pitch the least it must.
import vec3 from 'vec3'
import { blocksSight } from './sight.mjs'
import constants from '../src/engine/constants.json' with { type: 'json' }

const { Vec3 } = vec3

const ENDERMAN_EYE = 2.55
const RANGE = 64
const PITCH_STEP = 0.05
const PITCH_TRIES = 60

const viewOf = (yaw, pitch) => new Vec3(-Math.sin(yaw) * Math.cos(pitch), Math.sin(pitch), -Math.cos(yaw) * Math.cos(pitch))

// whether looking along (yaw, pitch) from eye meets the enderman eyes at target
export function stares (eye, yaw, pitch, target) {
  const d = target.minus(eye)
  const dist = d.norm()
  if (dist === 0) return false
  return viewOf(yaw, pitch).dot(d.scaled(1 / dist)) > 1 - 0.025 / dist
}

const seen = (bot, eye, target) => {
  const d = target.minus(eye)
  const n = Math.ceil(d.norm() * 2)
  for (let i = 1; i < n; i++) {
    const at = eye.plus(d.scaled(i / n))
    if (blocksSight(bot.blockAt(new Vec3(Math.floor(at.x), Math.floor(at.y), Math.floor(at.z))))) return false
  }
  return true
}

const NONE = []
const eyesInSight = bot => {
  const entities = bot.entities
  if (!entities) return NONE
  let eye = null
  let out = NONE
  for (const id in entities) {
    const e = entities[id]
    if (e?.name !== 'enderman' || !e.position) continue
    eye ??= bot.entity.position.offset(0, bot.entity.eyeHeight ?? constants.eyeHeight, 0)
    if (e.position.distanceTo(eye) > RANGE) continue
    const target = e.position.offset(0, ENDERMAN_EYE, 0)
    if (!seen(bot, eye, target)) continue
    if (out === NONE) out = []
    out.push({ eye, target })
  }
  return out
}

export const safePitch = (bot, yaw, pitch) => {
  const eyes = eyesInSight(bot)
  const bad = p => eyes.some(({ eye, target }) => stares(eye, yaw, p, target))
  if (!bad(pitch)) return pitch
  for (let k = 1; k <= PITCH_TRIES; k++) {
    for (const p of [pitch - k * PITCH_STEP, pitch + k * PITCH_STEP]) {
      if (Math.abs(p) <= Math.PI / 2 && !bad(p)) return p
    }
  }
  return pitch
}

// returns the function that stops the per-tick check
export function avoidEnderStare (bot) {
  if (!bot.look.avoidsEnderStare) {
    const look = bot.look.bind(bot)
    bot.look = (yaw, pitch, force) => look(yaw, safePitch(bot, yaw, pitch), force)
    bot.look.avoidsEnderStare = true
  }
  // an enderman that walks or teleports into the view of a still head is looked away from at once
  const onTick = () => {
    const { yaw, pitch } = bot.entity
    if (!Number.isFinite(yaw) || !Number.isFinite(pitch)) return
    if (safePitch(bot, yaw, pitch) !== pitch) bot.look(yaw, pitch, true)
  }
  bot.on('physicsTick', onTick)
  return () => bot.removeListener('physicsTick', onTick)
}
