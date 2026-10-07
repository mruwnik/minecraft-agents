// Why JavaScript: Mineflayer boundary; applies controls and reports pose on the bot.
// steer: a cljs executor holds the body's controls for a while. Every physics tick `decide(pose)` answers either
// {done: <plain object>} or {controls: {name: bool}, yaw?, pitch?}; JS only reports the pose and applies the answer.
// pathWorld: the planner's input from the live world (a snapshot) for one plan; jobs.lib.walk.world adds the block table and space.
import vec3 from 'vec3'
import { liveSnapshot } from './path/live-snapshot.mjs'

const { Vec3 } = vec3

const DEFAULT_TIMEOUT_S = 60
const MAX_TIMEOUT_S = 120
const CLIMBABLE = new Set(['ladder', 'vine', 'scaffolding', 'twisting_vines', 'twisting_vines_plant', 'weeping_vines', 'weeping_vines_plant', 'cave_vines', 'cave_vines_plant'])
const isNum = n => typeof n === 'number' && Number.isFinite(n)
const reasonOf = err => String(err).slice(0, 200)

// what the client climbs at the feet: a climbable, or an open trapdoor over a ladder of its own facing (vanilla, and
// prismarine-physics once tools/patch-deps.mjs has run)
const climbableAt = (bot, cell) => {
  const block = bot.blockAt(cell)
  if (CLIMBABLE.has(block?.name)) return true
  if (!/_trapdoor$/.test(block?.name ?? '')) return false
  const below = bot.blockAt(cell.offset(0, -1, 0))
  const props = block.getProperties()
  return below?.name === 'ladder' && props.open === true && props.facing === below.getProperties().facing
}

export function createSteer ({ act, getBot, badArgs }) {
  const pose = bot => {
    const { position, velocity, onGround, isInWater, isInLava, isCollidedHorizontally, yaw } = bot.entity
    const cell = new Vec3(Math.floor(position.x), Math.floor(position.y), Math.floor(position.z))
    return {
      x: position.x,
      y: position.y,
      z: position.z,
      vx: velocity.x,
      vy: velocity.y,
      vz: velocity.z,
      onGround,
      onClimbable: climbableAt(bot, cell),
      inWater: isInWater,
      inLava: isInLava,
      collided: isCollidedHorizontally,
      yaw,
      t: Date.now()
    }
  }

  const apply = (bot, { controls = {}, yaw, pitch = 0 }) => {
    Object.entries(controls).forEach(([name, on]) => bot.setControlState(name, Boolean(on)))
    if (isNum(yaw)) bot.look(yaw, pitch, true)
  }

  const pathWorld = () => {
    const bot = getBot()
    if (!bot.world) return null
    return { snapshot: liveSnapshot(bot.world, { minY: bot.game.minY, height: bot.game.height }) }
  }

  const steer = (token, a = {}) => {
    const decide = a.decide
    if (typeof decide !== 'function') throw badArgs('steer needs decide, a function')
    const timeoutS = a.timeoutS ?? DEFAULT_TIMEOUT_S
    if (!isNum(timeoutS) || timeoutS <= 0 || timeoutS > MAX_TIMEOUT_S) throw badArgs(`steer timeoutS must be a number in (0, ${MAX_TIMEOUT_S}]`)
    const bot = getBot()
    return act(token, { boundS: timeoutS, onTimeout: () => ({ status: 'timeout', pose: pose(getBot()) }) }, ctx => new Promise((resolve, reject) => {
      let ticks = 0
      const release = () => {
        bot.removeListener('physicsTick', onTick)
        bot.clearControlStates()
      }
      const onTick = () => {
        try {
          ctx.alive()
          ticks += 1
          const out = decide(pose(bot))
          if (out.done) {
            release()
            return resolve({ status: 'done', result: out.done, ticks })
          }
          apply(bot, out)
        } catch (err) {
          release()
          if (err?.code === 'cut') return reject(err)
          resolve({ status: 'failed', reason: reasonOf(err) })
        }
      }
      ctx.onAbort(release)
      bot.on('physicsTick', onTick)
    }))
  }

  return { steer, pathWorld }
}
