// What every part of the body shares: the mineflayer client, its registry, whether it is in the world, and the task
// that owns it. An ESM binding can only be reassigned by the module that declares it, so each one another module
// reassigns has its setX beside it (the same holds for the bindings that live with their own code elsewhere).
import pf from 'mineflayer-pathfinder'
import vec3 from 'vec3'
import { createSlowScanReporter } from '../performance.mjs'
import { emit } from './events.mjs'

export const { pathfinder, Movements, goals } = pf
export const { Vec3 } = vec3
export const reportPerformance = createSlowScanReporter({ emit })

export let bot = null
export const setBot = v => { bot = v }
export let mcData = null
export const setMcData = v => { mcData = v }
export let ready = false
export const setReady = v => { ready = v }
export let task = null // { id, name, gen, started }
export const setTask = v => { task = v }
export let gen = 0
export const setGen = v => { gen = v }
// a long action calls `const alive = cancelGuard()` when it starts and `alive()` in every loop: once it has been cancelled
// or superseded it must stop, or it keeps fighting the next command for the body
export const cancelGuard = () => { const mine = gen; return () => { if (gen !== mine) throw new Error('cancelled') } }
// A composite may finish restoring one job block after cancellation. This private token cannot be supplied by a CLI caller.
export const ROLLBACK_PLACE = Symbol('rollback-place')

export const pos = () => bot?.entity ? roundVec(bot.entity.position) : null
export const roundVec = v => ({ x: Math.round(v.x * 10) / 10, y: Math.round(v.y * 10) / 10, z: Math.round(v.z * 10) / 10 })
