// Driving the body (help section control).
import { controlTrace } from '../control-trace.mjs'
import { helpText, argsUsage, docText, PRIMITIVES, mayDig } from '../../lib.mjs'
import { executeFlow, executeLegacySteps, parseFlowEDN, resolveFlowAction, FLOW_OBSERVATIONS as observations } from '../../flow.mjs'
import { composites, CLI_ONLY } from '../runner.mjs'
import { bot, task, cancelGuard, pos, roundVec } from '../state.mjs'
import { reflexes, setReflexes, fighting, setFighting, fightStart, setFightStart } from '../reflexes.mjs'
import { refusalFor, scheduler, stopAllJobs } from '../jobs.mjs'
import { explainFailure } from '../explain.mjs'
import { long, quick } from './tables.mjs'
import { useMoves } from './move.mjs'

export const controlLong = {
  // several actions in one call; stops at the first failure and reports how far it got
  async run (a) {
    const program = typeof a.steps === 'string'
      ? parseFlowEDN(a.steps)
      : a.steps
    const host = {
      actions: [...new Set([...Object.keys(long), ...Object.keys(quick)])],
      observations,
      alive: cancelGuard(),
      waitTicks: n => bot.waitForTicks(n),
      observe: async (name, args) => {
        const read = quick[name]
        if (!observations.includes(name) || typeof read !== 'function') throw new Error(`run: observation ${name} is unavailable`)
        return read(args)
      },
      onProgress: detail => {
        if (!task) return
        task.progress = { ...(task.progress ?? {}), ...detail }
        if (task.jobId) scheduler?.report(task.jobId, detail.waiting ? 'job_waiting' : 'job_progress', {
          ...detail, ...(detail.waiting ? {} : { waiting: false }), progress: task.progress
        })
      },
      act: async (name, args, legacyStep) => {
        const fn = resolveFlowAction(name, long, quick)
        const where = legacyStep ? `step ${legacyStep.index}/${legacyStep.total} (${name})` : `flow/${name}`
        if (!fn) throw new Error(legacyStep ? `${where}: unknown action` : `flow: no action called ${name}`)
        const refusal = refusalFor(name, args)
        if (refusal) throw new Error(`${where}: ${refusal}`)
        useMoves(mayDig(name, args))
        return Promise.resolve().then(() => fn(args)).catch(e => { throw new Error(`${where}: ${explainFailure(e.message)}`) })
      }
    }
    if (Array.isArray(program) && ['action', 'seq', 'when', 'any'].includes(program[0])) return executeFlow(program, host)
    return executeLegacySteps(program, host)
  }
}

export const controlQuick = {
  // the catalogue every driver starts from: each action with its arguments, and for a composite what hands the body back.
  // It is built from the dispatch tables themselves, so it cannot drift from what this body can actually do.
  help (a) {
    const served = name => Boolean(long[name] || quick[name]) || CLI_ONLY.includes(name)
    const entries = [
      ...Object.entries(PRIMITIVES).filter(([name]) => served(name)).map(([name, p]) => ({ name, ...p })),
      ...[...composites].map(([name, mod]) => ({ name, args: argsUsage(mod.args), doc: docText(mod.doc), stops: mod.stops ?? 'the usual hand-backs' }))
    ]
    const catalogue = helpText(a.topic, entries)
    return { text: `${catalogue}\n\nLong and body-changing actions queue by default and return a job ID. Add wait=true for a bounded synchronous reply; use interrupt=true to replace the current owner after cleanup. Inspect with job/jobs, cancel one ID, resume or discard a held queue, and stop to cancel all work.` }
  },

  // raw movement for debugging: hold a control (forward/back/left/right/jump/sprint) for ms
  async control (a) {
    const from = pos()
    const finishTrace = a.trace ? controlTrace(bot, { duration: a.ms ?? 1000 }) : null
    const alive = cancelGuard()
    try {
      bot.setControlState(a.state ?? 'forward', true)
      const until = Date.now() + (a.ms ?? 1000)
      while (Date.now() < until) { alive(); await bot.waitForTicks(1) }
      return { from, to: pos(), onGround: bot.entity.onGround, velocity: roundVec(bot.entity.velocity), ...(finishTrace ? { trace: finishTrace() } : {}) }
    } finally {
      finishTrace?.()
      bot.setControlState(a.state ?? 'forward', false)
    }
  },

  // stop is an explicit all-work cancellation; normal chat never calls it implicitly.
  stop () { return stopAllJobs() },
  // without on= it only tells: a bare `reflexes` "to look" used to switch them all off, silently
  reflexes (a) {
    if (a.on === undefined) return { reflexes, note: 'unchanged: reflexes on=true|false switches them' }
    setReflexes(!!a.on)
    if (!reflexes) { bot.pvp.stop(); setFighting(null); setFightStart(null) }
    return { reflexes }
  }
}
