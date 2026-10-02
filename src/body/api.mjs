// The HTTP control API: POST /<action> with the arguments as JSON. src/bot.mjs serves it on cfg.apiPort.
import { didYouMean, trackReads, ignoredParams, offlineError } from '../lib.mjs'
import { neededArgs } from '../needs.mjs'
import { mayRunBesideOwner } from '../job-policy.mjs'
import { ready } from './state.mjs'
import { yieldUntil, reconnectTimer, connect, lastDriven, setLastDriven } from './connection.mjs'
import { cancelTask, taskId, setTaskId, jobShelf, scheduler, submitJob, stopAllJobs, jobControl } from './jobs.mjs'
import { long, quick } from './actions/tables.mjs'

export const controlApi = (req, res) => {
  let body = '', bodyBytes = 0, bodyTooLarge = false
  req.on('data', c => {
    bodyBytes += c.length
    if (bodyBytes > 2 * 1024 * 1024) { bodyTooLarge = true; body = ''; return }
    if (!bodyTooLarge) body += c
  })
  req.on('end', async () => {
    if (bodyTooLarge) {
      res.writeHead(413, { 'content-type': 'application/json' })
      res.end(JSON.stringify({ ok: false, error: 'request body exceeds 2 MiB' }))
      return
    }
    const name = new URL(req.url, 'http://x').pathname.slice(1)
    let out
    try {
      // a primitive's required argument is named before it runs (find_blocks name=oak_log used to answer "unknown block
      // name: undefined"), and the spelling a driver reaches for is folded into the one the action reads (src/needs.mjs)
      const { args: filled, error: lacking } = neededArgs(name, body ? JSON.parse(body) : {})
      const tracked = trackReads(filled)
      const args = tracked.args
      // help is answered even before the body is connected: a driver reads it first of all
      if (name === '' || name === 'help') out = { ok: true, ...quick.help(args) }
      else if (args.queue === false && args.interrupt !== true) out = { ok: false, error: 'queue=false is no longer supported; jobs queue by default. Use interrupt=true to cancel the current job safely before urgent work' }
      else if (!ready && !['events', 'job', 'jobs', 'cancel', 'resume', 'discard', 'stop'].includes(name)) out = { ok: false, error: offlineError(yieldUntil, Date.now()) }
      else if (name === 'resume' && !ready && yieldUntil > Date.now()) { clearTimeout(reconnectTimer); connect(); out = { ok: true, reconnecting: true } }
      else if (lacking) out = { ok: false, error: lacking }
      else if (['job', 'jobs', 'cancel', 'resume', 'discard'].includes(name)) out = jobControl(name, args)
      else if (name === 'stop') out = stopAllJobs()
      else if (name === 'quit') {
        const stopping = stopAllJobs()
        if (stopping.restorationPending) {
          out = { ok: false, cleanup: 'failed', error: stopping.restorationPending, note: 'body remains online for recovery' }
        } else if (stopping.stopped != null) {
          const settled = await scheduler.wait(stopping.stopped, 120000)
          if (!settled || !['completed', 'failed', 'cancelled', 'interrupted'].includes(settled.status)) {
            out = { ok: false, cleanup: 'pending', job: stopping.stopped, error: 'the active job has not finished cancellation cleanup; body remains online' }
          } else if (settled.status === 'failed') out = { ok: false, cleanup: 'failed', job: stopping.stopped, error: settled.error ?? settled.result?.error ?? 'job cleanup failed; body remains online for recovery' }
          else out = { ok: true, ...quick.quit(args) }
        } else out = { ok: true, ...quick.quit(args) }
      }
      else if (mayRunBesideOwner(name, { quick, long })) { out = { ok: true, ...(await quick[name](args)) } }
      else if (long[name] || quick[name]) {
        const { queue: _queue, interrupt = false, sync = false, verbose = false, wait, waitMs, ...rest } = args
        const waitForResult = sync || (name !== 'smelt' && wait === true)
        const waitDuration = Number.isFinite(Number(waitMs)) ? Math.max(0, Math.min(120000, Number(waitMs))) : 120000
        const actionArgs = name === 'smelt' && wait !== undefined ? { ...rest, wait } : rest
        setLastDriven(Date.now())
        if (interrupt) {
          const { job: accepted, afterCleanup } = scheduler.interrupt({ name, args: actionArgs, given: tracked.given }, reason => cancelTask(reason), { verbose, notify: tracked.given?.automatic !== true })
          setTaskId(Math.max(taskId, accepted.id))
          out = { ok: true, status: 'queued', job: accepted.id, urgent: true, afterCleanup, held: jobShelf.snapshot().held?.reason }
          if (waitForResult) {
            const finished = await scheduler.wait(accepted.id, waitDuration)
            if (finished && ['completed', 'failed', 'cancelled', 'interrupted'].includes(finished.status)) out = { ok: finished.status === 'completed', job: accepted.id, ...finished.result }
            else out = { ok: true, status: jobShelf.get(accepted.id)?.status ?? 'queued', job: accepted.id, urgent: true, afterCleanup, held: jobShelf.snapshot().held?.reason }
          }
        } else {
          const accepted = submitJob(name, actionArgs, tracked.given, { verbose, notify: tracked.given?.automatic !== true })
          out = { ok: true, status: jobShelf.get(accepted.id)?.status ?? 'queued', job: accepted.id, held: jobShelf.snapshot().held?.reason }
          if (waitForResult) {
            const finished = await scheduler.wait(accepted.id, waitDuration)
            if (finished && ['completed', 'failed', 'cancelled', 'interrupted'].includes(finished.status)) out = { ok: finished.status === 'completed', job: accepted.id, ...finished.result }
            else out = { ok: true, status: jobShelf.get(accepted.id)?.status ?? 'queued', job: accepted.id, held: jobShelf.snapshot().held?.reason }
          }
        }
      }
      else out = { ok: false, error: didYouMean(name, [...Object.keys(long), ...Object.keys(quick)]) }
      const ignored = ignoredParams(tracked.unread(), out.ok, String(quick[name] ?? long[name] ?? ''))
      if (ignored && out.status !== 'running') out = { ...out, ignored }
    } catch (e) {
      out = { ok: false, error: e.message }
    }
    res.writeHead(200, { 'content-type': 'application/json' })
    res.end(JSON.stringify(out))
  })
}
