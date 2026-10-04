// Which event a task's result is written under (src/taskresult.mjs). Every task's result must reach events.jsonl and bot.log
// once: a fast one's as task_result (the caller already holds it, so a wait must not wake for it), a slow one's as task_done
import test from 'node:test'
import assert from 'node:assert/strict'
import { resultEvent } from '../src/taskresult.mjs'
import { wakeWorthy } from '../src/cli.mjs'

const result = { task: 7, action: 'goto', seconds: 3, ok: true, pos: { x: 1, y: 64, z: 2 } }
const failed = { task: 8, action: 'flock.lead', seconds: 29, ok: false, error: 'no path', brought: 'cow:0' }
for (const [title, finished, given, expected] of [
  ['finished inside its timeout: task_result, the log copy of what the caller got', true, result, { type: 'task_result', data: result }],
  ['a failure inside the timeout is a task_result too', true, failed, { type: 'task_result', data: failed }],
  ['outlasted its timeout: task_done, which the wait stream carries to the caller', false, result, { type: 'task_done', data: result }],
  ['a failure after the timeout is a task_done', false, failed, { type: 'task_done', data: failed }]
]) test(`resultEvent: ${title}`, () => assert.deepEqual(resultEvent(finished, given), expected))

// the wait contract (AGENT_GUIDE.md): ./mc wait wakes for task_done, never for task_result
for (const [title, event, expected] of [
  ['task_result never ends a wait: the caller already holds the result', { type: 'task_result', ...result }, false],
  ['a failed task_result neither', { type: 'task_result', ...failed }, false],
  ['task_done still does', { type: 'task_done', ...result }, true],
  ['a failed task_done still does', { type: 'task_done', ...failed }, true]
]) test(`wakeWorthy: ${title}`, () => assert.equal(wakeWorthy(event, 'Steve'), expected))
