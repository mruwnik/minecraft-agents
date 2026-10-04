// A body started by ./start must outlive whoever ran ./start: the driver's shell times out, gets stopped, or ends its
// turn, and each of those used to SIGTERM the body (12 kills on 2026-10-01). detach.mjs puts the command in its own session.
import test from 'node:test'
import assert from 'node:assert/strict'
import { execFileSync } from 'node:child_process'

const groupOf = pid => execFileSync('ps', ['-o', 'pgid=', '-p', String(pid)], { encoding: 'utf8' }).trim()

test('detach: the command runs on after the launcher has returned, in a process group of its own', () => {
  const pid = Number(execFileSync('node', [`${import.meta.dirname}/../tools/detach.mjs`, 'sleep', '30'], { encoding: 'utf8' }).trim())
  try {
    assert.ok(Number.isInteger(pid) && pid > 0, 'prints the pid of what it started')
    assert.doesNotThrow(() => process.kill(pid, 0), 'still running once detach.mjs has exited')
    assert.notEqual(groupOf(pid), groupOf(process.pid), 'a kill of the launcher\'s group does not reach it')
  } finally {
    process.kill(pid, 'SIGKILL')
  }
})
