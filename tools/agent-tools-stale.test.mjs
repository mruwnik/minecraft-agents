// Why JavaScript: node --test file for tools/agent-tools-stale (the check tools/compile uses to add the agent-tools build to a world-test compile).
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { spawnSync } from 'node:child_process'

const script = path.resolve(import.meta.dirname, 'agent-tools-stale')
const source = 'dashboard/src/agent_tools/api.cljs'

const repo = () => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'ats-test-'))
  fs.mkdirSync(path.join(root, 'dashboard/src/agent_tools'), { recursive: true })
  fs.mkdirSync(path.join(root, 'dashboard/out'), { recursive: true })
  fs.writeFileSync(path.join(root, source), '')
  return root
}
const needsBuild = root => spawnSync(script, [root]).status === 0
const touch = (file, secondsAgo) => {
  const t = new Date(Date.now() - secondsAgo * 1000)
  fs.utimesSync(file, t, t)
}

test('a missing bundle needs a build', () => {
  const root = repo()
  assert.equal(needsBuild(root), true)
  fs.rmSync(root, { recursive: true })
})

test('a bundle newer than every source is current, one older than a source is stale', () => {
  const root = repo()
  const bundle = path.join(root, 'dashboard/out/agent-tools.cjs')
  fs.writeFileSync(bundle, '')
  touch(path.join(root, source), 60)
  assert.equal(needsBuild(root), false)
  touch(bundle, 120)
  assert.equal(needsBuild(root), true)
  fs.rmSync(root, { recursive: true })
})
