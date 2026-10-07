// Why JavaScript: node --test file for tools/integration-worktree.sh, run against a throwaway fake repo.
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { spawnSync } from 'node:child_process'

const script = path.resolve(import.meta.dirname, 'integration-worktree.sh')

const sh = (cwd, cmd, args) => spawnSync(cmd, args, { cwd, encoding: 'utf8' })

function fakeRepo() {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'iwt-test-'))
  const repo = path.join(root, 'repo')
  for (const d of ['tools/view', 'engine', 'dashboard']) fs.mkdirSync(path.join(repo, d), { recursive: true })
  for (const d of ['engine', 'dashboard']) fs.writeFileSync(path.join(repo, d, 'keep'), '')
  fs.copyFileSync(script, path.join(repo, 'tools/integration-worktree.sh'))
  fs.writeFileSync(path.join(repo, 'tools/compile'), '#!/bin/sh\nexit 0\n', { mode: 0o755 })
  fs.writeFileSync(path.join(repo, 'tools/view/build-cljs.mjs'), '')
  fs.writeFileSync(path.join(repo, '.gitignore'), 'worlds/\ntextures/\nnode_modules/\nengine/test/fixtures/\n')
  for (const d of ['.', 'engine', 'dashboard']) fs.mkdirSync(path.join(repo, d, 'node_modules'), { recursive: true })
  fs.mkdirSync(path.join(repo, 'worlds/.accounts'), { recursive: true })
  fs.mkdirSync(path.join(repo, 'worlds/claude'), { recursive: true })
  fs.writeFileSync(path.join(repo, 'worlds/claude/biomes.json'), '{}')
  fs.writeFileSync(path.join(repo, 'worlds/claude/world.json'), '{"fake":true}')
  fs.writeFileSync(path.join(repo, 'worlds/.accounts/cache'), 'secret')
  fs.mkdirSync(path.join(repo, 'textures'))
  fs.mkdirSync(path.join(repo, 'engine/test/fixtures/pathfinding'), { recursive: true })
  sh(repo, 'git', ['init', '-q'])
  sh(repo, 'git', ['add', '.'])
  sh(repo, 'git', ['-c', 'user.name=t', '-c', 'user.email=t@t', 'commit', '-q', '-m', 'x'])
  return { root, repo }
}

test('the worktree links worlds/claude to the main one and nothing else under worlds/, and --remove keeps the target', () => {
  const { root, repo } = fakeRepo()
  try {
    const wt = path.join(root, 'wt')
    const r = sh(repo, path.join(repo, 'tools/integration-worktree.sh'), ['HEAD', wt])
    assert.equal(r.status, 0, r.stderr)
    assert.equal(fs.readlinkSync(path.join(wt, 'worlds/claude')), path.join(repo, 'worlds/claude'))
    assert.equal(fs.lstatSync(path.join(wt, 'worlds')).isSymbolicLink(), false)
    assert.deepEqual(fs.readdirSync(path.join(wt, 'worlds')), ['claude'])
    assert.equal(fs.existsSync(path.join(wt, 'worlds/claude/world.json')), true)
    const rm = sh(repo, path.join(repo, 'tools/integration-worktree.sh'), ['--remove', wt])
    assert.equal(rm.status, 0, rm.stderr)
    assert.equal(fs.existsSync(wt), false)
    assert.equal(fs.existsSync(path.join(repo, 'worlds/claude/world.json')), true)
  } finally { fs.rmSync(root, { recursive: true, force: true }) }
})
