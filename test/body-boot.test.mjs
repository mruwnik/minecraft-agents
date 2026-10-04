// src/bot.mjs evaluates its whole module graph at start: a name read before the module that declares it has run
// (a TDZ in an import cycle) only shows when that graph really loads. Here the real body starts from a throwaway home
// whose world points at a local socket that hangs up at once: nothing can log in, so it never reaches a world, but
// every module runs, the composites register and the control API answers.
import test from 'node:test'
import assert from 'node:assert/strict'
import { spawn } from 'node:child_process'
import fs from 'node:fs'
import net from 'node:net'
import os from 'node:os'
import path from 'node:path'

const ROOT = path.join(import.meta.dirname, '..')

const listening = server => new Promise(resolve => server.listen(0, '127.0.0.1', () => resolve(server.address().port)))
const freePort = async () => {
  const probe = net.createServer()
  const port = await listening(probe)
  await new Promise(resolve => probe.close(resolve))
  return port
}
const waitUntil = async (fn, ms) => {
  const start = Date.now()
  while (!fn()) {
    if (Date.now() - start > ms) throw new Error('timed out waiting')
    await new Promise(r => setTimeout(r, 50))
  }
}
const post = (port, action, args = {}) => fetch(`http://127.0.0.1:${port}/${action}`, { method: 'POST', body: JSON.stringify(args) }).then(r => r.json())

test('the body boots offline: every module evaluates, the control API answers, the login is refused', async () => {
  const hangUp = net.createServer(socket => socket.destroy())
  const serverPort = await listening(hangUp)
  const apiPort = await freePort()
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'body-boot-'))
  const home = path.join(root, 'worlds', 'main', 'agents', 'BootProbe')
  const worldDir = path.join(root, 'worlds', 'main')
  fs.mkdirSync(home, { recursive: true })
  fs.mkdirSync(worldDir, { recursive: true })
  fs.writeFileSync(path.join(worldDir, 'world.json'), JSON.stringify({ host: '127.0.0.1', port: serverPort }))
  fs.writeFileSync(path.join(home, 'config.json'), JSON.stringify({ username: 'BootProbe', auth: 'offline', apiPort }))
  const body = spawn(process.execPath, [path.join(ROOT, 'src', 'bot.mjs'), home], { cwd: home })
  let out = ''
  body.stdout.on('data', d => { out += d })
  body.stderr.on('data', d => { out += d })
  try {
    await waitUntil(() => out.includes('control API on') || body.exitCode !== null, 30000)
    assert.equal(body.exitCode, null, out)
    const help = await post(apiPort, 'help')
    assert.equal(help.ok, true)
    const state = await post(apiPort, 'state')
    assert.equal(state.ok, false)
    const events = () => fs.existsSync(path.join(home, 'events.jsonl')) ? fs.readFileSync(path.join(home, 'events.jsonl'), 'utf8') : ''
    await waitUntil(() => events().includes('"disconnected"'), 10000)
    assert.doesNotMatch(out + events(), /ReferenceError|before initialization|is not defined|uncaught/)
  } finally {
    body.kill('SIGKILL')
    hangUp.close()
    fs.rmSync(root, { recursive: true, force: true })
  }
})
