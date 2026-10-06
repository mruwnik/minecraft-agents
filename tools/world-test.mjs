#!/usr/bin/env node
// Why JavaScript: thin entry point; the runner is cljs (dashboard/src/world_test/runner.cljs), run from the
// ahead-of-time compiled bundle dashboard/out/world-test.cjs (build it with: tools/compile dashboard world-test).
//   node tools/world-test.mjs [fixture.edn|dir ...] [--tag T] [--match TEXT] [--repeat N] [--body NAME] [--world W] [--first-plot I] [--card ID] [--results FILE] [--list] [--allow-time --time-log F]
import fs from 'node:fs'
import path from 'node:path'
import { createRequire } from 'node:module'
import { spawn } from 'node:child_process'
import { slotArgv, bodyName, claimBody, releaseBody } from './world-test-slots.mjs'

const bundle = path.join(import.meta.dirname, '..', 'dashboard', 'out', 'world-test.cjs')
if (!fs.existsSync(bundle)) {
  console.error(`the world-test bundle is not built (${bundle}); build it with: tools/compile dashboard world-test`)
  process.exit(2)
}
// A second run on the same --body is refused (exit 75, like a busy slot). One body slot for the whole run (a run keeps one probe body at a time, restarted per case), plus the time slot with --allow-time: it re-executes itself under tools/res-slot.
const args = process.argv.slice(2)
if (!process.env.WORLD_TEST_SLOT_HELD && !args.includes('--list')) {
  const claimDir = process.env.RES_SLOT_DIR ?? '/tmp/mc-res', body = bodyName(args)
  const claim = claimBody(claimDir, body, process.pid, (pid) => { try { process.kill(pid, 0); return true } catch (e) { return e.code === 'EPERM' } })
  if (!claim.ok) { console.error(`world-test: ${claim.why}`); process.exit(75) }
  process.on('exit', () => releaseBody(claimDir, body, process.pid))
  const [cmd, ...argv] = slotArgv(args, path.join(import.meta.dirname, 'res-slot'), process.execPath, process.argv[1], process.env.RES_SLOT_HELD ?? '')
  // async, so INT/TERM reach us: pass them on to the run and exit (releasing the claim) when it ends
  const child = spawn(cmd, argv, { stdio: 'inherit', env: { ...process.env, WORLD_TEST_SLOT_HELD: '1' } })
  for (const sig of ['SIGINT', 'SIGTERM']) process.on(sig, () => child.kill(sig))
  child.on('close', (code) => process.exit(code ?? 1))
} else {
  process.env.WORLD_TEST_REPO = path.join(import.meta.dirname, '..')
  process.exitCode = await createRequire(import.meta.url)(bundle).main(args)
}
