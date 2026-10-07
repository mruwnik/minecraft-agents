// Why JavaScript: thin helper for the tools/world-test.mjs launcher (which res-slot kinds a run holds).
import fs from 'node:fs'
import path from 'node:path'
// A run holds the body slot (unless an outer res-slot already does: RES_SLOT_HELD lists those kinds). --allow-time takes no further slot: the runner's phase-shared time lock (/tmp/mc-time-lock) lets day cases of several runs go together and night cases together.
export const slotKinds = (args, heldKinds = '') => ['body'].filter((k) => !heldKinds.split(',').includes(k))

export const slotArgv = (args, resSlot, node, script, heldKinds = '') =>
  [...slotKinds(args, heldKinds).flatMap((k) => [resSlot, k, '--']), node, script, ...args]

// One body name = one account and one plot: a second concurrent run on it would kick the first. A claim file <dir>/world-body.<name>.pid holds the run's pid; a dead holder is stale.
export const bodyName = (args) => (args.includes('--body') ? args[args.indexOf('--body') + 1] : 'ProbeFixture')

const claimFile = (dir, name) => path.join(dir, `world-body.${name}.pid`)
export const claimBody = (dir, name, pid, alive) => {
  fs.mkdirSync(dir, { recursive: true })
  for (let tries = 0; tries < 3; tries++) {
    try { fs.writeFileSync(claimFile(dir, name), String(pid), { flag: 'wx' }); return { ok: true } } catch (e) { if (e.code !== 'EEXIST') throw e }
    const holder = Number(fs.readFileSync(claimFile(dir, name), 'utf8'))
    if (holder && alive(holder)) return { ok: false, holder, why: `body ${name} is already in use by a world-test run (pid ${holder}); wait for it or use another --body` }
    // another runner may have replaced the stale claim while we checked it: remove only the one we saw
    if (Number(fs.readFileSync(claimFile(dir, name), 'utf8')) === holder) fs.rmSync(claimFile(dir, name), { force: true })
  }
  return { ok: false, holder: 0, why: `body ${name}: could not take the claim file, retry` }
}
export const releaseBody = (dir, name, pid) => {
  try { if (Number(fs.readFileSync(claimFile(dir, name), 'utf8')) === pid) fs.rmSync(claimFile(dir, name), { force: true }) } catch {}
}
