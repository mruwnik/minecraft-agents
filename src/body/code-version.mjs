// Which code this body runs, and a word to the driver when the shared code changes under it.
import fs from 'node:fs'
import path from 'node:path'
import { execFileSync } from 'node:child_process'
import { restartAdvice } from '../restart.mjs'
import { staleKey, staleCode, codeVersion, isNight } from '../lib.mjs'
import { ROOT } from './home.mjs'
import { emit } from './events.mjs'
import { libraryFiles } from './runner.mjs'
import { bot, ready } from './state.mjs'

// the shared code is loaded once, at start: tell the driver when it has changed since, once per batch of edits
const codeLoaded = Date.now()
// and WHICH code that was, read from git once at start and said in the join line. A body started between two saves of
// a shared tree runs half of somebody's change and throws something that is in nobody's diff (#140). Never fatal: a
// body with no git, or no repo, joins anyway and says it does not know.
export const codeHere = (() => {
  const run = args => execFileSync('git', args, { cwd: ROOT, encoding: 'utf8', timeout: 5000, stdio: ['ignore', 'pipe', 'ignore'] })
  try {
    return codeVersion({
      head: run(['rev-parse', '--short', 'HEAD']).trim(),
      changed: run(['status', '--porcelain']).split('\n').filter(Boolean).map(line => line.slice(3).trim())
    })
  } catch {
    return codeVersion({ head: null })
  }
})()
let staleTold = ''
setInterval(() => {
  if (!ready) return
  // was a hard-coded list of src/ files: a split into src/lib/ or src/body/ modules would announce nothing for
  // an edit to any of them, so this now walks src/ itself, the same way tools/check-code.mjs's codeFiles() does
  const files = [...fs.readdirSync(path.join(ROOT, 'src'), { recursive: true }).map(f => path.join('src', f)).filter(f => f.endsWith('.mjs')), ...libraryFiles().map(f => `library/${f}`)]
  const mtimes = Object.fromEntries(files.map(f => [f, fs.statSync(path.join(ROOT, f), { throwIfNoEntry: false })?.mtimeMs]))
  const stale = staleCode(codeLoaded, mtimes, Date.now())
  if (!stale || staleKey(stale, mtimes) === staleTold) return
  staleTold = staleKey(stale, mtimes)
  emit('code_updated', { files: stale.join(' '), advice: restartAdvice({ day: !isNight(bot.time.timeOfDay), timeOfDay: bot.time.timeOfDay, at: Date.now() }) })
}, 60000)
