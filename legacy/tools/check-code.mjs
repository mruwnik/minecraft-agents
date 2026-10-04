// the start gate (#148): run by start-body before anything else touches the code. A body that starts on a half-saved file dies
// at its first import with a stack nobody reads; this names the file and line instead. An editor mid-save is back in seconds, so a
// failing check is tried again every 5 s for [wait] seconds (60) before it gives up.
// node tools/check-code.mjs <bot root> [wait seconds]: prints one line per broken file, exit 1; silent, exit 0 when all parse
import fs from 'node:fs'
import path from 'node:path'
import { spawnSync } from 'node:child_process'
import { checkFailure } from '../src/cli.mjs'

const root = path.resolve(process.argv[2] ?? path.join(import.meta.dirname, '..'))
const wait = Number(process.argv[3] ?? 60)

const codeFiles = () => ['src', 'library']
  .filter(dir => fs.existsSync(path.join(root, dir)))
  .flatMap(dir => fs.readdirSync(path.join(root, dir), { recursive: true }).map(f => path.join(dir, f)))
  .filter(file => file.endsWith('.mjs'))
  .sort()

const failures = () => codeFiles()
  .map(file => ({ file, run: spawnSync(process.execPath, ['--check', file], { cwd: root, encoding: 'utf8' }) }))
  .filter(({ run }) => run.status !== 0)
  .map(({ file, run }) => checkFailure(file, run.stderr))

const deadline = Date.now() + wait * 1000
let broken = failures()
while (broken.length && Date.now() < deadline) {
  await new Promise(resolve => setTimeout(resolve, 5000))
  broken = failures()
}
if (broken.length) process.stdout.write(broken.map(line => line + '\n').join(''))
process.exit(broken.length ? 1 : 0)
