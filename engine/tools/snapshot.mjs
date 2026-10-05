#!/usr/bin/env node
// Why JavaScript: joins the AOT cljs tool (agent-tools.snapshot) to the JS software renderer tools/view/render.mjs (graphics/binary); no compiler or JVM start, 500 ms budget.
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { loadTools } from './agent-tools-loader.mjs'
const tools = loadTools(['snapshotMain', 'snapshotUsage'])

export const usage = tools.snapshotUsage
// the renderer is imported only when drawing, so --help and refused calls start fast
const render = async opts => (await import('../../tools/view/render.mjs')).renderView(opts)
export const main = (argv = process.argv.slice(2)) => tools.snapshotMain(argv, render)
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  if (process.argv.length === 3 && ['--help', '-h'].includes(process.argv[2])) {
    process.stdout.write(`${usage}\n`)
  } else {
    process.exitCode = await main()
  }
}
