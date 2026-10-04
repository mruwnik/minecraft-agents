#!/usr/bin/env node
// Why JavaScript: launcher joining the AOT cljs tool (agent-tools.snapshot) to the JS software renderer (tools/view/render.mjs, graphics/binary); Thin Node launcher over the AOT cljs bundle dashboard/out/agent-tools.cjs; runs without starting a compiler or JVM (500 ms startup budget).
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import tools from './agent-tools-loader.mjs'

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
