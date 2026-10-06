#!/usr/bin/env node
// Why JavaScript: ESM boundary over the AOT cljs bundle; routing lives in cljs, the real tool is loaded lazily in this process to avoid a second Node start (500 ms budget).
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { loadTools } from './agent-tools-loader.mjs'
const tools = loadTools(['ednWrite', 'workspaceGenerate', 'workspaceRoute', 'workspaceUsage', 'workspacePlayerUsage'])

const repo = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..')

export async function runBound (context, command, argv) {
  try {
    const args = tools.workspaceRoute(fileURLToPath(context), command, argv)
    const module = await import(new URL(`./${command}.mjs`, import.meta.url))
    if (argv.length === 1 && ['--help', '-h'].includes(argv[0])) {
      // each launcher checks only its own exports: usage comes from the tool's module, plans from the plan library
      const usage = command === 'plans' || command === 'blueprints' ? (await import('./plan-tools-lib.mjs')).usage[command] : module.usage
      process.stdout.write(`Workspace ${command}:\n${tools.workspacePlayerUsage(usage)}\n`)
      return 0
    }
    // a tool's own error text carries its raw usage line: cut the body/world plumbing from it too
    const write = process.stderr.write.bind(process.stderr)
    process.stderr.write = (text, ...rest) => write(typeof text === 'string' ? tools.workspacePlayerUsage(text) : text, ...rest)
    try {
      return await module.main(args)
    } finally {
      process.stderr.write = write
    }
  } catch (error) {
    process.stderr.write(`${error.message}\n`)
    return 2
  }
}
export async function main (argv = process.argv.slice(2)) {
  if (argv.length === 1 && ['--help', '-h'].includes(argv[0])) {
    process.stdout.write(`${tools.workspaceUsage}\n`)
    return 0
  }
  try {
    process.stdout.write(`${tools.ednWrite(tools.workspaceGenerate(argv, repo))}\n`)
    return 0
  } catch (error) {
    process.stderr.write(`${error.message}\n`)
    return 2
  }
}
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) process.exitCode = await main()
