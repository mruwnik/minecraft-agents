#!/usr/bin/env node
// Why JavaScript: ESM boundary over the AOT cljs bundle; routing lives in cljs, the real tool is loaded lazily in this process to avoid a second Node start (500 ms budget).
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { loadTools } from './agent-tools-loader.mjs'
const tools = loadTools(['ednWrite', 'workspaceGenerate', 'workspaceRoute', 'workspaceUsage', 'workspacePlayerUsage', 'workspacePlayerError', 'workspacePlayerEdn', 'workspaceErrorEdn'])

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
    // a tool's own error carries its raw usage line (EDN on stdout, text on stderr): cut the body/world plumbing from both
    const writeErr = process.stderr.write.bind(process.stderr)
    const writeOut = process.stdout.write.bind(process.stdout)
    process.stderr.write = (text, ...rest) => writeErr(typeof text === 'string' ? tools.workspacePlayerError(text) : text, ...rest)
    process.stdout.write = (text, ...rest) => writeOut(typeof text === 'string' ? tools.workspacePlayerEdn(text) : text, ...rest)
    try {
      return await module.main(args)
    } finally {
      process.stderr.write = writeErr
      process.stdout.write = writeOut
    }
  } catch (error) {
    process.stdout.write(`${tools.workspaceErrorEdn(tools.workspacePlayerError(error.message))}\n`)
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
    process.stdout.write(`${tools.workspaceErrorEdn(error.message)}\n`)
    return 2
  }
}
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) process.exitCode = await main()
