#!/usr/bin/env node
// Why JavaScript: Thin Node launcher over the AOT cljs bundle dashboard/out/agent-tools.cjs; runs without starting a compiler or JVM (500 ms startup budget).
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import tools from './agent-tools-loader.mjs'

const repo = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..')

export async function runBound (context, command, argv) {
  try {
    const args = tools.workspaceRoute(fileURLToPath(context), command, argv)
    const module = await import(new URL(`./${command}.mjs`, import.meta.url))
    if (argv.length === 1 && ['--help', '-h'].includes(argv[0])) {
      const usage = command === 'drive' ? tools.driveUsage
        : command === 'plans' || command === 'blueprints' ? tools.planUsage[command] : module.usage
      process.stdout.write(`Workspace ${command}: omit the body and --world/--worlds/--state/--repo/--repo-root shown below; these are supplied from context.edn.\n${usage}\n`)
      return 0
    }
    return await module.main(args)
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
