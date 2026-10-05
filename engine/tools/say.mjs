#!/usr/bin/env node
// Why JavaScript: Node entry point/launcher for the AOT bundle; the command logic is agent-tools.say.
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { loadTools } from './agent-tools-loader.mjs'
const tools = loadTools(['sayMain', 'sayRequestFor', 'sayUsage'])

export const usage = tools.sayUsage
export const requestFor = argv => tools.sayRequestFor(argv)
export const main = (argv = process.argv.slice(2)) => tools.sayMain(argv)
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) process.exitCode = await main()
