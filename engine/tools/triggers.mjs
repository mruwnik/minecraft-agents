#!/usr/bin/env node
// Why JavaScript: Node entry point/launcher for the AOT bundle; the command logic is agent-tools.triggers.
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import tools from './agent-tools-loader.mjs'

export const usage = tools.triggersUsage
export const requestFor = argv => tools.triggersRequestFor(argv)
export const main = (argv = process.argv.slice(2)) => tools.triggersMain(argv)
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) process.exitCode = await main()
