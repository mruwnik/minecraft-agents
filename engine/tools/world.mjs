#!/usr/bin/env node
// Why JavaScript: Node entry point/launcher for the AOT bundle; the command logic is agent-tools.world.
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { loadTools } from './agent-tools-loader.mjs'
const tools = loadTools(['worldMain', 'worldRequestFor', 'worldUsage'])

export const usage = tools.worldUsage
export const requestFor = argv => tools.worldRequestFor(argv)
export const main = (argv = process.argv.slice(2)) => tools.worldMain(argv)
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) process.exitCode = await main()
