#!/usr/bin/env node
// Why JavaScript: thin CLI launcher over the AOT cljs bundle; the command logic is agent-tools.changes (no compiler or JVM start, 500 ms budget).
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { loadTools } from './agent-tools-loader.mjs'
const tools = loadTools(['changesExecute', 'changesMain', 'changesOptions', 'changesUsage'])

export const usage = tools.changesUsage
export const options = tools.changesOptions
export const execute = tools.changesExecute
export const main = (argv = process.argv.slice(2)) => tools.changesMain(argv)
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) process.exitCode = await main()
