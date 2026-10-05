#!/usr/bin/env node
// Why JavaScript: thin launcher over the AOT cljs bundle; the world clock logic is agent-tools.time (no compiler or JVM start, 500 ms budget).
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { loadTools } from './agent-tools-loader.mjs'
const tools = loadTools(['timeClock', 'timeExecute', 'timeMain', 'timeOptions', 'timeUsage'])

export const usage = tools.timeUsage
export const clock = tools.timeClock
export const options = tools.timeOptions
export const execute = tools.timeExecute
export const main = (argv = process.argv.slice(2)) => tools.timeMain(argv)
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) process.exitCode = await main()
