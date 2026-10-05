#!/usr/bin/env node
// Why JavaScript: Node entry point/launcher for the AOT bundle; the command logic is agent-tools.jobs.
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { loadTools } from './agent-tools-loader.mjs'
const tools = loadTools(['jobsMain', 'jobsRequestFor', 'jobsUsage'])

export const usage = tools.jobsUsage
export const requestFor = argv => tools.jobsRequestFor(argv)
export const main = (argv = process.argv.slice(2)) => tools.jobsMain(argv)
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) process.exitCode = await main()
