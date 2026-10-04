#!/usr/bin/env node
// Why JavaScript: Thin Node launcher over the AOT cljs bundle dashboard/out/agent-tools.cjs; runs without starting a compiler or JVM (500 ms startup budget).
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import tools from './agent-tools-loader.mjs'

export const usage = tools.changesUsage
export const options = tools.changesOptions
export const execute = tools.changesExecute
export const main = (argv = process.argv.slice(2)) => tools.changesMain(argv)
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) process.exitCode = await main()
