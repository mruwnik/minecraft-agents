#!/usr/bin/env node
// Why JavaScript: stable CLI launcher, command logic lives in agent-tools.map; Thin Node launcher over the AOT cljs bundle dashboard/out/agent-tools.cjs; runs without starting a compiler or JVM (500 ms startup budget).
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import tools from './agent-tools-loader.mjs'

export const filters = tools.mapFilters
export const summary = tools.mapSummary
export const validateZone = tools.mapValidateZone
export const usage = tools.mapUsage
export const options = tools.mapOptions
export const execute = tools.mapExecute
export const ttl = tools.mapTtl
export const main = (argv = process.argv.slice(2)) => tools.mapMain(argv)
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) process.exitCode = await main()
