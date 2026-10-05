#!/usr/bin/env node
// Why JavaScript: thin CLI launcher over the AOT cljs bundle; the command logic is agent-tools.map (no compiler or JVM start, 500 ms budget).
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { loadTools } from './agent-tools-loader.mjs'
const tools = loadTools(['mapExecute', 'mapFilters', 'mapMain', 'mapOptions', 'mapSummary', 'mapTtl', 'mapUsage', 'mapValidateZone'])

export const filters = tools.mapFilters
export const summary = tools.mapSummary
export const validateZone = tools.mapValidateZone
export const usage = tools.mapUsage
export const options = tools.mapOptions
export const execute = tools.mapExecute
export const ttl = tools.mapTtl
export const main = (argv = process.argv.slice(2)) => tools.mapMain(argv)
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) process.exitCode = await main()
