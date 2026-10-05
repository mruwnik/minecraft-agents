#!/usr/bin/env node
// Why JavaScript: Node entry point/launcher for the AOT bundle; the command logic is agent-tools.entities.
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { loadTools } from './agent-tools-loader.mjs'
const tools = loadTools(['entitiesMain', 'entitiesOptions', 'entitiesUsage'])

export const usage = tools.entitiesUsage
export const options = tools.entitiesOptions
export const main = (argv = process.argv.slice(2)) => tools.entitiesMain(argv)
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) process.exitCode = await main()
