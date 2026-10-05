#!/usr/bin/env node
// Why JavaScript: thin CLI entry point over plan-tools-lib and the AOT cljs bundle (no compiler or JVM start, 500 ms budget).
import { fileURLToPath } from 'node:url'
import path from 'node:path'
import { execute } from './plan-tools-lib.mjs'

export const main = (argv = process.argv.slice(2), output = text => process.stdout.write(text)) => execute('blueprints', argv, output)
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) process.exitCode = await main()
