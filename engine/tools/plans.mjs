#!/usr/bin/env node
// Why JavaScript: thin CLI entry point over plan-tools-lib; Thin Node launcher over the AOT cljs bundle dashboard/out/agent-tools.cjs; runs without starting a compiler or JVM (500 ms startup budget).
import { fileURLToPath } from 'node:url'
import path from 'node:path'
import { execute } from './plan-tools-lib.mjs'

export const main = (argv = process.argv.slice(2), output = text => process.stdout.write(text)) => execute('plan', argv, output)
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) process.exitCode = await main()
