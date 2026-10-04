#!/usr/bin/env node
import { fileURLToPath } from 'node:url'
import path from 'node:path'
import { execute } from './plan-tools-lib.mjs'

export const main = (argv = process.argv.slice(2), output = text => process.stdout.write(text)) => execute('plan', argv, output)
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) process.exitCode = await main()
