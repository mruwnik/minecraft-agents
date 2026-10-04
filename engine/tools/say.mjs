#!/usr/bin/env node
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import tools from './agent-tools-loader.mjs'
import { post } from './jobs.mjs'
import { writeEDN, keyword } from './observe-lib.mjs'

export const usage = tools.sayUsage
export const requestFor = argv => tools.sayRequestFor(argv)
export function failureFor (error) {
  if (error.code === 'EPERM' || error.code === 'EACCES') {
    return { ok: false, reason: keyword('socket-access-denied'), message: 'Permission denied connecting to the body event socket; the message was not sent.' }
  }
  return { ok: false, reason: keyword(['ENOENT', 'ECONNREFUSED'].includes(error.code) ? 'no-running-body' : 'transport-error'),
    confirmation: keyword('unknown'), message: 'Chat confirmation is unknown; inspect server chat before sending again.' }
}

export async function main (argv = process.argv.slice(2)) {
  const r = requestFor(argv)
  if (r.error) {
    process.stdout.write(writeEDN({ ok: false, reason: keyword('bad-args'), message: r.error }) + '\n')
    return 2
  }
  try {
    const response = await post(r.socketPath, { message: r.message, to: r.to }, { path: '/chat', timeoutMs: 10000 })
    if (!/^application\/edn(?:;|$)/i.test(response.contentType ?? '')) throw new Error('unexpected response format')
    process.stdout.write(response.text.endsWith('\n') ? response.text : response.text + '\n')
    return response.status === 200 ? 0 : 1
  } catch (error) {
    process.stdout.write(writeEDN(failureFor(error)) + '\n')
    return 2
  }
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) process.exitCode = await main()
