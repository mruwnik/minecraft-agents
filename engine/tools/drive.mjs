#!/usr/bin/env node
import fs from 'node:fs'
import http from 'node:http'
import { requestFor, exitCodeFor, socketPathFor, usage } from './drive-lib.mjs'

const send = (socketPath, { method, path, body }) => new Promise((resolve, reject) => {
  const payload = body === null ? undefined : JSON.stringify(body)
  const req = http.request({ socketPath, method, path, headers: { 'content-type': 'application/json' } }, (res) => {
    let text = ''
    res.on('data', (c) => { text += c })
    res.on('end', () => resolve({ status: res.statusCode, text }))
  })
  req.on('error', reject)
  req.end(payload)
})

const main = async () => {
  const req = requestFor(process.argv.slice(2))
  if (req.error) {
    console.error(`${req.error}\n${usage}`)
    return 2
  }
  const socketPath = socketPathFor(req)
  const reply = await send(socketPath, req).catch(() => null)
  if (reply === null) {
    const why = fs.existsSync(socketPath) ? 'connection refused' : 'no control socket'
    console.error(`no running body ${req.agent} (${why === 'no control socket' ? `no control socket at ${socketPath}` : `connection refused at ${socketPath}`})`)
    return 2
  }
  console.log(reply.text)
  const json = (() => { try { return JSON.parse(reply.text) } catch { return null } })()
  return exitCodeFor({ status: reply.status, json })
}

process.exitCode = await main()
