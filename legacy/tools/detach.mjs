// Runs a command in a session of its own and prints its pid. The one caller is start-body: the shell that ran ./start
// is a driver's tool call, and that tool's timeout or stop sends SIGTERM to the whole process group.
import { spawn } from 'node:child_process'

const [command, ...args] = process.argv.slice(2)
const child = spawn(command, args, { detached: true, stdio: 'ignore' })
child.unref()
process.stdout.write(`${child.pid}\n`)
