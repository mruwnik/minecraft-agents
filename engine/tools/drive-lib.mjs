// Why JavaScript: Node entry point/launcher for the AOT bundle; the logic is agent-tools.drive. The tools still
// written in JavaScript import the default state directory and socket path from here until they move too.
import tools from './agent-tools-loader.mjs'

export const defaultStateDir = tools.driveDefaultStateDir
export const usage = tools.driveUsage
export const socketPathFor = request => tools.driveSocketPathFor(request)
export const requestFor = argv => tools.driveRequestFor(argv)
