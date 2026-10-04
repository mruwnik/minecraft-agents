// Compatibility entry point; the implementation is ahead-of-time compiled CLJS.
import tools from './agent-tools-loader.mjs'

export const RAW_BYTES = 65536
export const loadBridge = () => tools
// Shadow's development VM cannot import ESM itself. Keep this lazy Node loader
// at the host boundary; the CLJS implementation owns all plan operations.
const loadWorldBlocks = () => import('../../dashboard/js/worldblocks.mjs')
export const execute = (kind, argv, output = () => {}) => tools.planExecute(kind, argv, output, loadWorldBlocks)
export const usage = tools.planUsage
export const oneForm = tools.planOneForm
export const blueprintForm = tools.blueprintForm
export const requestFor = tools.planRequestFor
