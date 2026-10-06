// Why JavaScript: ESM boundary over the AOT cljs bundle; plan generation, validation and routing live in cljs (no compiler or JVM start, 500 ms budget).
import { loadTools } from './agent-tools-loader.mjs'
const tools = loadTools(['blueprintForm', 'planExecute', 'planOneForm', 'planRequestFor', 'planUsage'])

export const RAW_BYTES = 65536
export const loadBridge = () => tools
// Shadow's development VM cannot import ESM itself, so this lazy Node loader
// stays at the host boundary; the cljs side owns all plan operations.
const loadWorldBlocks = () => import('../../dashboard/js/seenblocks.mjs')
export const execute = (kind, argv, output = () => {}) => tools.planExecute(kind, argv, output, loadWorldBlocks)
export const usage = tools.planUsage
export const oneForm = tools.planOneForm
export const blueprintForm = tools.blueprintForm
export const requestFor = tools.planRequestFor
