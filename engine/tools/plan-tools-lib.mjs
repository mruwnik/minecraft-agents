// Why JavaScript: ESM boundary: generation, validation and routing policy live in AOT CLJS, this is a compatibility entry point; Thin Node launcher over the AOT cljs bundle dashboard/out/agent-tools.cjs; runs without starting a compiler or JVM (500 ms startup budget).
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
