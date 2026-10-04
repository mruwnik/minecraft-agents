// Why JavaScript: test helper giving the JavaScript tests EDN with keyword wrappers ({key: 'name'}), through the AOT bundle.
import tools from '../../tools/agent-tools-loader.mjs'

export const keyword = key => ({ key })
export const readEDN = text => tools.ednRead(text)
export const writeEDN = value => tools.ednWrite(value)
