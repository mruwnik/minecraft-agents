import { runTree, TREE_ARGS } from '../../src/tree/actions.mjs'
export default {
  doc: 'tree.plant x= y= z= [species=] [form=auto] [place=] [flower=]: plant one tree at its ground-level NW planting anchor; unsafe or incomplete work reports forestry_attention',
  args: TREE_ARGS,

  async run (api, a) { return runTree(api, a, 'plant') }
}
