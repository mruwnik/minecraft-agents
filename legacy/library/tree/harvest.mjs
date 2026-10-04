import { runTree, TREE_ARGS } from '../../src/tree/actions.mjs'
export default {
  doc: 'tree.harvest x= y= z= [species=] [form=auto] [place=] [flower=] [scaffold=true]: harvest wood at its ground-level NW planting anchor, retaining leaves for decay; unsafe or incomplete work reports forestry_attention',
  args: TREE_ARGS,

  async run (api, a) { return runTree(api, a, 'harvest') }
}
