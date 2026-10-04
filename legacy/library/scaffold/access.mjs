import { planScaffoldAccess, buildScaffoldAccess, scaffoldId } from '../../src/scaffold/access.mjs'
import { checkTree, harvestStands } from '../../src/tree/inspect.mjs'
import { treeContext, treeAttention, TREE_ARGS } from '../../src/tree/actions.mjs'
import { farmApi } from '../../src/farm/attention.mjs'
export default {
  doc: 'scaffold.access x= y= z= [species=] [form=] [place=] [check=true]: plan or build journaled clear scaffold columns covering the complete tree. check defaults true; false builds access without cutting the tree. Cleanup by the same ground anchor',
  args: {...TREE_ARGS,check:'boolean'},
  async run(api,a) {
    api=farmApi(api)
    const {root,cells}=treeContext(api,a)
    const tree=checkTree(api.block,root,a.species,a.form,cells)
    const report={root,attention:[...tree.attention]}
    if(report.attention.length)return treeAttention(api,'scaffold.access',report)
    if(tree.state!=='mature'){report.attention.push('no mature tree requiring scaffold access');return treeAttention(api,'scaffold.access',report)}
    const zones=(await api.act('zones')).zones??[]
    const plan=planScaffoldAccess(api,tree,harvestStands(tree,api.block),zones)
    Object.assign(report,plan)
    if(a.check!==false||plan.attention.length)return treeAttention(api,'scaffold.access',report)
    if(api.scaffolds?.(scaffoldId(root))){report.attention.push('journaled scaffold already exists; clean it before building another access plan');return treeAttention(api,'scaffold.access',report)}
    await buildScaffoldAccess(api,tree,plan,report)
    return treeAttention(api,'scaffold.access',report)
  }
}
