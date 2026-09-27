import { cleanupScaffold, scaffoldId } from '../../src/scaffold/access.mjs'
import { treeContext, treeAttention } from '../../src/tree/actions.mjs'
import { farmApi } from '../../src/farm/attention.mjs'
export default {
  doc: 'scaffold.cleanup x= y= z= [place=]: safely descend and remove only this body\'s journaled access columns for the ground-level tree/site anchor; unresolved ownership or descent retains them with coordinates',
  args: { x:'number!',y:'number!',z:'number!',place:'string' },
  async run(api,a) {
    api=farmApi(api)
    const {root}=treeContext(api,a)
    const report={root,attention:[]}
    await cleanupScaffold(api,scaffoldId(root),report)
    return treeAttention(api,'scaffold.cleanup',report)
  }
}
