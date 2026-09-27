import { inspectVillage } from '../../src/villager/maintenance.mjs'
export default {
  doc:'village.check (place= | name=/plan= x= y= z=) [inspect=true freshFor=120]: observe the canonical blueprint population requirements, exact UUID roles and actual habitat; unknown/stale evidence is not absence',
  args:{place:'string',name:'string',plan:'any',file:'string',origin:'string',x:'number',y:'number',z:'number',facing:'string',inspect:'boolean',freshFor:'number'},
  async run(api,a){return (await inspectVillage(api,a)).report}
}
