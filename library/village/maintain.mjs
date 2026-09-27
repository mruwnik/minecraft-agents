import { maintainVillage } from '../../src/villager/maintenance.mjs'
import { workRefusal } from '../../src/lib/places.mjs'
export default {
  doc:'village.maintain place= [passes=12 tries=40 imports=<explicit UUID transport routes> breed=<existing verified habitat args> planOnly=true]: inspect unknown evidence, repair empty missing-only structures, ensure housing, import or breed, and lock missing distinct roles; never remove surplus or change locked traders',
  args:{place:'string',name:'string',plan:'object',file:'string',origin:'string',x:'number',y:'number',z:'number',facing:'string',supply:'string',passes:'number',tries:'number',freshFor:'number',inspect:'boolean',imports:'array',breed:'object',planOnly:'boolean'},
  async run(api,args){
    if(args.place){const place=api.places().find(p=>p.name===args.place);if(place){const refusal=workRefusal(place,api.me());if(refusal)throw new Error(refusal)}}
    return maintainVillage(api,args)
  }
}
