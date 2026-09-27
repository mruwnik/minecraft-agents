import test from 'node:test'
import assert from 'node:assert/strict'
import { openingProgress, surfaceWay } from '../src/navigation/surface.mjs'
test('up/sideways water oscillation cannot reset a failed opening allowance',()=>{
 const to={x:-15,y:62,z:-102},way={way:'sideways',to,dist:0.8}
 let tracks={}
 tracks=openingProgress(tracks,way,0).tracks
 tracks=openingProgress(tracks,{way:'up'},1000).tracks
 tracks=openingProgress(tracks,{...way,dist:0.9},1500).tracks
 tracks=openingProgress(tracks,{way:'up'},2000).tracks
 const result=openingProgress(tracks,way,2500)
 assert.equal(result.failed,'-15,62,-102')
 const next=surfaceWay({column:['stone'],openings:[to,{x:-16,y:62,z:-102}],me:{x:-14.5,y:61.2,z:-101},tried:[result.failed]})
 assert.deepEqual(next.to,{x:-16,y:62,z:-102})
})
test('real horizontal progress renews opening allowance; unrelated targets retain separate clocks',()=>{
 const a={way:'sideways',to:{x:1,y:2,z:3},dist:4},b={way:'sideways',to:{x:4,y:2,z:3},dist:5}
 let tracks=openingProgress({},a,0).tracks
 tracks=openingProgress(tracks,b,1000).tracks
 assert.equal(openingProgress(tracks,{...a,dist:3},1900).failed,null)
 assert.equal(openingProgress(tracks,b,3100).failed,'4,2,3')
})
