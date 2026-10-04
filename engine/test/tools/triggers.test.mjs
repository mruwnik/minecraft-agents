import test from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { requestFor, compact, duration, oneForm, post } from '../../tools/triggers.mjs'
import { readEDN, writeEDN, keyword as k } from '../../tools/observe-lib.mjs'
const request = args => requestFor(['Probe','--world','test',...args])
test('ad hoc and predefined upserts preserve native EDN list/map values',()=>{
  const a=request(['add','near-home','--when','(and (< (inventory "bread") 8) (< (distance-to (place :home)) 16))','--job','(jobs.movement.look-around)','--for','10m'])
  assert.equal(a.request.op.key,'put');assert.equal(a.request['ttl-s'],600)
  const text=writeEDN(a.request);assert.match(text,/:when \(and \(< \(inventory "bread"\) 8\)/);assert.match(text,/:job \(jobs.movement.look-around\)/)
  const b=request(['put','health-watch','--trigger','health-low','--args','{:health 12}','--backoff','{:base-ms 1000}'])
  assert.match(writeEDN(b.request),/:args \{:health 12\}/);assert.match(writeEDN(b.request),/:backoff \{:base-ms 1000\}/)
})
test('mute/unmute/order/reset map onto existing register API semantics',()=>{
  assert.equal(request(['mute','hungry','--for','30s']).request['ttl-s'],30)
  assert.equal(request(['unmute','hungry']).request.property.key,'mute')
  assert.equal(request(['move','hungry','--before','health-low','--for','2m']).request.above.key,'health-low')
  assert.equal(request(['move','hungry','--after','health-low']).request.below.key,'health-low')
  assert.equal(request(['reset','hungry','--property','position']).request.op.key,'clear')
  assert.equal(request(['remove','custom']).request.op.key,'remove')
  assert.ok(request(['move','hungry','--before','hungry']).error)
  assert.ok(request(['move','hungry','--before','x','--after','y']).error)
  assert.ok(request(['reset','hungry']).error)
})
test('validation requires world/name, one EDN form, compatible flags and bounded durations',()=>{
  assert.ok(requestFor(['Probe','list']).error);assert.ok(requestFor(['../body','--world','test','list']).error)
  assert.ok(request(['add','x','--when','true']).error)
  assert.ok(request(['put','x','--when','true','--trigger','hungry','--job','(jobs.movement.look-around)']).error)
  assert.ok(request(['remove','x','--for','30s']).error)
  assert.ok(request(['list','--limit','99']).error)
  assert.throws(()=>oneForm('(one) (two)'))
  assert.throws(()=>oneForm('x'.repeat(13000)))
  assert.throws(()=>duration('0s'));assert.throws(()=>duration('forever'));assert.equal(duration('250ms'),0.25)
})
test('list is paged and compact; detail exposes bounded conditions/explanations; mutations stay small',()=>{
  const value=readEDN('{:ok true :generation-id "uuid" :total 2 :order [:first :second] :items [{:id :first :trigger :condition :job (jobs.movement.look-around) :when (and (< (inventory "bread") 8) (< (distance-to (place :home)) 16)) :builtin? false} {:id :second :trigger :hungry :builtin? true :muted {:until 123} :job (jobs.survival.eat)}]}')
  const list=compact(request(['list','--limit','1']),value)
  assert.equal(list.items.length,1);assert.equal(list['next-offset'],1);assert.equal(list.items[0].job,'jobs.movement.look-around')
  assert.equal(list.items[0].when.list[0].sym,'and')
  assert.equal(list.items[0].when.list[1].list[0].sym,'<')
  assert.match(writeEDN(list),/:when \(and \(< \(inventory "bread"\) 8\)/)
  assert.equal('generation-id' in list,false)
  assert.equal(list.items[0].priority,1)
  const moved=compact(request(['list']),{...value,order:[k('second'),k('first')]})
  assert.deepEqual(moved.items.map(e=>e.priority),[2,1])
  const muted=compact(request(['list']),{...value,order:[k('first')]})
  assert.equal('priority' in muted.items[1],false)
  const show=compact(request(['show','first']),{...value,explain:{terms:Array(100).fill({form:true,value:true})}})
  assert.equal(show.id.key,'first');assert.equal(show.explain.length,16)
  const timed=compact(request(['show','first']),{...value,explain:{terms:[{form:{list:[{sym:'held-for'},5]},value:false,'remaining-ms':2500}]}})
  assert.equal(timed.explain[0]['remaining-ms'],2500)
  assert.equal(compact(request(['show','unknown']),value).reason.key,'trigger-not-found')
  const mutation=compact(request(['mute','first']),{ok:true,id:k('first'),op:k('mute'),trigger:{muted:{until:123},job:Array(100).fill('large')}})
  assert.deepEqual(mutation,{ok:true,id:k('first'),op:k('mute'),muted:true})
})
test('structured compiler errors remain data but bounded',()=>{
  const r=compact(request(['add','x','--when','(unknown)','--job','(jobs.movement.look-around)']),{ok:false,reason:k('bad-condition'),at:[k('when')],message:'x'.repeat(2000),condition:{reason:k('unknown-function'),allowed:Array(100).fill('func')}})
  assert.equal(r.reason.key,'bad-condition');assert.equal(r.message.length,240);assert.equal(r.condition.allowed.length,16)
})
test('mutation transport has a finite total deadline',async()=>{
  class Pending extends EventEmitter {end(){}destroy(error){this.emit('error',error)}}
  await assert.rejects(post('/unused',{op:k('mute')},{timeoutMs:5,requestImpl:()=>new Pending()}),{code:'ETIMEDOUT'})
})
