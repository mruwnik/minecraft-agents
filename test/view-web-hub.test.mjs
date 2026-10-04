// hub.mjs, the hub's WebGL surface: without WebGL2 it gives view.hub no surface. The hub's behaviour is tested in cljs
// (dashboard/test/view/hub_test.cljs, schedule_test.cljs).
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createViewHub } from '../tools/view/web/hub.mjs'

test('without WebGL2 the hub does not throw, says supported: false and leaves the canvas alone', async () => {
  const warn = console.warn
  console.warn = () => {}
  globalThis.document = { createElement: () => ({ getContext: () => null }) }
  try {
    const hub = createViewHub({})
    const scene = hub.addScene({ agent: 'w/Bob' })
    const canvas = { width: 300, height: 150, getContext: () => assert.fail('the canvas must not be drawn on') }
    scene.attach(canvas, { width: 320, height: 180 })
    assert.equal(hub.supported, false)
    assert.deepEqual([canvas.width, canvas.height], [300, 150])
    assert.equal(scene.stats().status, 'unsupported')
    await assert.rejects(scene.snapshot({ width: 8, height: 8 }))
    scene.detach(canvas)
    scene.close()
    hub.close()
  } finally {
    console.warn = warn
    delete globalThis.document
  }
})
