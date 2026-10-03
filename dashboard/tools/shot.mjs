// Screenshot a page in headless Chromium: node tools/shot.mjs <url> <out.png> [--width 1920 --height 1080] [--click <css selector>]
// Loads the url, waits 2 s for the page to settle, optionally clicks one element, waits again, saves a PNG.
// Dialogs are never opened by this script; the page is only loaded, clicked and photographed.
import fs from 'node:fs'
import path from 'node:path'
import { parseArgs } from 'node:util'
import { withPage } from '../../tools/view/headless.mjs'

const { values, positionals } = parseArgs({
  allowPositionals: true,
  options: { width: { type: 'string', default: '1920' }, height: { type: 'string', default: '1080' }, click: { type: 'string' } }
})
const [url, out] = positionals
if (!url || !out) {
  console.error('usage: node tools/shot.mjs <url> <out.png> [--width 1920 --height 1080] [--click <css selector>]')
  process.exit(2)
}

await withPage({ url, width: Number(values.width), height: Number(values.height) }, async page => {
  await page.waitUntil("document.readyState === 'complete'", 'page load')
  await page.sleep(2000)
  if (values.click) {
    const clicked = await page.evaluate(`(() => { const el = document.querySelector(${JSON.stringify(values.click)}); if (!el) return false; el.click(); return true })()`)
    if (!clicked) throw new Error(`no element matches ${values.click}`)
    await page.sleep(1000)
  }
  fs.mkdirSync(path.dirname(path.resolve(out)), { recursive: true })
  fs.writeFileSync(out, await page.screenshot())
  for (const line of page.logs) console.error(line)
})
console.log(`saved ${out}`)
