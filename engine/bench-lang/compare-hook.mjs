// `node --import ./bench-lang/compare-hook.mjs js/path/planner-water.test.mjs`: runs a planner test file with its `./planner.mjs`
// replaced by compared-planner.mjs, which plans every query with the JS planner and with both builds of the ClojureScript port and
// fails the test on the first difference. The test itself still asserts on the JS planner's answers.
import { register } from 'node:module'

register('./compare-loader.mjs', import.meta.url)
