// A goto that fails leaves no goal behind. mineflayer-pathfinder's goto rejects on a search timeout or a no-path but keeps
// the goal it set, and the pathfinder walks the best partial path of each new search for as long as the goal stands: Jizo's
// goto ran out of time at 19.7,65,-96.5 and 25 s later, no task running, the next goto started from 19.5,65,-91.5 (09-26
// 23:18Z, no forcedMove: the server agreed with every step). clearGoalOnFailure clears the goal the walk set, and only that
import test from 'node:test'
import assert from 'node:assert/strict'
import { clearGoalOnFailure } from '../src/lib/path.mjs'

// a pathfinder whose goto sets the goal, then does what `outcome` says: 'ok', 'fail', or 'replaced' (a flee took over)
const fakePathfinder = outcome => {
  const pf = { goal: null, cleared: 0, setGoal: g => { pf.goal = g; if (g === null) pf.cleared++ } }
  pf.goto = goal => {
    pf.goal = goal
    if (outcome === 'replaced') pf.goal = 'flee'
    return outcome === 'ok' ? Promise.resolve('there') : Promise.reject(new Error('Took to long to decide path to goal!'))
  }
  return pf
}
const settle = p => p.then(v => `ok ${v}`, e => `failed ${e.message}`)

for (const [name, outcome, expected] of [
  ['a walk that arrives: resolved, the goal left alone', 'ok', { result: 'ok there', goal: 'home', cleared: 0 }],
  ['a walk that fails: the failure passed on, its goal cleared', 'fail', { result: 'failed Took to long to decide path to goal!', goal: null, cleared: 1 }],
  ['a walk whose goal was replaced (a flee): the new goal stays', 'replaced', { result: 'failed Took to long to decide path to goal!', goal: 'flee', cleared: 0 }]
]) {
  test(`clearGoalOnFailure: ${name}`, async () => {
    const pf = fakePathfinder(outcome)
    const result = await settle(clearGoalOnFailure(pf, goal => pf.goto(goal))('home'))
    assert.deepEqual({ result, goal: pf.goal, cleared: pf.cleared }, expected)
  })
}
