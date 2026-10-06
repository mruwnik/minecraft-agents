// Why JavaScript: a node:test reporter module (node --test --test-reporter=...) that prints live-tests @@test result lines.
// With TEST_EVENTS=1 each test prints one result line; otherwise it falls back to node's own default (spec on a terminal, tap when piped).
import { spec, tap } from 'node:test/reporters'

const outcomeOf = (type, data) => {
  if (data.skip || data.todo) return 'skipped'
  return type === 'test:pass' ? 'passed' : 'failed'
}

async function* events(source) {
  const names = [] // names by nesting depth, from test:start, so a nested result is named outer/inner
  for await (const { type, data } of source) {
    if (type === 'test:start') names[data.nesting] = data.name
    if (type !== 'test:pass' && type !== 'test:fail') continue
    if (data.details?.type === 'suite') continue
    const message = type === 'test:fail' ? String(data.details?.error?.message ?? data.details?.error ?? '').slice(0, 2000) : undefined
    yield `@@test ${JSON.stringify({ event: 'result', name: names.slice(0, data.nesting + 1).join('/'), outcome: outcomeOf(type, data), message })}\n`
  }
}

export default process.env.TEST_EVENTS === '1' ? events : process.stdout.isTTY ? spec : tap
