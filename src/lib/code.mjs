// Whether the running code is stale against disk: the version key, the path pattern, and the comparison.

// a body loads the shared code once, at start: which files have changed since then (null: none), so the driver knows a restart brings fixes
// While any file is under 3 minutes old the change may be half made (a body started then might not even load): say nothing yet
export function staleCode (started, mtimes, now) {
  const newer = Object.entries(mtimes).filter(([, mtime]) => mtime > started)
  // quiet for 15 minutes, or for 2 when this body is already 45 minutes behind: waiting for a lull starved the long-running bodies of every fix
  const quiet = now - started >= 2700000 ? 120000 : 900000
  if (newer.some(([, mtime]) => now - mtime < quiet)) return null
  return newer.length ? newer.map(([file]) => file) : null
}

// Which code this body is ACTUALLY running, for the line it says when it joins. Two implementers share this tree, so a
// body started between two saves loads the new helper with the old caller and throws something that is in nobody's
// diff: mine threw `Cannot destructure property 'edible'` against a tree that was mid-commit, not broken, and the
// twenty minutes I spent on it were spent on a ghost (#140). Only files a body LOADS count as dirty - an edited guide
// or a note in state/ changes nothing it runs, and a join line that cried wolf would stop being read.
export const CODE_PATH = /^(src|library|tools)\/.*\.mjs$/
const SHOWN = 4
export function codeVersion ({ head, changed = [] }) {
  if (!head) return { code: 'unknown', advice: 'this body could not read its own git HEAD, so it cannot tell you which code it is running: it may be running anything' }
  const code = changed.filter(file => CODE_PATH.test(file)).sort()
  if (!code.length) return { code: head }
  const shown = [...code.slice(0, SHOWN), ...(code.length > SHOWN ? [`and ${code.length - SHOWN} more`] : [])].join(' ')
  return {
    code: `${head}+${code.length}`,
    dirty: shown,
    advice: `this body is NOT running ${head} as committed: ${code.length} code file${code.length > 1 ? 's were' : ' was'} edited and uncommitted when it started, so it may hold half of somebody's change. A tool that misbehaves here is worth a \`git status\` and a restart before it is worth debugging`
  }
}

// what a code_updated notice was about: the same files edited AGAIN are news again (a body told once was never told of the later batches)
export const staleKey = (stale, mtimes) => `${stale.join()}@${Math.max(...stale.map(f => mtimes[f]))}`
