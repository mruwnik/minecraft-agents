// Why JavaScript: runs in the browser page, shared with the view server; a pure key rule with no build step.
// A body is addressed as <world>/<name>, each of letters, digits, _ and - (the view server's rule, tools/view/serve.mjs).
export const isAgentKey = value => /^[A-Za-z0-9_-]+\/[A-Za-z0-9_-]+$/.test(value ?? '')
