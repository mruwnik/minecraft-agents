// Why JavaScript: a process-level stdout hook on the shared tool launcher, before any cljs bundle loads.
// A reader that exits early (`| head`) closes the pipe; end quietly like a normal CLI instead of an unhandled 'error' stack trace.
process.stdout.on('error', error => {
  if (error.code === 'EPIPE') process.exit(0)
  throw error
})
