// Which event a task's result is written under. The runner (src/body/jobs.mjs runLong) returns a result that lands inside
// args.timeout straight to the caller, and used to log nothing for it: five 29 s flock.lead attempts left only their
// `[task N]` start lines, and nobody could reconstruct from bot.log why the cows never followed. Now every result is
// written once, under one of two types:
//   task_result: the task finished inside its timeout. A log copy of a result the caller already holds, so it is NOT a
//                wake type (src/cli.mjs WAKE_TYPES): ./mc wait must not wake a driver for what ./mc already printed.
//   task_done:   the task outlasted its timeout and the caller got `running`; this is how the wait stream hands it the result.
// The data is the result as finish() built it (task, action, seconds, ok, gains, pos, ...), the same fields either way.
export const resultEvent = (finished, result) => ({ type: finished ? 'task_result' : 'task_done', data: result })
