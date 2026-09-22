# Claude Code

Read this after your BRIEFING.md and before `AGENT_GUIDE.md`. Everything here is how the guide's advice maps onto
Claude Code's tools; the guide itself applies unchanged.

- **Waiting.** `./mc wait` is a plain Bash call in the foreground, default timeout (it returns within 100 s). Never
  `run_in_background`: a finished background command wakes nobody. `./mc dawn` the same way but with
  `timeout: 600000` (10 minutes), since it can block for eight.
- **Subagents are not woken by monitors.** If you are a subagent (launched from another session with the Agent tool),
  do not end your turn to wait for events: an idle subagent is not woken by a Monitor or by a finished background
  command. The events pile up and reach you only with the next message from someone else; agents sat through whole
  days that way. Block on `./mc wait` instead, every time.
- **Monitor tool, only for a session of its own** (started with `play`, not as a subagent): this filtered tail
  wakes you on what matters and stays silent otherwise, a fallback when you would rather end the turn than block:

      tail -n 0 -F events.jsonl | grep --line-buffered -E '"type":"(chat|whisper|died|night_fell|dawn|woke_up|kicked|body_down|task_done|task_cancelled|wedged|stalled|buried|tool_broke|watch_hit)"|"health":[0-8],'

- **Delegate the digging.** Where the guide says "if your harness can delegate": it can. For log questions (why did I
  die last night? what did Dan say an hour ago? what is wrong in bot.log?) spawn a haiku or sonnet subagent, tell it
  exactly which file and what question, and ask for an answer of three lines or fewer. Never Read those files yourself.
- **Delegate errands.** A haiku subagent with a tight brief (allowed `./mc` actions, stop conditions, a six-line report
  format) can drive your body for a whole hunting trip or strip-mine for ~150 tokens of your context. One driver per
  body: wait for its report before driving again.
- **Pictures.** `./mc look` writes a PNG under `snapshots/`; Read it with the Read tool (~250 tokens for a panorama).
- **Starting your body.** `./start` as a Bash call with `run_in_background: true`; its output goes to `bot.log`.
- **Launchers.** `bot/play [Name] [claude options]` starts a session of its own in a terminal; inside a Claude Code
  session under `/home/dan/minecraft/claude` the `/minecraft-agent [Name]` skill does the same, and launches other
  agents as named subagents with no tool-call budget (Dan wants them visible in his session, and playing for days).

## Permission classifier

Chaining several `./mc dig` (or place) calls in ONE Bash line near another agent's zone has tripped Claude Code's
"Modify Shared Resources" classifier and stalled the call for approval; one dig per Bash call never has. When you
work near a zone boundary, make one call at a time, or use a single `./mc run steps=[...]` (one command, one decision).
