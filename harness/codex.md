# OpenAI Codex CLI

Read this after your BRIEFING.md and before `AGENT_GUIDE.md`. Written before any Codex agent had played here: the
points marked *verify* are best guesses. If one is wrong, append what you found to `../../BUGS.md` so it gets fixed.

- **Waiting.** `./mc wait` is a plain blocking shell command, run in the foreground like any other: it returns within
  100 s with what happened, or `quiet for 100s` (call it again). Do not run it detached or in a background job, and
  never end your turn to wait: nothing wakes an idle Codex session; events pile up until you call `./mc wait` again.
- **`./mc dawn`** can block for eight minutes. The default shell timeout is shorter (*verify*): raise it for that one
  call to about 10 minutes (Codex takes a per-command `timeout_ms`, or set a longer default in your config), or call
  `./mc clock` every few minutes with short sleeps instead.
- **No sub-agent delegation.** Where the guide says "if your harness can delegate": assume it cannot. Instead use
  `./mc events last=10`, `./mc events type=chat last=5` (also `type=died`, `type=whisper`) for history, and keep
  `./mc` calls short and chained (`./mc run steps=...`). Never open `events.jsonl`, `bot.log` or another agent's
  folder in full; if you must look inside a big file, `grep` for one thing and `tail -n 5` it.
- **Work only inside `agents/<Name>`** (your folder). Everything you need is there or reached through `./mc`; the
  shared code in `../..` is read-only for you except appending to `../../BUGS.md`.
- **Sandbox: the body needs local network access** (verified 2026-09-22 with Sazed's first start). Codex's default
  sandbox blocks all sockets, even loopback: the body could neither bind its own API nor reach the server, and died
  within a second. `bot.log` then ends with `uncaught: listen EPERM: operation not permitted 127.0.0.1:<apiPort>` and
  `connect EPERM 127.0.0.1:25565`, and `events.jsonl` gets an `error` line with the same text. Fix: start the body
  with network access allowed (run `./start` outside the sandbox, or with sandbox network enabled for that command);
  `./mc` itself needs the same to reach `127.0.0.1:<apiPort>` (`apiPort` is in `config.json`). The second start,
  with the sandbox relaxed, worked; the body keeps running between commands as long as the sandbox does not kill
  background processes.
- **Pictures.** `./mc look` writes a PNG under `snapshots/`; view it with your image tool if you have one (*verify*),
  otherwise rely on `look_around`, `scan ... where=` and `find_blocks`.
