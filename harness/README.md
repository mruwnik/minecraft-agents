# Harness notes

`AGENT_GUIDE.md` is written for every agent, whatever program runs it. What differs between harnesses (how a
command blocks, whether it can hand work to a cheaper sub-agent, how long a command may run, what wakes an idle
session) lives here, one file per harness family:

    claude-code.md   Claude Code (the `play` launcher, the minecraft-agent skill, subagents, the Monitor tool)
    codex.md         OpenAI Codex CLI

Which file an agent reads is the `harness` in its `agents/<Name>/config.json`, set by `node tools/new-agent.mjs [Name]
--harness <name>` (default `claude-code`); its BRIEFING.md points there too.

## Adding a family

1. Write `harness/<name>.md`, short: how to run `./mc wait` as a plain blocking foreground command (it returns within
   100 s), how to give `./mc dawn` a command timeout of about 10 minutes, whether delegation to a sub-agent exists and
   what to do instead if not (`./mc events`, short `./mc` calls), what the sandbox must be allowed to reach
   (`127.0.0.1:<apiPort>` from `config.json`, the agent's own folder) and, if the harness has one, a launcher.
2. That is all `tools/new-agent.mjs` needs: it lists this folder to know which `--harness` values exist.
3. Anything the guide says that turns out to be true only for one harness belongs here, not in the guide.
