# Microsoft login: a body on an online-mode server

Until now every body logged in with `auth: 'offline'`, which only an offline-mode server accepts. The server this
is for runs Paper in online mode with existing players, and switching it to offline mode would re-key every
player's UUID. So a body may instead log in with a real Microsoft account that owns Minecraft Java: one account per
body, its in-game name being the account's profile name. The first body is AmethystFan7865, played as the character
Breq.

## Config

`config.json` gains one field:

- `auth`: `"offline"` (default, unchanged behaviour) or `"microsoft"`. Any other value is refused by `readConfig`
  with a message naming the two choices.

`username` stays the account's profile name (the gamertag). The folder name, the API port map, the shared clock
and every `bot.username` comparison already assume the folder is named after the player, and Mojang decides the
player name, so the two must agree. The persona lives in `character` as it does today; the briefing says both names.

Tokens are cached in `state/agents/<Name>/auth/` (mode 700). `state/` is gitignored already.

## One-time sign-in: `tools/login.mjs`

    node tools/login.mjs AmethystFan7865

1. Reads the agent's config and refuses unless `auth` is `microsoft`.
2. Runs prismarine-auth's device-code flow with exactly the arguments minecraft-protocol uses
   (`Authflow(cfg.username, authDir, { authTitle: Titles.MinecraftNintendoSwitch, deviceType: 'Nintendo', flow: 'live' })`),
   so the cache it writes is the cache the body later reads. Prints the URL and code, waits for the sign-in.
3. Fetches the profile and checks its name equals `cfg.username` (case-insensitive). On a mismatch it deletes the
   cache and exits with "signed in as X, but this folder is Y": a wrong account must not be cached under a name it
   does not own.
4. Writes `auth/profile.json` (`{ name, id, at }`) as the marker that a sign-in has happened.

## The body

`connect()` in `src/bot.mjs` passes `auth: cfg.auth` and, for microsoft, `profilesFolder: authDir`. Two refusals:

- Before the first connect, a microsoft body without `auth/profile.json` writes a `body_down` event with the advice
  "run `node tools/login.mjs <Name>` once" and exits 6. Without this, prismarine-auth would start a device-code flow
  inside a background process and hang on it for fifteen minutes, then the reconnect loop would do it again.
- If a device code is requested at runtime anyway (the refresh token expired after months of disuse), the
  `onMsaCode` callback emits a `login_needed` event with the same advice and exits 6, for the same reason.

Chat signing stays on: online-mode servers on 1.19+ expect it, and minecraft-protocol fetches the keys itself.

## Agent folder

`state/agents/AmethystFan7865/config.json`: `auth: "microsoft"`, host and port of the remote server, character
Breq (Ancillary Justice). The BRIEFING.md line about the name becomes: "Your player name is AmethystFan7865; you
are Breq, from Ancillary Justice (an AI reduced to one human body). Others will call you by the player name." The
nine other agent folders made earlier are deleted. `tools/new-agent.mjs` is not changed: one folder is faster to
edit by hand than to grow a flag for.

## Tests

- `test/config.test.mjs`: `auth` defaults to offline; `microsoft` passes through; another value is refused.
- A pure `profileMismatch(profile, username)` in `src/auth.mjs` (also `authDir`, `loginAdvice`) with tests for
  same name, different case, different name, missing profile.
- The body's pre-connect refusal: a test that the advice names the login tool and the agent.
- The live sign-in and join are verified by hand on the server: no test talks to Microsoft.

## Out of scope

More than one account, FastLogin or proxies, copying tokens from the launcher (its refresh credential is
encrypted and its session token lives a day), and any change to `./play` beyond what the briefing says.
