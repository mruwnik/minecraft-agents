# Engine event stream

This document defines the engine's event contract and implementation boundaries. It separates observations and notifications from the engine's saved
state. The event stream is not a replayable database: `engine.edn` and
`memory.edn` remain authoritative for current engine and job state.

## Previous behavior and migration

Previously the engine wrote unbounded JSONL with logger levels and flat
kind-specific fields. The dashboard tailed that file without a reliable
consumer cursor. That format is superseded by the contract below. Dashboard
support for older JSONL bodies is read-only and isolated from canonical EDN
streams. The legacy `./mc wait` and `docs/events.md` describe the older body's
stream, not this engine API.

## Encoding

Use one canonical **EDN map per line** in
`state/worlds/<world>/agents/<name>/engine/events.edn`. This matches the engine's EDN state
and scenario files and the design's preference for EDN on disk. Event APIs, snapshots, attention
requests, and dashboard event responses also use EDN (`application/edn`).
ClojureScript clients read EDN directly; there is no JSON conversion boundary
for event data. Do not write both formats.

Each line is one complete EDN map followed by a newline. Keep event maps
plain, compact data: keywords, strings, numbers, booleans, nil, vectors, and
maps. Do not put executable forms or large snapshots in events. A short
optional `:message` is for display; consumers must make decisions from
structured fields.

## Event contract

The file and reader subscription identify the body, so events do not repeat a
`:body` field. Stream metadata provides a stable `stream-id`; the cursor is
`[stream-id, seq]`, so replacing the whole stream is distinguishable from log
rotation. `:seq` is strictly increasing for that stream across process
restarts, rotation, and `--fresh`. Each engine-state generation also has a
stable `generation-id`, changed only by `--fresh`; events carry it so persisted
job IDs remain identifiable across restarts and are not confused with reused
IDs after a fresh reset. A per-process `run-id` may appear on startup events
for diagnosing restart boundaries; it need not be repeated on every event.

```clojure
{:seq 1042
 :generation-id "gen-31c7"
 :time-ms 1791023456789
 :source :job
 :kind :blocked
 :context {:job-id "j17" :round 7 :chain ["j17" "j17/fell"]}
 :attention :notice
 :data {:target {:x 12 :y 64 :z -4}
        :reason :no-reachable-tree}
 :message "No reachable tree in range."}
```

Required fields are `:seq`, `:generation-id`, `:time-ms`, `:source`, and
`:kind`. `:source` identifies the producer area (`:job`, `:reflex`, `:action`,
`:body`, `:system`, `:memory`, or `:attention`). `:kind` identifies the event
within that source.
Use `:context` for applicable correlation fields: `:job-id`, `:round`,
`:chain`, `:reflex-id`, `:action-id`, and `:cause-seq`. Omit inapplicable
context keys. `:data` is a map whose fields are defined by the particular
`source/kind` contract. `:message` is optional display text and is never a
machine-readable reason code.

Allocate an `:action-id` when an action starts and reuse it on its completion
or failure, so repeated same-name actions in a round can be paired. Job IDs
are only unique within an engine-state generation; use `:generation-id` with
`:job-id` when retaining or joining records. `:cause-seq` refers to an event
in the same body stream.
`:time-ms` is wall-clock UTC epoch milliseconds, not Minecraft world time;
there is no cross-body total order.

A required request carries its stable ID in the event:

```clojure
{:seq 1043 :generation-id "gen-31c7" :time-ms 1791023460123
 :source :job :kind :storage-blocked
 :context {:job-id "j17" :round 8}
 :attention :required :request-id "req-8ad9"
 :data {:reason :chest-full :item "oak_log" :count 24}}
```

There is no schema-version field. Readers ignore unknown optional keys. Add
optional keys and new `source/kind` values compatibly; never silently change
the meaning or type of an existing field or event. For a rename or removal,
define an explicit mapping in the reader and migration notes before retiring
the old name. Keep each `source/kind` contract small and documented, including
its required `:data` keys and whether it can request attention.

The engine event contract has no severity field. Ordinary process logging may
assign levels to its own diagnostic messages, but those levels do not define
event meaning or whether an agent must act.

## Core event cases

| Source / kind | Structured fields | Attention |
|---|---|---|
| `:action / :started` | `:context :action-id` and job correlation; `:data :name`, `:args` | Routine |
| `:action / :done` | Same action ID; `:data :name`, `:status`, optional `:reason`, `:error`, `:distance` | Routine |
| `:job / :job.notify` and `:job / :notify.chat-failed` | Display message; chat failure carries result fields | Notice |
| `:job / :completed` | Job correlation; `:data :status :completed` | Notice |
| `:job / :failed` for a parked listed job | Job correlation; `:data :reason :round-failed`, `:error`; stable request ID | Required |
| `:attention / :resolved` | Request ID; job correlation when applicable; `:data :handled`, `:reason` | Routine transition |

Action status preserves primitive result strings; an exception produces
`:failed`, and ownership interruption produces `:cut`. A failed reflex event
is not automatically a durable request: reflex retry and persistence policy
still govern its lifecycle. Jobs can explicitly request and resolve durable
attention through their context. Existing domain event kinds retain their
names; former flat payload fields move into `:data`, correlation fields into
`:context`, and display `:text` becomes `:message`. Logger `:level` is dropped.

## Attention and resolution

`:attention` is optional; omission means `:none`.

| Value | Contract |
|---|---|
| `:none` | Routine observation. Do not notify an agent. |
| `:notice` | Make visible to the agent. No response is required; delivery may be batched. |
| `:required` | The agent needs to handle an outstanding request. Notify promptly and keep it outstanding until resolved. |

An event with `:attention :required` includes a stable `:request-id`. The
engine persists open required requests in its engine state independently of
event-log retention. A notice is a retained event only, not a durable inbox
item. Re-observations update the same required request ID and notify only when
its meaning materially changes; they do not open a new request or spam
repeated notices. Resolution emits `:source :attention`,
`:kind :resolved`, with the same request ID and structured outcome fields,
including whether it was handled and the reason (for example, `:handled`,
`:condition-recovered`, or `:job-cancelled`). Resolution is a state transition,
not proof that a consumer received or acted on a notification. Required
attention does not automatically pause the engine or all jobs.

The outstanding-request map is keyed by request ID. Each value contains
`:request-id`, `:job-id`, `:reason`, `:updated-at`, and an `:event` payload
with source, kind, context, data, and optional message. That nested payload
is a notification template, not a previously appended log record: sequence,
generation, and timestamp are assigned when it is emitted or replayed.

The outstanding-request map is the resynchronization authority. A retained
resolution event explains why a request closed; if a consumer missed that
event because of a retention gap, a fresh snapshot still tells it that the
request is no longer outstanding. Do not keep an unbounded resolution
archive. A small bounded receipt list is only warranted if consumers later
need exact resolution reasons after they have missed the event.

Because the state snapshot and event append are separate files, they cannot
be committed atomically. Persist a new required request before emitting its
event. On startup, re-emit still-open requests with their original request
IDs; consumers upsert request state by ID and notify only on a new or
meaningfully changed request. This gives at-least-once notification, not
exactly-once delivery. On resolution, atomically remove the request from the
persisted outstanding map in `engine.edn`, then append the resolved event. A
crash between those writes can lose the precise resolution reason from the
log; after restart or a cursor gap, the snapshot still tells the consumer the
request is no longer outstanding. Do not claim cross-file transaction
guarantees. `engine.edn` stays the durable state, not the event log.

## Retention and reading

Use a bounded rolling appender with a default **64 MiB total per body** across
the active file and rotated segments. The cap is a storage bound, not a
promise about how many hours or days of history it holds; action and debug
traffic vary. Rotate only between complete records and evict oldest segments
to stay within the aggregate cap. Keep each event compact and reject an
oversized record with a separate stderr diagnostic instead of splitting or
silently truncating it. Reject invalid caps at startup. Store `stream-id`
and `last-seq` in small atomically replaced appender metadata; the
`generation-id` belongs to the authoritative engine state in `engine.edn`. Reserve
sequence values before append; after a crash, recover the next value from the
maximum of metadata and the last valid complete record. This can leave a
sequence gap after a crash, but will not reuse a cursor. Trim incomplete
trailing bytes on startup; report malformed complete records rather than
silently treating them as valid. `--fresh` changes `generation-id`, not
`stream-id` or the sequence.

A consumer stores `[stream-id, seq]` and processes later events, upserting
required requests by ID. The engine needs a serialized local snapshot/cursor
and read-after operation: current engine state, outstanding requests, and
cursor must describe one consistent point. Independently reading `engine.edn`
and the event file cannot guarantee that. Read-after returns later events,
or an explicit gap with the oldest available sequence when retention has
removed the requested range. On connection or reconnection, and after a gap,
reconcile outstanding requests against a fresh snapshot before continuing.
This also repairs missed resolution events after a crash. This supports replay
and deduplication without a public
network event server; local Unix-socket IPC or a coordinated local reader is
sufficient. The dashboard exposes the same EDN representation to its client.

## Runtime configuration

Keep appender policy out of job scenarios. Put per-body settings in the
agent's existing `state/worlds/<world>/agents/<name>/config.json`, under a runtime section,
for example:

```json
{
  "engine": {
    "events": {
      "maxBytes": 67108864
    }
  }
}
```

Precedence is built-in defaults, then per-agent settings, then explicit
launch overrides such as `--events-max-bytes`. The cap must be a safe integer
of at least 1,024 bytes. `main` validates and passes resolved event settings to `core/create`, which passes them to the
appender. The existing injected `:events` stream remains available to tests
and embedders. Keep the first configuration surface to `maxBytes`; segment
sizing can remain an appender detail until there is a concrete need to tune
it.

## Implementation boundaries

1. Define and document the per-kind event fields and attention cases; keep
   domain meaning in structured `:data`, with no `:level` or version field.
2. Add the EDN-line appender, stable stream metadata, optional startup run ID,
   monotonic sequence reservation, rotation, complete-record recovery, and
   cap validation. Persist `generation-id` in `engine.edn`; change it on
   `--fresh` while preserving the stream cursor.
3. Persist open required requests in engine state and emit stable request IDs,
   meaningful updates, and resolution events. Re-emit open requests on
   startup; make readers idempotent by request ID.
4. Add a local snapshot/cursor and read-after API with explicit retention-gap
   reporting. It need not listen on a public port.
5. Pass validated `engine.events.maxBytes` through `main` to `core/create`,
   with launch overrides taking precedence.
6. Migrate dashboard and local readers from JSONL to EDN lines, with EDN also
   used at HTTP/browser boundaries. Retain explicitly identified read-only
   legacy-log support during migration; do not mix encodings in one file or
   silently reinterpret old records.

The implementation preserves engine scheduling and uses snapshots as the
state authority. It improves observation, bounded retention, and durable
agent requests without turning every internal mutation into a database
event.

## Local API

The engine serves HTTP over `engine/events.sock` with owner-only permissions,
separately from the manual-control socket. Bodies and responses use
`application/edn`:

- `GET /snapshot` returns engine state, outstanding requests, and stream cursor.
- `GET /events?stream-id=…&after=…&limit=…` reads retained events after a cursor
  and reports retention gaps. The maximum page size is 1,000.
- `POST /attention/resolve` accepts `{:request-id "req-…" :reason :handled}`.
  Handling a request acknowledges it; it does not retry its job.

The dashboard reads this API and exposes EDN state, event, and attention
responses to its ClojureScript client. Ordinary observations, notices, and
outstanding required requests are displayed separately.
