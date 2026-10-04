# Recent world entity observations

Each engine body now keeps an in-memory cache of the entities the Minecraft
server tracks for that connection. "Seen" means server-loaded/tracked data,
not raycast visibility. It includes players, mobs and other entity types,
without the ordinary primitive's radius or 32-result limits. The body's own
player entity is included when tracked and marked `:self? true`. Mineflayer's
own player-list UUID supplies its identity when the login entity omits the UUID.

`engine.entity-observations` samples the local Mineflayer entity collection once
per second. Every tracked entity refreshes its `:observed-at` timestamp and its
`:expires-at` timestamp 120000 milliseconds later. Unloaded entities remain as
last-known observations until that expiry. Disconnects, deliberate offline time
and stale physics stop refreshing the old connection's entities. Reads never
refresh timestamps. The cache is cleared by process restart and writes no notes,
roster, entity files or events. Recording sends no extra Minecraft queries and
makes no model calls.

Each entity contains its world, dimension, position and normalized entity type.
When Mineflayer supplies a UUID, its public key is `<type>/<UUID>`. The cache is scoped by world; dimension belongs to the latest location, so a
known entity moving between dimensions replaces its earlier location. Without a UUID, the key contains the type, body,
process session, connection generation and entity-object generation. These
explicitly ephemeral keys survive repeated samples of one object but are not
stable after entity reload or reconnect. They do not merge unrelated runtime IDs
between bodies. Player usernames are display data; UUIDs remain identity.

Explicit `entityDead` removes the matching observation immediately. The same
old entity object cannot be resampled after death; a newly spawned object can be
observed normally. Ordinary `entityGone` or chunk unloading is not death and
retains the last observation for the remaining TTL. One observer may still have
older information than another; the world reader merges valid observations by
world and key, preferring the newest observed timestamp.

The read-only endpoint is `GET /entities` on each body's existing HTTP control
Unix socket. It needs no manual-control lease and returns `application/edn`:

```clojure
{:ok true :world "claude" :body "Probe" :now 1000
 :ttl-ms 120000 :online? true
 :entities [{:key "villager/<uuid>" :type "villager" :uuid "<uuid>"
             :identity :uuid :id 123 :world "claude" :dimension "overworld"
             :pos {:x 1 :y 64 :z 2} :observed-at 1000 :expires-at 121000}]
 :count 1 :cached-count 1 :cap 10000 :snapshot-cap-bytes 4194304
 :truncated? false :dropped 0}
```

The cache holds at most 10000 entities. Responses additionally fit within 4 MiB;
`:truncated?` and `:dropped` explicitly report omitted observations. TTL expiry
is also applied when snapshots are read. There is no persistent census or
historical archive. The dashboard reads snapshots and preserves their remaining
expiry; it does not extend TTL during polling. Older running engines may lack
this endpoint and must be reported as such without inventing entity data.

Existing UUID-based search notes in `engine.notes` remain a separate feature,
with their existing TTL and 2000-entry cap. Authored village membership and job
assignments remain separate from automatic entity observations. The compact
agent map query and durable change ledger currently cover markers, zones,
claims, plans and blueprints; observed entities are not yet part of that delta
API.

The dashboard polls engine sockets in the background when its state or entity
pages are requested. At most four reads run at once, each with a one second
wall clock timeout and a 4 MiB response limit. The dashboard retains at most
128 body sources and 20000 observation copies, and returns at most 10000 merged
entities within a 4 MiB aggregate budget. Limit truncation is explicit. Bodies
whose running engine predates `/entities` show a restart required status; the
dashboard does not restart them. Socket failures preserve the previous actual
observation time only until its original expiry.

UUID observations merge within their world before the requested dimension is
selected. A newer Nether observation therefore replaces an older Overworld
location. Map markers and villager cards also expire against a browser clock
once per second, including while dashboard requests fail. Engine body markers
remain distinct; the same players are excluded from the generic entity layer.
The canvas clips generic markers to the viewport and limits zoomed labels to
200 entities. Pointer hover uses the drawn layout rather than rebuilding it.
