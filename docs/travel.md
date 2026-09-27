# Checked travel itineraries

`travel x= y= z= plan=true` compares complete trips: approaching a vehicle, boarding, riding, landing, and the final walk. Without a usable supplied vehicle it selects walking. Remove `plan=true` to execute; `mode=walk|rail|horse|boat` requires that mode. `return=true` adds the estimated cost of walking back but does not execute a return.

Use only explicitly authorized vehicles. The returned `resume` command preserves the itinerary, which is checked again each time.

- **Boat:** `boat=<id> shore=<x:y:z>` supplies an ordinary wooden boat and an integer dry landing feet cell. The planner finds a dry boarding stance and derives the fractional water approach needed for the server's dismount rule. The loaded route must be level source water with room for the entire hull and rider, at most 128 blocks per leg. Chest boats, rafts, currents, bubbles, ice and land travel are unavailable. A traveler already alone in that boat's controlling seat may continue directly. Walking resumes only after the server confirms the checked landing.
- **Rail:** `cart=<id> track=<x:y:z,x:y:z> exit=<x:y:z>` supplies an empty cart, a straight flat track, and a dry exit. The track needs a normal launch rail, powered middle section, three unpowered braking rails, an end buffer, and safe platforms.
- **Horse:** `horse=<id>` supplies an adult, already tamed and saddled horse, donkey, or mule with a confirmed movement-speed attribute. Its loaded corridor must be flat, dry, clear for horse and rider, and at most 128 blocks. Preparation remains explicit through `tame` and `horse_saddle`.

Example: `travel x=120 y=64 z=8 boat=42 shore=112:64:6 plan=true` compares reaching the requested destination via that shore with walking and any other supplied itinerary.

ETAs are estimates based on geometric walking distances and conservative vehicle speeds, including setup and arrival time. Vehicle failure or cancellation does not imply a safe dismount. Inspect the reported mounted state; failed or unconfirmed landing stops the itinerary before its final walk. Routes do not build infrastructure, clear obstacles, harvest crops, or borrow unobserved vehicles.

For resource discovery along the way, use `forage.search transport=auto|boat boat=<id>` with short water legs and mounted scans. Use `travel` for a known destination.
