---
name: villager-house-10
title: Ten-bed villager house
tags: shelter
description: An 8x8 sleeping room with ten beds, six floor lights, a roof and an east service airlock.
front: south
foundation: flat
clearance: 1
difficulty: medium
params: block=cobblestone, wood=oak, bed=white
by: blueprint
notes: The anchor is the northwest outside floor cell at foot height; the sleeping-room anchor is x+1,z+1. Close and verify gates before admitting residents.
---

# Ten-bed villager house

Build with the existing blueprint.check and blueprint.build commands before transporting residents.
For the west river site, the blueprint anchor is (-144,65,-176). The matching breeder
uses x=-143 y=65 z=-175 size=8 airlock=true entryX=-135 entryZ=-168, target up to ten.
The east airlock's inner gate keeps existing residents apart from later arrivals.
The roof is sealed; no separate claim is made about lighting its outside surface.

```legend
S {block}|@solid
W {block}|cobblestone|cobbled_deepslate|stone|oak_planks|spruce_planks|birch_planks|jungle_planks|acacia_planks|dark_oak_planks|mangrove_planks|cherry_planks|pale_oak_planks|bamboo_planks|crimson_planks|warped_planks
G {wood:fence_gate}[facing=east,open=false]|{wood:fence_gate}[facing=west,open=false]|oak_fence_gate[facing=east,open=false]|oak_fence_gate[facing=west,open=false]|spruce_fence_gate[facing=east,open=false]|spruce_fence_gate[facing=west,open=false]|birch_fence_gate[facing=east,open=false]|birch_fence_gate[facing=west,open=false]|jungle_fence_gate[facing=east,open=false]|jungle_fence_gate[facing=west,open=false]|acacia_fence_gate[facing=east,open=false]|acacia_fence_gate[facing=west,open=false]|dark_oak_fence_gate[facing=east,open=false]|dark_oak_fence_gate[facing=west,open=false]|mangrove_fence_gate[facing=east,open=false]|mangrove_fence_gate[facing=west,open=false]|cherry_fence_gate[facing=east,open=false]|cherry_fence_gate[facing=west,open=false]|pale_oak_fence_gate[facing=east,open=false]|pale_oak_fence_gate[facing=west,open=false]|bamboo_fence_gate[facing=east,open=false]|bamboo_fence_gate[facing=west,open=false]|crimson_fence_gate[facing=east,open=false]|crimson_fence_gate[facing=west,open=false]|warped_fence_gate[facing=east,open=false]|warped_fence_gate[facing=west,open=false] #entrance
F {bed}_bed[facing=south,part=foot]|white_bed[facing=south,part=foot]|orange_bed[facing=south,part=foot]|magenta_bed[facing=south,part=foot]|light_blue_bed[facing=south,part=foot]|yellow_bed[facing=south,part=foot]|lime_bed[facing=south,part=foot]|pink_bed[facing=south,part=foot]|gray_bed[facing=south,part=foot]|light_gray_bed[facing=south,part=foot]|cyan_bed[facing=south,part=foot]|purple_bed[facing=south,part=foot]|blue_bed[facing=south,part=foot]|brown_bed[facing=south,part=foot]|green_bed[facing=south,part=foot]|red_bed[facing=south,part=foot]|black_bed[facing=south,part=foot]
H {bed}_bed[facing=south,part=head]|white_bed[facing=south,part=head]|orange_bed[facing=south,part=head]|magenta_bed[facing=south,part=head]|light_blue_bed[facing=south,part=head]|yellow_bed[facing=south,part=head]|lime_bed[facing=south,part=head]|pink_bed[facing=south,part=head]|gray_bed[facing=south,part=head]|light_gray_bed[facing=south,part=head]|cyan_bed[facing=south,part=head]|purple_bed[facing=south,part=head]|blue_bed[facing=south,part=head]|brown_bed[facing=south,part=head]|green_bed[facing=south,part=head]|red_bed[facing=south,part=head]|black_bed[facing=south,part=head]
T torch
```

## y-1

```layer
SSSSSSSSSS
SSSSSSSSSS
SSSSSSSSSS
SSSSSSSSSS
SSSSSSSSSS
SSSSSSSSSS
SSSSSSSSSS
SSSSSSSSSS
SSSSSSSSSS
SSSSSSSSSS
```

## y0

```layer
WWWWWWWWWW
W.F.F.F.FW
W.H.H.H.HW
W.T...T.TW
W.F.F.F.FW
G.H.H.H.HW
W.T...T.TW
W.F.F.WWWW
W.H.H.G..G
WWWWWWWWWW
```

## y1

```layer
WWWWWWWWWW
W........W
W........W
W........W
W........W
.........W
W........W
W.....WWWW
W.........
WWWWWWWWWW
```

## y2

```layer
WWWWWWWWWW
W........W
W........W
W........W
W........W
W........W
W........W
W.....WWWW
W.....W..W
WWWWWWWWWW
```

## y3

```layer
WWWWWWWWWW
WWWWWWWWWW
WWWWWWWWWW
WWWWWWWWWW
WWWWWWWWWW
WWWWWWWWWW
WWWWWWWWWW
WWWWWWWWWW
WWWWWWWWWW
WWWWWWWWWW
```
