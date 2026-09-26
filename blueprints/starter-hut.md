---
name: starter-hut
title: Starter hut
tags: shelter, storage
description: A 5x5 one-room hut with a bed, a chest, a crafting table, a torch on the floor and a closed interior.
front: south
foundation: flat
clearance: 1
difficulty: easy
params: wood=oak, stone=cobblestone, bed=white
by: blueprint
notes: The floor is dug one block into the ground (layer y-1). Place it on flat ground; the door is laid last from outside.
---

# Starter hut

Written for this library, not transcribed. The door faces the blueprint's south; `facing=` turns the whole hut. The
torch stands on the floor: a wall torch needs a click face `place` cannot yet be told, and a torch over the crafting
table is clicked against the plainer wall beside it and comes out as a wall torch (Pacer's hut, 09-26).

```legend
S  {stone:block}
L  {wood:log}[axis=y]
P  {wood:planks}
G  glass_pane
D  {wood:door}[facing=north,half=lower,hinge=left,open=false]   #entrance
d  {wood:door}[facing=north,half=upper,hinge=left,open=false]   #entrance
F  {bed}_bed[facing=east,part=foot]
H  {bed}_bed[facing=east,part=head]
C  chest[facing=south,type=single]
T  crafting_table
^  {wood:stairs}[facing=west,half=bottom]
i  torch
```

## y-1

```layer
SSSSS
SPPPS
SPPPS
SPPPS
SSSSS
```

## y0

```layer
LSSSL
S^.CS
SFH.S
ST.iS
LSDSL
```

## y1

```layer
LSSSL
S...S
G...G
S...S
LSdSL
```

## y2

```layer
LSSSL
S...S
S...S
S...S
LSSSL
```

## y3

```layer
PPPPP
PPPPP
PPPPP
PPPPP
PPPPP
```
