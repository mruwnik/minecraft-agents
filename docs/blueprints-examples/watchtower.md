---
name: watchtower
title: Ladder watchtower
tags: lookout, decorative
description: A 3x3 stone shaft with a ladder, a door at the foot and a fenced 5x5 lookout platform eleven blocks up.
front: south
foundation: flat
clearance: 2
difficulty: medium
params: wood=oak, stone=cobblestone
by: blueprint
notes: The platform overhangs the shaft by one block on every side, so it is laid outward from the shaft top. Layers y10 to y12 are out of reach from the ground and need a scaffold.
---

# Ladder watchtower

Written for this library. `_` cells are not part of the tower: whatever stands there is left alone. The lantern under the
shaft floor lights the ladder all the way up, so the shaft never reads as an unlit room.

```legend
S  {stone:block}
J  jack_o_lantern
H  ladder[facing=south]
D  {wood:door}[facing=north,half=lower,hinge=right,open=false]   #entrance
d  {wood:door}[facing=north,half=upper,hinge=right,open=false]   #entrance
P  {wood:planks}
^  {wood:trapdoor}[facing=south,half=top,open=false]
#  {wood:fence}
i  torch
```

## y-1

```layer
_____
_SSS_
_SJS_
_SSS_
_____
```

## y0

```layer
_____
_SSS_
_SHS_
_SDS_
_____
```

## y1

```layer
_____
_SSS_
_SHS_
_SdS_
_____
```

## y2..y9

The same layer eight times: a header naming a range repeats its grid, the one shorthand the format allows.

```layer
_____
_SSS_
_SHS_
_SSS_
_____
```

## y10

```layer
PPPPP
PPPPP
PP^PP
PPPPP
PPPPP
```

## y11

```layer
#####
#...#
#...#
#...#
#####
```

## y12

```layer
i___i
_____
_____
_____
i___i
```
