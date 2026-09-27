---
name: wheat-field
title: Fenced wheat field
tags: farm
description: 36 wheat around one covered channel, fenced, with a gate onto the channel lane and a torch on every corner post.
front: east
foundation: flat
clearance: 2
difficulty: easy
params: wood=oak
by: blueprint
notes: The same field a farm.plan map of five rows would describe, written in the blueprint format to show how farms migrate.
---

# Fenced wheat field

The channel is a row of waterlogged top slabs laid in the ground layer: water under a floor the body walks on. Every
wheat cell is within two blocks of it, so the whole field is served from the lane and stays wet.

```legend
r  dirt|grass_block                         #ground
f  farmland                                 #ground
=  {wood:slab}[type=top,waterlogged=true]   #ground #cover #lane
w  wheat[age=0]                             #crop
#  {wood:fence}
G  {wood:fence_gate}[facing=east,open=false]
i  torch
.  air                                      #lane
```

## y-1

```layer
rrrrrrrrrrr
rfffffffffr
rfffffffffr
r=========r
rfffffffffr
rfffffffffr
rrrrrrrrrrr
```

## y0

```layer
###########
#wwwwwwwww#
#wwwwwwwww#
#.........G
#wwwwwwwww#
#wwwwwwwww#
###########
```

## y1

```layer
i_________i
___________
___________
___________
___________
___________
i_________i
```
