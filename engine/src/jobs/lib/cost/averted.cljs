(ns jobs.lib.cost.averted
  "Broad-guess priors (Q49/Q115) of the loss a reflex averts where its trigger prices none (Q151): seconds of the body's
  time, per opening, slot, cell or call, or hp. To be tuned from the bodies' events.")

(def open-door-s "A door a walk left open lets a mob in: a fight or a way round it, about a minute." 60)
(def open-gate-s "A pen gate left open lets an animal stray: herding it back or losing it, a couple of minutes." 120)
(def short-slot-s "A slot short of the free ones wanted leaves the next pickups' stack on the ground: gathering it again, about half a minute." 30)
(def left-cell-s "A cell left broken in another's ground costs its owner the find and the fix: about a minute." 60)
(def riding-s "A body left riding walks nowhere: its jobs' walks fail until it gets off, about a minute." 60)
(def wedged-s "A body wedged in a block walks nowhere: its jobs' walks fail until it is free, about a minute." 60)
(def burn-hp "hp a body still alight once out of the flames loses: vanilla burns it an hp a second, 8 s after fire, 15 s after lava; the longer." 15)
