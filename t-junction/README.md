# Notes for the T junction setup #

## Congestion creation ##
Very high density setup (see "flows-full-gridlock.json" file) shows good "crash" in pedestrian flow using Edie's definitions of flows.
The density-speed diragram looks also very nice. For the density-flow diagram, from approx. 1.7pax/m² there flows starts breaking up.
Hence the target density inside the zone is set to **1.5pax/m²**.

## Standard horizontal flow ##
The maximum flow which can be allowed along the main part of the corridor is **0.6pax/s** per direction. This is based on 
plain observation of the video. *This could be investigated deeper.* By increasing the opposing flows to 1.0pax/s (light) congestion can be created in the main corridor.

The pedestrain flow coming in from the side should be set at 1.8pax/s in order to have congestion occuring at the intersection
but not gridlock. *Again, this value is determined by observation.* This total flow is split into two flows each going left and right
after the intersection. This situation is setup in the file "flows-standard-gate.json". 

## Parameter exploration for gating ##





