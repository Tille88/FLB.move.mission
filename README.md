# FLB.move.mission
Utility functions to read in and alter location/orinetation of .mission files

## Installation
```r
devtools::install_github("Tille88/FLB.move.mission/FLB.move.mission")
```

## Example code of usage
```r
###############
# EXAMPLE
library(FLB.move.mission)
library(maptools)

setwd("./location_with_files")
# If .plan
coordinates_and_file = read_in_mission_file("./missions-3-flying8wfence.plan")
# If .mission
#coordinates_and_file = read_in_mission_file("./01_First_mission.mission")
coordinates = coordinates_and_file[[1]]


rotated = rotate_geo_formatted(coordinates, 90, 2)

plot(rotated)
plot(rbind(coordinates, rotated))
text(coordinates[,'alt'],  row.names(coordinates),
     cex=0.65, pos=3,col="red")

#(LAT, LON) maps to (Y, X)
moved = move_to_coordinates(rotated, 2, new_lon = coordinates(rotated)[1,2]+0.005)
#default plot does have axes mixed up, will map as above when read into QGC
plot(rbind(coordinates, rotated, moved))

rotated2 = rotate_geo_formatted(moved, 90, 2)
plot(rbind(coordinates, rotated, moved, rotated2))
text(rbind(coordinates, rotated, moved, rotated2)[,'alt'],  row.names(coordinates),
     cex=0.65, pos=3,col="red")

# Read in .plan files are written in .mission format
# This takes care of landing, and waypoints
# Takeoff seems to be stuck at planned home position, which is untouched.
write_mission_file(rotated2,
                   filepath = "./01_mission_altered.mission",
                   coordinates_and_file[[2]],
                   coordinates_and_file[[3]])

```
