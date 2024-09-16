library(readxl)
library(tidyverse)

data.path <- c("dat")

# observations with eDNA
dna.df <- read_xlsx(path = file.path(data.path, "eDNA_detections.xlsx"))
names(dna.df) <- tolower(names(dna.df))

# observations with camera
cam.df <- read_xlsx(path = file.path(data.path, "Camera_detections.xlsx"))
names(cam.df) <- tolower(names(cam.df))

# metadata
met.df <- read_xlsx(path = file.path(data.path, "Method.xlsx"))
names(met.df) <- tolower(names(met.df))
met.df$species <- tolower(met.df$species)




#Three files:
#  1. camera detections (sample, trip, tree, orchard, temperature, canopy, species-detections)
#  2. eDNA detections (sample, trip, tree, orchard, temperature, canopy, species-detections)
#  3. method: group that each column of species belongs to.  Method for splits between the two detection methods.