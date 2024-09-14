library(readxl)
library(tidyverse)

data.path <- c("dat")

# observations with eDNA
dna.df <- read_xlsx(path = file.path(data.path, "eDNA_detections.xlsx"))

# observations with camera
cam.df <- read_xlsx(path = file.path(data.path, "Camera_detections.xlsx"))

# metadata
met.df <- read_xlsx(path = file.path(data.path, "Method.xlsx"))




#Three files:
#  1. camera detections (sample, trip, tree, orchard, temperature, canopy, species-detections)
#  2. eDNA detections (sample, trip, tree, orchard, temperature, canopy, species-detections)
#  3. method: group that each column of species belongs to.  Method for splits between the two detection methods.