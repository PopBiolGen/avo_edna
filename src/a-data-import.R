library(tidyverse)

data.path <- c("dat/Occupancy")

# species data
sp.df <- read.csv(file = file.path(data.path, "eDNA_all_species.txt"), sep = "\t")

# environment data (trip = season; trip 2 is peak)
env.df <- read.csv(file = file.path(data.path, "eDNA_env.txt"), sep = "\t")

comb.df <- cbind(env.df, sp.df)
