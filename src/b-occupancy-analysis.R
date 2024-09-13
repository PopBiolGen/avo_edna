# import the data
source("src/a-data-import.R")

library(unmarked)

# pick a species
d.select <- comb.df |>
    select(tree, orchard, trip, canopy, contains("Apis")) |> # pick a species
    mutate(site = paste(orchard, tree, sep = ".")) |> # make a site identifier
    arrange(site, trip) |>
    group_by(site, trip) |>
    mutate(sample.no.within.trip = row_number()) |> # make a within-trip sample number identifier
    ungroup() |>
    group_by(site) |>
    mutate(sample.no.within.site = row_number()) |> # make a within site sample number identifier
    ungroup() |>
    rename(pres = Apis_mellifera) # give a generic name to presence/absence data

# Some dimensions
# MM = number of sites
# TT is number of "seasons"
# JJ = maximum sampling periods per site.season
MM <- length(unique(d.select$site)) # total number of trees
TT <- max(d.select$trip)
JJ <- max(d.select$sample.no.within.trip)


# Organise data for unmarked...
# observations... an M x TJ matrix
y <- d.select |>
  select(pres, site, sample.no.within.site) |>
  pivot_wider(names_from = sample.no.within.site, values_from = pres) |>
  select(-site) |>
  as.matrix()

# Site covariates dataframe with M rows, column for each covariate
sc <- d.select |>
    select(orchard, site) |>
    group_by(site) |>
    summarise(orchard = first(orchard))

# site.year covariates, a data frame with MT rows (site-major, season-minor order)
syc <- d.select |>
    select(site, trip, sample.no.within.trip) |>
    mutate (trip2 = trip) |>
    pivot_wider(names_from = sample.no.within.trip, values_from = trip2) |>
    select(-site, -trip)
    
    

# Observation covariates (there is only one at this point)
oc <- d.select |>
  select(canopy, site, sample.no.within.site) |>
  pivot_wider(names_from = sample.no.within.site, values_from = canopy) |>
  select(-site) |>
  as.matrix()

umf <- unmarkedMultFrame(y = y, siteCovs = sc, obsCovs = list(canopy = oc), numPrimary = TT, yearlySiteCovs = syc)

fit <- occu(~ 1 + canopy # detection
            ~ 1 + trip + , # occupancy
            data = umf)
summary(fit)
