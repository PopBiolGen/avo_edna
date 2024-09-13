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
# R =- number of sites
# J = maximum sampling periods per site
R <- length(unique(paste0(comb.df$orchard, comb.df$tree))) # total number of trees
J <- max(comb.df$trip)

# Organise data for unmarked...
# observations... and R x J matrix of detection non-detection
y <- d.select |>
  select(pres, trip, site, sample.no.within.site) |>
  pivot_wider(names_from = sample.no.within.site, values_from = pres) |>
  select(-trip, -site) |>
  as.matrix()

# Site covariates R rows, column for each covariate
sc <- d.select |>
    select(orchard, site) |>
    group_by(site) |>
    summarise(orchard = first(orchard))

# site.year covariates
#syc <- d.select()

# Observation covariates (there is only one at this point)
# a named list of matrices each RxJ
oc <- d.select |>
  select(canopy, trip, site, sample.no.within.site) |>
  pivot_wider(names_from = sample.no.within.site, values_from = canopy) |>
  select(-trip, -site) |>
  as.matrix()

umf <- unmarkedFrameOccu(y = y, siteCovs = sc, obsCovs = list(canopy = oc))
