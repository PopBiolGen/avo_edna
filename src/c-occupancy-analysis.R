# import the data
source("src/b-data-organise.R")

library(unmarked)

# pick a species
# selects a data frame with just the explanatory variables and target species
select_species <- function(comb.df, sp.name) {
    d.select <- comb.df |>
      select(tree, orchard, trip, canopy, method, contains(sp.name)) |> # pick a species
      mutate(site = paste(orchard, tree, trip, sep = ".")) |> # make a site identifier
                                                              #treat trip as different site for changing occupancy)
      arrange(site, trip, method) |>
      group_by(site, trip) |>
      mutate(sample.no.within.trip = row_number()) |> # make a within-trip sample number identifier
      ungroup() |>
      group_by(site) |>
      mutate(sample.no.within.site = row_number()) |> # make a within site sample number identifier
      ungroup() |>
      rename(pres = {{sp.name}}) # give a generic name to presence/absence data
  d.select
}

d.select <- select_species(comb.df, sp.name = "scaptomyza_australis")

# Some dimensions
# R =- number of sites
# J = maximum sampling periods per site
R <- length(unique(d.select$site)) # total number of trees
J <- max(d.select$sample.no.within.site)

# Organise data for unmarked...
# observations... and R x J matrix of detection non-detection
y <- d.select |>
  select(pres, site, sample.no.within.site) |>
  mutate(sample.no.within.site = as.factor(sample.no.within.site)) |>
  complete(sample.no.within.site = levels(sample.no.within.site), fill = list(pres = 0)) |> # if observations missing for this species, fill with zeros
  pivot_wider(names_from = sample.no.within.site, values_from = pres) |>
  select(-site) |>
  as.matrix()

# Site covariates R rows, column for each covariate
sc <- d.select |>
    select(orchard, trip, site) |>
    group_by(site) |>
    summarise(orchard = first(orchard), season = as.character(first(trip))) |>
    ungroup() |>
    select(-site)

# site.year covariates
#syc <- d.select()

# Observation covariates (canopy and method)
# a named list of matrices each RxJ
canopy <- d.select |>
  select(canopy, trip, site, sample.no.within.site) |>
  pivot_wider(names_from = sample.no.within.site, values_from = canopy) |>
  select(-trip, -site) |>
  as.matrix()

method <- d.select |>
  select(method, trip, site, sample.no.within.site) |>
  pivot_wider(names_from = sample.no.within.site, values_from = method) |>
  select(-trip, -site) |>
  as.matrix()

oc <- list(canopy = canopy, method = method)

umf <- unmarkedFrameOccu(y = y, siteCovs = sc, obsCovs = oc)

fit <- occu(~ 1 + canopy + method # detection
            ~ 1 + orchard + season, # occupancy
            data = umf)
summary(fit)
