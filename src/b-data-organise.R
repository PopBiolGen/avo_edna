source("src/a-data-import.R")

# select out the two sets of meta data
met.edna <- met.df |>
  filter(method == "eDNA") |>
  select(-method)

met.cam <- met.df |>
  filter(method == "Camera") |>
  select(-method)

# to get column IDs for a given group
id_cols <- function(group.name, method.df){
  method.df$species[method.df$group == group.name]
}

# to aggregate up to the broader taxonomic groups
aggregate_groups <- function(method.df, obs.df) {
  other.cols <- id_cols("Other", method.df)
  dipt.cols <- id_cols("Diptera", method.df)
  hym.cols <- id_cols("Hymenoptera", method.df)
  
  obs.df.groups <- obs.df |>
    mutate(other = rowSums(pick(any_of(other.cols))),
           dipt = rowSums(pick(any_of(dipt.cols))),
           hym = rowSums(pick(any_of(hym.cols)))) |>
    mutate(other = as.numeric(other > 0),
            dipt = as.numeric(dipt > 0),
            hym = as.numeric(hym > 0))

  obs.df.groups
}

# add aggregate groups to each data frame
dna.df <- aggregate_groups(met.edna, dna.df)
cam.df <- aggregate_groups(met.cam, cam.df)

dna.agg <- dna.df |>
  rename(canopy = upper_lower) |>
  mutate(method = rep("eDNA", n()))

cam.agg <- cam.df |>
  rename(canopy = upper_lower) |>
  mutate(method = rep("Camera", n()))

comb.df <- merge(dna.agg, cam.agg, all = TRUE)

rm(dna.df, cam.df, dna.agg, cam.agg, met.cam, met.df, met.edna)
