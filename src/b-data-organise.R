source("src/a-data-import.R")

# select out the two sets of meta data
met.edna <- met.df |>
  filter(Method == "eDNA") |>
  select(-Method)

met.cam <- met.df |>
  filter(Method == "Camera") |>
  select(-Method)

# to get column IDs for a given group
id_cols <- function(group.name, method.df){
  method.df$Species[method.df$Group == group.name]
}

# to aggregate up to the broader taxonomic groups
aggregate_groups <- function(method.df, obs.df) {
  other.cols <- id_cols("Other", method.df)
  dipt.cols <- id_cols("Diptera", method.df)
  hym.cols <- id_cols("Hymenoptera", method.df)
  
  obs.df.groups <- obs.df |>
    mutate(other = rowSums(across(other.cols)),
           dipt = rowSums(across(dipt.cols)),
           hym = rowSums(across(hym.cols))) |>
    select(other, dipt, hym) |>
    mutate(other = as.numeric(other > 0),
            dipt = as.numeric(dipt > 0),
            hym = as.numeric(hym > 0))
  
  cbind(obs.df, obs.df.groups)
}

# add aggregate groups to each data frame
dna.df <- aggregate_groups(met.edna, dna.df)
cam.df <- aggregate_groups(met.cam, cam.df)

dna.agg <- dna.df |>
  select(-contains("_")) |>
  mutate(method = rep("eDNA", n()))

cam.agg <- cam.df |>
  select(-contains("_"), -contains("dae")) |>
  mutate(method = rep("Camera", n()))

comb.df <- rbind(dna.agg, cam.agg)
