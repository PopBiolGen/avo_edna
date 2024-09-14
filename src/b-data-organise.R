met.edna <- met.df |>
  filter(Method == "eDNA") |>
  select(-Method)

met.cam <- met.df |>
  filter(Method == "Camera") |>
  select(-Method)
  
id_cols <- function(group.name, method.df){
  method.df$Species[method.df$Group == group.name]
  #which(method.df$Group == group.name)
}
other.cols <- id_cols("Other", met.edna)
dipt.cols <- id_cols("Diptera", met.edna)
hym.cols <- id_cols("Hymenoptera", met.edna)

dna.df.groups <- dna.df |>
  mutate(other = rowSums(across(other.cols)),
         dipt = rowSums(across(dipt.cols)),
         hym = rowSums(across(hym.cols))) |>
  select(other, dipt, hym) |>
  mutate(other = as.numeric(other > 0),
          dipt = as.numeric(dipt > 0),
          hym = as.numeric(hym > 0))
