# to fit all species and store results in an R object.
source("src/c-occupancy-analysis.R")

sp.names <- colnames(comb.df)[!(colnames(comb.df) %in% 
                                c("sample", 
                                  "trip", 
                                  "tree", 
                                  "orchard", 
                                  "temperature",
                                  "canopy",
                                  "method"))]

sp.fits <- lapply(sp.names, FUN = "fit_species")
names(sp.fits) <- sp.names

save(sp.fits, file = "out/species-fits.RData")
