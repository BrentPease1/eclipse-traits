library(tidyverse)
library(here)
library(glmmTMB)
library(spdep)

load( here::here("Results/total_eclipse_maximum.RData"))

resi <- resid(m) # residuals for total eclipse, p(voc) analysis

df <- final |> 
  add_column(resid = resi) 

# setup for Moran's I test
coords <- cbind(df$lon, df$lat)
knn <- knearneigh(coords, k = 5, longlat = TRUE)
nb  <- knn2nb(knn)
lw  <- nb2listw(nb, style = "W")

( mt <- moran.test(df$resid, lw) ) # Moran's I test across species

# same thing, but doing it individually for species
species <- unique(df$sci)
sp.p <- list(list())
for(i in 1:length(species)){
  
  df2 <- df |>
    filter(sci == species[i])
  
  coords <- cbind(df2$lon, df2$lat)
  
  if(nrow(coords) < 6) next # omit species that are detected at fewer than 6 sites
  
  knn <- knearneigh(coords, k = 5, longlat = TRUE)
  nb  <- knn2nb(knn)
  lw  <- nb2listw(nb, style = "W")
  
  mt <- moran.test(df2$resid, lw)
  
  sp.p[[i]] <- tibble(
    sci = species[i],
    p = mt$p.value
  )
  
  print(paste("finished", i, "of", length(species)))
}

# 6/161 speices with significant p-values
bind_rows(sp.p) |> 
  arrange(p) |> 
  filter(p < 0.05)
