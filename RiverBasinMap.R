#################################################
# River Basin Map with R
# 10/12/2023
# Jahir Raihan
#
#################################################

# Library
libs <- c(
  "tidyverse", "sf", 
  "giscoR"
) 

installed_libraries <- libs %in% rownames(
  install.packages()
)

if(any(installed_libraries == F)){
  install.packages(
    libs[!installed_libraries]
  )
}

invisible(
  lapply(
    libs, library,
    character.only = T
  )
)

# 1. GET COUNTRY BORDERS
#--------------------------------

get_county_borders <- function(){
  country_borders <- giscoR::gisco_get_countries(
    resolution = "3",
    country = "BR"
  )
  return(country_borders)
}

country_borders <- get_county_borders()


# 1. GET BASIN
#--------------------------------
# https://www.hydrosheds.org/products/hydrorivers
# https://www.hydrosheds.org/products/hydrobasins
# https://www.hydrosheds.org/file/HydroBASINS/standard/hybas_sa_lev03_v1c.zip

get_basin <- function(){
  url <- "https://www.hydrosheds.org/file/HydroBASINS/standard/hybas_sa_lev03_v1c.zip"
  file_name <- "hybas_sa_lev03_v1c.zip"

  download.file(
    url = url ,
    destfile = file_name,
    mode = "wb"
  )

  unzip(file_name)
}

get_basin()

list.files('E:/Programming/Rstudio/R_Project/youtube/MilosMakesMaps/hybas_sa_lev03_v1c')

load_basin <- function(){
  filenames <- list.files(
    path = 'E:/Programming/Rstudio/R_Project/youtube/MilosMakesMaps/hybas_sa_lev03_v1c',
    pattern = ".shp$",
    full.names = T
  )
  samerica_basin <- sf::st_read(
   filenames 
  )

  return(samerica_basin)
}

samerica_basin <- load_basin()

sf::sf_use_s2(F)

brazil_basin <- samerica_basin |>
  sf::st_intersection(
    country_borders
  ) |>
  dplyr::select(
    HYBAS_ID
    )



# 1. GET RIVER DATA
#--------------------------------
# https://www.hydrosheds.org/products/hydrorivers
# https://www.hydrosheds.org/products/hydrobasins
# https://www.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_sa.zip


get_rivers <- function(){
  url <- "https://www.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_sa_SHP.zip"
  file_name <- "HydroRIVERS_v10_sa.zip"

  download.file(
    url = url,
    destfile = file_name,
    mode = "wb"
  )

  unzip(file_name)
}

get_rivers()

list.files('E:/Programming/Rstudio/R_Project/youtube/MilosMakesMaps/HydroRIVERS_v10_sa_SHP')

load_rivers <- function(){
  filenames <- list.files(
    path = 'E:/Programming/Rstudio/R_Project/youtube/MilosMakesMaps/HydroRIVERS_v10_sa_SHP',
    pattern = ".shp$",
    full.names =  T
  )

  samerica_rivers <- sf::st_read(
    filenames
  )

  return(samerica_rivers)
}

samerica_rivers <- load_rivers()

brazil_rivers <- samerica_rivers |>
  dplyr::select(
    ORD_FLOW

  ) |>
  sf::st_intersection(
    country_borders
  )

# 4. Determine Basin for Every River
#--------------------------------------

brazil_river_basin <- sf::st_intersection(
  brazil_rivers,
  brazil_basin
)

# 5. River Width
# ==============================

unique(brazil_river_basin$ORD_FLOW)

brazil_river_basin_width <- brazil_river_basin |>
  dplyr::mutate(
    width = as.numeric(
      ORD_FLOW
    ),
    width = dplyr::case_when(
      width == 1 ~  .8,
      width == 2 ~  .7,
      width == 3 ~  .6,
      width == 4 ~  .45,
      width == 5 ~  .35,
      width == 6 ~  .25,
      width == 7 ~  .2,
      width == 8 ~  .15,
      width == 9 ~  .1,
      TRUE ~ 0
    )
  ) |>
  sf::st_as_sf()


  # 6. Plot
  # ----------------

  unique(
    brazil_river_basin_width$HYBAS_ID
  )

  hcl.pals("qualitative")

  p <- ggplot() +
  geom_sf(
    data = brazil_river_basin_width,
    aes(
      color = factor(
        HYBAS_ID
      ),
      size = width,
      alpha = width
    )
  ) +
  scale_color_manual(
    name = "",
    values = hcl.colors(
      14, "Dark 3",
      alpha = 1
    )
  ) +
  scale_size(
    range = c(.1, .7)
  ) +
  scale_alpha(
    range= c(.01, .3)
  ) +
  theme_void()+
  theme(
    legend.position = "none",
    plot.caption = element_text(
      size = 9, color = "grey60",
      hjust = .1, vjust = 10
    ),
    plot.margin = unit(
      c(t = 0, r = 0, b =0, l = 0),
      "lines"
    ),
    plot.background = element_rect(
      fill = "black",
      color = NA
    ),
    panel.background = element_rect(
      fill = "black",
      color = NA 
    )
  )+ 
  labs(
    title = "",
    x = "",
    y = "",
    caption = "Source: @World Wildlife Fund, Inc. (2006-2013) HydroSHED"
  )
  
  
  ggsave(
    filename = "us-river-basins.png",
    width = 7, height = 7.75, dpi = 600,
    bg = "white", device = "png", p
  )
  
  st_write(brazil_river_basin_width, "us-river-basins.geojson", layer = NULL, driver = "GeoJson")
  