### Time to visualize the numbers
# 25 Feb 2025 by CRD

# mapping trees to core!
library(leaflet)
library(tidyverse)
library(sf)
library(mapview)
library(leaflet.extras2)
library(htmlwidgets)
library(mapview)
# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# set wd
setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/")
# read the list of trees I downloaded from the arboretum's website
d <- read.csv("output/cleanTS.csv", header=TRUE)
nrow(d)
# the count per individual
nbobsperID <- d %>% count(plantNickname)
head(nbobsperID)
sum(nbobsperID$n)

# select only one row per ID
dnodup <- d[!duplicated(d$plantNickname),]
color_palette <- c(
  "#d62728",  # red oak
  "#aec7e8",  # river birch
  "#ff7f0e",  # sugar maple
  "#1f77b4",  # yellow buckeye
  "#bcbd22",  # yellow birch
  "#8c564b",  # Shagbark Hickory
  "#e377c2",  # american basswood
  "#7f7f7f",  # eastern cottonwood
  "#9467bd",  # pignut hickory
  "#17becf"  # white oak
   # red maple
)

# Ensure enough colors for unique species
sppvec <- unique(dnodup$latbi)
color_palette_extended <- rep(color_palette, length.out = length(sppvec))

# Convert spp_colors to a named list
spp_colors <- as.list(setNames(color_palette_extended, sppvec))

# Calculate size range based on n
size_range <- range(nbobsperID$n)
size_labels <- round(seq(size_range[1], size_range[2], length.out = 3))  # 3 labels for small, medium, large
size_sizes <- sqrt(size_labels) / 20  # Match the radius scaling

tree_map <- leaflet(dnodup) %>%
  addTiles() %>%  
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%  
  addCircleMarkers(
    lng = ~long, 
    lat = ~lat, 
    popup = ~paste("Species:", latbi, "<br>Nickname:", plantNickname),
    radius = 5,#sqrt(nbobsperID$n) / 10,  # Scale radius by sqrt(n)
    color = ~unname(spp_colors[latbi]),  # Convert named list to values
    fillOpacity = 5
  ) %>%
  # Add a special symbol at the given coordinates
  addAwesomeMarkers(
    lng = -71.13373297370092, lat = 42.29513825149586,
    icon = awesomeIcons(
      icon = "tree",
      iconColor = "white",
      markerColor = "red",
      library = "fa"
    ),
    popup = "Weld Hill Common Garden"
  ) %>%
  addLayersControl(
    baseGroups = c("Satellite", "OSM"),  
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # Size legend
  addControl(
    html = paste(
      "<div style='background: white; padding: 10px; border: 1px solid black; border-radius: 5px;'>",
      "<strong> Observations/individual</strong><br>",
      "<svg width='100' height='90'>",
      "<circle cx='20' cy='30' r='", size_sizes[1], "' fill='gray' /><text x='40' y='35'>", size_labels[1], "</text>",
      "<circle cx='20' cy='50' r='", size_sizes[2], "' fill='gray' /><text x='40' y='55'>", size_labels[2], "</text>",
      "<circle cx='20' cy='70' r='", size_sizes[3], "' fill='gray' /><text x='40' y='75'>", size_labels[3], "</text>",
      "</svg>",
      "</div>"
    ),
    position = "bottomright",  # Place size legend at bottom right
    layerId = "size_legend"  # Assign a unique layer ID
  ) %>%
  addControl(
    html = paste(
      "<div style='background: white; padding: 10px; border: 1px solid black; border-radius: 5px;'>",
      "<strong>Species Legend</strong><br>",
      paste(
        sapply(names(spp_colors), function(spp) {
          paste0(
            "<div style='display: flex; align-items: center; margin-bottom: 5px;'>",
            "<div style='width: 10px; height: 10px; background: ", spp_colors[spp], "; margin-right: 5px;'></div>",
            spp,
            "</div>"
          )
        }),
        collapse = ""),
      "</div>"
    ),
    position = "bottomright",  
    layerId = "species_legend" 
  )

# Display the map
tree_map

saveWidget(tree_map, file = "figures/mapTrees2Core.html")

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Map trees to core – ggplot version
# 25 Feb 2025 by CRD
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)

rm(list = ls())
options(stringsAsFactors = FALSE)
setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/")

d <- read.csv("output/cleanTS.csv", header = TRUE)
dnodup <- d[!duplicated(d$plantNickname), ]
library(sf)
library(ggplot2)
library(ggspatial)
library(maptiles)

# --------------------------------------------------
# Study area extent
# --------------------------------------------------

pad <- 0.008

lon_min <- min(dnodup$long) - pad
lon_max <- max(dnodup$long) + pad
lat_min <- min(dnodup$lat) - pad
lat_max <- max(dnodup$lat) + pad
# --------------------------------------------------
# Basemap
# --------------------------------------------------

osm <- get_tiles(
  st_as_sfc(
    st_bbox(
      c(
        xmin = lon_min,
        xmax = lon_max,
        ymin = lat_min,
        ymax = lat_max
      ),
      crs = 4326
    )
  ),
  provider = "CartoDB.Positron",
  crop = TRUE,
  zoom = 16
)

# --------------------------------------------------
# Main map
# --------------------------------------------------

main_map <- ggplot() +
  
  layer_spatial(osm) +
  
  geom_sf(
    data = dnodup_sf,
    aes(fill = latbi),
    shape = 21,
    size = 3,
    colour = "white",
    stroke = 0.4,
    alpha = 0.95
  ) +
  
  geom_sf(
    data = special_sf,
    shape = 23,
    size = 5,
    fill = "black",
    colour = "white",
    stroke = 1
  ) +
  
  geom_text(
    data = special_point,
    aes(x = lon, y = lat, label = name),
    hjust = 0,
    nudge_x = 0.0015,
    size = 3.2,
    fontface = "bold"
  ) +
  
  scale_fill_manual(
    values = spp_colors,
    name = "Species"
  ) +
  
  coord_sf(
    xlim = c(lon_min, lon_max),
    ylim = c(lat_min, lat_max),
    expand = FALSE
  ) +
  
  annotation_scale(
    location = "bl",
    width_hint = 0.25
  ) +
  
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_minimal()
  ) +
  
  theme_minimal() +
  
  theme(
    panel.grid = element_blank(),
    aspect.ratio = 2.5,
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    ),
    
    legend.position = "left",
    
    legend.title = element_text(
      face = "bold",
      size = 10
    ),
    
    legend.text = element_text(
      size = 8
    ),
    
    legend.background = element_rect(
      fill = scales::alpha("white", 0.9),
      colour = "grey70"
    )
  )

ggsave(
  filename = "figures/empiricalData/tree_species_map.pdf",
  plot = main_map,
  width = 7,
  height = 10,
  units = "in"
)
