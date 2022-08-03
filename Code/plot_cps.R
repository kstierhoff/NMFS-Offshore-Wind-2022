pacman::p_load(tidyverse, sf, atm, here, fs, scatterpie, 
               concaveman, rnaturalearth, shadowtext, atm)

# Load catch data
# 2017 
load("C:/KLS/CODE/Github/EstimateCPS/1707RL/Output/catch_final.Rdata")
load("C:/KLS/CODE/Github/EstimateCPS/1707RL/Output/haul_info.Rdata")
load("C:/KLS/CODE/Github/EstimateCPS/1707RL/Data/CTD/ctd_data.Rdata")

source(here("Code/process_catch_1707RL.R"))

haul.pie.all    <- haul.pie %>% mutate(survey = "Summer 2017")
cluster.pie.all <- cluster.pie %>% mutate(survey = "Summer 2017")

load("C:/KLS/CODE/Github/EstimateCPS/1707RL/Data/Nav/nav_data.Rdata")
nav.all <- nav %>% mutate(survey = "Summer 2017")

# 2015------------------------------------------------------------
load("C:/KLS/CODE/Github/estimATM/1507SH/Output/catch_info.Rdata")
haul.pie.all    <- haul.pie %>% mutate(survey = "Summer 2015") %>% 
  bind_rows(haul.pie.all)
cluster.pie.all <- cluster.pie %>% mutate(survey = "Summer 2015") %>% 
  bind_rows(cluster.pie.all)

load("C:/KLS/CODE/Github/estimATM/1507SH/Data/Nav/nav_data.Rdata")
nav.all <- nav %>% 
  select(-time) %>% mutate(survey = "Summer 2015") %>% 
  bind_rows(nav.all)

# 2016------------------------------------------------------------
load("C:/KLS/CODE/Github/estimATM/1606RL/Output/catch_info.Rdata")
haul.pie.all    <- haul.pie %>% mutate(survey = "Summer 2016") %>% 
  bind_rows(haul.pie.all)
cluster.pie.all <- cluster.pie %>% mutate(survey = "Summer 2016") %>% 
  bind_rows(cluster.pie.all)

load("C:/KLS/CODE/Github/estimATM/1606RL/Data/Nav/nav_data.Rdata")
nav.all <- nav %>% 
  select(-time) %>% mutate(survey = "Summer 2016") %>% 
  bind_rows(nav.all)

# 2018------------------------------------------------------------
load("C:/KLS/CODE/Github/estimATM/1807RL/Output/catch_info.Rdata")

haul.pie.all    <- haul.pie %>% mutate(survey = "Summer 2018") %>% 
  bind_rows(haul.pie.all)
cluster.pie.all <- cluster.pie %>% mutate(survey = "Summer 2018") %>% 
  bind_rows(cluster.pie.all)

load("C:/KLS/CODE/Github/estimATM/1807RL/Data/Nav/nav_data.Rdata")
nav.all <- nav %>% mutate(survey = "Summer 2018") %>% 
  bind_rows(nav.all)

# 2019------------------------------------------------------------
load("C:/KLS/CODE/Github/estimATM/1907RL/Output/catch_info.Rdata")
haul.pie.all    <- haul.pie %>% mutate(survey = "Summer 2019") %>% 
  bind_rows(haul.pie.all)
cluster.pie.all <- cluster.pie %>% mutate(survey = "Summer 2019") %>% 
  bind_rows(cluster.pie.all)

load("C:/KLS/CODE/Github/estimATM/1907RL/Data/Nav/nav_data.Rdata")
nav.all <- nav %>% mutate(survey = "Summer 2019") %>% 
  bind_rows(nav.all)

# 2021------------------------------------------------------------
load("C:/KLS/CODE/Github/estimATM/2107RL/Output/catch_info.Rdata")
haul.pie.all    <- haul.pie %>% mutate(survey = "Summer 2021") %>% 
  bind_rows(haul.pie.all)
cluster.pie.all <- cluster.pie %>% mutate(survey = "Summer 2021") %>% 
  bind_rows(cluster.pie.all)

load("C:/KLS/CODE/Github/estimATM/2107RL/Data/Nav/nav_data.Rdata")
nav.all <- nav %>% 
  select(-time) %>% mutate(survey = "Summer 2021") %>% 
  bind_rows(nav.all)

# Convert nav to sf----------------------------------------------
nav.sf <- nav.all %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  group_by(survey) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") 

# Export data
# Write nav lines to shapefile
nav.sf %>% 
  st_write(here("Output/cps_tracks_2015_2021.shp"), delete_layer = TRUE)

# Write haul locations to CSV
haul.pie.all %>% 
  select(haul, lat, long, survey) %>% 
  write_csv(here("Output/cps_trawls_2015_2021.csv"))

# Write haul locations to shapefile
hauls.sf <- haul.pie.all %>%
  st_as_sf(coords = c("long","lat"), crs = 4326)

hauls.sf %>% 
  select(haul, survey) %>% 
  st_write(here("Output/cps_trawls_2015_2021.shp"), delete_layer = TRUE)

wpts <- read_csv(here("Data/GIS/cps_transect_wpts.csv")) %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326)

transects <- wpts %>% 
  group_by(survey, type, line) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  filter(type != "Nearshore", type != "Lisa Marie")

transects %>% 
  st_write(here("Output/cps_transects_2015_2021.shp"), delete_layer = TRUE)

# ggplot(transects, aes(colour = type)) + geom_sf() + facet_wrap(~survey)

# Export survey footprint
nav.footprint <- concaveman(nav.sf, concavity = 300) %>% 
  st_zm(drop = TRUE)

st_write(nav.footprint, here("Output/footprint_2015_2021.shp"), 
         delete_layer = TRUE)

# ggplot(nav.footprint) + geom_sf()

# Read bathy contours shapefile 
bathy <- st_read(here("C:/KLS/CODE/Github/estimATM/1907RL/Data/GIS/bathy_contours.shp")) %>% 
  st_transform(4326) %>% 
  rename(Depth = Contour)

# Get land features
# Map landmarks
label.list <- c("Monterey Bay","San Francisco","Cape Flattery","Crescent City",
                "Newport","Point Conception","Cape Mendocino","Columbia River",
                "Cape Blanco","Bodega Bay","Westport","Fort Bragg",
                "Morro Bay","Long Beach","Cape Scott","San Diego")

# Import landmarks
locations <- filter(read.csv(here("C:/KLS/CODE/Github/estimATM/1907RL/Data/Map/locations.csv")), 
                    name %in% label.list) %>% 
  project_df(to = 3310)

# Get land features --------------------------
# Get state data
states <- ne_states(country = 'United States of America', returnclass = 'sf')
ca     <- filter(states, name == "California")

# Get countries
countries <- ne_countries(scale = "large", returnclass = "sf") %>%
  filter(subregion %in% c("Northern America","Central America"))

# Load OR areas
areas <- st_read(here("Data/GIS/CallAreas_OR_Outline_2022_04_22.shp")) %>% 
  st_transform(crs = 3310)

aliquots <- st_read(here("Data/GIS/CallArea_OR_BlocksAliquots_2022_04_22.shp")) %>% 
  st_transform(crs = 3310)

# wpts <- st_read(here("Data/GIS/OR_CallAreas_Waypoints_2022_04_22.shp")) %>% 
#   st_transform(crs = 3310)

# Quick map of OR area
# ggplot() +
#   geom_sf(data = areas) + 
#   geom_sf(data = aliquots, aes(fill = Call_Area_))

map.bounds <- areas %>% 
  st_transform(crs = 3310) %>%
  st_bbox()

# # Calculate pie radius based on latitude range
# pie.radius <- as.numeric(abs(map.bounds$ymin - map.bounds$ymax)*0.0125)
# 
# # Filter for positive hauls and clusters
# haul.pos <- filter(haul.pie.all, AllCPS > 0) %>% 
#   arrange(X) %>% 
#   mutate(group = paste(survey, haul), 
#          r = pie.radius) %>% 
#   select(-radius)
# 
# cluster.pos <- filter(cluster.pie.all, AllCPS > 0) %>% 
#   arrange(X)%>% 
#   mutate(group = paste(survey, cluster),
#          r = pie.radius) %>% 
#   select(-radius)
# 
# # Substitute very small value for species with zero catch, just for pie charts
# if (nrow(haul.pos) > 0) {
#   haul.pos <- haul.pos %>% 
#     replace(. == 0, 0.0000001) 
#   
#   cluster.pos <- cluster.pos %>% 
#     replace(. == 0, 0.0000001) 
# }
# 
# # Filter for empty trawls
# haul.zero    <- filter(haul.pie.all, AllCPS == 0)
# 
# cluster.zero <- filter(cluster.pie.all, AllCPS == 0)

# Create base map -------------------------------------------
base.map <- atm::get_basemap(hauls.sf, states, countries, locations, bathy, map.bounds, crs = 3310)

# Figure preferences ------------------------------------------------------
# Set species colors
sardine.color      <- '#FF0000'
anchovy.color      <- '#00CD66'
jack.mack.color    <- '#0000FF'
jacksmelt.color    <- '#A020F0'
pac.mack.color     <- '#00FFFF'
pac.herring.color  <- '#F5DEB3'

# Create trawl cluster figure
base.map +
  geom_sf(data = areas) + 
  geom_sf(data = aliquots, aes(fill = Call_Area_), show.legend = FALSE) +
  # plot ship track data
  geom_sf(data = nav.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
  geom_sf(data = transects, colour = "black", size = 1) +
  geom_sf(data = hauls.sf, size = 3, shape = 21, fill = "yellow") +
  # # Plot trawl pies
  # geom_scatterpie(data = cluster.pos,
  #                 aes(X, Y, group = group,
  #                     r = r),
  #                 cols = c("Anchovy", "JackMack", "Jacksmelt",
  #                          "PacHerring", "PacMack", "Sardine"),
  #                 color = 'black', alpha = 0.8, sorted_by_radius = TRUE) +
  # # Configure trawl scale
  # scale_fill_manual(name = 'Species',
  #                   labels = c("Anchovy", "J. Mackerel", "Jacksmelt",
  #                              "P. herring", "P. mackerel", "Sardine"),
  #                   values = c(anchovy.color, jack.mack.color, jacksmelt.color,
  #                              pac.herring.color, pac.mack.color, sardine.color)) +
  # # Plot empty cluster locations
  # geom_point(data = cluster.zero, aes(X, Y),
  #            size = 3, shape = 21, fill = 'black', colour = 'white') +
  # facet_wrap(~survey, nrow = 1) +
  # theme(strip.background.x = element_blank(),
  #       strip.text.x = element_text(face = "bold")) +
  coord_sf(crs = 3310, 
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))

ggsave("Figs/fig_transects_trawls.png",
       height = 10, width = 5)


# # Create trawl haul figure
# base.map +
#   # plot ship track data
#   geom_sf(data = nav.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
#   geom_scatterpie(data = haul.pos,
#                   aes(X, Y, group = group,
#                       r = r),
#                   cols = c("Anchovy", "JackMack", "Jacksmelt",
#                            "PacHerring", "PacMack", "Sardine"),
#                   color = 'black', alpha = 0.8, sorted_by_radius = TRUE) +
#   # Configure trawl scale
#   scale_fill_manual(name = 'Species',
#                     labels = c("Anchovy", "J. Mackerel", "Jacksmelt",
#                                "P. herring", "P. mackerel", "Sardine"),
#                     values = c(anchovy.color, jack.mack.color, jacksmelt.color,
#                                pac.herring.color, pac.mack.color, sardine.color)) +
#   # Plot empty cluster locations
#   geom_point(data = haul.zero, aes(X, Y),
#              size = 3, shape = 21, fill = 'black', colour = 'white') +
#   facet_wrap(~survey, nrow = 1) +
#   theme(strip.background.x = element_blank(),
#         strip.text.x = element_text(face = "bold")) +
#   coord_sf(crs = 3310, 
#            xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
#            ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))
# 
# ggsave("fig_haul_weight.png",
#        height = 8, width = 20)
# 
