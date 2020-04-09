# Clear workspace, as needed
rm(list=ls())

## set directory

setwd("~/Desktop/Classwork/CEC_Project/Mapping")

# Load packages

library(dplyr)
library(coda)
library(xtable)
library(ggplot2)

library(data.table)
library(ggsn) # add N arrow
library(tmap)
library(rgdal)

library(maps)
library(mapdata)
library(ggmap)
library(sf)
library(ggrepel) # for labeling map
library(doBy)

library(grid)
library(gridExtra) #combining maps and figures
library(ggthemes)
library(ggspatial)

library(usmap)
library(Hmisc) #for capitalizing state names




# Read in all states from map library
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

#head(states)

# Captitalize state names, inca case you want to show them on the map (but likely not necessary)

states$ID <- capitalize(states$ID)

## compute the centroid of each state polygon as the coordinates where to draw their names ##
states <- cbind(states, st_coordinates(st_centroid(states)))

## select and subset states of interest

states.I.want <- c("Connecticut", "Massachusetts","New hampshire", "New jersey","New york","Pennsylvania",
                  "Rhode island","Vermont", "Virginia", "West virginia","Maryland", "Delaware")

states <- states %>% filter(ID %in% states.I.want) 

# Look at projection
st_crs(states)

## Convert to sf object ##

states_subset <- st_as_sf(states)

#### Load geodatbase ####

# Look at what layers are in the geodatabase 
st_layers(dsn="EDC_FinalShapefiles.gdb")

## Load individual layers
rivers<- st_read(dsn="EDC_FinalShapefiles.gdb", layer='CbayRivers')
states <- st_read(dsn="EDC_FinalShapefiles.gdb", layer='CbayStates_Physical')
sites <- st_read(dsn="EDC_FinalShapefiles.gdb", layer='Integrator_sites')
ChesBay <- st_read(dsn="EDC_FinalShapefiles.gdb", layer='CbayOutline_NHDv2Dissolve')


st_crs(sites)

## Change projection of states and rivers to match sites ##
states2 = st_transform(states, st_crs(sites))
rivers2 = st_transform(rivers, st_crs(sites))

# Look at fields contained within the shapefile 'sites'
#sites

## Subset out integrator sites of interest ##

sites.I.want <- c("Wyalusing Creek", "Pine Creek at Ramsey","Chillisquaque Creek", "Mahantango Creek, WB",
                  "South Branch, Old Fields", "Potomac at Antietam Creek mouth")
sites <- sites %>% filter(SITE_NAME %in% sites.I.want) 
 
### Merge in new column with new site names ###
## sites names were reduced to letters to reduce clutter on map ##

new.names <- c("A", "B","C","D","E", "F")
new.names_df <- data.frame(new.names,sites.I.want)
head(new.names_df)

## Convert columns to characters to avoid warnings when merging below ##

for(i in 1:2){
  new.names_df[,i] <- as.character(as.factor(new.names_df[,i]))
}

# Rename columns to match sites
colnames(new.names_df) <- c("new_names", "SITE_NAME")
# Merge
(sites <- sites %>%
    left_join(new.names_df, by = c("SITE_NAME")))


#######################################################################
##### Map of united states to use for inset ###########################
#######################################################################

## grab all states
states_all <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

## Captitalize state names

states_all$ID <- capitalize(states_all$ID)

## Compute the centroid of each state polygon as the coordinates where to draw their names
states_all <- cbind(states_all, st_coordinates(st_centroid(states_all)))

##subset states to draw box around
states.I.want.2 <- c("New jersey","New york","Pennsylvania", "Virginia", "West virginia","Maryland", "Delaware")

states_2 <- states_all %>% filter(ID %in% states.I.want.2) 

#create box
states2_box= st_as_sfc(st_bbox(states_2))

## generate map and save
p_all <- ggplot() +
  geom_sf(data=states_all, fill="grey85")+
  geom_sf(data = states2_box, fill = NA, color = "red", size = 1.2)+
  theme_bw() +
  theme_void()

ggsave("inset_map.tiff")

#######################################################################
#### Plot study map, with states,hesapeake Bay outline, rivers, ########
#### integrator sites, and inset of USA ################################
########################################################################

## WARNING: mapping of rivers adds significant time to map generation (i.e., ~1-2 hours) ##

p <- ggplot() + geom_sf(data = states2, fill="grey85") + 
  geom_sf(data = ChesBay, fill=NA, lwd=0.75, color='grey60') +
  geom_sf(data=rivers2, color='royalblue2')+
  geom_sf(data=sites, size=2.5, color = 'gray1') + 
  geom_sf_label(data = sites, aes(LONG, LAT, label = new_names), size = 2.5, nudge_y = -500, nudge_x=-25000)+
  geom_sf_text(data = sites, aes(LONG, LAT, label = new_names), size = 2.5) +
  geom_sf_text(data=states, aes(label=STUSPS), size=2, fontface = "bold") +
  geom_point() + 
  labs(title="", y="Latitude", x="Longitude") + 
  annotation_scale(location = "br", width_hint = 0.5) +
  theme_bw() +
  theme(panel.background = element_blank()) + 
  theme(panel.border = element_blank()) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  north(data = states2, location = "topright", scale =0.1, symbol = 1)


### add inset map of eastern USA
#name_figure <- "Study_Map_New_MS.pdf"
pdf("Study_Map_New_MS_2.pdf")
grid.newpage()
mainmap <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
insetmap <- viewport(width = 0.36, height = 0.3, x = 0.2, y = 0.8) #plot area for the inset map
print(p, vp = mainmap) 
print(p_all, vp = insetmap)
dev.off()

###






