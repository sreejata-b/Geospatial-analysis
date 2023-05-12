#code below might be neccessary to update Rcpp package
#update.packages("Rcpp") or install.packages("Rcpp")
#install.packages("sf")
library(sf)
library(dplyr)
library(ggplot2)
#library(leaflet)
library(scales)
library(ggmap)

## Read in shapefile using sf
ak_regions <- read_sf("shapefile_demo_data/ak_regions_simp.shp")
plot(ak_regions)

#what is the issue? projection 
class(ak_regions)
View(ak_regions)
head(ak_regions)
st_crs(ak_regions)
ak_regions_3338 <- ak_regions %>%
  st_transform(crs = 3338)

st_crs(ak_regions_3338)
plot(ak_regions_3338)

View(ak_regions_3338)
ak_regions_3338 %>%
  select(region)
ak_regions_3338 %>%
  filter(region == "Southeast")
pop <- read.csv("shapefile_demo_data/alaska_population.csv")
View(pop)
pop_4326 <- st_as_sf(pop, 
                     coords = c('lng', 'lat'),
                     crs = 4326,
                     remove = F)

head(pop_4326)
View(pop_4326)
pop_joined <- st_join(pop_4326, ak_regions_3338, join = st_within)

pop_3338 <- st_transform(pop_4326, crs = 3338)
View(pop_3338)
pop_joined <- st_join(pop_3338, ak_regions_3338, join = st_within)

head(pop_joined)

pop_region <- pop_joined %>% 
  as.data.frame() %>% 
  group_by(region) %>% 
  summarise(total_pop = sum(population))
head(pop_region)
pop_region_3338 <- left_join(ak_regions_3338, pop_region)

#plot to check
plot(pop_region_3338["total_pop"])

pop_mgmt_338 <- pop_region_3338 %>% 
  group_by(mgmt_area) %>% 
  summarize(total_pop = sum(total_pop))
plot(pop_mgmt_338["total_pop"])

View(pop_mgmt_338)

pop_mgmt_3338 <- pop_region_3338 %>% 
  group_by(mgmt_area) %>% 
  summarize(total_pop = sum(total_pop), do_union = F)

plot(pop_mgmt_3338["total_pop"])
write_sf(pop_region_3338, "shapefile_demo_data/ak_regions_population.shp", delete_layer = TRUE)

ggplot(pop_region_3338) +
  geom_sf(aes(fill = total_pop)) +
  theme_bw() +
  labs(fill = "Total Population") +
  scale_fill_continuous(low = "khaki", high =  "firebrick", labels = comma)


##plot multiple shapefiles in same plot

rivers_3338 <- read_sf("shapefile_demo_data/ak_rivers_simp.shp")
st_crs(rivers_3338)
View(rivers_3338)
ggplot() +
  geom_sf(data = pop_region_3338, aes(fill = total_pop)) +
  geom_sf(data = rivers_3338, aes(size= StrOrder), color = "black") +
  geom_sf(data = pop_3338, aes(), size = 0.5) +
  scale_size(range = c(0.01, 0.2), guide = F) +
  theme_bw() +
  labs(fill = "Total Population") +
  scale_fill_continuous(low = "khaki", high =  "firebrick", labels = comma)

##incorporate base maps into static maps using ggmap
pop_3857 <- pop_3338 %>%
  st_transform(crs = 3857)


# Define a function to fix the bbox to be in EPSG:3857
# See https://github.com/dkahle/ggmap/issues/160#issuecomment-397055208
ggmap_bbox_to_3857 <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}
bbox <- c(-170, 52, -130, 64)   # This is roughly southern Alaska
ak_map <- get_stamenmap(bbox, zoom = 4)
ak_map_3857 <- ggmap_bbox_to_3857(ak_map)


ggmap(ak_map_3857) + 
  geom_sf(data = pop_3857, aes(color = population), inherit.aes = F) +
  scale_color_continuous(low = "khaki", high =  "firebrick", labels = comma)

##making world map
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
install.packages("rgeos")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
View(world)
plot(world["name"]) 
st_crs(world)

##world map projections

https://semba-blog.netlify.app/01/26/2020/world-map-and-map-projections/ 
https://r-spatial.org/r/2018/10/25/ggplot2-sf.html

#Lambert Azimuthal Equal-Area projection
ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = st_crs(3035))
  
#plot options

ggplot(data = world) + 
  geom_sf(color = "black", fill = "lightgreen")

library("ggspatial")
ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) +  coord_sf(xlim=c(-180,+190), ylim=c(-100,+100), expand=FALSE)

data<- read.csv(file='allsites.csv', na.strings="")
View(data)
plot_data <- merge(world, data, by = "name", all = TRUE)
View(plot_data)

palette=c("grey74","grey74", "grey74","grey74","grey74","grey74","grey74","grey74","grey74","grey74","grey74",
          "grey74","grey74","grey74","grey74","grey74","grey74","grey74","grey74","grey74","grey74")

palette=c(palette)
plot<-ggplot(data = plot_data) +
  geom_sf(data=plot_data, aes(fill = country)) + scale_fill_manual(values=palette)+
  geom_point(data = plot_data, aes(x = longitude_deg, y = latitude_deg), size = 3, 
             shape = 21, colour="white", fill = "black") +
  theme(plot.background = element_rect(fill = "#BFD5E3"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        # surpress legend
        legend.position = "none")+
  #geom_text(data= world_points,aes(x=X, y=Y, label=country), nudge_x=-12,nudge_y=2,
  #color = "black", fontface = "bold", check_overlap = FALSE) +
  #annotate(geom = "text", x = -50, y = 95,
  #fontface = "italic", color = "black", size = 6, label=) + 
  coord_sf(xlim = c(-180, 190), ylim = c(-100, 100), expand = TRUE)+theme(axis.title.x=element_blank(),
                                                                          axis.text.x=element_blank(),
                                                                          axis.ticks.x=element_blank())+ theme(axis.title.y=element_blank(),
                                                                                                               axis.text.y=element_blank(),
                                                                                                               axis.ticks.y=element_blank())

plot

###making maps of different countries

library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
install.packages("rgeos")
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
View(world)
library(tidyverse)


library("ggspatial")


world_india<-world %>% filter(world$sovereignt=="India")
plot(world_india)

ggplot(data = world_india) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) # coord_sf(xlim=c(-180,+190), ylim=c(-100,+100), expand=FALSE)

data<- read.csv("allsites.csv")
data_india<- data %>% filter(name=="India")
plot_data <- merge(world_india, data_india, by = "name", all = TRUE)
View(plot_data)

plot<-ggplot(data = plot_data) +
  geom_sf(data=plot_data, aes(fill = country)) + #scale_fill_manual(values=palette)+
  geom_point(data = plot_data, aes(x = longitude_deg, y = latitude_deg), size = 3, 
             shape = 21, colour="white", fill = "black") +
  theme(plot.background = element_rect(fill = "#BFD5E3"))+
  #theme(panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        # surpress legend
        theme(legend.position = "none")+
  #geom_text(data= world_points,aes(x=X, y=Y, label=country), nudge_x=-12,nudge_y=2,
  #color = "black", fontface = "bold", check_overlap = FALSE) +
  #annotate(geom = "text", x = -50, y = 95,
  #fontface = "italic", color = "black", size = 6, label=) + 
  #coord_sf(xlim = c(-180, 190), ylim = c(-100, 100), expand = TRUE)+
  theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
    axis.ticks.x=element_blank())+ theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
    axis.ticks.y=element_blank())

plot

st_crs(plot_data)


plot(plot_data)
#above code plots separate maps for all columns we dont need that hence subset to column of interest, eg. ph

#plot india map by ph
plot(plot_data["ph"])
##doesnt differentiate between ph colors as we had observed in the Alaska tutorial. reason is that there is no geometry for the regions that have different ph values.
##hence the second ph value color is selected ph 6.4 which is blue.

##save india map as shp file
world_india<- world_india %>% select(sovereignt) %>% summarize(do_union=F)
plot(world_india)
write_sf(world_india, "world_india.shp", delete_layer = TRUE)


##read back shape file India



##how to get states within countries 

#download shape files from https://gadm.org/download_country_v3.html , make sure all shp files are accompanied with shx, dbf and prf files as well
#choose level 1 for state shape file

india_regions <- read_sf("gadm36_IND_shp/gadm36_IND_1.shp")

head(india_regions)
View(india_regions)
st_crs(india_regions)
india_states<-india_regions%>% select(NAME_1)


install.packages("Polychrome")
library(Polychrome)

# create your own color palette (36 colors) based on `seedcolors`
P36<- createPalette(36,  c("#ff0000", "#00ff00", "#0000ff"))
swatch(P36)


head(india_states)

plot<-ggplot(india_states)+ geom_sf(aes(fill=NAME_1))+scale_fill_manual(values=as.vector(P36))
plot


##adding names of states within india 


india_points<- st_centroid(india_regions)
india_points <- cbind(india_regions, st_coordinates(st_centroid(india_regions$geometry)))

ggplot(data = india_regions) +
  geom_sf() +
  geom_text(data= india_points,aes(x=X, y=Y, label=NAME_1),
            color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  annotate(geom = "text", x = 70, y = 40, label = "India", 
           fontface = "italic", color = "grey22", size = 0.2) 
  #coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)
