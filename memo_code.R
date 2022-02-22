library(tidyverse) #used for data wrangling and viz
library(here) #simplifies file paths
library(rsample) #used to split data

us_wildfire <- read_csv(here("archive", "FW_Veg_Rem_Combined.csv"))

#show head
slice_max(us_wildfire, 5)

us_wildfire_clean <- us_wildfire %>% 
  select(fire_name:remoteness)

us_wildfire_clean <- us_wildfire_clean %>% 
  filter(weather_file != "File Not Found")

options(scipen = 100)

#summarise acres per year burned
acres_per_year <- us_wildfire_clean %>% 
  group_by(disc_pre_year) %>% 
  summarise(acres_burned = sum(fire_size))

#fire size (finalized graph)
ggplot(data = acres_per_year) + 
  geom_point(aes(x = disc_pre_year, 
                 y = acres_burned, 
                 size = acres_burned, 
                 color = acres_burned)) +
  scale_color_continuous(high = "firebrick", low = "goldenrod1") +
  labs(x = "Year", y = "Total Acres Burned", 
       title = "Total acres burned per year from 1990 to 2015") +
  theme_minimal() +
  theme(legend.position = "none")

#remoteness (unfinalized)
ggplot(data = us_wildfire_clean) +
  geom_point(aes(x = remoteness, y = fire_size))

#most common causes of fire
fire_causes <- us_wildfire_clean %>% 
  group_by(stat_cause_descr) %>% 
  count()

#cause (finalized)
ggplot(data = fire_causes, aes(y = reorder(stat_cause_descr, n), x = n)) +
  geom_col(aes(fill = n)) +
  scale_fill_gradient(high = "firebrick", low = "goldenrod1") +
  labs(x = "Number of Fires", 
       y = "Cause",
       tite = "Number of fires per listed starting cause") +
  theme_minimal() +
  theme(legend.position = "none")

#distribution of fire size  
ggplot(data = us_wildfire_clean, aes(x = fire_size)) +  
  geom_histogram(bins = 100)

us_wildfire_split <- us_wildfire_clean %>% 
  initial_split(prop = 0.8, strata = "fire_size")

#write split data to data frames
fire_train <- training(us_wildfire_split)
fire_test <- testing(us_wildfire_split)
# 
# library(leaflet)
# 
# m <- leaflet() %>%
#   addTiles() %>%
#   addMarkers(lng=us_wildfire_clean$longitude, lat=us_wildfire_clean$latitude)
# m

library(maptools)
library(RColorBrewer)
library(classInt)
library(ggplot2)
library(ggrepel)
library(mapproj)
library(viridis)

us_wildfire_clean$class_fac = factor(us_wildfire_clean$fire_size_class, levels = c("A", "B", "C", "D", "E", "F", "G"))

us <- map_data("world", 'usa')
# 
# ggplot()+
#   geom_polygon(data=us, aes(x=long, y=lat, group = group), colour="grey20", fill="grey80")+
#   geom_point(data = us_wildfire_clean, aes(x=longitude, y = latitude, color = class_fac)) +
#   scale_color_brewer(palette = "YlOrRd")+
#   ggtitle("US Wildfire Distribution")+
#   guides(color=guide_legend(title="Wild Fire Scale"))+
#   coord_map(projection = "sinusoidal", xlim=c(-170, -50))

state <- map_data("state")
ggplot() + 
  geom_polygon(data=state, aes(x=long, y=lat, group=group), color = "white", fill = "grey") + 
  geom_point(data = us_wildfire_clean, aes(x=longitude, y = latitude, color = class_fac)) +
  scale_color_brewer(palette = "YlOrRd")+
  ggtitle("US Wildfire Distribution")+
  guides(color=guide_legend(title="Wild Fire Scale"))+
  coord_map(projection = "sinusoidal", xlim=c(-120, -75), ylim = c(25, 50))

ggplot() + 
  geom_polygon(data=state, aes(x=long, y=lat, group=group), color = "white", fill = "grey") + 
  geom_point(data = us_wildfire_clean[which(us_wildfire_clean$wstation_byear < 1970),], aes(x=longitude, y = latitude, color = class_fac)) +
  scale_color_brewer(palette = "YlOrRd")+
  ggtitle("US Wildfire Distribution before 1970")+
  guides(color=guide_legend(title="Wild Fire Scale"))+
  coord_map(projection = "sinusoidal", xlim=c(-120, -75), ylim = c(25, 50))

ggplot() + 
  geom_polygon(data=state, aes(x=long, y=lat, group=group), color = "white", fill = "grey") + 
  geom_point(data = us_wildfire_clean[which(us_wildfire_clean$wstation_byear >= 1970 & us_wildfire_clean$wstation_byear < 2000),], aes(x=longitude, y = latitude, color = class_fac)) +
  scale_color_brewer(palette = "YlOrRd")+
  ggtitle("US Wildfire Distribution 1970-2000")+
  guides(color=guide_legend(title="Wild Fire Scale"))+
  coord_map(projection = "sinusoidal", xlim=c(-120, -75), ylim = c(25, 50))

ggplot() + 
  geom_polygon(data=state, aes(x=long, y=lat, group=group), color = "white", fill = "grey") + 
  geom_point(data = us_wildfire_clean[which(us_wildfire_clean$wstation_byear >= 200),], aes(x=longitude, y = latitude, color = class_fac)) +
  scale_color_brewer(palette = "YlOrRd")+
  ggtitle("US Wildfire Distribution after 2000")+
  guides(color=guide_legend(title="Wild Fire Scale"))+
  coord_map(projection = "sinusoidal", xlim=c(-120, -75), ylim = c(25, 50))

ggplot() + 
  geom_density(data= us_wildfire_clean[which(us_wildfire_clean$wstation_byear <= 1970 & us_wildfire_clean$fire_size > 100),], aes(x = fire_size, y=..density..),
               alpha=.3,
               colour="dodgerblue", fill="dodgerblue") + 
  geom_density(data= us_wildfire_clean[which(us_wildfire_clean$wstation_byear >= 1970 & us_wildfire_clean$fire_size > 100 & us_wildfire_clean$wstation_byear < 2000),], aes(x = fire_size, y=..density..),
               alpha=.3,
               colour="yellow3", fill="yellow3") + 
  geom_density(data= us_wildfire_clean[which(us_wildfire_clean$wstation_byear >= 2000 & us_wildfire_clean$fire_size > 100),], aes(x = fire_size, y=..density..),
               alpha=.3,
                 colour="firebrick3", fill="firebrick3") + 
  xlim(10000, 100000) + 
  ggtitle("Wildfire Severeity")
  
########### not used
require(albersusa) || install.packages(albersusa, dependencies = TRUE)
library(ggplot2)
library(sp)

us_wildfire_clean = cbind(us_wildfire_clean$latitude, us_wildfire_clean)
us_wildfire_clean = cbind(us_wildfire_clean$longitude, us_wildfire_clean)
us_wildfire_tr = usmap_transform(us_wildfire_clean)
names(us_wildfire_tr)[1] = "wf_long"
names(us_wildfire_tr)[2] = "wf_lat"

plot_usmap() + 
  geom_point(data = us_wildfire_tr, aes(x=wf_long, y = wf_lat, color = class_fac)) +
  scale_color_brewer(palette = "YlOrRd")+
  ggtitle("US Wildfire Distribution")+
  guides(color=guide_legend(title="Wild Fire Scale"))
