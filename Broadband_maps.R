library(dplyr)
library(tidyverse)
library(tmap)
library(raster)
library(leaflet)
library(spData)
library(sf)
library(tmaptools)
library(rmapshaper)
library(shinyjs)
library(shiny)
library(tigris)
library(conflicted)
library(gifski)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

source("Prep_data.R")
prep_connections_data()
prep_policies()

#prep geographies
us_geo <- tigris::states(class="sf")
contiguous_states <- us_geo %>% 
  filter(REGION != 9) %>% 
  filter(STUSPS != "AK") %>% 
  filter(STUSPS != "HI")

# Define the shape and the layer elements
cuts <- c(0, 1, 2, 3, 4, 5)

#Animated map of connectivity
shp <- st_read("census_maps/acs_2012_2016_county_us_B27001.shp",
               stringsAsFactors = FALSE) %>%
  rename(uninsured_2012 = un_2012,
         uninsured_2016 = un_2016,
         uninsured_diff = unnsrd_) %>%
  mutate(STFIPS = stringr::str_sub(GEOID, 1, 2)) %>%
  mutate(fips=as.numeric(GEOID)) %>% inner_join(connections_long)

county_connections_year <- tm_shape(shp, projection = 2163) +
  tm_polygons("connectivity",
              breaks = cuts,
              palette = "BuPu", 
              border.col = "white", 
              border.alpha = 0.1) +
  tm_legend(legend.position = c("left", "bottom")) + 
  tm_borders(col = "black",lwd=0.1) +
  tm_facets(along="year",free.coords = FALSE)
tmap_animation(county_connections_year, 
               filename = "county_connections.gif", delay = 120)

#connectivity aggregated at state level, animated
connections_state <- connections_long %>% 
  filter(!state_code %in% c(11,66,60,69,72,78)) %>%
  group_by(year,state,state_code) %>% 
  summarise(con=mean(connectivity,na.rm=TRUE))

state_map_data <- inner_join(contiguous_states, #mean connectivity
                             connections_state,by=c("STUSPS"="state"))

anim <- tm_shape(state_map_data, projection = 5070) + tm_polygons("con") + 
        tm_facets(along="year",free.coords = FALSE)

tmap_animation(anim, filename = "state_connections.gif", delay = 120)

#animated map of restriction policies 
restrictions_state <- restrictions_long %>% 
  filter(!state_code %in% c(11,66,60,69,72,78)) %>%
  group_by(year,state,state_code) %>% 
  summarise(restrictiveness=mean(restrictiveness,na.rm=TRUE))

state_res_data <- inner_join(contiguous_states, #mean connectivity
                             restrictions_state,by=c("STUSPS"="state"))

anim <- tm_shape(state_res_data, projection = 5070) + 
  tm_polygons("restrictiveness",palette = "-BrBG", style="fixed",
              breaks=c(-2.5,-1.5,-0.5,0.5,1.5,2.5,3.5,4.5),
              labels=c("-2","-1","0","1","2","3","4")) + 
  tm_facets(along="year",free.coords = FALSE)  
  

tmap_animation(anim, filename = "state_restrictions.gif", delay = 120)

#animated map of funding policies 
funds_state <- funds_long %>% 
  filter(!state_code %in% c(11,66,60,69,72,78)) %>%
  group_by(year,state,state_code) %>% 
  summarise(funds=mean(funding))

state_funds_data <- inner_join(contiguous_states, #mean connectivity
                             funds_state,by=c("STUSPS"="state"))

anim <- tm_shape(state_funds_data, projection = 5070) + 
  tm_polygons("funds",
              palette = "Greens",
              style="fixed",
              breaks=c(-0.5,0.5,1),
              labels=c("No state funding","State funding")) + 
  tm_facets(along="year",free.coords = FALSE)  

tmap_animation(anim, filename = "state_funds.gif", delay = 120)

