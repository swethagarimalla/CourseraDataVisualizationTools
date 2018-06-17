library(magrittr)
library(dplyr)
library(tidyr)
library(grid)
library(ggplot2)
install.packages("ggmap")
library(ggmap)

setwd("/Users/swethagarimalla/Desktop/Coursera/DataScientist/DataScientistPreReq/Course4BuildingDataVisualizationTools/")


load_data <- function(filename="ebtrk_atlc_1988_2015.txt", ...){
  
  ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                         4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1);
  ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                           "hour", "year", "latitude", "longitude",
                           "max_wind", "min_pressure", "rad_max_wind",
                           "eye_diameter", "pressure_1", "pressure_2",
                           paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                           paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                           paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                           "storm_type", "distance_to_land", "final");
  
  ext_tracks <- read_fwf(filename, 
                         fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                         na = "-99");
  return(ext_tracks);
}
clean_data <- function(raw.data, ...){
  data <- raw.data %>%  
    mutate(storm_id=paste0(storm_name,"-",year), 
           date = lubridate::parse_date_time(paste0(month, day, hour,year), 'mdHy')) %>% 
    gather(key = key, value = value, radius_34_ne:radius_64_nw) %>% 
    separate(key, sep = "_", into = c("radius", "wind_speed", "direction")) %>%
    spread(key = direction, value = value ) %>%
    mutate(longitude = as.numeric(longitude) * -1) %>%
    select(storm_id, date, latitude, longitude, wind_speed, ne, nw, se, sw);
  
  return(data)
}

load_data2 <- function(filename="ebtrk_atlc_1988_2015.txt", ...){

  raw.data <- readr::read_table(file = filename, col_names = FALSE);
  colnames <- c("storm_id", "storm_name", "mdH", "year", "latitude", "longitude", "max_wind_speed", 
                "min pressure", "rad_windspeed", "eye_d", "press_outer", "rad_outer", "n34", "n50",
                "n64", "code", "dist");

  colnames(raw.data) <- colnames;
  return(raw.data);
}
clean_data2 <- function(raw.data, ...){
  data <- raw.data %>%  
    mutate(storm_id=paste0(storm_name,"-",year), 
           date = lubridate::parse_date_time(paste0(mdH,year), 'mdHy')) %>% 
    gather(key = key, value = value, n34:n64) %>% 
    separate(value, sep = c(-9, -6, -3), into = c("ne", "se", "sw", "nw")) %>%
    mutate(wind_speed = substring(key, 2), longitude = as.numeric(longitude) * -1, 
           ne = as.integer(ne), se = as.integer(se), nw = as.integer(nw),  sw = as.integer(sw)) %>%
    select(storm_id, date, latitude, longitude, wind_speed, ne, nw, se, sw);
  
  return(data)
}

select_hurricane <- function(data, starts_with="IKE", ...){  
  hurricane <- data %>% filter(stringr::str_detect(storm_id, paste0(starts_with)));
  return(hurricane);
}


#build geom here#

#build ggproto
GeomHurricane <- ggproto("GeomHurricane", Geom,
                   required_aes = c("x", "y",
                                    "r_ne", "r_nw", "r_se", "r_sw" ),
                   default_aes = aes(fill = 1, color = 1, alpha = 0.5 ),
                   draw_key = draw_key_polygon,
                   draw_panel = function(data, panel_scales, coord, scale_radii = 1) {
                     
                     data <- data %>% mutate( r_ne = r_ne * 1853 * scale_radii,
                                              r_se = r_se * 1853 * scale_radii,
                                              r_sw = r_sw * 1853 * scale_radii,
                                              r_nw = r_nw * 1853 * scale_radii)
                     
                     ## Transform the data first
                     coords <- coord$transform(data, panel_scales)
                     
                     if(fill == 64)
                       coords$alpha <- 0.25
                     else if(fill == 50)
                       coords$alpha <- 0.50
                     else
                       coords$alpha <- 1
                     
                     ## Construct a grid grob
                     ne_arc <- grid::curveGrob(
                       x1 = coord$x, y1 = coord$y + r_ne, 
                       x2 = coord$x + r_ne, y2 = coord$y, 
                       curvature = 1,angle = 90, 
                       ncp = 1, shape = 0.5, square = TRUE, squareShape = 1,
                       inflect = FALSE, arrow = NULL, open = TRUE, debug = FALSE, name = NULL, 
                       gp = grid::gpar(alpha = coords$alpha), vp = NULL
                     )
                     nw_arc <- grid::curveGrob(
                       x1 = coord$x, y1 = coord$y + r_nw, 
                       x2 = coord$x + r_nw, y2 = coord$y, 
                       curvature = 1,angle = 90, 
                       ncp = 1, shape = 0.5, square = TRUE, squareShape = 1,
                       inflect = FALSE, arrow = NULL, open = TRUE, debug = FALSE, name = NULL, 
                       gp = grid::gpar(alpha = coords$alpha), vp = NULL
                     )
                     se_arc <- grid::curveGrob(
                       x1 = coord$x, y1 = coord$y + r_se, 
                       x2 = coord$x + r_se, y2 = coord$y, 
                       curvature = 1,angle = 90, 
                       ncp = 1, shape = 0.5, square = TRUE, squareShape = 1,
                       inflect = FALSE, arrow = NULL, open = TRUE, debug = FALSE, name = NULL, 
                       gp = grid::gpar(alpha = coords$alpha), vp = NULL
                     )
                     sw_arc <- grid::curveGrob(
                       x1 = coord$x, y1 = coord$y + r_sw, 
                       x2 = coord$x + r_sw, y2 = coord$y, 
                       curvature = 1,angle = 90, 
                       ncp = 1, shape = 0.5, square = TRUE, squareShape = 1,
                       inflect = FALSE, arrow = NULL, open = TRUE, debug = FALSE, name = NULL, 
                       gp = grid::gpar(alpha = coords$alpha), vp = NULL
                     )
                     
                   }
)

#build geom function

geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE, 
                         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomHurricane, mapping = mapping,  
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

##


#run data#
hurricane <- load_data() %>% clean_data() %>% select_hurricane("IKE")
hurricane2 <- load_data2() %>% clean_data2() %>% select_hurricane("IKE")


map_data <- get_map("Louisiana", zoom = 6, maptype = "toner-background")
base_map <- ggmap(map_data, extent = "device")

base_map +
  geom_hurricane(data = katrina, aes(x = longitude, y = latitude,
                                     r_ne = ne, r_se = se,
                                     r_nw = nw, r_sw = sw,
                                     fill = wind_speed,
                                     color = wind_speed)) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))
