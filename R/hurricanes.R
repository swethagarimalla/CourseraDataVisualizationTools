library(magrittr)
library(dplyr)
library(tidyr)
library(grid)
library(ggplot2)
library(devtools)
install_github("dkahle/ggmap")
library(ggmap)
library(readr)

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

select_hurricane <- function(data, storm_date='KATRINA-2005', date_time = '2005290812', ...){  
  hurricane <- data %>% filter(storm_id == storm_date, date == lubridate::ydm_h(date_time))
  return(hurricane);
}


#build geom here#
geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...){
  ggplot2::layer(
    geom = GeomHurricane, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,...)
  )
}

GeomHurricane <- ggproto("GeomHurricane", Geom,
                         required_aes = c("x", "y", 
                                          "r_ne", "r_se", "r_sw", "r_nw"),
                         default_aes = aes(color = "grey", fill = 1, size = 1, 
                                           alpha = 1, scale_radii = 1),
                         draw_key = draw_key_polygon,
                         draw_group = function(data, panel_scales, coord){
                           
                           center <- c(data[1,]$x, data[1,]$y) #initialize center point
                           scale_radii <- data[1,]$scale_radii
                           naut <- 1852 * scale_radii
                           
                           if(scale_radii == 64)
                             alpha = 0.2
                           else if (scale_radii == 50)
                             alpha = 0.5
                           else
                             alpha = 1
                           
                           #NE
                           ne_quad <- destPoint(p = center,
                                                b=1:90,
                                                d = data[1,]$r_ne * naut)
                           ne <- data.frame(x = c(ne_quad[,"lon"], center[1]),
                                                 y = c(ne_quad[,"lat"], center[2]))
                           
                           #NW
                           nw_quad <- destPoint(p = center,
                                                b=270:360,
                                                d = data[1,]$r_nw * naut)
                           nw <- data.frame(x = c(nw_quad[,"lon"], center[1]),
                                                 y = c(nw_quad[,"lat"], center[2]))
                           
                           #SE
                           se_quad <- destPoint(p = center, 
                                                b = 90:180,
                                                d = data[1,]$r_se * naut)
                           
                           se <- data.frame(x = c(se_quad[,"lon"], center[1]),
                                                 y = c(se_quad[,"lat"], center[2]))
                           
                           #SW
                           sw_quad <- destPoint(p = center,
                                               b=180:270, 
                                               d = data[1,]$r_sw * naut)
                           sw <- data.frame(x = c(sw_quad[,"lon"], center[1]),
                                                 y = c(sw_quad[,"lat"], center[2]))
                           
                           
    
    four_quads <- rbind(ne, se, nw, sw)
    coords <- coord$transform(four_quads, panel_scales)
    
    polygonGrob(x = coords$x,
                y = coords$y,
                gp = gpar(col = data[1,]$color, 
                          fill = data[1,]$fill, 
                          alpha = alpha))
  }
)

