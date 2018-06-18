library(testthat)
library(CourseraDataVisualizationTools)

test_check("CourseraDataVisualizationTools")

#run data#
hurricane <- load_data() %>% clean_data() %>% select_hurricane()
hurricane2 <- load_data2() %>% clean_data2() %>% select_hurricane("IKE")


map_data <- get_map("Louisiana", zoom = 6, maptype = "toner-background")
base_map <- ggmap(map_data, extent = "device")

base_map +
  geom_hurricane(data = hurricane, aes(x = longitude, y = latitude,
                                       r_ne = ne, r_se = se,
                                       r_nw = nw, r_sw = sw,
                                       fill = wind_speed,
                                       color = wind_speed)) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))
