library(openxlsx)
library(readxl)
library(dplyr)
library(tidyverse)
library(gganimate)
library(sf)
library(classInt)

### Import Data and shapefile
admin1 <- st_read("C:/Users/Eng Sacad/Desktop/Portfolio Project/Animation/Shapefile/som_admbnda_adm1_ocha_20230308.shp")

PRMN_data <- read_excel("C:/Users/Eng Sacad/Desktop/Portfolio Project/Animation/UNHCR-PRMN-Displacement-Dataset.xlsx",
                        sheet = "OutputsArrivals")



crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

admin1 <- admin1 |>
  sf::st_as_sf() |>
  sf::st_transform(crsLONGLAT)


plot(sf::st_geometry(admin1))



### Summarize the data by region and year
PRMN_data_summ <- PRMN_data |>
  group_by(`Current (Arrival) Region`, Year) |>
  summarize(Total_newarrivals = sum(`Number of Individuals`))


### Join the shapefile and the data
PRMN_admin1_sf <- inner_join(
  admin1, PRMN_data_summ, 
by = c("ADM1_EN" = "Current (Arrival) Region"))



plot(sf::st_geometry(PRMN_admin1_sf))  


# 4. PROJECTION
#--------------
# Robinson
robinson_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
newarrivals_somalia_sf_robinson <- PRMN_admin1_sf |>
  sf::st_transform(robinson_crs)
plot(sf::st_geometry(newarrivals_somalia_sf_robinson))


# BREAKS
#----------
vmin <- min(PRMN_admin1_sf$Total_newarrivals, na.rm = T)
vmax <- max(PRMN_admin1_sf$Total_newarrivals, na.rm = T)
brk <- round(classIntervals(
  PRMN_admin1_sf$Total_newarrivals,
  n = 6,
  style = "fisher"
)
$brks, 1) |>
  head(-1) |>
  tail(-1) |>
  append(vmax)
breaks <- c(vmin, brk)

cols <- rev(c(
  "#0033a0","#4066b8", "#8099d0", "#84adec",
           "#adc9f2",  "#cedef7",
           "#e6effb"
))





get_animated_somalia_map <- function() {
  somalia_map <- ggplot(
    data = PRMN_admin1_sf,
    aes(fill = Total_newarrivals)
  ) +
    geom_sf(color = "white", size = 0.025) +
    scale_fill_gradientn(
      name = "",
      colours = cols,
      breaks = breaks,
      labels = round(breaks, 1),
      limits = c(vmin, vmax),
      na.value = "grey70"
    ) +
    coord_sf(crs = robinson_crs) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = .5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = c(.5, -.015),
      legend.text = element_text(size = 11, color = "grey10"),
      panel.grid.major = element_line(color = "white", size = .2),
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        face = "bold", size = 20,
        color = "grey10", hjust = .5, vjust = -3
      ),
      plot.subtitle = element_text(
        size = 30, color = "#0033a0",
        hjust = .5, vjust = -1
      ),
      plot.caption = element_text(
        size = 10, color = "grey10",
        hjust = .5, vjust = -10
      ),
      plot.margin = unit(c(t = 2, r = 2, b = 2, l = 2), "lines"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank()
    ) +
    labs(
      x = "",
      y = "",
      title = "Somalia IDP new arrivals-PRMN data (2016-2023)",
      subtitle = "Year: {as.integer(closest_state)}",
      caption = "©2024 Sacad Mohamed - PRMN Data"
    )
  
  return(somalia_map)
}


somalia_map <- get_animated_somalia_map()
print(somalia_map)




gganimate::anim_save(
  "IDP_newarrivals_somalia3.gif", animated_somalia
)
