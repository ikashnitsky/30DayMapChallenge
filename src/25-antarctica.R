#===============================================================================
# 2023-11-25 -- 30DayMapChallenge
# Antarctica -- get rid of
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky.phd
#===============================================================================

library(tidyverse)
library(magrittr)
library(sf)
library(cowplot)



# get world map outline (you might need to install the package)
world_outline <- spData::world %>%
    st_as_sf()

# let's use a fancy projection
world_outline_robinson <- world_outline %>%
    st_transform(crs = "ESRI:54030")

# borders between countries
country_borders <- world_outline_robinson %>%
    rmapshaper::ms_innerlines()

# with Antarctica
world_outline_robinson %>%
    ggplot()+
    geom_sf(fill = "#006064", color = NA)+
    geom_sf(data = country_borders, size = .25, color = "#CDDC39")+
    geom_sf(
        data = . %>% filter(iso_a2 == "AQ"),
        fill = "#dafa26", color = NA
    )+
    theme_map()+
    theme(
        plot.background = element_rect(color = NA, fill = "#002626")
    )

map_aq <- last_plot()

ggsave("fig/25-aq.pdf", map_aq, width = 5, height = 2.5, bg = "#002626", device = cairo_pdf)

# without Antarctica
world_outline_robinson %>%
    filter(!iso_a2 == "AQ") %>% # get rid of Antarctica
    ggplot()+
    geom_sf(fill = "#5C6BC0", color = NA)+
    geom_sf(data = country_borders, size = .25, color = "#80DEEA")+
    theme_map()+
    theme(
        plot.background = element_rect(color = NA, fill = "#002644")
    )

map_no_aq <- last_plot()

ggsave("fig/25-no-aq.pdf", map_no_aq, width = 5, height = 2, bg = "#002644", device = cairo_pdf)


