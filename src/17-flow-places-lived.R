#===============================================================================
# 2023-11-17 -- 30DayMapChallenge
# Flow -- path of lived places
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky.phd
#===============================================================================

library(tidyverse)
library(magrittr)
library(gsheet)
library(sf)
library(rmapshaper)
library(cowplot)
library(ggimage)
library(tidygeocoder)
# library(ggflags)
# library(geomtextpath)
library(showtext)
sysfonts::font_add_google("Atkinson Hyperlegible", "ah")
sysfonts::font_add_google("Pacifico", "pa")
showtext_auto()
library(countrycode)
library(rnaturalearth)
library(paletteer)
library(prismatic)


# download the data
raw <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1tZE9WiRLGqsjCdwXZA1acopDtIpVWiRRauS10CuURYQ") %>%
    set_colnames(c("timestamp", "place", "name"))

# clean a bit and join both fields in one text string
df <- raw %>%
    separate_rows(place, sep = ";") %>%
    geocode(place, method = "osm") %>%
    drop_na()

# save(df, file = "dat/17-places.rda")
load("dat/17-places.rda")

# fix -- remove Serena as she doesn't want  photo shared
df <- df %>% filter(!name == "Serena")

# geocode
places <- df %>%
    st_as_sf(
        coords = c("long", "lat"),
        crs = 4326
    ) %>%
    group_by(name) %>%
    mutate(
        place_id = row_number()
    )

# get photos
photos <- df %>%
    distinct(name) %>%
    arrange(name) %>%
    bind_cols(
        tibble(
            img = fs::dir_ls("dat/cpop/")
        )
    )

# get world map outline (you might need to install the package)
world_outline <- spData::world %>%
    st_as_sf()

# world_outline <- ne_countries(scale = 'small', type = 'map_units', returnclass = 'sf')

# let's use a fancy projection
world_rob <- world_outline %>%
    filter(!iso_a2 == "AQ") %>% # get rid of Antarctica
    st_transform(crs = "ESRI:54009")

country_borders <- world_rob %>%
    rmapshaper::ms_innerlines()

# points to lines
# https://gis.stackexchange.com/a/294399/49606
foo <- df %>%
    group_by(name) %>%
    mutate(
        lineid = row_number(), # create a lineid
        long_end = lead(long), # create the end point coords for each start point
        lat_end = lead(lat)
    ) %>%
    unite(start, long, lat) %>% # collect coords into one column for reshaping
    unite(end, long_end, lat_end) %>%
    filter(end != "NA_NA") %>% # remove nas (last points in a NOMBRE group don't start lines)
    gather(start_end, coords, start, end) %>% # reshape to long
    separate(coords, c("long", "lat"), sep = "_") %>% # convert our text coordinates back to individual numeric columns
    mutate_at(vars(long, lat), as.numeric) %>%
    st_as_sf(coords = c("long", "lat")) %>% # create points
    group_by(name, lineid) %>%
    summarise() %>% # union points into lines using our created lineid
    st_cast("LINESTRING") %>%
    st_set_crs(4326)



world_rob %>%
    ggplot()+
    geom_sf(color = NA, fill = "#002644" %>% clr_lighten(.2))+
    geom_sf(data = country_borders, size = .3, color = "#002644" %>% clr_lighten(.4))+
    geom_sf(data = foo, aes(color = name))+
    geom_sf(
        data = places %>%
            filter(place_id == 1),
        aes(color = name),
        size = 2, shape = 21, fill = "#ccffff"
    )+
    geom_sf(
        data = places %>%
            filter(!place_id == 1),
        aes(color = name),
        size = 1
    )+
    coord_sf(crs = "ESRI:54030")+
    facet_wrap(~name, ncol = 3)+
    theme_map(font_family = "ah")+
    theme(
        plot.background = element_rect(color = NA, fill = "#002644"),
        strip.text = element_blank(),
        plot.title = element_text(face = 2, size = 24, color = "#ccffff", hjust = .5),
        plot.caption = element_text(size = 8, color = "#92A1B9FF")
    )+
    scale_color_paletteer_d("Polychrome::light", guide = NULL)+
    # add photos
    labs(
        title = "Places where we ever lived",
        caption = "#30DayMapChallenge 17: Flow // Idea: Julia Callaway // Design: Ilya Kashnitsky @ikashnitsky.phd"
    )

main <- last_plot()

ggsave("fig/17-lived-places.pdf", main, width = 12, height = 6, bg = "#002644", device = cairo_pdf)
