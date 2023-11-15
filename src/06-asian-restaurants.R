#===============================================================================
# 2023-11-06 -- 30DayMapChallenge
# ASIA -- restaurants in Denmark
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky.phd
#===============================================================================

library(tidyverse)
library(magrittr)
library(osmdata)
# hotfix https://stackoverflow.com/a/69350806/4638884v
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
library(eurostat)
library(sf)
library(rmapshaper)
library(ggdark)
library(prismatic)
library(ggtext)
library(showtext)
sysfonts::font_add_google("Atkinson Hyperlegible", "ah")
showtext_auto()


# the built-in dataset of EU boundaries
gd <- eurostat_geodata_60_2016 %>%
    janitor::clean_names() %>%
    st_transform(crs = 3035)

# filters out only DK NUTS-3 regions
gd_dk <- gd %>%
    filter(
        levl_code == 3,
        str_sub(id, 1,2) == "DK"
    )

# the lines level with borders at country level
bord <- gd_dk %>% rmapshaper::ms_innerlines()

# bbox DK
bb_dk <- gd_dk %>% st_bbox()


# OSM data
q <- opq(bb_dk) %>%
    add_osm_feature (key = "amenity", value = "restaurant")

rest <- osmdata_sf(q)

# filter only DK
rest_dk <- rest$osm_points %>%
    select(osm_id, name, cuisine) %>%
    drop_na(cuisine) %>%
    separate_rows(cuisine) %>%
    group_by(osm_id) %>%
    mutate(w = 1/n()) %>%
    ungroup() %>%
    # only DK
    ms_clip(gd_dk) %>%
    # spatial intersection
    st_intersection(gd_dk)


# select all Asian cuisines
rest_dk$cuisine %>% unique()
asian_cuisines <- c(
    "asian", "japanese", "vietnamese", "indian", "turkish", "chinese", "thai",
    "sushi", "persian", "pakistani", "malaysian", "mongolian", "cantonese",
    "arab", "pujabi", "oriental", "lebanese", "nepalese", "filipino", "hotpot",
    "korean", "nikkey", "indonesian", "kumpir", "uzbek", "ramen"
)

# calculate prop of asian by NUTS-3 regions
rest_dk <- rest_dk %>%
    mutate(
        asian = (cuisine %in% asian_cuisines) %>% as.numeric()
    )

# calc prop in regions
prop_asian <- rest_dk %>%
    st_drop_geometry() %>%
    group_by(nuts_id) %>%
    summarise(prop = sum(asian*w)/sum(w)) %>%
    ungroup() %>%
    left_join(gd_dk, .)

# map!
prop_asian %>%
    ggplot()+
    geom_sf(aes(fill = prop * 100), color = NA)+
    geom_sf(data = bord, size = 3/4, color = "#B2EBF2")+
    geom_sf(
        data = rest_dk,
        aes(color = asian %>% as_factor),
        size = .2, shape = 16
    )+
    scale_color_manual(values = c("#4DD0E1", "#dafa26"), guide = "none")+
    scale_fill_viridis_b(
        option = "G", begin = .15, end = .75,
        guide = guide_colorsteps(
            barwidth = 15, barheight = 1,
            direction = "horizontal", title.position = "top"
        )
    )+
    coord_sf(datum = NA)+
    dark_theme_minimal(base_family = "ah")+
    labs(
        title = "<span style='color:#dafa26;'>Asian</span>       restaurants<br>in Denmark",
        # subtitle = "",
        fill = "Percentage of all restaurants, %",
        caption = "#30DayMapChallenge 6: Asia // Data: Open Street Maps // Analysis and graphic: Ilya Kashnitsky, @ikashnitsky.phd"
    )+
    theme(
        plot.background = element_rect(color = NA, fill = "#002644"),
        plot.title = element_markdown(family = "ah", face = 2, size = 24),
        plot.caption = element_text(size = 8),
        legend.position = c(.75, 1)
    )

ggsave("fig/asian-rest.pdf", width = 6, height = 6, bg = "#002644", device = cairo_pdf)
