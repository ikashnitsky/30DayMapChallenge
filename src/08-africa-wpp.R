#===============================================================================
# 2023-11-08 -- 30DayMapChallenge
# AFRICA -- WPP growth
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky.phd
#===============================================================================

library(tidyverse)
library(magrittr)
library(sf)
library(rmapshaper)
library(ggdark)
library(prismatic)
library(cowplot)
library(patchwork)
library(ggtext)
library(showtext)
sysfonts::font_add_google("Atkinson Hyperlegible", "ah")
showtext_auto()
# WPP 2022 data
# remotes::install_github("PPgp/wpp2022")
library(wpp2022)
library(countrycode)



# prepare geodata ---------------------------------------------------------

# get world map outline (you might need to install the package)
world_outline <- spData::world %>%
    st_as_sf()

# let's use a fancy projection
world_outline_robinson <- world_outline %>%
    st_transform(crs = "ESRI:54030") %>%
    filter(!iso_a2 == "AQ") # get rid of Antarctica

# borders between countries
country_borders <- world_outline_robinson %>%
    rmapshaper::ms_innerlines()

# centroids of the countries
country_centroids <- world_outline_robinson %>%
    st_centroid() %>%
    drop_na(pop) %>%
    mutate(
        order = iso_a2 %>% fct_reorder(pop %>% desc)
    ) %>%
    arrange(order)


# wpp2022 data ------------------------------------------------------------

data(pop1dt)
data(popproj1dt)

df_wpp <- bind_rows(
    pop1dt %>%
        transmute(
            year, name,
            iso_a2 = country_code %>% countrycode(origin = "iso3n", destination = "iso2c"),
            pop_total = pop,
            pop_cat = pop %>% cut(c(0, 10e3, 50e3, 100e3, 250e3, 500e3, Inf)) %>%
                lvls_revalue(c(10, 50, 100, 250, 500, "")) %>%
                fct_rev()
        ),
    popproj1dt %>%
        transmute(
            year, name,
            iso_a2 = country_code %>% countrycode(origin = "iso3n", destination = "iso2c"),
            pop_total = pop,
            pop_cat = pop %>%
                cut(c(0, 10e3, 50e3, 100e3, 250e3, 500e3, Inf)) %>%
                lvls_revalue(c(10, 50, 100, 250, 500, "")) %>%
                fct_rev()
        )
)

df_plot <- country_centroids %>%
    left_join(df_wpp, by = "iso_a2") %>%
    arrange(order, year)




# map  --------------------------------------------------------------------


df_plot %>%
    filter(year == 2090) %>%
    arrange(order, pop_total) %>%
    ggplot()+
    geom_sf(data = world_outline_robinson, fill = "#bbffff", color = NA)+
    geom_sf(data = country_borders, size = .25, color = "#ffffff")+
    geom_sf(aes(size = pop_total, color = pop_cat, geometry = geom), alpha = .75)+
    scale_size_area(max_size = 12, guide = "none")+
    scale_color_viridis_d(
        option = "D", direction = -1,
        end = .95,
        guide = guide_legend(
            barwidth = 15, barheight = 1,
            override.aes = list(size = 5),
            direction = "horizontal",
            title.position = "top",
            title.hjust = .5,
            nrow = 1, reverse = T
        )
    )+
    theme_map(font_family = "ah")+
    theme(
        legend.position = "top",
        legend.justification = c(.5, .5),
        legend.background = element_rect(color = NA, fill = "#bbffff"),
        text = element_text(face = 2, color = "#044444"),
        plot.caption = element_text(color = "#dafa26"),
        plot.background = element_rect(color = NA, fill = "#eeffff"),
    )+
    labs(
        color = "Population size, million",
        caption = "#30DayMapChallenge 8: Africa // Data: UN World Population Prospects 2022 // Ilya Kashnitsky, @ikashnitsky.phd"
    )+
    annotate("text",
        label = 2090 %>% paste,
        x = 1e6, y = -5e6,
        size = 7, color = "#044444",
        fontface = 2, family = "ah"
    )


# map x4 ---------------------------------------------------------------------

map_func <- function(choose_year = 2020){
    df_plot %>%
        arrange(order, pop_total) %>%
        filter(year == choose_year) %>%
        ggplot()+
        geom_sf(data = world_outline_robinson, fill = "#bbffff", color = NA)+
        geom_sf(data = country_borders, size = .25, color = "#ffffff")+
        geom_sf(aes(size = pop_total, color = pop_cat, geometry = geom), alpha = .75)+
        scale_size_area(max_size = 12, guide = "none")+
        scale_color_viridis_d(
            option = "D", direction = -1,
            end = .95,
            guide = guide_legend(
                barwidth = 15, barheight = 1,
                override.aes = list(size = 5),
                direction = "horizontal",
                title.position = "top",
                title.hjust = .5,
                nrow = 1, reverse = T
            )
        )+
        theme_map(font_family = "ah")+
        theme(
            legend.position = "top",
            legend.justification = c(.5, .5),
            legend.background = element_rect(color = NA, fill = "#bbffff"),
            text = element_text(face = 2, color = "#044444"),
            plot.caption = element_text(color = "#dafa26"),
            plot.background = element_rect(color = NA, fill = "#eeffff"),
        )+
        labs(
            color = "Population size, million"
        )+
        annotate("text",
                 label = choose_year %>% paste,
                 x = 2e6, y = -5e6,
                 size = 7, color = "#044444",
                 fontface = 2, family = "ah"
        )
}

# arrange 4 selective years
map_1950 <- map_func(1950)
map_2020 <- map_func(2020)
map_2050 <- map_func(2050)
map_2100 <- map_func(2100)


map_4 <-
    (
        map_1950 + map_2020 + map_2050 + map_2100 +
            plot_layout(guides = "collect") &
            theme(legend.position = "top")
    )+
    labs(
        caption = "#30DayMapChallenge 8: Africa // Data: UN World Population Prospects 2022 // Ilya Kashnitsky, @ikashnitsky.phd"
    )+
    theme(
        plot.background = element_rect(color = NA, fill = "#eeffff")
    )

ggsave("fig/08-pop-growth-x4.pdf", width = 8.7, height = 4.85, bg = "#eeffff", device = cairo_pdf)


