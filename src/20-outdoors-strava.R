#===============================================================================
# 2023-11-20 -- 30DayMapChallenge
# Outdoors -- my Strava
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky.phd
#===============================================================================

library(tidyverse)
library(magrittr)
library(rStrava)
library(sf)
library(leaflet)
library(ggmap)
library(cowplot)

# Get your credentials from https://www.strava.com/settings/api
app_name <- Sys.getenv("STRAVA_APP_NAME")
client_id <- Sys.getenv("STRAVA_CLIENT_ID")
client_secret <- Sys.getenv("STRAVA_CLIENT_SECRET")



# https://r.iresmi.net/posts/2023/map_your_strava_activities/
#' Convert Google polylines from Strava activities to {sf} polylines
#'
#' @param gp string : encoded polyline
#'
#' @return {sf} polyline
gp2sf <- function(gp) {
    gp |>
        googlePolylines::decode() |>
        map_dfr(
            function(df) {
                df |>
                    st_as_sf(coords = c("lon", "lat")) |>
                    st_combine() |>
                    st_cast("LINESTRING") |>
                    st_sf()
            }) |>
        pull(1)
}



# Get activities
raw <- httr::config(
    token = strava_oauth(
        app_name,
        client_id,
        client_secret,
        app_scope = "activity:read_all",
        cache = TRUE)
) |>
    get_activity_list() |>
    compile_activities()

# Map
activities <- raw %>%
    janitor::clean_names() %>%
    transmute(
        start_date, id, sport_type,
        moving_time, distance,
        max_heartrate,
        map_summary_polyline, start_latlng1
    ) %>%
    arrange(start_date) %>%
    filter(
        sport_type == "Run",
        # filter only Odense
        start_latlng1 > 55,
        start_latlng1 < 55.5
    ) %>%
    drop_na(map_summary_polyline) %>%
    mutate(geom = gp2sf(map_summary_polyline)) %>%
    st_as_sf(crs = "EPSG:4326")


# activities %>%
#     leaflet() |>
#     addTiles() |>
#     addPolylines()

# get stamen toner layer
activities %>% sf::st_bbox()

stadiamaps_key <- Sys.getenv("STADIAMAPS_API")
register_stadiamaps(stadiamaps_key)

zoom_box <- c(10.3, 55.36, 10.45, 55.428)
foo <- get_stadiamap(zoom_box, zoom = 13, maptype = "stamen_terrain")

# https://stackoverflow.com/a/32466251/4638884
# invert colors in raster
invert <- function(x) rgb(t(255-col2rgb(x))/255)
foo_inv <- as.raster(apply(foo, 2, invert))

# copy attributes from original object
class(foo_inv) <- class(foo)
attr(foo_inv, "bb") <- attr(foo, "bb")


ggmap(foo_inv)

# now visualize with ggplot

activities %>%
    ggplot() +
    inset_ggmap(foo_inv)+
    geom_sf(aes(geometry = geom), color = "#dafa2610", size = 1)+
    theme_map()+
    theme(
        plot.background = element_rect(color = NA, fill = "#040026FF"),
        plot.title = element_text(face = 2, size = 24, color = "#ccffff", hjust = .5),
        plot.caption = element_text(size = 7.7, color = "#92A1B9FF")
    )+
    labs(
        title = "My 342 Strava runs in Odense",
        caption = "#30DayMapChallenge 20: Outdoors // Data: Strava via {rStrava} // Ilya Kashnitsky @ikashnitsky.phd"
    )

ggsave("fig/20-outdoors-strava.png", width = 5, height = 5, bg = "#040026FF")
