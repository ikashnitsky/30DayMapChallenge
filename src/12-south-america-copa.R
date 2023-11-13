#===============================================================================
# 2023-11-12 -- 30DayMapChallenge
# South America -- Copa Libertadores
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky.phd
#===============================================================================

library(tidyverse)
library(magrittr)
library(rvest)
library(sf)
library(rmapshaper)
library(cowplot)
library(ggimage)
library(ggflags)
library(showtext)
sysfonts::font_add_google("Atkinson Hyperlegible", "ah")
showtext_auto()
library(countrycode)



# wiki page
url <- "https://en.wikipedia.org/wiki/Copa_Libertadores#List_of_finals"


# copy xpath to the table
raw <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/table[6]') %>%
  html_table(fill = T) %>%
  extract2(1) %>%
    janitor::clean_names()

# !!!
# I ended up manually copying clubs' logos and locations, dropped in as a tibble
df <- tibble::tribble(
                       ~club, ~wins,                                     ~location,                               ~coords,                                                                                                                                                                                                     ~logo,
             "Independiente",    7L, "Avellaneda, Greater Buenos Aires, Argentina",   "-34.66981836521815, -58.37123704861423",                                     "https://upload.wikimedia.org/wikipedia/commons/thumb/d/db/Escudo_del_Club_Atl%C3%A9tico_Independiente.svg/270px-Escudo_del_Club_Atl%C3%A9tico_Independiente.svg.png",
              "Boca Juniors",    6L,            "La Boca, Buenos Aires, Argentina",  "-34.634484429553034, -58.36327055244764",                                                                                     "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e3/Boca_Juniors_logo18.svg/240px-Boca_Juniors_logo18.svg.png",
                   "Peñarol",    5L,                "Peñarol, Montevideo, Uruguay",  "-34.82465335958416, -56.197719497774756",                                                                       "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ac/Escudo_del_C_A_River_Plate.svg/225px-Escudo_del_C_A_River_Plate.svg.png",
               "River Plate",    4L,           "Belgrano, Buenos Aires, Argentina",  "-34.561829709416465, -58.45739529245968",                                                                       "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ac/Escudo_del_C_A_River_Plate.svg/225px-Escudo_del_C_A_River_Plate.svg.png",
               "Estudiantes",    4L,                         "La Plata, Argentina",    "-34.91965669447612, -57.9538580580413",                                                         "https://upload.wikimedia.org/wikipedia/commons/thumb/d/da/Escudo_de_Estudiantes_de_La_Plata.svg/165px-Escudo_de_Estudiantes_de_La_Plata.svg.png",
                   "Olimpia",    3L,                          "Asunción, Paraguay",  "-25.26547518730986, -57.578524621112024",                                                                                              "https://upload.wikimedia.org/wikipedia/en/thumb/4/45/Club_Olimpia_logo.svg/330px-Club_Olimpia_logo.svg.png",
                  "Nacional",    3L,                         "Montevideo, Uruguay",  "-34.904194705908075, -56.17483011344663",                                                                                         "https://upload.wikimedia.org/wikipedia/commons/thumb/8/82/Escudo_actual_bolso.png/330px-Escudo_actual_bolso.png",
                 "São Paulo",    3L,                  "Morumbi, São Paulo, Brazil",  "-23.597679748139235, -46.71867988636414",                                                         "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6f/Brasao_do_Sao_Paulo_Futebol_Clube.svg/270px-Brasao_do_Sao_Paulo_Futebol_Clube.svg.png",
                 "Palmeiras",    3L,                 "Perdizes, São Paulo, Brazil",   "-23.53691613725343, -46.67432482686674",                                                                                               "https://upload.wikimedia.org/wikipedia/commons/thumb/1/10/Palmeiras_logo.svg/285px-Palmeiras_logo.svg.png",
                    "Santos",    3L,                "Vila Belmiro, Santos, Brazil",  "-23.950808668136524, -46.33815338824351",                                                                                                         "https://upload.wikimedia.org/wikipedia/commons/thumb/1/15/Santos_Logo.png/300px-Santos_Logo.png",
                    "Grêmio",    3L,                        "Porto Alegre, Brazil",  "-30.037461097773814, -51.19432691371831",                                                                                                                    "https://upload.wikimedia.org/wikipedia/en/thumb/f/f1/Gremio.svg/270px-Gremio.svg.png",
                  "Flamengo",    3L,               "Gávea, Rio de Janeiro, Brazil",  "-22.977858155847777, -43.21948284635971",                                                                                       "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2e/Flamengo_braz_logo.svg/240px-Flamengo_braz_logo.svg.png",
                  "Cruzeiro",    2L,        "Belo Horizonte, Minas Gerais, Brazil",  "-19.92007125858516, -43.912572289414726",                                                         "https://upload.wikimedia.org/wikipedia/commons/thumb/9/90/Cruzeiro_Esporte_Clube_%28logo%29.svg/270px-Cruzeiro_Esporte_Clube_%28logo%29.svg.png",
             "Internacional",    2L,                        "Porto Alegre, Brazil",  "-30.030922175768136, -51.19020704076724",                                                       "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f1/Escudo_do_Sport_Club_Internacional.svg/330px-Escudo_do_Sport_Club_Internacional.svg.png",
         "Atlético Nacional",    2L,                          "Medellín, Colombia",     "6.24726858088337, -75.56439410562959",                                                               "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9a/Escudo_de_Atl%C3%A9tico_Nacional.png/165px-Escudo_de_Atl%C3%A9tico_Nacional.png",
                 "Colo-Colo",    1L,                      "Macul, Santiago, Chile",  "-33.490278794666935, -70.59387645441006",                                                                                                              "https://upload.wikimedia.org/wikipedia/en/thumb/b/be/Colo-Colo.svg/330px-Colo-Colo.svg.png",
                "Fluminense",    1L,         "Laranjeiras, Rio de Janeiro, Brazil",  "-22.933233986467847, -43.18601401167058",                                                                                            "https://upload.wikimedia.org/wikipedia/en/thumb/9/9e/Fluminense_fc_logo.svg/270px-Fluminense_fc_logo.svg.png",
                    "Racing",    1L, "Avellaneda, Greater Buenos Aires, Argentina",    "-34.6681948123612, -58.36825443193808",                                                           "https://upload.wikimedia.org/wikipedia/commons/thumb/5/56/Escudo_de_Racing_Club_%282014%29.svg/210px-Escudo_de_Racing_Club_%282014%29.svg.png",
        "Argentinos Juniors",    1L,        "La Paternal, Buenos Aires, Argentina",  "-34.605594059683284, -58.47291505015016", "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1b/Escudo_de_la_Asociaci%C3%B3n_Atl%C3%A9tica_Argentinos_Juniors.svg/270px-Escudo_de_la_Asociaci%C3%B3n_Atl%C3%A9tica_Argentinos_Juniors.svg.png",
           "Vélez Sársfield",    1L,            "Liniers, Buenos Aires, Argentina",  "-34.64576029002884, -58.519536706757265",                       "https://upload.wikimedia.org/wikipedia/commons/thumb/2/21/Escudo_del_Club_Atl%C3%A9tico_V%C3%A9lez_Sarsfield.svg/240px-Escudo_del_Club_Atl%C3%A9tico_V%C3%A9lez_Sarsfield.svg.png",
             "Vasco da Gama",    1L,            "Maracanã, Rio de Janeiro, Brazil",  "-22.912334249964594, -43.22668882875736",                                                                                "https://upload.wikimedia.org/wikipedia/en/thumb/0/03/CR_Vasco_da_Gama_2021_logo.png/240px-CR_Vasco_da_Gama_2021_logo.png",
               "Once Caldas",    1L,                         "Manizales, Colombia",    "5.063562540383496, -75.49944127780456",                                                                                               "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2f/Logo_Once_Caldas.png/225px-Logo_Once_Caldas.png",
                 "LDU Quito",    1L,                              "Quito, Ecuador", "-0.17697825971869977, -78.46633240403335",                                                                       "https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/Liga_Deportiva_Universitaria.png/285px-Liga_Deportiva_Universitaria.png",
               "Corinthians",    1L,                  "Tatuapé, São Paulo, Brazil",  "-23.535280747631194, -46.57421135132732",                                                      "https://upload.wikimedia.org/wikipedia/en/thumb/5/5a/Sport_Club_Corinthians_Paulista_crest.svg/270px-Sport_Club_Corinthians_Paulista_crest.svg.png",
          "Atlético Mineiro",    1L,        "Belo Horizonte, Minas Gerais, Brazil",  "-19.926989823861785, -43.94610687859127",                                                              "https://upload.wikimedia.org/wikipedia/en/thumb/5/5f/Clube_Atl%C3%A9tico_Mineiro_crest.svg/255px-Clube_Atl%C3%A9tico_Mineiro_crest.svg.png",
               "San Lorenzo",    1L,              "Boedo, Buenos Aires, Argentina",   "-34.63045607753427, -58.41969996941627",                                                                           "https://upload.wikimedia.org/wikipedia/commons/thumb/5/56/San_lorenzo_almagro_logo.svg/285px-San_lorenzo_almagro_logo.svg.png"
        ) %>%
  separate(coords, into = c("lat", "long"), sep = ", ") %>%
    st_as_sf(
        coords = c("long", "lat"),
        crs = 4326
    ) %>%
    mutate(
        country = sub('.*\\,', '', location),
        cntr = country %>%
            countrycode(origin = "country.name", destination = "iso2c") %>%
            str_to_lower()
    )


# logos dataset -----------------------------------------------------------

# attach years info
logos_years <- left_join(
    df %>% select(club, logo, cntr) %>% st_drop_geometry(),
    raw %>% select(club, seasons_won)
) %>%
    separate_rows(seasons_won) %>%
    filter(seasons_won %>% nchar() == 4) %>%
    mutate(year = seasons_won %>% as.numeric()) %>%
    arrange(year) %>%
    mutate(
        year_lab = case_when(
            year %% 10 == 0 ~ year %>% paste,
            # year %% 10 == 5 ~ year %>% paste %>% str_sub(3, 4) %>% paste0("'", .),
            TRUE ~ ""
        )
    )

# plot logos
logos_years %>%
    ggplot()+
    geom_text(
        aes(label = year_lab, x = seq_along(seasons_won), y = 11),
        size = 7, fontface = 2, color = "#044444", alpha = .75
    )+
    geom_image(
        aes(
            image = logo,
            x = seq_along(seasons_won),
            y = seq_along(seasons_won) %>% scale %>% multiply_by(12) %>% sin %>% add(9)
        )
    )+
    ggflags::geom_flag(
        aes(
            country = cntr,
            x = seq_along(seasons_won),
            y = seq_along(seasons_won) %>% scale %>% multiply_by(12) %>% sin %>% add(10)
        ),
        size = 3
    )+
    coord_polar(direction = -1, start = .3)+
    scale_x_continuous(limits = c(0, 70))+
    scale_y_continuous(limits = c(0, 13))+
    theme_void(base_family = "ah")+
    theme(
        plot.background = element_rect(color = NA, fill = "#eeffff")
    )

plot_logos <- last_plot()



# prepare geodata ---------------------------------------------------------

# get world map outline (you might need to install the package)
world_outline <- spData::world %>%
    st_as_sf()

# let's use a fancy projection
lat_outline <- world_outline %>%
    st_transform(crs = 8980) %>%
    filter(
        subregion == "South America"
    ) # get rid of Antarctica

# borders between countries
country_borders <- lat_outline %>%
    rmapshaper::ms_innerlines()

# tally number of wins by country
n_wins <- df %>%
    st_drop_geometry() %>%
    group_by(cntr) %>%
    summarise(n_wins = n()) %>%
    ungroup()

# attach to geodata

df_map <- lat_outline %>%
    mutate(cntr = iso_a2 %>% str_to_lower) %>%
    left_join(n_wins, by = "cntr") %>%
    mutate(n_wins = n_wins %>% replace_na(0))


# map ---------------------------------------------------------------------

df_map %>%
    ggplot()+
    geom_sf(color = NA, aes(fill = n_wins))+
    geom_sf(data = country_borders, color = "#eeffff")+
    geom_sf(
        data = df, alpha = .25, size = 3, color = "#dafa26"
    )+
    scale_fill_viridis_b(option = "G", breaks = 1:10, begin = .1)+
    theme_map(font_family = "ah")+
    theme(legend.position = "none")

the_map <- last_plot()

# draw legend manually
df_map %>%
    st_drop_geometry() %>%
    arrange(n_wins %>% desc) %>%
    group_by(n_wins) %>%
    mutate(
        x_ind = seq_along(n_wins)
    ) %>%
    ungroup() %>%
    ggplot()+
    geom_point(
        aes(x = 1 -sin(((n_wins)/3))/5, y = n_wins, color = n_wins),
        size = 12, shape = 16
    )+
    geom_flag(
        aes(country = cntr, x = 1 + x_ind/5 -sin(((n_wins)/3))/5, y = n_wins), size = 7
    )+
    scale_color_viridis_b(option = "G", breaks = 1:10, begin = .1, limits = c(0, 11))+
    annotate(
        "text",
        label = c(11, 8, 2, 1, 0),
        x = 1 -sin(((c(11, 8, 2, 1, 0))/3))/5, y = c(11, 8, 2, 1, 0),
        size = 5, fontface = 2,
        color = viridis::mako(12, begin = .1)[c(12, 11, 10, 4, 1)] %>% rev
    )+
    annotate(
        "point",
        x = 1 -sin(((c(10, 9, 7:3))/3))/5, y = c(10, 9, 7:3),
        size = 9, shape = 16,
        color = viridis::mako(12, begin = .1)[c(10, 9, 7:3)+1]
    )+
    theme_void(base_family = "ah")+
    theme(legend.position = "none")+
    coord_flip(xlim = c(.5, 2), ylim = c(0, 12))

the_legend <- last_plot()


# assemble
ggdraw()+
    draw_plot(plot_logos)+
    draw_plot(the_map, x = .35, width = .3, y = .3, height = .4)+
    draw_plot(the_legend, x = .05, width = .95, y = .01, height = .3)+
    labs(
        caption =
    )+
    annotate(
        "text",
        label = c(
            "Copa Libertadores winners",
            "#30DayMapChallenge 12: South America // Data: Wikipedia // Ilya Kashnitsky, @ikashnitsky.phd"
        ),
        x = .5, y = c(.9, .03), size = c(9, 2.7), color = c("#044444", "#269292"),
        family = "ah", fontface = 2
    )

out <- last_plot()

ggsave("fig/12-south-america-copa.pdf", out, width = 5, height = 5, bg = "#eeffff")


