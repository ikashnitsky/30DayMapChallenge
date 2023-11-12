library(tidyverse)
library(magrittr)
library(rvest)


# wiki page
url <- "https://en.wikipedia.org/wiki/Copa_Libertadores#List_of_finals"


# # copy xpath to the table
# raw <- url %>% 
#   read_html() %>%
#   html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/table[6]') %>%
#   html_table(fill = T) %>%
#   as.data.frame() %>% 
# 
# df <- raw %>% 
#   filter(Titles > 0) %>% 
#   transmute(club = Club, wins = Titles)


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
  separate(coords, into = c("lat", "long"), sep = ", ")

  