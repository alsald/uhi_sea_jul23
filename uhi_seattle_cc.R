library(tidycensus)
library(tidyverse)
install.packages(c("mapview", "mapedit", "mapboxapi",
                   "leafsync", "spdep", "segregation",
                   "ggiraph"))

cc_sea <-  read_csv("/Users/asaldanha/Downloads/Climate_Central_Urban_heat_islands_Seattle.csv")

glimpse(cc_sea)

king_race <- get_acs(
  geography = "tract",
  variables = c(
    Hispanic = "DP05_0071P",
    White = "DP05_0077P",
    Black = "DP05_0078P",
    Asian = "DP05_0080P",
    AIAN = "DP05_0079P",
    NHPI = "DP05_0081P",
    Multiracial = "DP05_0083P"
  ),
  state = "WA",
  county = "King",
  geometry = TRUE
)

glimpse(king_race)

#CATEGORIZING AS WHITE AND NON-WHITE
library(dplyr)

kingrace2 <- king_race %>%
  mutate(race_new=
           case_when(variable== "White" ~ "White",
                     TRUE ~ "Non-white"))
glimpse(kingrace2)

colnames(cc_sea)

library(janitor)
cc_sea2 <- clean_names(cc_sea)

glimpse(cc_sea2)

cc_sea3 <- rename(cc_sea2,
                  GEOID=census_tract_number)
glimpse(cc_sea3)

cc_sea3$GEOID <-as.character(cc_sea3$GEOID)

sea_cc_race <- left_join(cc_sea3, kingrace2, by = "GEOID")

glimpse(sea_cc_race)

library(mapview)
library(tigris)
library(sf)
sf_use_s2(FALSE)

sea_cc_race$geometry <- NULL
glimpse(sea_cc_race)

#pull in geometry for mapview
king_tracts <- tracts("WA", "King", year = 2021, cb = TRUE)

cc_sea_geo <- left_join(king_tracts, sea_cc_race, by = "GEOID")


mapview(cc_sea_geo, zcol = "urban_heat_island_effect_temperature_in_degrees_f")

seageo_nowater <- erase_water(cc_sea_geo,
                              area_threshold = 0.9,
                              year = 2021)
glimpse (seageo_nowater)
#Has a column on city so can filter for just Seattle by census tract
seageo_nowater2 <- filter(seageo_nowater, city == "Seattle")

mapview(seageo_nowater2, zcol = "urban_heat_island_effect_temperature_in_degrees_f")
mapview(seageo_nowater2, zcol = "race_new") #turned out to be a dud, drop race by census tract -- need to do a block analysis)

seageo3 <- seageo_nowater2 %>%
  select(-c(STATEFP, COUNTYFP))
glimpse(seageo3)

seageo4 <- rename(seageo3,
                  uhi_c=urban_heat_island_effect_temperature_in_degrees_c,
                  uhi_f=urban_heat_island_effect_temperature_in_degrees_f)


st_write(seageo4, "seageo4.shp")

# write sea_cc_race into a csv

glimpse(seageo4)

seageo5 <- seageo4 %>%
  select(TRACTCE, GEOID, city, uhi_f)

seageo5$geometry <- NULL

glimpse(seageo5)

write.csv(seageo5, "/Users/asaldanha/Downloads/seageo5.csv", row.names=FALSE)
