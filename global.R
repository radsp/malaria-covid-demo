# load("appdata.RData")
# xctry <- xctry %>%
#   filter()
#   mutate(combogroup = factor(combogroup, levels = c("2020", "Other Years", "Historical Mean", "COVID-19", "COVID-19 Forecast", "Report Rate")))

# xx0 <- read_civis(120454693, using = readRDS)
# 
# xctry <- xx0$xctry
# xprov <- xx0$xprov
# xrf_adm0 <- xx0$xrf_adm0
# xrf_adm1 <- xx0$xrf_adm1
# xrf <- xx0$xrf
# wpop_adm1 <- xx0$wpop_adm1
# wpop_adm0 <- xx0$wpop_adm0
# rm(xx0)

# nctry <- length(unique(xctry$country))

# xctry <- xctry %>%
#   mutate(country = factor(country, levels = unique(country), labels = paste("Country ", 1:nctry, sep = "")))


s0 <- get_shapefile(admin_level = 0)
s1 <- get_shapefile(admin_level = 1)


srank <- readOGR("Benin Trimmed/Population_Layer.shp")


wpop_adm1 <- wpop_adm1 %>% 
  mutate(population_60_years_and_over = rowSums(select(., matches("60_to_64|65_to_69|70_to_74|75_to_79|80_years"))),
         population_40_to_60_years = rowSums(select(., matches("40_to_44|45_to_49|50_to_54|55_to_59")))) %>%
  mutate(popden_60_years_and_over = population_60_years_and_over / square_km,
         popden_40_to_60_years = population_40_to_60_years / square_km) %>%
  rename(admin1 = admin_level_1)


spop_adm1 <- merge(s1, wpop_adm1, by = c("country", "admin1"))

