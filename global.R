# load("appdata.RData")

xrf <- xrf %>% 
  mutate(mygroup = case_when(str_detect(variable, "ltm") ~ "Historical Mean",
                             (rf_type %in% "acc_ssn") & !(str_detect(variable, "ltm")) ~ as.character(year_ssn),
                             TRUE ~ as.character(year))) 

xctry <- xctry %>%
  mutate(country = as.character(country)) %>%
  filter(!((country %in% "Malawi") & (variable %in% "anc1_visit")))

xprov <- xprov %>%
  filter(!((country %in% "Malawi") & (variable %in% "anc1_visit")))

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

s1$admin_level_1 <- s1$admin1

s0_ctr <- st_centroid(s0$geometry) %>% st_geometry() %>% st_coordinates()
s0$lat_ctr <- s0_ctr[,"Y"]
s0$lon_ctr <- s0_ctr[,"X"]

s1_ctr <- st_centroid(s1$geometry) %>% st_geometry() %>% st_coordinates()
s1$lat_ctr <- s1_ctr[,"Y"]
s1$lon_ctr <- s1_ctr[,"X"]

srank <- readOGR("Benin Trimmed/Population_Layer.shp")


wpop_adm1 <- wpop_adm1 %>% 
  mutate(population_60_years_and_over = rowSums(select(., matches("60_to_64|65_to_69|70_to_74|75_to_79|80_years"))),
         population_40_to_60_years = rowSums(select(., matches("40_to_44|45_to_49|50_to_54|55_to_59")))) %>%
  mutate(popden_60_years_and_over = population_60_years_and_over / square_km,
         popden_40_to_60_years = population_40_to_60_years / square_km) %>%
  rename(admin1 = admin_level_1)


spop_adm1 <- merge(s1, wpop_adm1, by = c("country", "admin1"))


region_list <- c("All Countries", "Northern Hemisphere", "Southern Hemisphere", 
                 "Western Africa", "Eastern Africa", "Central/Southern Africa", 
                 "Southeast Asia")

cc_list <- as.character(unique(xctry$country))



mal_vars <- c("allcause_cases", "tested_cases", "confirmed_cases", 
              "tpr", "severe_cases", "malaria_deaths", "anc1_visit")

indicator_labels <- setNames(c("All Cause Consultations", "Tested Cases", "Malaria Confirmed Cases", "Test Positivity Ratio", 
                               "Malaria Severe Cases" ,"Malaria Deaths", "ANC (1st) Visit"), mal_vars)

# Current month 
mo_now <- as.numeric(format(Sys.Date(), "%m"))


# Google mobility data
xmob <- xvul0 %>% select(c("country", "admin_level_1", "admin_level_2", "date", "year", "month", "day", ends_with("baseline"))) %>%
  filter( (admin_level_2 == "") & (!is.na(day)) ) %>%
  select(-admin_level_2) %>%
  select(-c("day", "date")) %>%
  group_by(country, admin_level_1, year, month) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  mutate(date = as.Date(paste(year, "-", month, "-01", sep = ""))) %>%
  relocate(date, .before = "year") %>%
  setNames(gsub("_percent_change_from_baseline", "", names(.))) %>%
  mutate(country = factor(country, levels = as.character(unique(country))))
