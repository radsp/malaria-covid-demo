

# To be used if using local data
# load("appdata.RData")

xrf <- xrf %>% 
  mutate(mygroup = case_when(str_detect(variable, "ltm") ~ "Historical Mean",
                             (rf_type %in% "acc_ssn") & !(str_detect(variable, "ltm")) ~ as.character(year_ssn),
                             TRUE ~ as.character(year))) 

# Filter out Malawi (data prior to Q2 2019 doesn't look correct)
xctry <- xctry %>%
  mutate(country = as.character(country)) %>%
  filter(!((country %in% "Malawi") & (variable %in% "anc1_visit")))
xprov <- xprov %>%
  filter(!((country %in% "Malawi") & (variable %in% "anc1_visit")))

# Extract shapefiles and change the admin level column name to match QR data
s0 <- get_shapefile(admin_level = 0)
s1 <- get_shapefile(admin_level = 1)
s1$admin_level_1 <- s1$admin1

# Calculate polygon centroid (to be used as point location for COVID data)
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
# mo_now <- as.numeric(format(Sys.Date(), "%m")) 
mo_now <- 9 # temporary fix - using our current month no longer works because now we're in 2021!

# Permission set variables
permission_set_id <- 165


# Reporting table data 
all_resources <- c('Angola','Benin','Burkina_Faso','Cambodia','Cameroon',
                   'Cote_DIvoire','DR_Congo','Ethiopia',
                   'Ghana','Guinea','Liberia','Madagascar','Malawi',
                   'Mali','Mozambique','Myanmar','Niger','Nigeria',
                   'Rwanda','Senegal','Sierra_Leone','Tanzania_Mainland',
                   'Tanzania_Zanzibar','Thailand','Uganda','Zambia',
                   'Zimbabwe')


# reporting table call (just for user-level filtering) # not the pmi reporting table bc we want Tanzania

query <- sql("SELECT country, admin_level_1, admin_level_2, date, year, month,
             new_consultation_all_cause, tested_cases, confirmed_cases,
             /* confirmed_cases_facility, confirmed_cases_community, */
             severe_cases, malaria_deaths, anc1_visit,
             reports_received__new_consultation, reports_expected__new_consultation,
             reports_received__confirmed_cases, reports_expected__confirmed_cases,
             reports_received__anc_consultation, reports_expected__anc_consultation,
             total_population
             FROM reporting_qr.qr_reporting_table
             WHERE
             (admin_level_2 IS NOT NULL OR admin_level_2 <> '') AND
             (admin_level_1 IS NOT NULL OR admin_level_1 <> '')")
reporting_tbl <- read_civis(query)

# all_countries <- all_resources # update this once pmi data is loaded in
all_countries <- sort(unique(reporting_tbl$country))

# Google mobility data
xmob <- xvul0 %>% 
  select(c("country", "admin_level_1", "admin_level_2", "date", "year", "month", "day", ends_with("baseline"))) %>%
  filter( (admin_level_2 == "") & (!is.na(day)) ) %>%
  select(-admin_level_2) %>%
  select(-c("day", "date")) %>%
  group_by(country, admin_level_1, year, month) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  mutate(date = as.Date(paste(year, "-", month, "-01", sep = ""))) %>%
  relocate(date, .before = "year") %>%
  setNames(gsub("_percent_change_from_baseline", "", names(.))) %>%
  mutate(country = factor(country, levels = as.character(unique(country))))

