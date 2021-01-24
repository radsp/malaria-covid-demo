# Data management - Custom Surgo Scores

# NOTES : 
# wpop1 - pop (read in 1X - yearly) - reported as counts 
# xmob - mobility - reported as % change in mobility
# xwash - hand-washing (TBC) - reported as % of hh/pop (my guess is that pop is extrapolated)
# mal - created in this file - reported as counts

vars_test <- c("pop_perc_65_years_and_over",
               "pop_perc_45_to_64_years"  , 
               "pop_perc_25_to_44_years",
               "pop_perc_15_to_24_years",
               "pop_perc_5_to_14_years",
               "pop_perc_0_to_4_years",
               "mob_grocery_and_pharmacy",
               "mob_parks",
               "mob_residential",
               "mob_retail_and_recreation",
               "mob_transit_stations",
               "mob_workplaces",
               "population_living_in_households_with_a_basic_handwashing_facility_with_soap_and_water_available",
               "population_with_a_place_for_handwashing_was_observed")

xvul0 <- read_civis("covid.vulnerability_reporting_table") %>%
  mutate(date = as.Date(as.character(date)))

wpop <- xvul0 %>% # this will require updating the platform script
  filter(date == "2020-01-01" & !(admin_level_2 == "") & !(admin_level_1 == "")) %>%
  select(country, admin_level_1, admin_level_2, date, year, square_km, starts_with("male"), starts_with("female")) %>%
  mutate(total_worldpop = rowSums(select(., starts_with("male"), starts_with("female")), na.rm = TRUE))

wpop_adm1 <- wpop %>% 
  select(-c(date, admin_level_2)) %>%
  group_by(country, admin_level_1, year) %>%
  summarise_all(.funs =  list(~sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(pop_perc_65_years_and_over = rowSums(select(., matches("65_to_69|70_to_74|75_to_79|80_years")))/total_worldpop,
         pop_perc_45_to_64_years = rowSums(select(., matches("45_to_49|50_to_54|55_to_59|60_to_64")))/total_worldpop,
         pop_perc_25_to_44_years = rowSums(select(., matches("25_to_29|30_to_34|35_t0_39|40_to_44")))/total_worldpop,
         pop_perc_15_to_24_years = rowSums(select(., matches("15_to_19|20_to_24")))/total_worldpop,
         pop_perc_5_to_14_years = rowSums(select(., matches("5_to_9|10_to_14")))/total_worldpop,
         pop_perc_0_to_4_years = rowSums(select(., matches("0_to_12|1_to_4")))/total_worldpop) %>%
  select(country, admin_level_1, year,starts_with("pop_perc_")) %>%
  mutate(date = "2020-01-01") %>%
  mutate(date = as.Date(as.character(date)))

# wpop_adm1_sub <- wpop_adm1[,-c(1:3, 10)] # AZ
# test <- data.frame(scale(wpop_adm1_sub))
# test1 <- cbind(wpop_adm1[,c(1:3, 10)], test)

str(wpop_adm1)

# malaria data - admin level 1
xvul1 <- xvul0 %>% 
  filter(date >= "2020-01-01" & !(admin_level_1 == "") & !(admin_level_2 == "")) %>%
  select(country, admin_level_1, date, malaria_deaths, confirmed_cases,
         suspected_cases, health_workers, new_consultation_all_cause) %>%
  group_by(country, admin_level_1, date) %>%
  summarise_all(.funs =  list(~sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(date = as.Date(as.character(date)))


colnames(xvul1)[4:8] <- paste("mal", colnames(xvul1)[4:8], sep = "_") # adding prefix to colnames

# mobility data
xmob <- xvul0 %>% select(c("country", "admin_level_1", "admin_level_2", "date", "year", "month", "day", ends_with("baseline"))) %>%
  filter(year==2020 & (admin_level_2 == "") & (!is.na(day)) ) %>% # adding filter for 2020
  select(-admin_level_2) %>%
  select(-c("day", "date")) %>%
  group_by(country, admin_level_1, year, month) %>%
  summarise_all(mean, na.rm = TRUE) %>% ungroup() %>%
  mutate(date = as.Date(paste(year, "-", month, "-01", sep = ""))) %>%
  relocate(date, .before = "year") %>%
  setNames(gsub("_percent_change_from_baseline", "", names(.))) %>%
  mutate(country = factor(country, levels = as.character(unique(country)))) %>%
  mutate(residential = -1 * residential)

colnames(xmob)[6:11] <- paste("mob", colnames(xmob)[6:11], sep = "_")

# handwashing data - missing for all of 2020

xwash1 <- xvul0 %>% select(c("country", "admin_level_1", "admin_level_2", "date", "year", "month", "day", contains("handwashing"))) %>%
  filter( (admin_level_2 == "") & (!is.na(day)) ) %>% 
  select(-c(admin_level_2, day, date)) %>%
  group_by(country, admin_level_1, year, month) %>%
  summarise_all(mean, na.rm = TRUE) %>% ungroup() %>%
  mutate(date = as.Date(paste(year, "-", month, "-01", sep = ""))) %>%
  relocate(date, .before = "year") %>%
  mutate(country = factor(country, levels = as.character(unique(country))))

# add in table from Tasmia 
# we should see if tasmia can also fetch the date -- not just the year -> the year doesn't uniquely identify some indicators !!!
# xwash2_long <- read_civis("covid.hand_washing_2")
# 
# xwash2 <- xwash2_long %>%
#   filter(characteristiccategory == "admin1" & indicatorid %in% c("WS_HNDW_P_SOP", "WS_HNDW_H_NM1","WS_HNDW_P_NUM")) %>%
#   select(countryname, characteristiclabel, surveyyear, surveyid,dataid, indicatorid, value)  %>%
#   pivot_wider(names_from = indicatorid, values_from = value) 
# 
#   xwash2 <- data.frame(lapply(xwash2, function(x) {gsub("NULL", NA, x)}))
# 
# 
# 

xwash <- data.frame(lapply(xwash, function(x) {gsub("NaN", NA, x)})) # setting NaNs to NA

mob_wash <- merge(xmob, xwash, by=c('country', 'admin_level_1', 'date', 'year', 'month'), all=T) 
mal_mob_wash <- merge(xvul1, mob_wash, by=c('country', 'admin_level_1', 'date'), all=T)  # full join

# updating to year month cols
mal_mob_wash <- mal_mob_wash %>%
  mutate(month = substr(date, start=6, stop=7),
         year = substr(date, start=1, stop=4),
         month = str_remove(month, "^0+")) 

# add in pop data - this should now have all data loaded
custom0 <- merge(mal_mob_wash, wpop_adm1, by=c('country', 'admin_level_1', 'year','date'), all=T) 
custom0 <- custom0 %>%
  filter(admin_level_1 != "")

vars<- colnames(custom0)[-c(1:4,10)] # character vars (i.e. month, country)
custom0[,vars] <- data.frame(lapply(custom0[,vars], function(x) as.numeric(as.character(x))))

custom0_long <- custom0 %>%
  group_by(country, admin_level_1, date, month, year) %>%
  pivot_longer(cols=c(starts_with("mal_"), starts_with("mob_"), starts_with("pop_"),
                      contains("washing")),names_to = "variable", values_to = "value") %>%
  mutate(value = ifelse(grepl("NaN", value), NA, value))

custom0_long$date <- as.Date(custom0_long$date)

# vars_test <- c("mal_confirmed_cases")

custom0_latest <- custom0_long %>%
  filter(!is.na(value)) %>%
  group_by(country, admin_level_1, variable) %>%
  slice(which.max(as.Date(date))) %>%
  pivot_wider(names_from = variable, values_from = value)

custom0_latest[,-c(1:5)] <- data.frame(lapply(custom0_latest[,-c(1:5)], function(x) as.numeric(as.character(x))))

# aggregating to remove time component
custom0_cont <- custom0_latest %>%  
  group_by(country, admin_level_1) %>%
  summarize_if(is.numeric, mean, na.rm=T) %>%
  ungroup()

# custom0_cont <- data.frame(lapply(custom0_cont, function(x) {gsub("NaN", NA, x)})) # setting NaNs to NA
# custom0_cont[,-c(1:2)] <- data.frame(lapply(custom0_cont[,-c(1:2)], function(x) as.numeric(as.character(x))))

# custom0_cont_perc <- custom0_cont %>%
#   select(-starts_with("households_")) %>%# removing hh-level handwashing data 
#   summarize_if(is.numeric, percent_rank) %>%
#   mutate_if(is.numeric, round, digits=3) 

custom0_cont_perc <- custom0_cont %>%
  select(-starts_with("households_")) %>%# removing hh-level handwashing data 
  # select(country, starts_with("theme"), starts_with("overall")) %>%
  group_by(country) %>%
  summarize_if(is.numeric, percent_rank) %>%
  # mutate_if(is.numeric, round, digits=3) %>%
  ungroup() %>%
  select(-country)


custom0_cont_theme <- cbind(custom0_cont[,c('country', 'admin_level_1')], custom0_cont_perc) # add back in char vars

custom0_cont_theme <- data.frame(lapply(custom0_cont_theme, function(x) {gsub("NaN", NA, x)})) # setting NaNs to NA
# custom0_cont_theme[,-c(1:2)] <- data.frame(lapply(custom0_cont_theme[,-c(1:2)], function(x) as.numeric(as.character(x))))

if(length(names(custom0_cont_theme))>3) {
  custom0_cont_theme[,-c(1:2)] <- data.frame(lapply(custom0_cont_theme[,-c(1:2)], function(x) as.numeric(as.character(x))))
  
}else{
  custom0_cont_theme[,3] <- as.numeric(as.character(custom0_cont_theme[,3]))
}

custom0_cont_theme <- custom0_cont_theme %>%
  mutate(theme_mal = round(rowMeans(select(., contains("mal_"))),3),
         theme_mob = round(rowMeans(select(., contains("mob_"))),3),
         theme_wash = round(rowMeans(select(., contains("washing"))),3),
         theme_pop = round(rowMeans(select(., contains("pop_"))),3))

custom0_cont_theme <- data.frame(lapply(custom0_cont_theme, function(x) {gsub("NaN", NA, x)})) # setting NaNs to NA
custom0_cont_theme[,-c(1:2)] <- data.frame(lapply(custom0_cont_theme[,-c(1:2)], function(x) as.numeric(as.character(x))))

custom0_cont_theme <- custom0_cont_theme %>%
  mutate(overall_custom_score = round(rowMeans(select(., contains("theme_")), na.rm = TRUE),3))

custom0_cont_theme[,-c(1:2)] <- data.frame(lapply(custom0_cont_theme[,-c(1:2)], function(x) as.numeric(as.character(x))))

# custom1 <- custom0_cont_theme %>%
#   # select(country, starts_with("theme"), starts_with("overall")) %>%
#   group_by(country) %>%
#   summarize_if(is.numeric, percent_rank) %>%
#   # mutate_if(is.numeric, round, digits=3) %>%
#   ungroup() %>%
#   select(-country)

custom1 <- custom0_cont_theme
# addings suffixes
colnames(custom1)[-c(1:2)] <- paste(colnames(custom1)[-c(1:2)], "country", sep = "_")

# custom0_cont_theme <- data.frame(lapply(custom0_cont_theme, function(x) {gsub("NaN", NA, x)})) # setting NaNs to NA
# custom0_cont_theme[,-c(1:2)] <- data.frame(lapply(custom0_cont_theme[,-c(1:2)], function(x) as.numeric(as.character(x))))
# 
# custom1 <- data.frame(lapply(custom1, function(x) {gsub("NaN", NA, x)})) # setting NaNs to NA
# custom1[,-c(1:2)] <- data.frame(lapply(custom1[,-c(1:2)], function(x) as.numeric(as.character(x))))
# 
# 
# names(custom1)[grepl("_continent_country", names(custom1))] <- gsub("_continent_country", "_country", names(custom1)[grepl("_continent_country", names(custom1))])
# 
# custom_all_themes <- cbind(custom0_cont_theme[,1:2],
#                            # custom0_cont_theme[, c(grepl("_continent", names(custom0_cont_theme)))],
#                            custom1) # add back in char vars

# merge with date components elements from 

custom_tot <- merge(custom0_latest, custom1, by=c("country", "admin_level_1"),all=T)

# custom_tot <- custom_tot %>%
#   select(-starts_with("households_", ))

custom0_latest <- custom0_latest %>%
  select(-starts_with("households_", ))

# custom_tot <- data.frame(lapply(custom_tot, function(x) {gsub("NaN", NA, x)})) # setting NaNs to NA
# custom_tot[,-c(1:2)] <- data.frame(lapply(custom_tot[,-c(1:2)], function(x) as.numeric(as.character(x))))


# asign NAs to percentiles where indicators are 0 in the raw sliced data
indic_vars <- colnames(custom0_latest)[-c(1:5)]
country_vars <- paste0(colnames(custom0_latest)[-c(1:5)],"_country")

for (i in 1:length(indic_vars)){
  custom_tot[,c(country_vars[i])]<- ifelse(is.na(custom_tot[,indic_vars[i]]), NA, custom_tot[,country_vars[i]])
}

custom_tot$date <- as.Date(custom_tot$date)
test <- custom_tot %>%
  group_by(country, admin_level_1) %>% 
  filter(date == max(date)) %>%
  mutate(date_max = date) %>%
  select(country, admin_level_1, date_max)

custom_tot <- merge(custom_tot, test, by=c("country", "admin_level_1"), all.x=T)
custom_tot <- custom_tot %>%
  mutate(date_max = ifelse(date == date_max, date, NA),
         overall_custom_score_country = ifelse(!is.na(date_max), overall_custom_score_country, NA))

# custom_tot$date_max <- as.Date(as.character(custom_tot$date_max))

custom_final <- custom_tot %>%
  select(country, admin_level_1, year, date, month, year, starts_with("theme_"), ends_with("_country"))


# vars_test <- c("pop_perc_15_to_24_years")

# one remaining issue:
# surgo score overall score construction logic - weekend (low-priority)

# past changes 
# getting the most recent date for each variable - easiest done in long (done)

# now we're going to adjust the age bands & make into % of pop (done)

# shading by custom score
# we'll provide indicator scores
# done at a continent level - should we do these at a country level too? YES
# indicator scores available in pop-ups

# maybe we don't include the themes? tool tip would be strange - no year on themed scores we want to give context to users
# they maybe confused if things are weighted equally, but the themes component isn't explained.
