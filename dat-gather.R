

## Libraries ------------------------------------------------------

library(civis)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)

## FUNCTIONS ------------------------------------------------------

get_meanval <- function(u){
  if(all(u==0) | all(is.na(u))){
    return(0)
  } else {
    return(mean(u, na.rm = TRUE))
  }
}


## START CODE/ALGORITHM --------------------------------------------


# Inputs
qr_date_max <- as.Date("2020-06-01")
fcst_date_min <- Sys.Date()
covid_date_max <- seq(Sys.Date(), length.out = 2, by = "-1 month")[2]

#-------------------------------------------------------------------
# 
# Malaria QR data
#
#-------------------------------------------------------------------

# Retrieve data directly from qr table only for those variables that 
# will be needed in the app.
# There's a probably a better way to select column names using pattern
# search so that we don't need to type out all the names but not sure
# how to do that in SQL

query <- sql("SELECT country, admin_level_1, admin_level_2, date, year, month,
             new_consultation_all_cause, tested_cases, confirmed_cases, 
             /* confirmed_cases_facility, confirmed_cases_community, */
             severe_cases, malaria_deaths, anc1_visit, 
             reports_received__new_consultation, reports_expected__new_consultation,
             reports_received__confirmed_cases, reports_expected__confirmed_cases,
             reports_received__anc_consultation, reports_expected__anc_consultation,
             total_population
             FROM reporting_qr.qr_reporting_table_pmi
             WHERE 
             (admin_level_2 IS NOT NULL OR admin_level_2 <> '') AND  
             (admin_level_1 IS NOT NULL OR admin_level_1 <> '')")  # This filtering is not working


xm0 <- read_civis(query) 

# In case date is read as factor/character
if(!(class(xm0$date) %in% "Date")) xm0$date <- as.Date(as.character(xm0$date))


vvpop <- c("allcause_cases", "tested_cases", "confirmed_cases", 
           "severe_cases", "malaria_deaths", "anc1_visit")

xmal0 <- xm0 %>%
  # Remove the aggregated country and level 1 rows
  filter( (admin_level_1 != "") & !(is.na(admin_level_1)) & 
            (admin_level_2 != "") & !(is.na(admin_level_2))) %>%
  # Rename variables to match the previous dataset
  dplyr::rename(allcause_cases = new_consultation_all_cause,
                reports_expected__allcause_cases = reports_expected__new_consultation, 
                reports_received__allcause_cases = reports_received__new_consultation) %>%
  # Since not all countries start data in 2012, remove earlier dates
  group_by(country, admin_level_1, admin_level_2) %>%
  mutate(datemin = if_else(confirmed_cases > 0, date, qr_date_max)) %>%
  mutate(datemin = if_else(is.na(datemin), qr_date_max, datemin)) %>%
  filter(date >= datemin)  %>%
  filter(date <= qr_date_max) %>%
  ungroup() %>% select(-datemin) %>%
  # add population for each indicator so that if the values are NA, population will 
  # be 0. This is to avoid having higher population denominator where the count is NA
  pivot_longer(cols = vvpop, names_to = "vpop", values_to = "tmp") %>%
  mutate(population = if_else(is.na(tmp), 0, total_population)) %>%
  pivot_wider(names_from = vpop, values_from = c(tmp, population)) %>%
  # Rename variables with tmp_ suffixes back to its original
  setNames(gsub("tmp_", "", names(.)))
  
  


##-- National level data for Malaria --##
  
tmp0_main <- xmal0 %>%
  group_by(country, date, year, month) %>%
  summarise_at(., vars(-c("admin_level_1", "admin_level_2")),
               .funs =  list(~sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  # Calculate derived variables (positivity rates and reporting rates) 
  mutate(tpr = confirmed_cases / tested_cases,
         reports_rate__allcause_cases = reports_received__allcause_cases / reports_expected__allcause_cases,
         reports_rate__confirmed_cases = reports_received__confirmed_cases / reports_expected__confirmed_cases, 
         reports_rate__anc1_visit = reports_received__anc_consultation / reports_expected__anc_consultation) %>%
  # remove unused variables
  select(-((starts_with("reports_received")) | starts_with("reports_expected")))

tmp0_pop <- tmp0_main %>%
  select(c("country", "date", "year", "month"), starts_with("population_")) %>%
  pivot_longer(cols = -c("country", "date", "year", "month"), 
               names_to = "variable", values_to = "population_by_variable") %>%
  mutate(variable = gsub("population_", "", variable))

tmp0 <- tmp0_main %>%
  select(!starts_with("population_")) %>%
  pivot_longer(cols = -c("country", "date", "year", "month", "total_population"), 
               names_to = "variable", values_to = "value") %>%
  full_join(., tmp0_pop) %>%
  mutate(value_rate = if_else(variable %in% 
                                c("tpr", "reports_rate__confirmed_cases", "anc1_visit",
                                  "reports_rate__allcause_cases", "reports_rate__anc1_visit"), 
                              value, 1000 * value / population_by_variable)) %>%
  mutate(value_rate = if_else(variable %in% "malaria_deaths", 1000000 * value / population_by_variable, value_rate)) %>%
  pivot_longer(cols = c("value", "value_rate"), names_to = "count_type", values_to = "value") 


popqr_adm0 <- select(tmp0, c("country", "year", "month", "total_population"))

# historical mean calculation
ltm0 <- tmp0 %>%
  filter(year != 2020) %>%
  group_by(country, month, variable, count_type) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!str_detect(variable, "^reports_rate")) %>%
  mutate(variable = paste(variable, "_ltm", sep = ""),
         year = 2018,
         date = as.Date(paste(year, "-" ,month, "-01", sep = "")))



xmal_reporting_tmp <- tmp0 %>%
  select(-c(population_by_variable, total_population)) %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  group_by(country, count_type) %>%
  mutate(reports_rate_scaled__allcause_cases =  rescale(reports_rate__allcause_cases, to = c(0, 0.8* get_meanval(allcause_cases))),
         reports_rate_scaled__confirmed_cases = rescale(reports_rate__confirmed_cases, to = c(0, 0.8 * get_meanval(confirmed_cases))),
         reports_rate_scaled__anc1_visit =  rescale(reports_rate__anc1_visit, to = c(0, 0.8 * get_meanval(anc1_visit)))) %>%
  select(country, date, year, month, count_type, starts_with("reports_")) %>%
  ungroup()

xmal_reporting_1 <- xmal_reporting_tmp %>%
  select(country, date, year, month, count_type, ends_with("allcause_cases")) %>%
  rename('info' = 'reports_rate__allcause_cases', 'value' = 'reports_rate_scaled__allcause_cases') %>%
  mutate(info = as.character(round(info, digits = 2)),
         variable = "reports_rate__allcause_cases", 
         colourgroup = "reports_rate",
         boxgroup = "allcause_cases")


xmal_reporting_2 <- xmal_reporting_tmp %>%
  select(country, date, year, month, count_type, ends_with("confirmed_cases")) %>%
  rename('info' = 'reports_rate__confirmed_cases', 'value' = 'reports_rate_scaled__confirmed_cases') %>%
  mutate(info = as.character(round(info, digits = 2)),
         variable = "reports_rate__confirmed_cases", 
         colourgroup = "reports_rate",
         boxgroup = "confirmed_cases") 


xmal_reporting_3 <- xmal_reporting_tmp %>%
  select(country, date, year, month, count_type, ends_with("anc1_visit")) %>%
  rename('info' = 'reports_rate__anc1_visit', 'value' = 'reports_rate_scaled__anc1_visit') %>%
  mutate(info = as.character(round(info, digits = 2)),
         variable = "reports_rate__anc1_visit", 
         colourgroup = "reports_rate",
         boxgroup = "anc1_visit")


xmal_reporting <- rbind(xmal_reporting_1, xmal_reporting_2, xmal_reporting_3) %>%
  mutate(linegroup = if_else(year == 2020, "2020", "Other Years"),
         mygroup = as.character(year),
         combogroup = "Report Rate")
  

xmal_adm0 <- tmp0 %>%
  select(-c(total_population, population_by_variable)) %>% 
  filter(!(str_detect(variable, "reports_rate"))) %>%
  bind_rows(., ltm0) %>%
  mutate(colourgroup = gsub("_ltm", "", variable)) %>%
  # mutate(colourgroup = if_else(str_detect(variable, "reports_rate"), "reports_rate", colourgroup)) %>%
  mutate(boxgroup = gsub("_ltm|reports_rate_", "", variable),
         linegroup = if_else(str_detect(variable, "ltm"), "Historical Mean", if_else(year == 2020, "2020", "Other Years"))) %>%
  mutate(linegroup = factor(linegroup, levels = c("2020", "Other Years", "Historical Mean"))) %>%
  mutate(mygroup = if_else(linegroup %in% "Historical Mean", "Historical Mean", as.character(year)),
         combogroup = linegroup) %>%
  bind_rows(., xmal_reporting)



# dat <- subset(xmal_adm0, country %in% "Madagascar") %>%
#   filter(!(str_detect(variable, "reports_rate")) & !(variable %in% c("health_workers", "health_workers_ltm")) &
#            count_type %in% "value_rate")
# 
# dat <- subset(xmal_adm0, country %in% "Liberia") %>%
#   filter(!( (str_detect(variable, "reports_rate")) & (year < 2020)) & !(variable %in% c("health_workers", "health_workers_ltm")) &
#            count_type %in% "value")

# ggplot(dat, aes(x = month)) +
#   geom_line(aes(y = value, colour = combogroup, alpha = combogroup, linetype = combogroup, 
#                 group = interaction(mygroup, combogroup))) +
#   geom_point(
#              aes(y = value, colour = combogroup, shape = combogroup,
#                  group = interaction(mygroup, combogroup)), size = 2) +
#   scale_colour_manual(values = c("red", "grey90", "red", "#A239CA")) +
#   scale_shape_manual(values = c(20, NA, NA, NA)) +
#   scale_alpha_manual(values = c(1, 1, 0.5, 1)) +
#   scale_linetype_manual(values = c("solid", "solid", "solid", "solid")) + 
#   facet_wrap(~boxgroup, scales = "free_y") +
#   theme_few()



rm(list = c("ltm0", "tmp0", "tmp0_main", "tmp0_pop", "xmal_reporting",
            "xmal_reporting_tmp",
            "xmal_reporting_1", "xmal_reporting_2", "xmal_reporting_3"))



##-- Sub-National level data for Malaria --##

tmp1_main <- xmal0 %>%
  group_by(country, admin_level_1, date, year, month) %>%
  summarise_at(., vars(-c("admin_level_2")),
               .funs =  list(~sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  # Calculate derived variables (positivity rates and reporting rates) 
  mutate(tpr = confirmed_cases / tested_cases,
         reports_rate__allcause_cases = reports_received__allcause_cases / reports_expected__allcause_cases,
         reports_rate__confirmed_cases = reports_received__confirmed_cases / reports_expected__confirmed_cases, 
         reports_rate__anc1_visit = reports_received__anc_consultation / reports_expected__anc_consultation) %>%
  # remove unused variables
  select(-((starts_with("reports_received")) | starts_with("reports_expected")))

tmp1_pop <- tmp1_main %>%
  select(c("country", "admin_level_1", "date", "year", "month"), starts_with("population_")) %>%
  pivot_longer(cols = -c("country", "admin_level_1", "date", "year", "month"), 
               names_to = "variable", values_to = "population_by_variable") %>%
  mutate(variable = gsub("population_", "", variable))

tmp1 <- tmp1_main %>%
  select(!starts_with("population_")) %>%
  pivot_longer(cols = -c("country", "admin_level_1", "date", "year", "month", "total_population"), 
               names_to = "variable", values_to = "value") %>%
  full_join(., tmp1_pop) %>%
  mutate(value_rate = if_else(variable %in% 
                                c("tpr", "reports_rate__confirmed_cases", "anc1_visit",
                                  "reports_rate__allcause_cases", "reports_rate__anc1_visit"), 
                              value, 1000 * value / population_by_variable)) %>%
  mutate(value_rate = if_else(variable %in% "malaria_deaths", 1000000 * value / population_by_variable, value_rate)) %>%
  pivot_longer(cols = c("value", "value_rate"), names_to = "count_type", values_to = "value") 


popqr_adm1 <- select(tmp1, c("country", "admin_level_1", "year", "month", "total_population"))

# historical mean
ltm1 <- tmp1 %>%
  filter(year != 2020) %>%
  group_by(country, admin_level_1, month, variable, count_type) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!str_detect(variable, "^reports_rate")) %>%
  mutate(variable = paste(variable, "_ltm", sep = ""),
         year = 2018,
         date = as.Date(paste(year, "-" ,month, "-01", sep = "")))



xmal1_reporting_tmp <- tmp1 %>%
  select(-c(population_by_variable, total_population)) %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  group_by(country, admin_level_1, count_type) %>%
  mutate(reports_rate_scaled__allcause_cases =  rescale(reports_rate__allcause_cases, to = c(0, 0.8* get_meanval(allcause_cases))),
         reports_rate_scaled__confirmed_cases = rescale(reports_rate__confirmed_cases, to = c(0, 0.8 * get_meanval(confirmed_cases))),
         reports_rate_scaled__anc1_visit =  rescale(reports_rate__anc1_visit, to = c(0, 0.8 * get_meanval(anc1_visit)))) %>%
  select(country, admin_level_1, date, year, month, count_type, starts_with("reports_")) %>%
  ungroup()

xmal1_reporting_1 <- xmal1_reporting_tmp %>%
  select(country, admin_level_1, date, year, month, count_type, ends_with("allcause_cases")) %>%
  rename('info' = 'reports_rate__allcause_cases', 'value' = 'reports_rate_scaled__allcause_cases') %>%
  mutate(info = as.character(round(info, digits = 2)),
         variable = "reports_rate__allcause_cases", 
         colourgroup = "reports_rate",
         boxgroup = "allcause_cases")


xmal1_reporting_2 <- xmal1_reporting_tmp %>%
  select(country, admin_level_1, date, year, month, count_type, ends_with("confirmed_cases")) %>%
  rename('info' = 'reports_rate__confirmed_cases', 'value' = 'reports_rate_scaled__confirmed_cases') %>%
  mutate(info = as.character(round(info, digits = 2)),
         variable = "reports_rate__confirmed_cases", 
         colourgroup = "reports_rate",
         boxgroup = "confirmed_cases")


xmal1_reporting_3 <- xmal1_reporting_tmp %>%
  select(country, admin_level_1, date, year, month, count_type, ends_with("anc1_visit")) %>%
  rename('info' = 'reports_rate__anc1_visit', 'value' = 'reports_rate_scaled__anc1_visit') %>%
  mutate(info = as.character(round(info, digits = 2)),
         variable = "reports_rate__anc1_visit", 
         colourgroup = "reports_rate",
         boxgroup = "anc1_visit") 


xmal1_reporting <- rbind(xmal1_reporting_1, xmal1_reporting_2, xmal1_reporting_3) %>%
  mutate(linegroup = if_else(year == 2020, "2020", "Other Years"),
         mygroup = as.character(year),
         combogroup = "Report Rate")



xmal_adm1 <- tmp1 %>%
  select(-c(total_population, population_by_variable)) %>%
  filter(!(str_detect(variable, "reports_rate"))) %>%
  bind_rows(., ltm1) %>%
  mutate(colourgroup = gsub("_ltm", "", variable)) %>%
  # mutate(colourgroup = if_else(str_detect(variable, "reports_rate"), "reports_rate", colourgroup)) %>%
  mutate(boxgroup = gsub("_ltm|reports_rate_", "", variable),
         linegroup = if_else(str_detect(variable, "ltm"), "Historical Mean", if_else(year == 2020, "2020", "Other Years"))) %>%
  mutate(linegroup = factor(linegroup, levels = c("2020", "Other Years", "Historical Mean"))) %>%
  mutate(mygroup = if_else(linegroup %in% "Historical Mean", "Historical Mean", as.character(year)),
         combogroup = linegroup) %>%
  bind_rows(., xmal1_reporting)


rm(list = c("tmp1", "tmp1_main", "tmp1_pop", "ltm1", "xmal1_reporting", "xmal1_reporting_tmp",
            "xmal1_reporting_1", "xmal1_reporting_2", "xmal1_reporting_3"))



#--------------------------------------------------------------------------------------------------
# 
# Vulnerable Reporting Table -- Civis
#
#--------------------------------------------------------------------------------------------------

xvul0 <- read_civis("covid.vulnerability_reporting_table") %>%
  mutate(date = as.Date(as.character(date)))


wpop <- xvul0 %>%
  filter((date == as.Date("2020-01-01")) & !(admin_level_2 == "") & !(admin_level_1 == "")) %>%
  select(country, admin_level_1, admin_level_2, date, year, square_km, starts_with("male"), starts_with("female")) %>%
  mutate(total_worldpop = rowSums(select(., starts_with("male"), starts_with("female")), na.rm = TRUE))

wpop_adm0 <- wpop %>%
  select(-c(date, admin_level_1, admin_level_2)) %>%
  group_by(country, year) %>%
  summarise_all(.funs =  list(~sum(., na.rm = TRUE))) %>%
  ungroup()

wpop_adm1 <- wpop %>%
  select(-c(date, admin_level_2)) %>%
  group_by(country, admin_level_1, year) %>%
  summarise_all(.funs =  list(~sum(., na.rm = TRUE))) %>%
  ungroup()

# If using world_pop table

# wpop0 <- read_civis("covid.world_pop") %>%
#   rename(country = admin0, admin_level_1 = admin1, admin_level_2 = admin2) %>%
#   mutate(country = recode(country, "Cote d'Ivoire" = "Cote D'Ivoire")) %>%
#   mutate(total_worldpop =  rowSums(select(., starts_with("male"), starts_with("female")), na.rm = TRUE))
# 
# wpop_adm0 <- wpop0 %>%
#   select(country, starts_with("male"), starts_with("female"),  total_worldpop) %>%
#   group_by(country) %>%
#   summarise_all(.funs =  list(~sum(., na.rm = TRUE))) %>%
#   mutate(year = 2020) %>%
#   ungroup()
# 
# 
# wpop_adm1 <- wpop0 %>%
#   select(country, admin_level_1, starts_with("male"), starts_with("female"),  total_worldpop) %>%
#   group_by(country, admin_level_1) %>%
#   summarise_all(.funs =  list(~sum(., na.rm = TRUE))) %>%
#   mutate(year = 2020) %>%
#   ungroup()



#--------------------------------------------------------------------------------------------------
#
# COVID-19 case data
#
#--------------------------------------------------------------------------------------------------


##-- National level --##

xcov_adm0_tmp1 <- read_civis("covid.national_wide_monthly") %>%
  filter(year >= 2020) %>%
  mutate(country = recode(country, "Cote d'Ivoire" = "Cote D'Ivoire", 
                          "Democratic Republic of Congo" = "DR Congo", 
                          "Tanzania" = "Tanzania (Mainland)")) %>%
  rename(covid_cases = new_cases, covid_deaths = new_deaths, covid_tests = new_tests) %>%
  select(-starts_with("total"), -starts_with("cumulative")) %>%
  left_join(., wpop_adm0[, c("country", "year", "total_worldpop")], by = c("country", "year")) %>%
  # left_join(., popqr_adm0[, c("country", "year", "total_population")], by = c("country", "year")) %>%
  mutate(covid_cases_rate = 1000 * covid_cases / total_worldpop,
         covid_deaths_rate = 1000000 * covid_deaths / total_worldpop) %>%
  pivot_longer(cols = c("covid_cases", "covid_cases_rate", "covid_deaths", "covid_deaths_rate"),
               names_to = "variable", values_to = "value") %>%
  mutate(count_type = if_else(str_detect(variable, "rate"), "value_rate", "value"),
         date = as.Date(paste(year, "-", month, "-01", sep = ""))) %>%
  select(-c(total_worldpop, covid_tests)) %>%
  relocate(country, date)


# need to repeat each covid cases for each box group (excluding deaths)
xcov_adm0_cases <- xcov_adm0_tmp1 %>% 
  filter(!str_detect(variable, "deaths")) %>%
  replicate(6, ., simplify = FALSE) %>%
  bind_rows() %>%
  mutate(boxgroup = c(rep("allcause_cases", n()/6),
                      rep("tested_cases", n()/6),
                      rep("confirmed_cases", n()/6),
                      rep("tpr", n()/6),
                      rep("severe_cases", n()/6),
                      rep("anc1_visit", n()/6)))

xcov_adm0 <- xcov_adm0_tmp1 %>%
  filter(str_detect(variable, "deaths")) %>%
  filter(date < covid_date_max) %>%
  mutate(boxgroup = "malaria_deaths") %>%
  bind_rows(xcov_adm0_cases, .) %>%
  mutate(variable = gsub("_rate", "", variable),
         colourgroup = "covid",
         linegroup = "2020",
         mygroup = "2020",
         combogroup = "COVID-19")


rm(list = c("xcov_adm0_cases", "xcov_adm0_tmp1"))


##-- Sub-National level --##

xc0 <- read_civis("covid.subnational_wide_monthly")

xcov_adm1_tmp1 <- xc0 %>%
  select(country, admin_level_1, month, new_cases, new_deaths) %>%
  rename(covid_cases = new_cases, covid_deaths = new_deaths) %>%
  group_by(country, admin_level_1, month) %>%
  summarise_all(.funs =  list(~sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(year = 2020) %>%
  left_join(., wpop_adm1[, c("country", "admin_level_1", "year", "total_worldpop")],
            by = c("country", "admin_level_1","year")) %>%
  mutate(covid_cases_rate = 1000 * covid_cases / total_worldpop,
         covid_deaths_rate = 1000000 * covid_deaths / total_worldpop) %>%
  pivot_longer(cols = c("covid_cases", "covid_cases_rate", "covid_deaths", "covid_deaths_rate"),
               names_to = "variable", values_to = "value") %>%
  mutate(count_type = if_else(str_detect(variable, "rate"), "value_rate", "value"),
         date = as.Date(paste(year, "-", month, "-01", sep = ""))) %>%
  select(-total_worldpop) %>%
  relocate(date, .before = "month")

# need to repeat each covid cases for each box group (excluding deaths)
xcov_adm1_cases <- xcov_adm1_tmp1 %>% 
  filter(!str_detect(variable, "deaths")) %>%
  replicate(6, ., simplify = FALSE) %>%
  bind_rows() %>%
  mutate(boxgroup = c(rep("allcause_cases", n()/6),
                      rep("tested_cases", n()/6),
                      rep("confirmed_cases", n()/6),
                      rep("tpr", n()/6),
                      rep("severe_cases", n()/6),
                      rep("anc1_visit", n()/6)))

xcov_adm1 <- xcov_adm1_tmp1 %>%
  filter(str_detect(variable, "deaths")) %>%
  filter(date < covid_date_max) %>%
  mutate(boxgroup = "malaria_deaths") %>%
  bind_rows(xcov_adm1_cases, .) %>%
  mutate(variable = gsub("_rate", "", variable),
         colourgroup = "covid",
         mygroup = "2020",
         linegroup = "2020",
         mygroup = "2020",
         combogroup = "COVID-19")


rm(list = c("xcov_adm1_cases", "xcov_adm1_tmp1", "xc0"))




#--------------------------------------------------------------------------------------------------
#
# COVID-19 projection - Imperial college
# 
# The projection has different versions and at this point the last version
# is version 5 -- need to figure out which version to use
#
#--------------------------------------------------------------------------------------------------

query_imp <- sql("SELECT country, date, compartment, y_mean, scenario, report_date
                 FROM covid.mrc_ide_raw
                 WHERE (version = 'v5') AND (compartment IN ('infections', 'deaths')) AND
                 (scenario IN ('Maintain Status Quo', 'Additional 50% Reduction', 'Relax Interventions 50%'))")

ximp0 <- read_civis(query_imp)


ximp <- ximp0 %>%
  mutate(date = as.Date(as.character(date)),
         year = as.numeric(format(date, "%Y")),
         month = as.numeric(format(date, "%m")),
         report_date = as.Date(as.character(report_date)),
         country = recode(country, 
                          "Cote d'Ivoire" = "Cote D'Ivoire", 
                          "Democratic Republic of Congo" = "DR Congo", 
                          "Tanzania" = "Tanzania (Mainland)")) %>%
  group_by(country) %>%
  filter(report_date == max(report_date)) %>%
  ungroup() %>%
  # Convert to monthly data
  group_by(country, year, month, compartment, scenario) %>%
  summarise(y_mean = sum(y_mean , na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = as.Date(paste(year, "-", month, "-01", sep = ""))) %>%
  # Calculate rate
  left_join(., wpop_adm0[, c("country", "year", "total_worldpop")], by = c("country", "year")) %>%
  mutate(value_rate = if_else(compartment %in% "infections", 1000 * y_mean / total_worldpop,
                              1000000 * y_mean / total_worldpop)) %>%
  rename('variable'='compartment', 'value'='y_mean') %>%
  pivot_longer(cols = c("value", "value_rate"), names_to = "count_type",
               values_to = "value") %>%
  select(-total_worldpop) %>%
  mutate(variable = if_else(variable %in% "deaths", "covid_deaths_fcst", 
                            "covid_cases_fcst"),
         colourgroup = "covid")


ximp_cases <- ximp %>%
  filter(!(variable %in% "covid_deaths_fcst")) %>%
  replicate(6, ., simplify = FALSE) %>%
  bind_rows() %>%
  mutate(boxgroup = c(rep("allcause_cases", n()/6),
                      rep("tested_cases", n()/6),
                      rep("confirmed_cases", n()/6),
                      rep("tpr", n()/6),
                      rep("severe_cases", n()/6),
                      rep("anc1_visit", n()/6)))

ximp_adm0 <- ximp %>%
  filter(str_detect(variable, "deaths")) %>%
  mutate(boxgroup = "malaria_deaths") %>%
  bind_rows(ximp_cases, .) %>%
  mutate(linegroup = "2020",
         mygroup = scenario,
         combogroup = "COVID-19 Forecast") %>%
  rename('info' = 'scenario') %>%
  filter(date >= fcst_date_min)



ts_mal_adm0 <- bind_rows(xmal_adm0, xcov_adm0, ximp_adm0) %>%
  mutate(combogroup = factor(combogroup, 
                             levels = c("2020", "Other Years", "Historical Mean", 
                                        "COVID-19", "COVID-19 Forecast", "Report Rate")),
         boxgroup = factor(boxgroup, 
                           levels = c("allcause_cases", "tested_cases", "confirmed_cases", "tpr", 
                                      "severe_cases", "malaria_deaths", "anc1_visit"),
                           labels = c("All Cause Consultations", "Tested Cases", "Malaria Confirmed Cases",
                                      "Test Positivity Ratio", "Malaria Severe Cases", "Malaria Deaths", "ANC (1st) Visit")))

ts_mal_adm1 <- bind_rows(xmal_adm1, xcov_adm1) %>%
  mutate(combogroup = factor(combogroup, 
                             levels = c("2020", "Other Years", "Historical Mean", 
                                        "COVID-19", "COVID-19 Forecast", "Report Rate")),
         boxgroup = factor(boxgroup, 
                           levels = c("allcause_cases", "tested_cases", "confirmed_cases", "tpr", 
                                      "severe_cases", "malaria_deaths", "anc1_visit"),
                           labels = c("All Cause Consultations", "Tested Cases", "Malaria Confirmed Cases",
                                      "Test Positivity Ratio", "Malaria Severe Cases", "Malaria Deaths", "ANC (1st) Visit")))


ts_mal <- bind_rows(ts_mal_adm0, ts_mal_adm1) %>%
  relocate(country, admin_level_1)

#--------------------------------------------------------------------------------------------------
#
# RAINFALL data -- currently using GPM
# will need to change to CHIRPS once it is ready
#
#--------------------------------------------------------------------------------------------------

# When data saved to MDIVE NA becomes ""

rf0 <- read_civis("covid.gpm_monthly") %>%
  mutate( #admin_level_1 = if_else(admin_level_1 == "", NA_character_, as.character(admin_level_1)),
         date = as.Date(as.character(date)))

  
xrf_tmp1 <- rf0 %>%
  pivot_longer(cols = starts_with("rf"), names_to = "variable", values_to = "value") %>%
  mutate(rf_type = case_when(variable %in% 'rf' ~ 'monthly',
                             variable %in% 'rf_acc_cy' ~ 'acc_cy',
                             variable %in% 'rf_acc_ssn' ~ 'acc_ssn',
                             variable %in%'rf_ltm' ~ 'monthly',
                             variable %in% 'rf_ltm_acc_cy' ~ 'acc_cy',
                             variable %in%'rf_ltm_acc_ssn' ~ 'acc_ssn'),
         count_type = "value_rf") 

xrf_tmp2 <- xrf_tmp1 %>%
  filter(!(xrf_tmp1$variable %in% c("rf_ltm", "rf_ltm_acc_cy", "rf_ltm_acc_ssn"))) %>%
  select(-c(count_type, variable)) %>%
  group_by(country, admin_level_1, rf_type) %>%
  summarise(rfmax = max(value, na.rm = TRUE)) %>%
  ungroup()

mal4rf <- subset(ts_mal, variable %in% c("allcause_cases", "tested_cases", "confirmed_cases", "severe_cases", 
                                         "malaria_deaths", "anc1_visit", "covid_cases", "covid_deaths")) %>%
  select(country, admin_level_1, date, year, month, variable, count_type, value) %>%
  # mutate(admin_level_1 = if_else(admin_level_1 == "", NA_character_, as.character(admin_level_1))) %>%
  mutate(admin_level_1 = if_else(is.na(admin_level_1), "", as.character(admin_level_1))) %>%
  merge(., xrf_tmp2, by = c("country", "admin_level_1"), all = TRUE) %>%
  group_by(country, admin_level_1, variable, count_type, rf_type) %>%
  mutate(scaling = rfmax / (1.1 * max(value, na.rm = TRUE))) %>%
         # scaling_acc = max(rf_acc_cy, na.rm = TRUE) / (1.1 * max(value, na.rm = TRUE))) %>%
  mutate(value_scaled = (value * scaling) + (0.1 * rfmax)) %>%
         # value_scaled_acc = (value * scaling_acc) + (0.1 * max(rf, na.rm = TRUE))) %>%
  ungroup() %>%
  # filter(!(is.na(count_type))) %>%
  select(country, admin_level_1, date, year, month, variable, count_type, rf_type, value_scaled) %>%
  left_join(., unique(xrf_tmp1[, c("country", "admin_level_1", "date", "month_order_ssn", "year_ssn")])) %>%
  rename('value' = 'value_scaled') %>%
  mutate(mal_label = recode(variable, 'confirmed_cases'  = "Malaria Confirmed Cases", 
                            'allcause_cases' = "New Consultations", 
                            'malaria deaths' = 'Malaria Deaths', 
                            'tested_cases' = "Tested Cases",
                            'severe_cases' = "Severe Cases",
                            'anc1_visit' = "ANC Visit", 
                            'covid_deaths' = "COVID-19 Deaths", 
                            'covid_cases' = "COVID-19 Cases")) %>%
    filter(!is.na(date)) # Some dates have NA values most likely because of admin that's not aligned

xrf <- bind_rows(xrf_tmp1, mal4rf) %>%
  mutate(combogroup = case_when(
    (variable %in% 'rf') & !(year %in% c(2020, 2019)) ~ 'Rainfall (Other Years)',
    (variable %in% 'rf') & (year %in% c(2020, 2019)) ~ paste('Rainfall (', year, ')', sep = ""),
    (variable %in% "rf_ltm") ~ 'Rainfall Historical Mean',
    (variable %in% "rf_acc_cy") & !(year %in% c(2020, 2019)) ~ 'Accumulated Rainfall (Other Years)',
    (variable %in% "rf_acc_cy") & (year %in% c(2020, 2019)) ~ paste('Accumulated Rainfall (', year, ')', sep = ""),
    (variable %in% "rf_ltm_acc_cy") ~ "Accumulated Rainfall Historical Mean",
    (variable %in% "rf_acc_ssn") & !(year %in% c(2018/2019, 2019/2020)) ~ 'Accumulated Rainfall (Other Years)',
    (variable %in% "rf_acc_ssn") & (year %in% c(2018/2019, 2019/2020)) ~ paste('Accumulated Rainfall (', year_ssn, ')', sep = ""),
    (variable %in% "rf_ltm_acc_ssn") ~ "Accumulated Rainfall Historical Mean",
    (count_type %in% c("value", "value_rate")) & (rf_type %in% c("monthly", "acc_cy")) ~ paste(mal_label, " (", year, ")", sep = ""),
    (count_type %in% c("value", "value_rate")) & (rf_type %in% "acc_ssn") ~ paste(mal_label, " (", year_ssn, ")", sep = ""),
    TRUE ~ 'Unknown'
  )) %>%
  filter((!is.na(year_ssn)) & !(combogroup %in% "Unknown"))


# dat1 <- xrf %>%
#   filter((variable %in% "confirmed_cases") & (count_type %in% "value_rate")) %>%
#   filter(year %in% c(2019, 2020)) %>%
#   filter(rf_type %in% "monthly")
# dat <- xrf %>%
#   filter((rf_type %in% "monthly") & (count_type %in% "value_rf")) %>%
#   bind_rows(., dat1) %>%
#   filter(admin_level_1 == "")
# 
# ggplot(dat, aes(x = month, y = value, colour = combogroup,  
#                 group = interaction(year, combogroup))) + 
#   geom_line() +
#   facet_wrap(~ country, scales = "free")
  

xctry <- ts_mal_adm0
xprov <- ts_mal_adm1

xrf_adm0 <- subset(xrf, admin_level_1 %in% "")
xrf_adm1 <- subset(xrf, admin_level_1 %in% "")

rm(list = setdiff(ls(), c("xctry", "xprov", "xrf_adm0", "xrf_adm1", "xvul0", "wpop_adm0", "wpop_adm1")))

# save("xctry", "xprov", "xrf_adm0", "xrf_adm1", "xvul0", "wpop_adm0", "wpop_adm1",
#      file = "appdata.RData")

