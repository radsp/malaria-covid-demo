
get_delta <- function(yr0, m0, mf, dat){
  
  m0 <- which(month.abb %in% m0)
  mf <- which(month.abb %in% mf)
  
  if(yr0 %in% "Historical mean"){
    yr0 <- NULL
    ltm_data <- TRUE
  } else {
    ltm_data <- FALSE
  }
  
  
  if(ltm_data){
    
    dat_delta <- dat %>% 
      filter( (year %in% c(2020, yr0) & (month %in% c(m0, mf)) &
                 !(colourgroup %in% c("covid")) ) ) %>%
      mutate(year = ifelse(linegroup %in% "Historical mean", 2019, year),
             variable = gsub("_ltm", "", variable))
    
  } else {
    
    dat_delta <- dat %>% 
      filter( (year %in% c(2020, yr0) & (month %in% c(m0, mf)) &
                 !(colourgroup %in% c("covid")) & !(linegroup %in% "Historical-term mean") ) )
  }
  

  dat_tmp <- dat_delta %>%
    group_by(country, year, boxgroup, variable) %>%
    summarise(value = if_else(all(is.na(value)), NA_real_, sum(value, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(year = if_else(as.character(year) %in% c("Historical mean", "2020"), as.character(year), "0")) %>%
    pivot_wider(names_from = "year", values_from = value, names_prefix = "y") 
  
  if( !("y2020" %in% colnames(dat_tmp))){
     dat_tmp <- dat_tmp %>% mutate(y2020 = NA)
  }
  
  if( !("y0" %in% colnames(dat_tmp))){
    dat_tmp <- dat_tmp %>% mutate(y0 = NA)
  }
  
  dat_delta <- dat_tmp %>%
    mutate(y2020 = ifelse( (y2020 == 0) , NA, y2020)) %>%
    mutate(delta = y2020 - y0) %>%
    mutate(delta_text = if_else(is.na(delta), "N/A", if_else(delta > 0, paste("+", sprintf("%1.1f", delta), sep = ""),
                                                             sprintf("%1.1f", delta))))
  
  return(dat_delta)
  
  
}



get_ribbon_data <- function(m0, mf, yf, dat){
  
  m0 <- which(month.abb %in% m0)
  mf <- which(month.abb %in% mf)
  
  dat <- dat %>% filter(stringr::str_detect(variable, "report", negate = TRUE))
  
  dfrbn1 <- subset(dat, (date >= as.Date(paste(2020, "-", m0, "-01", sep = ""))) & 
                     (date <= as.Date(paste(2020, "-", mf, "-01", sep = ""))) ) %>%
    filter((stringr::str_detect(variable, "ltm", negate = TRUE)))
  
  if ( nrow(dfrbn1) == 0){
    dfrbn <- dat %>%
      mutate(minval = NA, maxval = NA)
  } else {
    if(yf == "Historical mean"){
      dfrbn2 <- dat %>% filter(stringr::str_detect(variable, "ltm"))
    } else {
      dfrbn2 <- dat %>% filter( (year == as.numeric(yf)) & (stringr::str_detect(variable, "ltm", negate = TRUE)) )
    }
    
    dfrbn <- rbind(dfrbn1, dfrbn2) %>%
      mutate(vartype = if_else(stringr::str_detect(variable, "ltm"), "ltm", "x0"))
    
    if ("admin_level_1" %in% colnames(dfrbn1)) {
      dfrbn <- merge(dfrbn1, dfrbn2[, c("country", "admin_level_1","month", "colourgroup", "value")],
                     by = c("country", "admin_level_1","month", "colourgroup"), all.x = TRUE, all.y = FALSE) %>%
        rename(ltm_value = value.y, value = value.x)
    } else {
      dfrbn <- merge(dfrbn1, dfrbn2[, c("country", "month", "colourgroup", "value")],
                     by = c("country", "month", "colourgroup"), all.x = TRUE, all.y = FALSE) %>%
        rename(ltm_value = value.y, value = value.x)
    }
    
    dfrbn <- dfrbn %>%
      rowwise() %>%
      mutate(minval = min(value, ltm_value), maxval = max(value, ltm_value))
  }
  
  return(dfrbn)
}

extract_countries <- function(dat, region_selected) {
  
  if (region_selected %in% "Northern Hemisphere") {
    yout <- subset(dat, country %in% ctry_nhem)
  } else if (region_selected %in% "Southern Hemisphere") {
    yout <- subset(dat, country %in% ctry_shem)
  } else if (region_selected %in% "Western Africa") {
    yout <- subset(dat, country %in% ctry_wafr)
  } else if (region_selected %in% "Eastern Africa") { 
    yout <- subset(dat, country %in% ctry_eafr)
  } else if (region_selected %in% "Central/Southern Africa") {
    yout <- subset(dat, country %in% ctry_csafr)
  } else if (region_selected %in% "Southeast Asia") {
    yout <- subset(dat, country %in% ctry_sea)
  } else if (region_selected %in% cc_list) {
    yout <- subset(dat, country %in% region_selected)
  } else {
    yout <- dat
  }

  return(yout)
}


filter_region_snap <- function(dat, region){
  
  if (region %in% region_list){
    yout <- extract_countries(dat, region)
  } else {
    yout <- filter(dat, country %in% region)
  }
  
  return(yout)
}

# filter_region_count <- function(dat, region, value_type){
# 
#   if(region %in% region_list){
#     xin <- extract_countries(dat, region_selected = region)
#   } else {
#     xin <- filter(dat, country %in% region)
#   }
#   
#   xin <- xin %>% filter(count_type %in% value_type)
#   
#   return(xin)
#   
# }


get_difference <- function(dat, var_name, m0, mf, yr0, yrf = 2020, as_total){
  
  u <- dat %>% 
    filter((stringr::str_detect(variable, var_name))) %>%
    filter(stringr::str_detect(variable, "reports", negate = TRUE))
  
  if (is.character(m0)) {
    m0 <- which(month.abb %in% m0)
    mf <- which(month.abb %in% mf)}
  
  
  # define the baseline period. when historical mean is the baseline period,
  # the data needs to be filtered by the variable name and the month only
  if (!(yr0 %in% "Historical Mean")) {
    bt0 <- as.Date(paste(yr0, "-", m0, "-01", sep = ""))
    btf <- as.Date(paste(yr0, "-", mf, "-01", sep = ""))
    btt <- seq(bt0, btf, by = "1 month")
    mm <- as.numeric(format(btt, "%m"))
    u1 <- u %>% 
      filter(variable %in% var_name) %>%
      filter(date %in% btt) %>%
      select(-c(ends_with("group"), starts_with("info"), "date", "year", "variable"))
  } else {
    mm <- c(m0:12, 1:mf)
    u1 <- u %>% 
      filter(stringr::str_detect(variable, "ltm")) %>%
      filter(month %in% mm) %>%
      select(-c(ends_with("group"), starts_with("info"), "date", "year", "variable")) %>%
      distinct()
  }
  
  # current year period 
  yrf_new <-  ifelse(m0 > mf, yrf + 1, yrf)
  et0 <- as.Date(paste(yrf_new, "-", m0, "-01", sep = ""))
  etf <- as.Date(paste(yrf_new, "-", mf, "-01", sep = ""))
  ett <- seq(et0, etf, by = "1 month")
  u2 <- u %>%
    filter((variable %in% var_name) & (date %in% ett)) %>%
    select(-c(ends_with("group"), starts_with("info"), "date"))
  
  
  if (!("admin_level_1" %in% colnames(u1))){
    u <- merge(u1, u2, by = c("country", "month"))
  } else {
    u <- merge(u1, u2, by = c("country", "admin_level_1", "month"))
  }
  
  # u <- u %>%
  #  mutate(delta = round(100 * (value.y - value.x) / value.x, digits = 2),
  #          colorgroup = if_else(delta > 0, "positive", "negative"))
  
  if(as_total){
    # Only add data that are available in both years
    # so that the comparison is with the same number of months
    # yout <- u %>% filter(!(is.na(value.x)) & !(is.na(value.y)))
    yout <- u %>% mutate(value.x = na_if(value.x, 0),
                         value.y = na_if(value.y, 0))
    if("admin_level_1" %in% colnames(u)){
      yout <- yout %>%
        group_by(country, admin_level_1, variable) %>%
        summarise(delta = round(100 * (sum(value.y) - sum(value.x)) / sum(value.x), digits = 2)) 
    } else {
      yout <- yout %>%
        group_by(country, variable) %>%
        summarise(delta = round(100 * (sum(value.y) - sum(value.x)) / sum(value.x), digits = 2))
    }
  } else {
    yout <- u %>% mutate(value.x = na_if(value.x, 0),
                         value.y = na_if(value.y, 0)) %>%
      mutate(delta = round(100 * (value.y - value.x) / value.x, digits = 2),
             colorgroup = if_else(delta > 0, "positive", "negative"))
  }
  
  return(yout)
}




get_snapshot_data <- function(dat, region, yr0, m0, mf, value_type){
  
    vars <- c("allcause_cases", "confirmed_cases", "tested_cases",
              "tpr", "severe_cases", "malaria_deaths", "anc1_visit")
    
      if (region %in% region_list) {
        xdat <- extract_countries(dat, region)
        total0 <- bymonth0 <- NULL
      } else {
        xdat <- subset(dat, country %in% region)
        # subnational data needs overview graphs at national level
        xdat0 <- subset(dat, country %in% region)
        yy0 <- lapply(vars, function(u)get_difference(dat = xdat, var_name = u, cnt = value_type,
                                                      m0 = m0, mf = mf,yr0 = yr0, yrf = 2020))
        total0 <- sapply(yy0, function(u) u[["tot"]], simplify = FALSE) %>%
          do.call(rbind, .)
        bymonth0 <- sapply(yy0, function(u) u[["bymonth"]], simplify = FALSE) %>%
          do.call(rbind, .)

      }

      yy <- lapply(vars, function(u) get_difference(dat = xdat, var_name = u, cnt = value_type,
                         m0 = m0, mf = mf,yr0 = yr0, yrf = 2020))
      total <- sapply(yy, function(u) u[["tot"]], simplify = FALSE) %>%
          do.call(rbind, .)
      bymonth <- sapply(yy, function(u) u[["bymonth"]], simplify = FALSE) %>%
          do.call(rbind, .)

      return(list(tot = total, bymonth = bymonth, tot0 = total0, bymonth0 = bymonth0))
}




get_shpdat_snap <- function(dat, var_name, shp, yr0, m0, mf, covid_name){
  
  u <- dat %>%
    filter((str_detect(variable, var_name)) & (str_detect(variable, "reports", negate = TRUE)))
  
  udelta <- get_difference(u, var_name, m0, mf, yr0, yrf = 2020, as_total = TRUE)
  
  # Covid data
  ucov <- dat %>%
    filter((variable %in% covid_name) & (month >= which(month.abb %in% m0)) &
             (month <= which(month.abb %in% mf)) & (year == 2020)) 
  
  if ("admin_level_1" %in% colnames(u)) {
    sout <- subset(shp, (country %in% unique(dat$country)) & 
                     (admin_level_1 %in% unique(dat$admin_level_1))) %>%
      merge(., udelta, by = c("country", "admin_level_1"))
    ucov <- ucov %>% 
      group_by(country, admin_level_1) %>%
      summarise(covid_cumu = sum(value, na.rm = TRUE)) %>%
      ungroup()
    if (nrow(ucov) > 1) {
      sout <-  merge(sout, ucov, by = c("country", "admin_level_1")) 
    } else {
      sout$covid_cumu <- NA
    }
    
    sout <- sout %>% mutate(label_region = admin_level_1)
    
  } else {
    sout <- subset(shp, country %in% unique(dat$country)) %>%
      merge(., udelta, by = "country")
    ucov <- ucov %>% 
      group_by(country) %>%
      summarise(covid_cumu = sum(value, na.rm = TRUE))
    sout <-  merge(sout, ucov, by = c("country")) %>%
      mutate(label_region = country)
  }
  
  if(unique(dat$count_type %in% "value_rate")){
    # min_covid <- if_else(min(sout$covid_cumu, na.rm = TRUE) != 0, 1, 0)
    # min_covid <- max(c(0, sout$covid_cumu, na.m = TRUE))
    min_covid <- 1
    max_covid <- 10
    new_value <- rescale(sout$covid_cumu, to = c(min_covid, max_covid))
    new_value[(sout$covid_cumu == 0) | (is.na(sout$covid_cumu))] <- NA
    if (str_detect(var_name, "deaths")) {
      csuff2 <- "per 1M people"
    } else if (str_detect(var_name, "anc|tpr", negate = TRUE)) {
      csuff2 <- "per 1K people"
    } else {
      csuff2 <- NULL
    }
  } else {
    new_value <- log(sout$covid_cumu)
    new_value[is.infinite(new_value)] <- NA
    csuff2 <- NULL
  }
  sout$covid_cumu_scaled <- new_value
  
  if( covid_name %in% "covid_deaths") {
    csuff1 <- "deaths"
  } else {
    csuff1 <- "cases"
  }
  
  sout <- sout %>%
    mutate(delta = if_else(is.infinite(delta), NA_real_, delta),
           txt_poly = paste(label_region, ": ", round(delta, digits = 2), "% change", sep = ""),
           txt_cov = paste(label_region, ": ", round(covid_cumu, digits = 2), " COVID-19 ", csuff1, " ", csuff2, sep = ""))
  
  
  return(sout)
  
}



get_snapshot_shp <- function(dat, var_name, shp){
  
  mtot <- subset(dat, variable %in% var_name)
  if ("admin_level_1" %in% colnames(dat)){
    sout <- subset(shp, (country %in% unique(dat$country)) & 
                     (admin_level_1 %in% unique(dat$admin_level_1))) %>%
      merge(., dat, by = c("country", "admin_level_1"))
  } else {
    sout <- subset(shp, country %in% unique(dat$country)) %>%
      merge(., dat, by = "country")
  }
  
  if (length(unique(dat$country)) > 20) {
    x_ctr <- 19; y_ctr <- 4; z_lev <- 4
  } else {
    sbbox <- st_bbox(sout)
    x_ctr <- as.vector((sbbox$xmax - sbbox$xmin)/2)
    y_ctr <- as.vector((sbbox$ymax - sbbox$ymin)/2)
  }
  
  bins <- c(min(dat$delta, na.rm = TRUE),
            seq(-75, 75, by = 25), 
            max(dat$delta, na.rm = TRUE))
  
  return(list(s = sout, sctr = c(x_ctr, y_ctr), bins = bins, myrank = srank))
  
}

get_latlon0 <- function(region, shpdat){
  
  if (region %in% "Southeast Asia") {
    ll0 <- c(103, 14, 4)
  } else if (region %in% str_subset(region_list, "Southeast Asia", negate = TRUE)) {
    ll0 <- c(19, 4, 3)
  } else {
    stmp <- subset(s0, country %in% region)
    ll0 <- c(stmp$lon_ctr, stmp$lat_ctr, 6)
  }
  
  return(ll0)
  
}


get_rrplot_snap <- function(dat, m0, mf, yr0, yrf = 2020, region, value_type) {
  
  m0 <- which(month.abb %in% m0)
  mf <- which(month.abb %in% mf)
  
  
  if (yr0 %in% "Historical Mean") {
    tt0 <- NULL
  } else {
    ttf <- seq(as.Date(paste(2020, "-", m0, "-01", sep = "")), 
               as.Date(paste(2020, "-", mf, "-01", sep = "")), by = "1 month")
  }
  
  tt0 <- seq(as.Date(paste(yr0, "-", m0, "-01", sep = "")), 
             as.Date(paste(yr0, "-", mf, "-01", sep = "")), by = "1 month")
  
  
  xrr1 <- filter_region_snap(xctry, region) %>%
    filter(count_type %in% value_type) %>%
    filter(str_detect(variable, "reports")) %>%
    filter( date %in% c(tt0, ttf) ) %>%
    select(year, variable, info) %>%
    mutate(info = as.numeric(as.character(info))) %>%
    group_by(variable, year) %>%
    summarise(rr_average = mean(info, na.rm = TRUE)) %>%
    mutate(variable = factor(variable, levels = rev(c("reports_rate__allcause_cases", "reports_rate__confirmed_cases", "reports_rate__anc1_visit")),
                             labels = rev(c("All Cause Consultations", "Confirmed Cases", "ANC (1st) Visit"))))
  
  
  fill_values <- setNames(c("#9e9ac8", "#efedf5"), c("2020", as.character(yr0)))
  clr_values <- setNames(c("#9e9ac8", "#efedf5"), c("2020", as.character(yr0)))
  
  
  ggout <- ggplot(xrr1, aes(x = rr_average, y = variable, 
                                     group = as.factor(year), fill = as.factor(year), colour = as.factor(year))) + 
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_text(aes(label = label_percent(accuracy = 0.1)(rr_average), group = as.factor(year)), 
              position = position_dodge(width = 1), hjust = 1.2, color = "black") + 
    geom_vline(xintercept = 1, linetype = "dotted") +
    scale_x_continuous(labels = percent) +
    theme_few(12) + xlab("Reporting Rate") + ylab("") +
    scale_fill_manual(values = fill_values) + scale_colour_manual(values = clr_values) +
    theme(legend.title = element_blank(), legend.position = "top")
  
  (ggout)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# get latest shapefile from platform
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Libraries used
library("civis")
library("sf")

# Download most recent shapefiles based on either admin level or file id
# both arguments cannont be used simultaniously 

# examples on how to call by admin or file_id
#sf<-get_shapefile(admin_level = 0)
#sf<-get_shapefile(file_id = 108057147)

get_shapefile <- function(admin_level, file_id){
  # stop if no input aruments are provided
  if (missing(admin_level) & missing(file_id) )
    stop("Please provide either an admin aggregation level (admin_level) or a file ID (file_id).")
  if (!missing(admin_level) & !missing(file_id) )
    stop("Please provide only one input arguement (admin_level) or (file_id).")
  
  # get file id or file name based on user input
  if (!missing(admin_level) & missing(file_id)){
    file_name <- paste0("combo_admin",admin_level,"_simple_aligned")
    # obtain a list of the files in civis PMI shapefile project using the civis function
    objects <- projects_get(110044)
    # flatten nested list into data frame for searching
    obj_files_list <- objects$files
    obj_files_df <- as.data.frame(matrix(unlist(obj_files_list), ncol=5,byrow = TRUE),stringsAsFactors = FALSE)[,c(1,4)]
    colnames(obj_files_df) <- c("file_id","file_name")
    # get file id
    idx <- which(obj_files_df$file_name ==file_name)
    file_id <- as.numeric(obj_files_df$file_id[idx])
  } else if (missing(admin_level) & !missing(file_id)){
    file_name<-files_get(file_id)$name 
  } 
  # create temp files and directories
  temp_dir <- tempdir()
  temp_file <- tempfile()
  temp_dir_sf_name <- paste0(temp_dir,"/",file_name,"/")  
  
  # extract country name from report title
  download_civis(file_id, file = temp_file, overwrite = TRUE) # download file
  unzip(temp_file, exdir = temp_dir)
  return(sf::st_read(dsn = temp_dir_sf_name, layer = file_name)) # load into R
  
}



# Defining the new Widget. 
customRadioGroupButtons <- function (inputId, label = NULL, choices, selected = NULL, status = "default", size = "normal", direction = "horizontal", justified = FALSE, individual = FALSE, checkIcon = list(), class=NULL) {
  choices <- shinyWidgets:::choicesWithNames(choices)
  if (!is.null(selected) && length(selected) > 1) 
    stop("selected must be length 1")
  if (is.null(selected)) 
    selected <- choices[1]
  size <- match.arg(arg = size, choices = c("xs", "sm", "normal", 
                                            "lg"))
  direction <- match.arg(arg = direction, choices = c("horizontal", 
                                                      "vertical"))
  status <- match.arg(arg = status, choices = c("default", 
                                                "primary", "success", "info", "warning", "danger"))
  divClass <- if (individual) 
    ""
  else "btn-group"
  if (!individual & direction == "vertical") {
    divClass <- paste0(divClass, "-vertical")
  }
  if (justified) {
    divClass <- paste(divClass, "btn-group-justified")
  }
  if (size != "normal") {
    divClass <- paste0(divClass, " btn-group-", size)
  }
  
  # Below here, the paste call is the only difference to the original function.
  radioGroupButtonsTag <- tagList(tags$div(id = inputId, class = paste("radioGroupButtons", class), 
                                           if (!is.null(label)) 
                                             tags$label(class = "control-label", `for` = inputId, label),
                                           if (!is.null(label)) 
                                             br(), style = "margin-top: 3px; margin-bottom: 3px; ", style = if (justified) "width: 100%;", 
                                           tags$div(class = divClass, role = "group", 
                                                    `aria-label` = "...", `data-toggle` = "buttons", 
                                                    class = "btn-group-container-sw", shinyWidgets:::generateRGB(inputId, choices, selected, status, size, checkIcon))))
  shinyWidgets:::attachShinyWidgetsDep(radioGroupButtonsTag)
}



addLegendCustom <<- function(map, colors, labels, sizes, shapes, 
                             borders, opacity = 0.5, ...){
  
  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("circle", "50%", shapes)
    shapes <- gsub("square", "0%", shapes)
    paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:1px solid ", 
           borders, "; border-radius:", shapes)
  }
  make_labels <- function(sizes, labels) {
    paste0("<div style='display: inline-block;height: ", 
           sizes, "px;margin-top: 0;line-height: ", 
           sizes, "px;'>", labels, "</div>")
  }
  
  legend_colors <- make_shapes(colors, sizes, borders, shapes)
  legend_labels <- make_labels(sizes, labels)
  
  return(addLegend(map, colors = legend_colors, labels = legend_labels, 
                   opacity = opacity, ...))
}

