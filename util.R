
get_delta <- function(yr0, m0, mf, dat){
  
  m0 <- which(month.abb %in% m0)
  mf <- which(month.abb %in% mf)
  
  if(yr0 %in% "Long-term mean"){
    yr0 <- NULL
    ltm_data <- TRUE
  } else {
    ltm_data <- FALSE
  }
  
  
  if(ltm_data){
    
    dat_delta <- dat %>% 
      filter( (year %in% c(2020, yr0) & (month %in% c(m0, mf)) &
                 !(colourgroup %in% c("covid")) ) ) %>%
      mutate(year = ifelse(linegroup %in% "Long-term mean", 2019, year),
             variable = gsub("_ltm", "", variable))
    
  } else {
    
    dat_delta <- dat %>% 
      filter( (year %in% c(2020, yr0) & (month %in% c(m0, mf)) &
                 !(colourgroup %in% c("covid")) & !(linegroup %in% "Long-term mean") ) )
  }
  

  dat_tmp <- dat_delta %>%
    group_by(country, year, boxgroup, variable) %>%
    summarise(value = if_else(all(is.na(value)), NA_real_, sum(value, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(year = if_else(as.character(year) %in% c("Long-term mean", "2020"), as.character(year), "0")) %>%
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
  
  dfrbn1 <- subset(dat, (date >= as.Date(paste(2020, "-", m0, "-01", sep = ""))) & 
                     (date <= as.Date(paste(2020, "-", mf, "-01", sep = ""))) ) %>%
    filter((stringr::str_detect(variable, "ltm", negate = TRUE)))
  
  if (nrow(dfrbn1) == 0){
    dfrbn <- dat %>%
      mutate(minval = NA, maxval = NA)
  } else {
    if(yf == "Long-term mean"){
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


