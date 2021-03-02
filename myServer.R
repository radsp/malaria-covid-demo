server <- function(input, output, session) {
  
    # Prevent "greying out" when running in Civis Platform
    session$allowReconnect("force")
  
  #------------------------------------------------------------------------------------------------
  # PERMISSION SETS
  #------------------------------------------------------------------------------------------------
  
  # get user ID locally or on platform:
  query <- parseQueryString(session$request$QUERY_STRING)
  # userid <- 6181
  userid <- query$civisuserid
  if (is.null(userid) && Sys.getenv("CIVIS_SERVICE_ID") == "") {
    message("Running locally. Getting user from API")
    userid <- civis::users_list_me()$id
  }

  # get resources from platform permission set
  # user filtered countries should replace any list of options for the malaria app that contains malaria data
  user_filtered_countries <- get_countries(user=userid,
                                           permissionset=permission_set_id, # defined in global
                                           all_resources = all_resources)
  
  # filtering based on reporting table
  user_filtered_countries <- user_filtered_countries[which(!is.na(user_filtered_countries))]
  
  # adding region filtering
  
  regions <- list(ctry_bylat, ctry_nhem, ctry_shem, ctry_wafr, ctry_eafr, ctry_csafr, ctry_sea)
  regions_nofilter <- list(ctry_bylat, ctry_nhem, ctry_shem, ctry_wafr, ctry_eafr, ctry_csafr, ctry_sea)
  
  names(regions) <- c("All Countries", "Northern Hemisphere",
                      "Southern Hemisphere", "Western Africa",
                      "Eastern Africa", "Central/Southern Africa",
                      "Southeast Asia")
  names(regions_nofilter) <- c("All Countries", "Northern Hemisphere",
                      "Southern Hemisphere", "Western Africa",
                      "Eastern Africa", "Central/Southern Africa",
                      "Southeast Asia")
  
  
  region_list_all <- c("All Countries", "Northern Hemisphere", "Southern Hemisphere", 
                       "Western Africa", "Eastern Africa", "Central/Southern Africa", 
                       "Southeast Asia")
  
  
  for (i in 1:7){
    regions[[i]] <- regions[[i]][regions[[i]] %in% user_filtered_countries] # filtering regions based on user-filtered countries
  }
  
  
  ctry_bylat <- regions[[1]]
  ctry_nhem <- regions[[2]]
  ctry_shem <- regions[[3]]
  ctry_wafr <- regions[[4]]
  ctry_eafr <- regions[[5]]
  ctry_csafr <- regions[[6]]
  ctry_sea <- regions[[7]]
  
  regions_present <- regions[lapply(regions,length)>0] # removing regions w/ no entries
  
  user_filtered_regions <- names(regions_present)
  region_list <- user_filtered_regions # test 

  # maybe these need to be added to an observe event ... ?   
if(length(user_filtered_countries) > 0){
  updateSelectInput(session, "in_region_snapshot", selected = "All Countries",
                    choices =   c(user_filtered_regions, "----------------------",
                                  as.character(user_filtered_countries)[order(unique(user_filtered_countries))]))
}
else{
  updateSelectInput(session, "in_region_snapshot", selected = "All Countries",
                    choices = c(user_filtered_regions, "----------------------",
                                as.character(unique(reporting_tbl$country)[order(unique(reporting_tbl$country))])))
}

  # INFO TAB #######################################################################

  
    
    observeEvent(input$go2snap, {
      updateNavbarPage(session, "top_page", selected = "Snapshot")
    })
    
    observeEvent(input$go2pcnt, {
      updateNavbarPage(session, "top_page", selected = "Changes In Indicator")
    })
    
    observeEvent(input$go2ts, {
      updateNavbarPage(session, "top_page", selected = "Historical time series")
    })
    
    observeEvent(input$go2rf, {
      updateNavbarPage(session, "top_page", selected = "Rainfall")
    })
    
    observeEvent(input$go2custom, {
      updateNavbarPage(session, "top_page", selected = "In-Country Calculator")
    })
    
    observeEvent(input$go2surgo, {
      updateNavbarPage(session, "top_page", selected = "Other Sources")
    })
    
  
  
  
  # MALARIA TAB ####################################################################

  
  screen_dim <- reactive({
    return(as.numeric(input$dimension))
  })
  
  
  ## MALARIA SNAPSHOT TAB PANEL ====================================================
  
  ## Malaria snapshot: dynamic main panel UI  --------------------------------------
  
  snapshot_region <- reactive({
    return(input$in_region_snapshot)
  })
  
    
  get_snapshot_panel <- eventReactive(input$goplot_snap, {
    myregion <- snapshot_region()
    
    if (snapshot_region() %in% region_list) {
      
      ### multiple countries view --------------------------------------------------
      
      fluidPage(style = "margin-left:20px;",
                
                fluidRow(column(6, # offset = 0.75,
                                HTML("Where have malaria-related indicators been increasing or decreasing as compared to the selected baseline year?
                             <i>(select indicator to map from the sidebar panel)</i><br/>&nbsp")),
                         column(6, style = 'padding-left:30px;',
                                HTML("Has mobility increased or decreased (represented through Google mobility data)? How does mobility compared to the selected indicator")
                         )
                ),
                fluidRow(column(6, htmlOutput("out_txt_maptitle_snapshot")),
                         column(6, style = 'padding-left:30px;', htmlOutput("out_txt_mobilitymaptitle_snapshot"))),
                fluidRow(
                  column(6, style = 'padding-right:20px;', leafletOutput("out_map_snapshot")),
                  column(6, style = 'padding-left:30px;', leafletOutput("out_mobilitymap_snapshot"))
                ),
                br(),
                br(),
                br(),
                fluidRow(column(12, style = "padding-bottom:2px", 
                                HTML("Which countries observed the largest decrease/increase in each malaria indicators?"))),
                fluidRow(column(12, style = "padding-bottom:5px", htmlOutput("out_title_malranking_snapshot"))),
                fluidRow(column(12, plotOutput(outputId = "out_malranking_snapshot")))
      )
      
    } else {
      
      ### one country view ---------------------------------------------------------
      ### Top part shows country-level bar chart, bottom part is subnational-level map + charts
      
      fluidPage(style = "margin-left:20px",
                
                ### National-level data --------------------------
                fluidRow(column(12, htmlOutput("out_txt_adm0title_snapshot"))),
                ### Key analytical questions ----------------------
                fluidRow(column(4, style = 'padding-left:10px; padding-right:20px; padding-top:1px; padding-bottom:5px;',
                                HTML("Have malaria-related indicators (at national level) decreased/increased as compared to the selected baseline year?")),
                         column(4, style = 'padding-left:30px; padding-right:10px; padding-top:1px; padding-bottom:5px;',
                                offset = 0, HTML("Have data been reported from all facilities in order to have a complete picture of the indicator?")),
                         column(4, offset = 0, style = 'padding-left:40px; padding-right:0px; padding-top:1px; padding-bottom:5px;',
                                HTML("Is mobility lower than average such that it potentially affect the malaria-related indicators?"))),
                #### Plot title --------------------------------------
                fluidRow(column(4, style = 'padding-left:10px; padding-right:20px; padding-top:1px; padding-bottom:5px;',
                                htmlOutput("out_plottitle_adm0indicator_snapshot")),
                         column(4, style = 'padding-left:30px; padding-right:10px; padding-top:1px; padding-bottom:5px;',
                                offset = 0, htmlOutput("out_plottitle_adm0reporting_snapshot")),
                         column(4, offset = 0, style = 'padding-left:40px; padding-right:0px; padding-top:1px; padding-bottom:5px;',
                                htmlOutput("out_plottitle_adm0mobility_snapshot"))),
                #### Plot --------------------------------------------
                fluidRow(style = 'height:250px', 
                         column(4, offset = 0, style = 'padding-left:0px; padding-right:35px; padding-top:1px; padding-bottom:5px;', 
                                plotOutput("out_adm0indicator_snapshot", height = 250)),
                         column(4, offset = 0, style = 'padding-left:30px; padding-right:60px; padding-top:1px; padding-bottom:5px',
                                plotOutput("out_adm0reporting_snapshot", height = 250), align = "left"),
                         column(4, offset = 0, style = 'padding-left:40px; padding-right:0px; padding-top:1px; padding-bottom:5px;',
                                plotOutput("out_adm0mobility_snapshot", height = 250))),
                br(), 
                hr(),
                
                ### Subnational-level data --------------------------
                fluidRow(column(12, htmlOutput("out_txt_adm1title_snapshot"))),
                fluidRow(column(6, style = "padding-bottom:5px",
                                HTML("Where have malaria-related indicators been increasing or decreasing as compared to the selected baseline year?<br/>
                             <i>(select indicator to map from the sidebar panel)</i>")),
                         column(5, style = "padding-bottom:5px; padding-left:10px;",
                                HTML("Has mobility increased or decreased (represented through Google mobility data)? How does mobility compared to the selected indicator"))),
                fluidRow(column(6, htmlOutput("out_txt_maptitle_snapshot")),
                         column(6, style = 'padding-left:30px;', htmlOutput("out_txt_mobilitymaptitle_snapshot"))),
                fluidRow(column(6, style = 'padding-right:20px;', leafletOutput("out_map_snapshot")),
                         column(6, style = 'padding-left:30px;',leafletOutput("out_mobilitymap_snapshot"))),
                br(),
                br(),
                fluidRow(column(12, style = "padding-bottom:2px", 
                                HTML("<br/>Which region observed the largest decrease/increase in each malaria indicators?"))),
                fluidRow(column(12, style = "padding-bottom:5px", htmlOutput("out_title_malranking_snapshot"))),
                fluidRow(column(12, plotOutput(outputId = "out_adm1malranking_snapshot"))))
    }
    
  }, ignoreNULL = FALSE)  
    
  
  
  output$out_snapshot_panel <- renderUI({ get_snapshot_panel() })
  
  
  output$add_datepicker_pand_snap <- renderUI({
    
    cal_tt <- filter_region_snap(xctry, input$in_region_snapshot, region_list, regions) %>%
      pull(date) %>% range(., na.rm = TRUE) 
    
    airDatepickerInput(inputId = "ttpand_snap", label = "Select pandemic date range to analyze",
                       range = TRUE, view = "months", clearButton = TRUE, minView = "months",
                       dateFormat = "M yyyy", monthsField =  "monthsShort", value = drangeF_snap + 1,
                       minDate = as.Date("2017-01-01"), maxDate = as.Date("2021-02-01"))
  })
  
  output$add_datepicker_comp_snap <- renderUI({
    if (input$in_ltm_or_yr_snap %in% "Historical Mean") {
      NULL
    } else {
      cal_tt <- filter_region_snap(xctry, input$in_region_snapshot, region_list, regions) %>%
        pull(date) %>% range(., na.rm = TRUE) 
      airDatepickerInput(inputId = "ttcomp_snap", label = NULL, range = TRUE, view = "months", minView = "months", clearButton = TRUE,
                         dateFormat = "M yyyy", monthsField = "monthsShort", value = drange0_snap + 1,
                         minDate = as.Date("2017-01-01"), maxDate = as.Date("2021-02-01"))
    }
  })
  



  ## Malaria snapshot: text and titles output --------------------------------------
  
  get_text_snap <- eventReactive(input$goplot_snap, {
    map_indi <- paste("<h5><b>Percentage changes for ", indicator_labels[[input$in_indicator_snapshot]],
                                   " in ", paste(format(input$ttpand_snap, "%b %Y"), collapse = " - "),
                                   " (from ", paste(format(input$ttcomp_snap, "%b %Y"), collapse = " - "), ")</b></h5>",sep = "")
    bar_multictry <- paste("<h5><b>Percentage changes for all indicators ",
                           " in ", paste(format(input$ttpand_snap, "%b %Y"), collapse = " - "),
                           " (from ", paste(format(input$ttcomp_snap, "%b %Y"), collapse = " - "), ")</b></h5>",sep = "")
    map_mob <- paste("<h5><b>Mean changes across Google's mobility categories ",
                     " in ", paste(format(input$ttpand_snap, "%b %Y"), collapse = " - "),
                     " (from baseline: Jan 3 - Feb 6, 2020)</b></h5>",sep = "")
    bar_indi_1ctry_natl <- paste("<h5><b>Percentage changes in all indicators in ", paste(format(input$ttpand_snap, "%b %Y"), collapse = " - "),
                                 " (from ", paste(format(input$ttcomp_snap, "%b %Y"), collapse = " - "), ")</b></h5>", sep = "")
    
    rr_1ctry_natl <- paste("<h5><b>Average monthly reporting rate in ", paste(format(input$ttpand_snap, "%b %Y"), collapse = " - "),
                           " and ", paste(format(input$ttcomp_snap, "%b %Y"), collapse = " - "), "</b></h5>", sep = "")
    
    natl_1ctry <- paste("<h4>", input$in_region_snapshot, " - National Level</h4><br/>", sep = "")
    
    subnatl_1ctry <- paste("<h4>", input$in_region_snapshot, " - Subnational Level</h4><br/>", sep = "")
    
    return(list(map_indi = map_indi, bar_multictry = bar_multictry, map_mob = map_mob, bar_indi_1ctry_natl = bar_indi_1ctry_natl,
                rr_1ctry_natl = rr_1ctry_natl, natl_1ctry = natl_1ctry, subnatl_1ctry = subnatl_1ctry))
  }, ignoreNULL = FALSE)
  
  # Indicator map title
  output$out_txt_maptitle_snapshot <- renderUI({
    txt <- get_text_snap()
    HTML(txt$map_indi)
  })
  
  # Indicator bar chart title 
  output$out_title_malranking_snapshot <- renderUI({
    txt <- get_text_snap()
    HTML(txt$bar_multictry)
  })
  
  # Mobility map title 
  output$out_txt_mobilitymaptitle_snapshot <- renderUI({
    txt <- get_text_snap()
    HTML(txt$map_mob)
  })
  
  ## One country national level title 
  output$out_txt_adm0title_snapshot <- renderUI({
    txt <- get_text_snap()
    HTML(txt$natl_1ctry)
  })
  
  ## One country subnational level title 
  output$out_txt_adm1title_snapshot <- renderUI({
    txt <- get_text_snap()
    HTML(txt$subnatl_1ctry)
  })
  
  ## One country title for national-level indicator ranking plot  
  output$out_plottitle_adm0indicator_snapshot <- renderUI({
    txt <- get_text_snap()
    HTML(txt$bar_indi_1ctry_natl)
  })
  
  ## One country title for national-level reporting rate plot 
  output$out_plottitle_adm0reporting_snapshot <- renderUI({
    txt <- get_text_snap()
    HTML(txt$rr_1ctry_natl)
  })
  
  ## One country title for national-level mobility bar plot 
  output$out_plottitle_adm0mobility_snapshot <- renderUI({
    txt <- get_text_snap()
    HTML(txt$map_mob)
  })
  
  
  ## Malaria snapshot: graphs and maps outputs -------------------------------------------------------
  
  
  ### Malaria snapshot: map for indicator percentage changes -----------------------------------------
  
  
  # Reactive function to obtain shapefile based on user-selected inputs
  

  get_mymap <- eventReactive(input$goplot_snap, {
    
    if ( (input$in_ltm_or_yr_snap %in% "Select a date range") & (!check_nmonth(input$ttpand_snap, input$ttcomp_snap)) ) {
      
      mymap <- NULL
      
    } else {
      
      if (input$in_region_snapshot %in% region_list) {
        mapdat <- xctry; sdat <- s0 
      } else {
        mapdat <- xprov; sdat <- s1
      }
      
      # use covid deaths if indicator is malaria deaths and set legend appropriately
      if ("All Cause Consultations" %in% "malaria_deaths") {
        covid_str <- "covid_deaths"
        covid_legend <- "COVID-19 Cumulative Deaths"
      } else {
        covid_str <- "covid_cases"
        covid_legend <- "COVID-19 Cumulative Cases"
      }
      
      xmap <- filter_region_snap(mapdat, input$in_region_snapshot, region_list, regions) %>%
        filter(count_type %in% input$in_yaxs_snap) %>%
        get_shpdat_snap(., input$in_indicator_snapshot, sdat, input$ttcomp_snap, input$ttpand_snap, covid_str)
      
      minval <- min(-100, min(xmap$delta, na.rm = TRUE), na.rm = TRUE)
      maxval <- max(100, max(xmap$delta, na.rm = TRUE), na.rm = TRUE)
      
      map_pal <- colorBin(delta_map_clr, domain = xmap$delta,
                          bins = c(minval, -50, -25, -10, 0, 10, 25, 50, maxval))
      
      pts_covid <- subset(xmap, !(is.na(covid_cumu_scaled)) | (covid_cumu_scaled > 0))
      
      view0 <- get_latlon0(input$in_region_snapshot, xmap)
      
      # view0 <- get_latlon0("All Countries", xmap)
      
      mymap <- leaflet(xmap) %>%
        setView(view0[1], view0[2], view0[3]) %>%
        addProviderTiles("CartoDB.Positron")  %>%
        addPolygons(color = "#444444", weight = 1, fillOpacity = 0.8,
                    fillColor = ~ map_pal(delta),
                    popup = ~htmlEscape(txt_poly)) %>% 
        addCircleMarkers(lng = pts_covid$lon_ctr, lat =  pts_covid$lat_ctr,
                         radius = ~ (covid_cumu_scaled),
                         # color = "#ffca05", fill = "#ffca05",
                         color = "#a65628", fill = "#a65628",
                         fillOpacity = 0.8,
                         popup = htmlEscape(pts_covid$txt_cov)) %>%
        addLegendCustom("bottomleft", colors = "#a65628",
                        sizes = 15, labels = covid_legend,
                        shapes = "circle", borders = "#a65628",
                        opacity = 0.96) %>%
        addLegend("bottomleft", pal = map_pal, values = ~ delta,
                  opacity = 0.8, labFormat = labelFormat(between = " to "),
                  title = "% Change")
      
      if (!("delta" %in% colnames(xmap))){
        xmap <- xmap %>% addLabelOnlyMarkers(lng = view0[1], lat = view0[2], label = "Data is unavailable")
      }
      
    }
    
    return(mymap)
  }, ignoreNULL = F)
  
  

  
  output$out_map_snapshot <- renderLeaflet({ 
    mymap <- get_mymap()
    if(is.null(mymap)) {
      shinyalert(text = "Oops... the number of months in selected pandemic period is not the same as in the baseline comparison period. Please change accordingly.", type = "warning")
    } 
    return(mymap) })
  
  
  ### Malaria snapshot: map for mobility -------------------------------------------
  
  get_mobilitymap <- eventReactive(input$goplot_snap, {
    
    ttp <- as.Date(paste(format(input$ttpand_snap, "%Y"), "-", format(input$ttpand_snap, "%m"), "-01", sep = ""))
    ttcov <- seq(ttp[1], ttp[2], by = "1 month")
    
    # data frame and shapefile selection (whether country-level or subnational-level)
    if (input$in_region_snapshot %in% region_list) {
      mobdat <- xmob %>% filter(admin_level_1 %in% "") 
      sdat <- s0; covdat <- xctry 
    } else {
      mobdat <- xmob %>% filter(!(admin_level_1 %in% ""))
      sdat <- s1; covdat <- xprov
    }
    
    # use covid deaths if indicator is malaria deaths and set legend appropriately
    if (input$in_indicator_snapshot %in% "malaria_deaths") {
      covid_str <- "covid_deaths"
      covid_legend <- "COVID-19 Cumulative Deaths"
    } else {
      covid_str <- "covid_cases"
      covid_legend <- "COVID-19 Cumulative Cases"
    }
    
    mobdat <- filter_region_snap(mobdat, input$in_region_snapshot, region_list_all, regions_nofilter) %>%
      ungroup() %>%
      filter(date %in% ttcov) %>%
      select(-c("date", "month", "year")) %>%
      group_by(country, admin_level_1) %>%
      summarise_all(mean, na.rm = TRUE) %>%
      ungroup() %>% rowwise() %>%
      mutate(allbuthome = mean(c(grocery_and_pharmacy, retail_and_recreation, transit_stations, workplaces, parks)))
    
    covdat <- filter_region_snap(covdat, input$in_region_snapshot, region_list_all, regions_nofilter) %>%
      ungroup() %>%
      filter((variable %in% covid_str) & (date %in% ttcov)) %>%
      filter(count_type %in% input$in_yaxs_snap) %>%
      select(-c(contains("group"), "count_type", "info", "info_txt")) %>%
      distinct()
    
    if ("admin_level_1" %in% colnames(covdat)) {
      covdat <- covdat %>% 
        group_by(country, admin_level_1) %>%
        summarise(covid_cumu = sum(value, na.rm = TRUE))
      cmdat <- merge(mobdat, covdat, by = c("country", "admin_level_1"), all = TRUE) %>%
        merge(sdat, ., by = c("country", "admin_level_1")) %>%
        mutate(label_region = admin_level_1)
    } else {
      covdat <- covdat %>%
        group_by(country) %>%
        summarise(covid_cumu = sum(value, na.rm = TRUE))
      cmdat <- merge(mobdat, covdat, by = c("country"), all = TRUE) %>%
        merge(sdat, ., by = c("country")) %>%
        mutate(label_region = country)
    }
    
    if (input$in_yaxs_snap %in% "value_rate") {
      min_covid <- 1; max_covid <- 10
      new_value <- rescale(cmdat$covid_cumu, to = c(min_covid, max_covid))
      new_value[(cmdat$covid_cumu == 0) | (is.na(cmdat$covid_cumu))] <- NA
      if (str_detect(input$in_indicator_snapshot, "deaths")) {
        csuff2 <- "per 1M people"
      } else if (str_detect(input$in_indicator_snapshot, "anc|tpr", negate = TRUE)) {
        csuff2 <- "per 1,000 pop."
      } else {
        csuff2 <- NULL
      }
    } else {
      new_value <- log(cmdat$covid_cumu)
      new_value[is.infinite(new_value)] <- NA
      csuff2 <- NULL
    }
    
    cmdat$covid_cumu_scaled <- new_value
    
    if (covid_str %in% "covid_deaths") {
      csuff1 <- "deaths"
    } else {
      csuff1 <- "cases"
    }
      
    cmdat <- cmdat %>%
      mutate(txt_poly = paste(label_region, ": ", round(allbuthome, digits = 2), "% change", sep = ""),
             txt_cov = paste(label_region, ": ", round(covid_cumu, digits = 2), " COVID-19 ", csuff1, " ", csuff2, sep = ""))
    
    minval <- min(-100, min(cmdat$allbuthome, na.rm = TRUE), na.rm = TRUE)
    maxval <- max(100, max(cmdat$allbuthome, na.rm = TRUE), na.rm = TRUE)
    
    map_pal <- colorBin(delta_map_clr, domain = cmdat$allbuthome,
                        bins = c(minval, -50, -25, -10, 0, 10, 25, 50, maxval))
    
    pts_covid <- subset(cmdat, !(is.na(covid_cumu_scaled)) | (covid_cumu_scaled > 0))
    
    view0 <- get_latlon0(input$in_region_snapshot, cmdat)
    
    cmmap <- leaflet(cmdat) %>%
      setView(view0[1], view0[2], view0[3]) %>%
      addProviderTiles("CartoDB.Positron")  %>%
      addPolygons(color = "#444444", weight = 1, fillOpacity = 0.8,
                  fillColor = ~ map_pal(allbuthome),
                  popup = ~htmlEscape(txt_poly)) %>% 
      addCircleMarkers(lng = pts_covid$lon_ctr, lat =  pts_covid$lat_ctr,
                       radius = ~ (covid_cumu_scaled),
                       # color = "#ffca05", fill = "#ffca05",
                       color = "#a65628", fill = "#a65628",
                       fillOpacity = 0.8,
                       popup = htmlEscape(pts_covid$txt_cov)) %>%
      addLegendCustom("bottomleft", colors = "#a65628",
                      sizes = 15, labels = covid_legend,
                      shapes = "circle", borders = "#a65628",
                      opacity = 0.96) %>%
      addLegend("bottomleft", pal = map_pal, values = ~ allbuthome,
                opacity = 0.8, labFormat = labelFormat(between = " to "),
                title = "Mean % change")
    
    return(cmmap)
  }, ignoreNULL = FALSE)

  
  output$out_mobilitymap_snapshot <- renderLeaflet(get_mobilitymap())
  
  

  ### Multi-country indicator ranking barplot ----------------------------------------
  
  get_barplot_multictry <- eventReactive(input$goplot_snap, {
    
    if ( (input$in_ltm_or_yr_snap %in% "Select a date range") & (!check_nmonth(input$ttpand_snap, input$ttcomp_snap)) ) {
      
      ggbar0 <- NULL
      
    } else {
      
      xbar0 <- filter_region_snap(xctry, input$in_region_snapshot, region_list, regions) %>%
        filter(count_type %in% input$in_yaxs_snap)
      
      xbar <- NULL
      for (i in mal_vars) {
        xi <-  get_shpdat_snap(xbar0, i, s0, input$ttcomp_snap, input$ttpand_snap, "covid_cases") %>%
          as.data.frame() %>%
          mutate(delta_sign = if_else(delta > 0, "positive", "negative"),
                 delta_order = order(delta)) %>%
          filter(!(is.na(delta)))
        xbar <- rbind(xbar, xi)
        
      }
      
      xbar <- xbar %>%
        mutate(variable = factor(variable, levels = mal_vars,
                                 labels = as.vector(indicator_labels))) %>%
        arrange(variable) %>%
        mutate(delta_order = sprintf("%03i", frank(., variable, -delta, ties.method = "first")))
      
      rank_order <- setNames(as.character(xbar$country), xbar$delta_order)
      
      
      ggbar0 <- ggplot(xbar, aes(x = delta_order, y = delta, fill = delta_sign)) +
        geom_hline(yintercept = 0) + geom_col() +
        coord_flip() +
        theme_few(12) + xlab("") + ylab("% Change") +
        facet_wrap(~variable, scales = "free", drop = TRUE, nrow = 1) +
        scale_x_discrete(labels = rank_order) +
        scale_fill_manual(values = snap_bar_clr) +
        theme(legend.position = "none")
      
    }
    
    
    
    return(ggbar0)
    
  }, ignoreNULL = FALSE)

  
  output$out_malranking_snapshot <- renderPlot({print(get_barplot_multictry())})


  ### One country national-level indicator plot -------------------------------------

  get_indi_natl_bar_snap <- eventReactive(input$goplot_snap, {
    
    if ( (input$in_ltm_or_yr_snap %in% "Select a date range") & (!check_nmonth(input$ttpand_snap, input$ttcomp_snap)) ) {
      
      ggbar1 <- NULL
      
    } else {
      
      if (input$in_region_snapshot %in% region_list) {
        
        return(NULL)
        
      } else {
        
        xbar1 <- filter_region_snap(xctry, input$in_region_snapshot, region_list, regions) %>%
          filter(count_type %in% input$in_yaxs_snap)
        xb1 <- data.frame()
        
        for (i in mal_vars) {
          bi <- as.data.frame(get_shpdat_snap(xbar1, i, s0, input$ttcomp_snap, input$ttpand_snap, "covid_cases"))
          if (nrow(bi) > 0) {
            bi$variable <- i
            xb1 <- rbind(xb1, bi)
          }
        }
        
        if (nrow(xb1) > 0) {
          xb1 <- xb1 %>%
            mutate(var_label = factor(variable, levels = mal_vars,
                                      labels = as.vector(indicator_labels)))
          
          vars_order <- rev(xb1$var_label[order(xb1$delta)])
          xb1 <- xb1 %>%
            mutate(delta_sign = if_else(delta > 0, "positive", "negative"),
                   var_label = factor(var_label, levels = vars_order),
                   delta = delta/100)
          
          xminval <- (ceiling(10 * max(abs(xb1$delta))) / 10) + 0.1
          xxlim <- c(-xminval, xminval)
          
          ggbar1 <- ggplot(xb1, aes(x = var_label, y = delta)) +
            geom_bar(aes(fill = delta_sign), stat = "identity") +
            geom_hline(yintercept = 0) +
            geom_text(aes(label = label_percent(accuracy = 0.1)(delta)),
                      hjust = "inward", color = "black") +
            coord_flip() +
            theme_few(12) +
            scale_fill_manual(values = snap_bar_clr, drop = FALSE) +
            scale_y_continuous(labels = percent, limits = xxlim) +
            xlab("") + ylab("Percentage change") +
            theme(legend.position = "none")
          
        } else {
          
          ggbar1 <- ggplot(data = data.frame(x = c(0,1), y = c(0,1)), aes(x = x, y = y)) +
            geom_text(aes(x = 0.5, y = 0.5, label = "Data unavailable")) +
            theme_few(12) + xlab("") + ylab("")
        }
      
    }
     }
    
    return(ggbar1)
    
  }, ignoreNULL = FALSE)
  
  
  output$out_adm0indicator_snapshot <- renderPlot({ print(get_indi_natl_bar_snap())  })


  ### One country national-level reporting rate plot --------------------------------

  
  get_rrplot_subnatl <- eventReactive(input$goplot_snap, {
    if ( (input$in_ltm_or_yr_snap %in% "Select a date range") & (!check_nmonth(input$ttpand_snap, input$ttcomp_snap)) ) {
      gg_rr <- NULL
    } else {
      gg_rr <- get_rrplot_snap(dat = xctry, input$ttcomp_snap, input$ttpand_snap, 
                               region = input$in_region_snapshot, value_type = input$in_yaxs_snap)
    }
    return(gg_rr)
  }, ignoreNULL = FALSE)
  
  
  output$out_adm0reporting_snapshot <- renderPlot({  print(get_rrplot_subnatl()) })
  
  
  ### One country national-level mobility bar plot -----------------------------------
  
  get_mobbar_snap <- eventReactive(input$goplot_snap, {
    if ( (input$in_ltm_or_yr_snap %in% "Select a date range") & (!check_nmonth(input$ttpand_snap, input$ttcomp_snap)) ) {
      gg_mob <- NULL
    } else {
      gg_mob <- get_mobplot_snap(dat = xmob_long, input$ttpand_snap, region = input$in_region_snapshot)
    }
    return(gg_mob)
  }, ignoreNULL = FALSE)
  
  
  output$out_adm0mobility_snapshot <- renderPlot({print(get_mobbar_snap())})
  



  ### Subnational snapshot map ----------------------------------

  get_barplot_1ctry <- eventReactive(input$goplot_snap, {
    
    if ( (input$in_ltm_or_yr_snap %in% "Select a date range") & (!check_nmonth(input$ttpand_snap, input$ttcomp_snap)) ) {
      ggbar1 <- NULL
      
    } else {
      xbar1 <- filter_region_snap(xprov, input$in_region_snapshot, region_list, regions) %>% # this may be the source of admin 1 errors 
        filter(count_type %in% input$in_yaxs_snap)
      
      xb1 <- NULL
      for (i in mal_vars) {
        covid_str <- if_else(i %in% "malaria_deaths", "covid_deaths", "covid_cases")
        x1i <-  get_shpdat_snap(xbar1, i, s1, input$ttcomp_snap, input$ttpand_snap, covid_str) %>%
          as.data.frame() %>%
          mutate(delta_sign = if_else(delta > 0, "positive", "negative"),
                 delta_order = order(delta)) %>%
          filter(!(is.na(delta)))
        xb1 <- rbind(xb1, x1i)
      }
      
      xb1 <- xb1 %>%
        mutate(variable = factor(variable, levels = mal_vars,
                                 labels = as.vector(indicator_labels))) %>%
        arrange(variable) %>%
        mutate(delta_order = sprintf("%03i", frank(., variable, -delta, ties.method = "first")),
               delta = delta/100,
               delta_pos = if_else(delta < 0, 0.1, - 0.1)) 
      
      rank_order <- setNames(as.character(xb1$admin_level_1), xb1$delta_order)
      
      if(nrow(xb1) > 0){
        ggbar1 <- ggplot(xb1, aes(x = delta_order, y = delta, fill = delta_sign)) +
          geom_hline(yintercept = 0) + geom_col() +
          # geom_text(aes(label = label_percent(accuracy = 0.1)(delta), y = delta_pos, x = delta_order),
          #           hjust = "inward", color = "black") +
          coord_flip() +
          theme_few(12) + xlab("") + ylab("Percentage change") +
          facet_wrap(~variable, scales = "free", drop = FALSE, nrow = 1) +
          scale_x_discrete(labels = rank_order) +
          scale_y_continuous(labels = percent) +
          scale_fill_manual(values = snap_bar_clr) +
          theme(legend.position = "none") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
      } else {
        ggbar1 <- ggplot(data = data.frame(x = c(0,1), y = c(0,1)), aes(x = x, y = y)) +
          geom_text(aes(x = 0.5, y = 0.5, label = "Data is unavailable")) +
          theme_few(12) + xlab("") + ylab("")
      }
    }
    return(ggbar1)
  }, ignoreNULL = FALSE)
  
  
  
  output$out_adm1malranking_snapshot <- renderPlot({ print(get_barplot_1ctry()) })
  
  
  
  ## MALARIA % CHANGE TIMESERIES ===================================================
  
  ### Malaria percent ts: dynamic input selector -----------------------------------
  
  # Region/Country selection based on user-selected spatial aggregation level
  # National: lists regions + countries; Subnational: only lists countries
  observeEvent(input$in_aggr_pcnt, ignoreNULL = FALSE, {
    if (input$in_aggr_pcnt %in% "admin0") {
      updateSelectInput(session, inputId = "in_region_pcnt", 
                        label = "Region/Country", 
                        choices =   c(user_filtered_regions, "----------------------",
                                      as.character(user_filtered_countries)[order(unique(user_filtered_countries))]),
                        selected = "All Countries")
    } 
    else {
      if(length(user_filtered_countries) > 0){ 
        updateSelectInput(session, inputId = "in_region_pcnt",
                          label = "Country:",
                          choices =   c(as.character(user_filtered_countries)[order(unique(user_filtered_countries))]))
      }
      else{
        updateSelectInput(session, "in_region_pcnt", selected = "All Countries",
                          choices = c(as.character(unique(reporting_tbl$country)[order(unique(reporting_tbl$country))]))) 
      }
    }
  })
  
  
  # Reactive function to obtain admin 1 names 
  get_adm1name_pcnt <- reactive({
    adm1dat <- subset(xprov, country %in% input$in_region_pcnt)
    return(as.character(unique(adm1dat$admin_level_1)))
  })
  
  # Populate admin level 1 names or return NULL
  observe({
    if ((input$in_aggr_pcnt %in% "admin1") & (input$in_region_pcnt %in% cc_list)) {
      output$out_filter_adm1_pcnt <- renderUI({
        selectInput(inputId = "in_adm1_pcnt", label = "Admin. Level 1", 
                    choices = c("View All", get_adm1name_pcnt()),
                    selected = "View All")
      })
    } else {
      output$out_filter_adm1_pcnt <- renderUI({NULL})
    }
  })
  
  # Indicator options based on user-selected country/region
  # If only 1 country (at national level) or 1 admin_1 (at subnational level), 
  # the option is "All Indicators". Otherwise lists indicator names
  observe({
    
    
    if ((input$in_aggr_pcnt %in% "admin0") & (input$in_region_pcnt %in% cc_list)) {
      updateSelectInput(session, inputId = "in_indicator_pcnt", label = "Indicator",
                        choices = c("All Indicators" = "all"), selected = "all")
    } else if ((input$in_aggr_pcnt %in% "admin1")) {
      req(input$in_adm1_pcnt) 
      if (!input$in_adm1_pcnt %in% "View All") {
        updateSelectInput(session, inputId = "in_indicator_pcnt", label = "Indicator",
                          choices = c("All Indicators" = "all"), selected = "all")
      } else {
        updateSelectInput(session, inputId = "in_indicator_pcnt", label = "Indicator",
                          choices = c("All Cause Consultations" = "allcause_cases", 
                                      "Tested Cases" = "tested_cases", 
                                      "Malaria Confirmed Cases" = "confirmed_cases",
                                      "Test Positivity Rate" = "tpr", 
                                      "Malaria Severe Cases" = "severe_cases", 
                                      "Malaria Deaths" = "malaria_deaths", 
                                      "ANC (1st) Visit" = "anc1_visit"))
      }
    } else {
      updateSelectInput(session, inputId = "in_indicator_pcnt", label = "Indicator",
                        choices = c("All Cause Consultations" = "allcause_cases", 
                                    "Tested Cases" = "tested_cases", 
                                    "Malaria Confirmed Cases" = "confirmed_cases",
                                    "Test Positivity Rate" = "tpr", 
                                    "Malaria Severe Cases" = "severe_cases", 
                                    "Malaria Deaths" = "malaria_deaths", 
                                    "ANC (1st) Visit" = "anc1_visit"))
    }
  })
  
  
  
  ### Malaria percent ts: Plot title --------------------------------------------
  
  output$out_plottitle_pcnt <- renderUI({
    if (input$in_indicator_pcnt %in% "all") {
      txt_indi <- "all malaria indicators"
    } else {
      txt_indi <- tolower(as.vector(indicator_labels[input$in_indicator_pcnt]))
    }
    txt <- paste("<h4><b>Monthly difference in ", txt_indi, " from ", input$in_yr0_pcnt, "</b></h4><br/>", sep = "")
    HTML(txt)
  })
  
  ### Malaria percent ts: data selector --------------------------------------------
  
  get_dat_pcnt_ts <- eventReactive(input$goplot_pcnt, {
    
    if (input$in_aggr_pcnt %in% "admin0") {
      p0 <- extract_countries(xctry, input$in_region_pcnt, regions)
      pdat0 <- p0 %>%
        filter(count_type %in%input$in_epiunit_pcnt)
    } else {
      req(input$in_adm1_pcnt) # prevent pre-load error message
      p0 <- extract_countries(xprov, input$in_region_pcnt, regions)
      pdat0 <- p0 %>%
        filter((count_type %in% input$in_epiunit_pcnt) & (country %in% input$in_region_pcnt)) 
      # if (input$in_indicator_pcnt %in% "all") {
      #   pdat0 <- filter(pdat0, admin_level_1 %in% input$in_adm1_pcnt)
      # }
      if ( (length(input$in_adm1_pcnt)) > 0 & (!input$in_adm1_pcnt %in% "View All")) {
        pdat0 <- filter(pdat0, admin_level_1 %in% input$in_adm1_pcnt)
      }
    }
    
    if (input$in_indicator_pcnt %in% "all") {
      vv <- mal_vars
    } else {
      vv <- input$in_indicator_pcnt
    }
    
    ttf <- as.Date(c("2020-01-01", paste(format(Sys.Date(), "%Y-%m"), "-01", sep = "")))
    tt0 <- as.Date(paste(input$in_yr0_pcnt, "-01-01", sep = ""))
    
    # loop around user-selected variable and calculate the difference
    pdat <- data.frame()
    for (j in vv) {
      # print(input$ttpand_pcnt)
      pj <- get_difference_ts(dat = pdat0, var_name = j, tt0 = tt0, ttf = ttf)
      if (nrow(pj) > 0) {
        pj$variable <- indicator_labels[[j]]
        pj <- pj %>% 
          mutate(boxgroup = variable, delta = delta / 100)
        pdat <- rbind(pdat, pj)
      } else{
        next()
      }
    }
    
    # extract covid data 
    
    pbox <- as.character(unique(pdat$boxgroup)) 
    
    ucov <- p0 %>% filter((boxgroup %in% pbox) & (count_type %in% input$in_epiunit_pcnt)) %>%
      filter(variable %in% c("covid_cases", "covid_deaths")) 
    
    # If covid data and indicator data in 2020 is all unavailable, set data to NULL
    # If covid data is unavailable but 2020 indicator available, set data frame such that
    # covid cases are NA
    if (nrow(ucov) == 0) {
      if (all(is.na(pdat$delta))) {
        ucov <- data.frame(NULL)
      } else {
        ucov <- p0 %>% filter((boxgroup %in% pbox) & (count_type %in% input$in_epiunit_pcnt))  %>%
          filter(stringr::str_detect(variable, "ltm|rate", negate = TRUE)) %>%
          mutate(variable = "covid_cases", 
                 value = NA)
      }
    }
    
    # If data is not NULL, Set covid cases & deaths unit; perform any mutations to set the df for plotting
    if (nrow(ucov) > 0) {
      ucov <- ucov %>%
        rowwise() %>%
        mutate(cov_unit = if_else(input$in_epiunit_pcnt %in% "value", "cases",
                                  if_else(variable %in% "covid_deaths", "/1 mil. pop.", "/1,000 pop."))) %>%
        mutate(txt_cov = paste(info_txt, " (", month.abb[month], " ", year, "): ", prettyNum(value), " ", cov_unit, sep = "")) %>%
        dplyr::select(-c(count_type, linegroup, mygroup, combogroup, info, info_txt, cov_unit)) %>%
        rename("covid_variable" = "variable", "covid_value" = "value")
      if ("admin_level_1" %in% colnames(pdat)) {
        col_core <- c("country", "admin_level_1") 
      } else {
        col_core <- c("country")
      }
      
      # Combine covid data with the percent change ts
      
      vars2merge <- c(col_core, "year", "month", "boxgroup")
      
      max_delta <- ifelse(all(is.na(pdat$delta)), 1, max(pdat$delta, na.rm = TRUE))
      
      max_cov <- ifelse(all(is.na(ucov$covid_value)), 1, max(ucov$covid_value, na.rm = TRUE))
      
      # Scaling factor for covid to enable double axis
      scale_factor <- max_delta / max_cov
      
      pout <- merge(pdat %>% select(-c(date.x, date.y)), ucov, by = vars2merge, all = TRUE) %>%
        mutate(yscl = scale_factor ) %>%
        mutate(covid_scaled = covid_value * yscl) %>%
        mutate(txt_pcnt = paste(boxgroup, " (", paste(format(input$ttpand_pcnt, "%b %Y"), collapse = " - "), "): ", ifelse(delta > 0, "+" , ""),
                                percent(delta, accuracy = 0.1), " change from ", paste(format(input$ttcomp_pcnt, "%b %Y"), collapse = " - "), sep = "")) 
    }
    
    
    if ( all(is.na(pdat$delta)) & (all(is.na(ucov$covid_value)) | (nrow(ucov) == 0)) ) {
      pout <- NULL
    }
    
    # Add reporting rate and mobility data if selected
    
    if(!is.null(pout)) {
      ttfseq <- seq(ttf[1], ttf[2], by = "1 month")
      if(input$in_yr0_pcnt %in% "Historical Mean") {
        tt0seq <- NULL
      } else {
        tt0seq <- seq(tt0[1], length.out = 12, by = "1 month")
      }
      if (is.null(input$in_overlay_pcnt)) {
        # In case reporting rate/mobility previously selected, remove this selection
        pout <- pout %>% filter(str_detect(variable, ("reports_rate|mob_"), negate = TRUE)) 
      } else {
        if (length(input$in_overlay_pcnt) == 1) {
          var2remove <- setdiff(c("reports_rate", "mob_"), input$in_overlay_pcnt)
          if(length(var2remove > 0)) { pout <- subset(pout, str_detect(variable, var2remove, negate = TRUE)) }
        }
        for(i in input$in_overlay_pcnt) {
          if (any(str_detect(replace_na(pout$variable, ""), i))) {
            next
          } else {
            if (i %in% "mob_") {
              pvul <- subset(xmob_long, country %in% unique(pout$country))
              if ("admin_level_1" %in% colnames(pout)) {
                pvul <- subset(pvul, !(admin_level_1 %in% ""))
              } else {
                pvul <- subset(pvul, admin_level_1 %in% "") %>% dplyr::select(-admin_level_1)
              }
              pbox <- unique(pout$boxgroup)
              p2add <- pvul %>% 
                rename(value.x = value, txt_pcnt = info_txt, colourgroup = var_label) %>%
                mutate(mygroup = factor(year), value.x = value.x/100, value.y = value.x,
                       boxgroup = pbox[1], date.x = date, date.y = date) %>%
                dplyr::select(-c(date))
              
              if (length(unique(pout$boxgroup)) > 1) {
                for (i in pbox[-1]) {
                  p2add <- bind_rows(p2add,
                                     p2add %>% mutate(boxgroup = i))
                }
              }
              pout <- bind_rows(pout, p2add)
              
            } else {
              
              if (any(c("allcause_cases", "confirmed_cases", "anc1_visit") %in% vv)) {
                rvv <- str_extract(vv, "confirmed_cases|allcause_cases|anc1_visit") %>%
                  paste("reports_rate__", ., sep = "")
                if(is.null(tt0seq)) {
                  p2add_t0 <- NULL
                } else {
                  p2add_t0_tmp1 <- subset(pdat0, (variable %in% rvv) & (date %in% tt0seq))  %>%
                    mutate(date = as.Date(paste("2020-", month, "-01", sep = "")))
                  tt0seq_cy21 <- tt0seq[1:(max(as.numeric(format(max(ttfseq), "%m"))))]
                  p2add_t0_tmp2 <- subset(pdat0, (variable %in% rvv) & (date %in% tt0seq_cy21)) %>%
                    mutate(date = as.Date(paste("2021-", month, "-01", sep = "")))
                  p2add_t0 <- rbind(p2add_t0_tmp1, p2add_t0_tmp2) %>%
                    mutate(mygroup = "baseline")
                }
                rrt0 <- ifelse(input$in_yr0_pcnt %in% "Historical Mean", NULL, input$in_yr0_pcnt)
                p2add <- subset(pdat0, (variable %in% rvv) & (date %in% ttfseq)) %>%
                  mutate(mygroup = "current") %>%
                  bind_rows(., p2add_t0) %>%
                  mutate(value = as.numeric(as.character(info)), date.x = date, date.y = date) %>%
                  rename(value.x = value, count_type.x = count_type, txt_pcnt = info_txt) %>%
                  dplyr::select(-c(date, linegroup, combogroup, info))
                pout <- bind_rows(pout, p2add)
              } else {
                next
              }
              
            }
          }
        }
        
      }
    }


    
    
    return(pout)
    
  }, ignoreNULL = FALSE)
  
  
  ### Malaria percent ts: plot -----------------------------------------------------
  
  output$out_plot_pcnt <- renderPlotly({
    
    pcnt_data <- get_dat_pcnt_ts()
    
    if (is.null(pcnt_data)) {
      dfnull <- data.frame(x = 0, y = 0)
      ggplotly(
        ggplot(dfnull, aes(x = x, y = y)) + 
        geom_point(size = 0) + 
        geom_text(aes(x = 6, y = 0.8, hjust = 0.5, label = "Indicator and COVID-19 data are \nunavailable for the selected period")) + 
        scale_x_continuous(limits = c(0, 12), breaks = seq(1, 12, by = 2), labels = month.abb[seq(1, 12, by = 2)]) +
        scale_y_continuous(limits = c(-1,1)) + 
        theme_few() + 
        theme(legend.position = "none"))
        
    } else {
      ssize <- screen_dim()
      gg_pcnt <- get_pcnt_ts_plot(pcnt_data, screen_size = ssize)
      
      rowjust <- ifelse(gg_pcnt$rows > 4, gg_pcnt$rows * 0.2 * ssize[2],
                        ifelse(gg_pcnt$rows == 4, 0.75 * ssize[2],
                               ifelse(gg_pcnt$rows == 3, 0.75 * ssize[2],
                                      ifelse(gg_pcnt$rows == 2, 0.7 * ssize[2], 0.4 * ssize[2]))))
      
      plot.new()
      plotly::subplot(gg_pcnt$gg, nrows = gg_pcnt$rows, margin = 0.03) %>% # adding back in previously removed nrows & margin arguments
      layout(width = 0.7 * ssize[1],
             height = rowjust)
    }

  })
  
  
  ## MALARIA VALUE BY YEAR TIMESERIES ===================================================
  
  ### Malaria value ts: dynamic input selector ------------------------------------------
  
  get_adm1name_ts <- reactive({
    adm1dat <- subset(xprov, country %in% input$in_region_ts)
    return(as.character(unique(adm1dat$admin_level_1)))
  })
  
  observeEvent(input$in_aggr_ts, ignoreNULL = FALSE, {
    if (input$in_aggr_ts %in% "admin0") {
      updateSelectInput(session, inputId = "in_region_ts", 
                        label = "Region/Country", 
                        choices =   c(user_filtered_regions, "----------------------",
                                      as.character(user_filtered_countries)[order(unique(user_filtered_countries))]),
                        selected = "All Countries")
    } 
    else {
      if(length(user_filtered_countries) > 0){ 
        updateSelectInput(session, inputId = "in_region_ts",
                          label = "Country:",
                          choices =   c(as.character(user_filtered_countries)[order(unique(user_filtered_countries))]))
      }
      else{
        updateSelectInput(session, "in_region_ts", selected = "All Countries",
                          choices = c(as.character(unique(reporting_tbl$country)[order(unique(reporting_tbl$country))]))) 
      }
    }
  })
  
  
  # Populate admin level 1 choices or return NULL
  observe({
    if ((input$in_aggr_ts %in% "admin1") & (input$in_region_ts %in% cc_list)) {
      output$out_filter_adm1_ts <- renderUI({
        selectInput(inputId = "in_adm1_ts", label = "Admin. Level 1", 
                    choices = c("View All", get_adm1name_ts()),
                    selected = "View All")
      })
    } else {
      output$out_filter_adm1_ts <- renderUI({NULL})
    }
  })
  
  
  observe({
    if ((input$in_aggr_ts %in% "admin0") & (input$in_region_ts %in% cc_list)) {
      updateSelectInput(session, inputId = "in_indicator_ts", label = "Indicator",
                        choices = c("All Indicators" = "all"), selected = "all")
    } else if ((input$in_aggr_ts %in% "admin1")) {
      req(input$in_adm1_ts) 
      if (!input$in_adm1_ts %in% "View All") {
        updateSelectInput(session, inputId = "in_indicator_ts", label = "Indicator",
                          choices = c("All Indicators" = "all"), selected = "all")
      } else {
        updateSelectInput(session, inputId = "in_indicator_ts", label = "Indicator",
                          choices = c("All Cause Consultations" = "allcause_cases", 
                                      "Tested Cases" = "tested_cases", 
                                      "Malaria Confirmed Cases" = "confirmed_cases",
                                      "Test Positivity Rate" = "tpr", 
                                      "Malaria Severe Cases" = "severe_cases", 
                                      "Malaria Deaths" = "malaria_deaths", 
                                      "ANC (1st) Visit" = "anc1_visit"))
      }
    } else {
      updateSelectInput(session, inputId = "in_indicator_ts", label = "Indicator",
                        choices = c("All Cause Consultations" = "allcause_cases", 
                                    "Tested Cases" = "tested_cases", 
                                    "Malaria Confirmed Cases" = "confirmed_cases",
                                    "Test Positivity Rate" = "tpr", 
                                    "Malaria Severe Cases" = "severe_cases", 
                                    "Malaria Deaths" = "malaria_deaths", 
                                    "ANC (1st) Visit" = "anc1_visit"))
    }
  })
  
  
  ### Malaria value ts: data selector --------------------------------------------
  
  get_dat_malts <- reactive({
    
    if (input$in_aggr_ts %in% "admin0") {
      tsdat <- xctry
    } else {
      tsdat <- xprov}
    tsdat1 <- extract_countries(tsdat, input$in_region_ts, regions) %>%
      filter(count_type %in% input$in_epiunit_ts)
    if (input$in_aggr_ts %in% "admin0") {
      if (input$in_region_ts %in% region_list_all) {
        tsdat1 <- subset(tsdat1, boxgroup %in% indicator_labels[input$in_indicator_ts])
      } else  {
        tsdat1 <- subset(tsdat1, country %in% input$in_region_ts)
      }
    } else if (input$in_aggr_ts %in% "admin1") {
      tsdat1 <- tsdat1 %>% filter(country %in% input$in_region_ts)
      if (input$in_indicator_ts %in% "all") {
        tsdat1 <- subset(tsdat1, admin_level_1 %in% input$in_adm1_ts)
      } else {
        tsdat1 <- subset(tsdat1, boxgroup %in% indicator_labels[input$in_indicator_ts])
      }
    } else {
      tsdat1 <- tsdat1 }
    if (!is.null(tsdat1)) {
      # Exclude forecasts if requested
      if (input$in_radio_include_ts == "None") {
        tsdat1 <- subset(tsdat1, !(combogroup %in% c("COVID-19 Forecast", "Report Rate")) )
      } else if (input$in_radio_include_ts == "COVID-19 Forecasts") {
        tsdat1 <- subset(tsdat1, !(combogroup %in% c("Report Rate")))
      } else {
        tsdat1 <- subset(tsdat1, !(combogroup %in% c("COVID-19 Forecast")))
      }
      tsdat1 <- tsdat1 %>% filter( !((variable %in% "reports_rate__confirmed_cases") & (year < 2020)) ) %>%
        filter( !((variable %in% "reports_rate__allcause_cases") & (year <  2020)) ) %>%
        filter( !((variable %in% "reports_rate__anc1_visit") & (year <  2020)) )
      m0 <- input$in_yr0m0_ts
      mf <- input$in_yr0mf_ts
      yf <- input$in_yr0_ts
      tsribbon <- get_ribbon_data(m0, mf, yf, tsdat1)
    } else {
      ribbon <- ylabel <- NULL
    }
    ylabel <- ifelse(input$in_epiunit_ts %in% "value_rate", "Cases per 1K population", "Cases")
    if (input$in_indicator_ts %in% "all") {
      if (input$in_aggr_ts %in% "admin0") {
        title_ts <- as.character(unique(tsdat1$country))
      } else {
        title_ts <- paste(unique(tsdat1$admin_level_1), ", ", unique(tsdat1$country), sep = "")
      }
    } else {
      if (input$in_aggr_ts %in% "admin0") {
        title_ts <- paste(input$in_region_ts, ": ", as.vector(indicator_labels[input$in_indicator_ts]), sep = "")
      } else {
        title_ts <- paste(unique(tsdat1$country), ": ", as.vector(indicator_labels[input$in_indicator_ts]), sep = "")
      }
    }
    return(list(dat = tsdat1, ribbon = tsribbon, ylabel = ylabel, gtitle = title_ts))


  })
  
  
  ### Malaria value ts: plot -------------------------------------------------------
  
  output$out_plot_ts <- renderPlotly({

    tsdat <- get_dat_malts()

    gg_ts <- ggplot(data = tsdat$dat, aes(x = month, y = value)) +
      geom_line(aes(colour = combogroup, text = info_txt, linetype = combogroup,
                    alpha = combogroup, group = interaction(mygroup, combogroup)), size = 0.5) +
      geom_point(aes(colour = combogroup, text = info_txt, shape = combogroup,
                     size = combogroup, alpha = combogroup, group = interaction(mygroup, combogroup))) +
      geom_ribbon(data = tsdat$ribbon, aes(x = month, ymin = minval, ymax = maxval, fill = combogroup),
                  colour = NA, alpha = 0.1) +
      xlab("") + ylab(tsdat$ylabel) +
      ggtitle(tsdat$gtitle) +
      scale_x_continuous(breaks = seq(from = 1, to = 12, by = 2), labels = month.abb[seq(from = 1, to = 12, by = 2)]) +
      theme_few(12) +
      scale_alpha_manual("", values = alpha_values, drop = FALSE) +
      scale_linetype_manual("", values = linetype_values, drop = FALSE) +
      scale_shape_manual(values = shape_values, drop = FALSE) +
      scale_size_manual(values = size_values, drop = FALSE) +
      scale_colour_manual("", values = clr_values, drop = FALSE) +
      scale_fill_manual("", values = clr_values, drop = FALSE) +
      theme(legend.title = element_blank(), legend.position = "none")
    
    if (input$in_aggr_ts %in% "admin0") {
      if (input$in_indicator_ts %in% "all") {
        gg_ts <- gg_ts + facet_wrap( ~ boxgroup, ncol = 4, scales = "free")
        narea <- 12
      } else {
        gg_ts <- gg_ts + facet_wrap( ~ country, ncol = 5, scales = "free")
        narea <- length(unique(tsdat$dat$country))
      }
    } else {
      if (input$in_indicator_ts %in% "all") {
        gg_ts <- gg_ts + facet_wrap( ~ boxgroup, ncol = 4, scales = "free")
        narea <- 12
      } else {
        gg_ts <- gg_ts + facet_wrap( ~ admin_level_1, ncol = 5, scales = "free")
        narea <- length(unique(tsdat$dat$admin_level_1))
    } }

    # To avoid having twenty-something plots in one view
    if (!(input$in_indicator_ts %in% "all")) {
      if (narea > 15) {
        pl_height <- 0.6 * ceiling(narea/12)
      } else {
        pl_height <- 0.7 * ceiling(narea/12)
      }
    } else {
      pl_height <- 0.8
    }
    hscrn <- screen_dim()
    ggplotly(gg_ts, tooltip = "text",
             width = (0.75 * hscrn[1]),
             height = pl_height*hscrn[2])
    
  })
  
  
  
  
  ## RAINFALL AND MALARIA ==========================================================
  
  observeEvent(input$in_aggr_rf, ignoreNULL = FALSE, {
    if (input$in_aggr_rf %in% "admin0") {
      updateSelectInput(session, inputId = "in_region_rf",
                        label = "Region/Country:",
                        choices =   c(user_filtered_regions, "----------------------",
                                      as.character(user_filtered_countries)[order(unique(user_filtered_countries))]),
                        selected = "All Countries")
    } 
    else {
      if(length(user_filtered_countries) > 0){ 
        updateSelectInput(session, inputId = "in_region_rf",
                          label = "Country:",
                          choices =   c(as.character(user_filtered_countries)[order(unique(user_filtered_countries))]))
      }
      else{
        updateSelectInput(session, "in_region_rf", selected = "All Countries",
                          choices = c(as.character(unique(reporting_tbl$country)[order(unique(reporting_tbl$country))]))) 
      }
    }
  })
  
  get_dat_rf <- reactive({
    rfdat_adm_level <- input$in_aggr_rf
    rfdat_ctry <- input$in_region_rf
    rfdat_indicator <- input$in_indicator_rf_adm0
    rfdat_rftype <- input$in_rftype_rf
    rfdat_count_type <- input$in_count_type
    
    # ctry_bylat <- regions[[1]]
    # ctry_nhem <- regions[[2]]
    # ctry_wafr <- regions[[4]]
    # ctry_eafr <- regions[[5]]
    # ctry_csafr <- regions[[6]]
    # ctry_sea <- regions[[7]]
    
    if (rfdat_adm_level %in% "admin0") {
      tmp <- xrf %>% filter(admin_level_1 %in% "")
      switch(rfdat_ctry,
             "All Countries" = {yrf0 <- tmp  %>%  mutate(country = factor(country, levels = ctry_bylat))},
             "Northern Hemisphere" = {yrf0 <-tmp %>% filter(country %in% ctry_nhem) %>% mutate(country = factor(country, levels = ctry_nhem))},
             "Southern Hemisphere" = {yrf0 <-tmp %>% filter(country %in% ctry_shem) %>% mutate(country = factor(country, levels = ctry_shem))},
             "Western Africa" = {yrf0 <-tmp %>% filter(country %in% ctry_wafr) %>% mutate(country = factor(country, levels = ctry_wafr))},
             "Eastern Africa" = {yrf0 <-tmp %>% filter(country %in% ctry_eafr) %>% mutate(country = factor(country, levels = ctry_eafr))},
             "Central/Southern Africa" = {yrf0 <-tmp %>% filter(country %in% ctry_csafr) %>% mutate(country = factor(country, levels = ctry_csafr))},
             "Southeast Asia" = {yrf0 <-tmp %>% filter(country %in% ctry_sea)},
             {yrf0 <- tmp %>% filter(country %in% rfdat_ctry)})
      
    } else {
      yrf0 <- xrf %>% filter((country %in% rfdat_ctry) & !(admin_level_1 %in% ""))
    }
    yrf <- yrf0 %>% 
      filter(rf_type %in% rfdat_rftype) %>%
      filter(stringr::str_detect(variable, paste("rf|", rfdat_indicator, sep = ""))) %>%
      filter(count_type %in% c("value_rf", rfdat_count_type)) 
    if (rfdat_rftype %in% "acc_ssn") {
      yrf <- yrf %>% filter(country %in% ctry_shem) %>%
        subset(., !( (variable %in% rfdat_indicator) & !(year_ssn %in% c("2019/2020", "2018/2019")) ))
    } else {
      yrf <- yrf %>% subset(., !((variable %in% rfdat_indicator) & (year < 2019)))
    }
    return(list(dat = yrf, rf_type = rfdat_rftype, admin = rfdat_adm_level))
  })
  
  
  output$out_rfplot_adm0 <- renderPlot({
    dat_rf <- get_dat_rf()
    
    dat_rf[[1]] <- subset(dat_rf[[1]], is.na(country)==F) # removing missing countries from df
    
    if (dat_rf$rf_type %in% "acc_ssn") {
      grf <- ggplot(dat_rf$dat, aes(x = month_order_ssn, y = value)) +
        scale_x_continuous(breaks = seq(1, 12, by = 2), labels = month.abb[c(8:12, 1:7)[seq(1, 12, by = 2)]]) +
        ylab("Cumulative Rainfall (mm)")
    } else {
      grf <- ggplot(dat_rf$dat, aes(x = month, y = value)) +
        scale_x_continuous(breaks = seq(1, 12, by = 2), labels = month.abb[seq(1, 12, by = 2)]) +
        ylab("Monthly Rainfall (mm)")
    }
    
    grf <- grf + 
      geom_line(aes(colour = combogroup_2, linetype = combogroup_2, alpha = combogroup_2,
                    group = interaction(mygroup, combogroup_2)), size = 0.5) +
      geom_point(aes(colour = combogroup_2, size = combogroup_2, alpha = combogroup_2,
                     shape = combogroup_2,
                     group = interaction(mygroup, combogroup_2))) +
      scale_colour_manual(values = grf_clr) +
      scale_alpha_manual(values = grf_alpha) + 
      scale_linetype_manual(values = grf_linetype) +
      scale_size_manual(values = grf_size) +
      scale_shape_manual(values = grf_shape) +
      theme_few(12) +
      xlab("") 
    
    if (dat_rf$admin %in% "admin0") {
      grf <- grf + 
        facet_wrap( ~ country, ncol = 4, scales = "free")
    } else {
      grf <- grf + 
        facet_wrap( ~ admin_level_1, ncol = 4, scales = "free")
    }
    
    print(grf)
  })

  

  
  
  
  
  
  # MALARIA TAB ####################################################################
  
  # Create 3rd input selection in covid-in-country tab
  
  # observeEvent(input$in_adminlevel_inctry, ignoreNULL = FALSE, {
  #   
  #   if (input$in_adminlevel_inctry %in% "Level 1") {
  #     
  #     output$out_filter_l3_inctry <- renderUI(
  #       checkboxGroupInput(inputId = "in_indicator_inctry", 
  #                          label = "Select indicators to include:",
  #               
  # choices = c("> 60 yrs population density", "30 - 60 yrs population density", 
  #                                      "20 - 30 yrs population density", "10 - 20 yrs population density",
  #                                      " 5 - 10 yrs population density", " 0 - 5 yrs population density",
  #                                      "Mobility (Google)", "Handwashing (Survey)",
  #                                      "Health workers per population",
  #                                      "Annual new consultations (fever) per 10K people"))
  #     )
  #     
  #   } else if (input$in_adminlevel_inctry %in% "Level 2") {
  #     
  #     output$out_filter_l3_inctry <- renderUI(
  #       checkboxGroupInput(inputId = "in_indicator_inctry", 
  #                          label = "Select indicators to include:",
  #                          choices = c(">60 population density", "30 - 60 population density", 
  #                                      "20 - 30 population density", "10 - 20 population density",
  #                                      " 5 - 10 population density", " 0 - 5 population density",
  #                                      "Health workers per population",
  #                                      "Annual new consultations (fever) per 10K people"))
  #     )
  #     
  #   } else {
  #     output$out_filter_l3_inctry <- renderUI(NULL)
  #   }
  #   
  # })
  
  
  
  # COVID GENERAL TAB ####################################################################
  
  # Create 3rd input selection
  
  # observeEvent(input$in_adminlevel_gen, ignoreNULL = FALSE, {
  #   
  #   if (input$in_adminlevel_gen %in% "Level 1") {
  #     
  #     output$out_filter_l3_gen <- renderUI(
  #       checkboxGroupInput(inputId = "in_indicator_inctry", 
  #                          label = "Select indicators to include:",
  #                          choices = c("> 60 yrs population density", "30 - 60 yrs population density", 
  #                                      "20 - 30 yrs population density", "10 - 20 yrs population density",
  #                                      " 5 - 10 yrs population density", " 0 - 5 yrs population density",
  #                                      "Mobility (Google)", "Handwashing (Survey)"))
  #     )
  #     
  #   } else if (input$in_adminlevel_gen %in% "Level 2") {
  #     
  #     output$out_filter_l3_gen <- renderUI(
  #       checkboxGroupInput(inputId = "in_indicator_gen", 
  #                          label = "Select indicators to include:",
  #                          choices = c(">60 population density", "30 - 60 population density", 
  #                                      "20 - 30 population density", "10 - 20 population density",
  #                                      " 5 - 10 population density", " 0 - 5 population density"))
  #     )
  #     
  #   } else {
  #     output$out_filter_l3_gen <- renderUI(NULL)
  #   }
  #   
  # })

  
  # OTHER SOURCES TAB ####################################################################
  
  # Create 2nd input selection in covid-other sources

  observeEvent(input$in_sources_osrc, ignoreNULL = FALSE,{

    if (input$in_sources_osrc == "Surgo Foundation") {

      output$out_filter_l2_osrc <- renderUI(

        selectInput(inputId = "in_splevel_osrc",
                    label = "Select spatial ranking level:",
                    choices = c("Between countries", "Within countries"),
                    selected = "Within countries"))
    } else {
        # updateSelectInput(session, inputId = "ctry",
        #             label = "Country:",
        #             choices = c('Angola','Benin','Burkina Faso','Cambodia','Cameroon',
        #                    "Cote D'Ivoire",'DR Congo','Ethiopia',
        #                    'Ghana','Guinea','Liberia','Madagascar','Malawi',
        #                    'Mali','Mozambique','Myanmar','Niger','Nigeria',
        #                    'Rwanda','Senegal','Sierra Leone','Tanzania (Mainland)',
        #                    'Tanzania (Zanzibar)','Thailand','Uganda','Zambia',
        #                    'Zimbabwe'))
      output$out_filter_l2_osrc <- renderUI(NULL)
    }

  })
  
  
  # update dropdown choices/columns based on between/within choice
observeEvent(input$in_splevel_osrc, {
    
  if (input$in_splevel_osrc == "Within countries") {
    
    output$out_surgo_bt_wi <- renderUI(
      
      selectInput(inputId = "ccvi",
                  label = "Vulnerability Index:",
                  choices = c("Overall CCVI" = "ccvi_compared_to_same_country",
                              "Socioeconomic" = "theme_1_compared_to_same_country",
                              "Population Density" = "theme_2_compared_to_same_country",
                              "Transport Availability and Housing" = "theme_3_compared_to_same_country",
                              "Epidemiological" = "theme_4_compared_to_same_country",
                              "Health System" = "theme_5_compared_to_same_country",
                              "Fragility" = "theme_6_compared_to_same_country",
                              "Age" = "theme_7_compared_to_same_country"),
                  selected = "Overall CCVI")
    )
  } else if (input$in_splevel_osrc == "Between countries") {
    
    output$out_surgo_bt_wi <- renderUI(
      
      selectInput(inputId = "ccvi",
                  label = "Vulnerability Index:",
                  choices = c("Overall CCVI" = "ccvi_compared_to_continent",
                              "Socioeconomic" = "theme_1_compared_to_continent",
                              "Population Density" = "theme_2_compared_to_continent",
                              "Transport Availability and Housing" = "theme_3_compared_to_continent",
                              "Epidemiological" = "theme_4_compared_to_continent",
                              "Health System" = "theme_5_compared_to_continent",
                              "Fragility" = "theme_6_compared_to_continent",
                              "Age" = "theme_7_compared_to_continent"),
                  selected = "Overall CCVI")
    )
  } else {
    
    output$out_surgo_bt_wi <- renderUI(NULL)
  }
})
  
  ## Plot COVID - In country -------------------------------------------------------
  
  s1_benin <- subset(s1, country %in% "Benin")
  
  output$out_vmap_inctry <- renderLeaflet(
    leaflet(srank) %>%
      setView(2.25, 9, zoom = 6) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  fillOpacity = 1,
                  fillColor = ~colorNumeric("YlOrRd", domain = srank$FinalRank, n = 5)(FinalRank)) 
  )
  
  output$out_pop60_inctry <- renderLeaflet(
    leaflet(spop_adm1) %>%
      setView(19, 4, zoom =4)  %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = "lightgray", weight = 0.85, # smoothFactor = 0.5,
                  fillOpacity = 0.65,
                  fillColor = ~colorQuantile("YlOrRd", domain = spop_adm1$popden_60_years_and_over, n = 5)(popden_60_years_and_over)) 
  )
  
  output$out_pop40_inctry <- renderLeaflet(
    leaflet(spop_adm1) %>%
      setView(19, 4, zoom =4)  %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = "lightgray", weight = 0.85, # smoothFactor = 0.5,
                  fillOpacity = 0.65,
                  fillColor = ~colorQuantile("YlOrRd", domain = spop_adm1$popden_40_to_60_years, n = 5)(popden_40_to_60_years)) 
  )
  
  output$out_mobile_inctry <- renderLeaflet(
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(19, 4, zoom =3) 
  )
  
  output$out_wash_inctry <- renderLeaflet(
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(19, 4, zoom =3) 
  )
  
  

  # ## Plot COVID - General ----------------------------------------------------------
  # output$out_vmap_gen <- renderLeaflet(
  #   leaflet() %>%
  #     addProviderTiles("CartoDB.Positron") %>%
  #     setView(19, 4, zoom =3) 
  # )
  # 
  # 
  # output$out_pop60_gen <- renderLeaflet(
  #   leaflet() %>%
  #     addProviderTiles("CartoDB.Positron") %>%
  #     setView(19, 4, zoom =3) 
  # )
  # 
  # output$out_pop40_gen <- renderLeaflet(
  #   leaflet() %>%
  #     addProviderTiles("CartoDB.Positron") %>%
  #     setView(19, 4, zoom =3) 
  # )
  # 
  # output$out_mobile_gen <- renderLeaflet(
  #   leaflet() %>%
  #     addProviderTiles("CartoDB.Positron") %>%
  #     setView(19, 4, zoom =3) 
  # )
  # 
  # output$out_wash_gen <- renderLeaflet(
  #   leaflet() %>%
  #     addProviderTiles("CartoDB.Positron") %>%
  #     setView(19, 4, zoom =3) 
  # )
  
  ## Plot COVID - Other Sources ----------------------------------------------------------

  
  # User-level filtering - Malaria vars
  observeEvent(input$in_ctry_covid_inctry, ignoreNULL = FALSE, {
    if (input$in_ctry_covid_inctry %in% user_filtered_countries) {
      if(input$in_ctry_covid_inctry %in% custom_mal_ctry_18){
      updateCheckboxGroupInput(session, inputId = "in_indicator_inctry_custom",
                         label = "Select indicators to include:",
                         choices = c("Population: 65 yrs and older (%)"="pop_perc_65_years_and_over",
                                     "Population: 45 - 64 yrs (%)"="pop_perc_45_to_64_years"  , 
                                     "Population: 25 - 44 yrs"="pop_perc_25_to_44_years",
                                     "Population: 15 - 24 years (%)"="pop_perc_15_to_24_years",
                                     "Population: 5 - 14 yrs (%)"="pop_perc_5_to_14_years",
                                     "Population 0 - 4 yrs (%)"="pop_perc_0_to_4_years",
                                     "Mobility: Grocery & Pharmacy"="mob_grocery_and_pharmacy",
                                     "Mobility: Parks"="mob_parks",
                                     "Mobility: Residential"="mob_residential",
                                     "Mobility: Retail & Recreation"="mob_retail_and_recreation",
                                     "Mobility: Transit Stations"="mob_transit_stations",
                                     "Mobility: Workplaces"="mob_workplaces",
                                     "Handwashing: Population living in households with access to soap and water (%)"="population_living_in_households_with_a_basic_handwashing_facility_with_soap_and_water_available",
                                     "Handwashing: Population living in households where a place for handwashing was observed (%)"="population_with_a_place_for_handwashing_was_observed",
                                     "Malaria: Health workers per 100K population (2020)"="mal_health_workers_20",
                                     "Malaria: Health workers per 100K population (2018)"="mal_health_workers_cont",
                                     "Malaria: Annual new consultations (fever) per 1K population (2020)"="mal_new_consultation_all_cause_20",
                                     "Malaria: Annual new consultations (fever) per 1K population (2018)"="mal_new_consultation_all_cause_cont"
                         ),
                         selected = "pop_perc_65_years_and_over")
      }
      else{
        updateCheckboxGroupInput(session, inputId = "in_indicator_inctry_custom",
                                 label = "Select indicators to include:",
                                 choices = c("Population: 65 yrs and older (%)"="pop_perc_65_years_and_over",
                                             "Population: 45 - 64 yrs (%)"="pop_perc_45_to_64_years"  , 
                                             "Population: 25 - 44 yrs"="pop_perc_25_to_44_years",
                                             "Population: 15 - 24 years (%)"="pop_perc_15_to_24_years",
                                             "Population: 5 - 14 yrs (%)"="pop_perc_5_to_14_years",
                                             "Population 0 - 4 yrs (%)"="pop_perc_0_to_4_years",
                                             "Mobility: Grocery & Pharmacy"="mob_grocery_and_pharmacy",
                                             "Mobility: Parks"="mob_parks",
                                             "Mobility: Residential"="mob_residential",
                                             "Mobility: Retail & Recreation"="mob_retail_and_recreation",
                                             "Mobility: Transit Stations"="mob_transit_stations",
                                             "Mobility: Workplaces"="mob_workplaces",
                                             "Handwashing: Population living in households with access to soap and water (%)"="population_living_in_households_with_a_basic_handwashing_facility_with_soap_and_water_available",
                                             "Handwashing: Population living in households where a place for handwashing was observed (%)"="population_with_a_place_for_handwashing_was_observed",
                                             "Malaria: Health workers per 100K population (2020)"="mal_health_workers_20",
                                             "Malaria: Health workers per 100K population (2019)"="mal_health_workers_cont",
                                             "Malaria: Annual new consultations (fever) per 1K population (2020)"="mal_new_consultation_all_cause_20",
                                             "Malaria: Annual new consultations (fever) per 1K population (2019)"="mal_new_consultation_all_cause_cont"
                                 ),
                                 selected = "pop_perc_65_years_and_over")
      }

    }
    else {
      updateCheckboxGroupInput(session, inputId = "in_indicator_inctry_custom",
                               label = "Select indicators to include:",
                               choices = c("Population: 65 yrs and older (%)"="pop_perc_65_years_and_over",
                                           "Population: 45 - 64 yrs (%)"="pop_perc_45_to_64_years"  ,
                                           "Population: 25 - 44 yrs"="pop_perc_25_to_44_years",
                                           "Population: 15 - 24 years (%)"="pop_perc_15_to_24_years",
                                           "Population: 5 - 14 yrs (%)"="pop_perc_5_to_14_years",
                                           "Population 0 - 4 yrs (%)"="pop_perc_0_to_4_years",
                                           "Mobility: Grocery & Pharmacy"="mob_grocery_and_pharmacy",
                                           "Mobility: Parks"="mob_parks",
                                           "Mobility: Residential"="mob_residential",
                                           "Mobility: Retail & Recreation"="mob_retail_and_recreation",
                                           "Mobility: Transit Stations"="mob_transit_stations",
                                           "Mobility: Workplaces"="mob_workplaces",
                                           "Handwashing: Population living in households with access to soap and water (%)"="population_living_in_households_with_a_basic_handwashing_facility_with_soap_and_water_available",
                                           "Handwashing: Population living in households where a place for handwashing was observed (%)"="population_with_a_place_for_handwashing_was_observed"),
                               selected = "pop_perc_65_years_and_over")
    }
  })
  
  
  # note: table is  missing Cambodia, Cote D'Ivoire, Malawi, Myanmar and Thailand completely
  # other issues with : Madagascar, Mali, Sierra Leone, both Tanzanias 
  surgo_scores0 <- read_civis("covid.surgo_africa_ccvi_regional_20201218") 
  
  vul_scores <- reactive({
    
    req(input$ccvi) # prevent pre-load error message
    
    # workaround to create a column (ccvi) of the selected CCVI indicator
      # this will supply values for palettes, so we can change polygon fill based on ccvi variables
    
    surgo_scores_long <- surgo_scores0 %>% 
      pivot_longer(
        cols = starts_with(c("theme", "ccvi")),
        names_to = "ccvi",
        values_to = "score")
    
    surgo_pal <- surgo_scores_long %>% 
      filter(ccvi == input$ccvi) %>%
      mutate(ccvi = "pal") %>%
      pivot_wider(names_from = "ccvi",
                  values_from ="score") %>%
      dplyr::select(country, admin_level_1, pal)
    
    scores_sf <- merge(surgo_scores0, s1, all.y = T, by = c("country", "admin_level_1"))
    scores_sf <-  merge(surgo_pal, scores_sf, all.y = T, by = c("country", "admin_level_1"))
    
    scores_sf <-  scores_sf %>%
      filter(country %in% c("Angola", "Benin", "Burkina Faso", "Cameroon",
                            "DR Congo", "Ethiopia", "Ghana", "Guinea", "Kenya", "Liberia", "Madagascar", "Mali",
                            "Mozambique", "Niger", "Nigeria", "Rwanda", "Senegal", "Sierra Leone",
                            "Tanzania (Mainland)", "Tanzania (Zanzibar)", "Uganda","Zambia", "Zimbabwe"))
    
    
    scores_sf <- scores_sf %>%
      filter(country == input$ctry) 
  })
  
  custom_final <- reactive({
    
    custom0_long <- unique(custom0_long)
    custom0_long <- custom0_long %>%
      ungroup()
    
    custom0_latest <- custom0_long_comb %>%
      filter(!is.na(value)) %>%
      group_by(country, admin_level_1, variable) %>%
      slice(which.max(as.Date(date))) %>%
      filter(variable %in% input$in_indicator_inctry_custom) %>% # here's where we'd filter our vars
      # filter(variable %in% vars_test) %>% # here's where we'd filter our vars
      pivot_wider(names_from = variable, values_from = value)
    
    custom0_latest[,-c(1:5)] <- data.frame(lapply(custom0_latest[,-c(1:5)], function(x) as.numeric(as.character(x))))
    
    # aggregating to remove time component
    custom0_cont <- custom0_latest %>%  
      group_by(country, admin_level_1) %>%
      summarize_if(is.numeric, mean, na.rm=T) %>%
      ungroup()
    
    custom0_cont[,-c(1:2)] <- data.frame(lapply(custom0_cont[,-c(1:2)], function(x) as.numeric(as.character(x))))
    custom0_cont <- data.frame(lapply(custom0_cont, function(x) {gsub("NaN", NA, x)})) # setting NaNs to NA 
    custom0_cont[,-c(1:2)] <- data.frame(lapply(custom0_cont[,-c(1:2)], function(x) as.numeric(as.character(x))))
    
    # custom0_cont_perc <- custom0_cont %>%
    #   dplyr::select(-starts_with("households_")) %>%# removing hh-level handwashing data 
    #   summarize_if(is.numeric, percent_rank) %>%
    #   mutate_if(is.numeric, round, digits=3) 
    
    custom0_cont_perc <- custom0_cont %>%
      dplyr::select(-starts_with("households_")) %>%# removing hh-level handwashing data 
      # dplyr::select(country, starts_with("theme"), starts_with("overall")) %>%
      group_by(country) %>%
      summarize_if(is.numeric, percent_rank) %>%
      # mutate_if(is.numeric, round, digits=3) %>%
      ungroup() %>%
      dplyr::select(-country)
    
    
    custom0_cont_theme <- cbind(custom0_cont[,c('country', 'admin_level_1')], custom0_cont_perc) # add back in char vars
    
    custom0_cont_theme <- data.frame(lapply(custom0_cont_theme, function(x) {gsub("NaN", NA, x)})) # setting NaNs to NA
    # custom0_cont_theme[,-c(1:2)] <- data.frame(lapply(custom0_cont_theme[,-c(1:2)], function(x) as.numeric(as.character(x))))
    
    if(length(names(custom0_cont_theme))>3) {
      custom0_cont_theme[,-c(1:2)] <- data.frame(lapply(custom0_cont_theme[,-c(1:2)], function(x) as.numeric(as.character(x))))
      
    }else{
      custom0_cont_theme[,3] <- as.numeric(as.character(custom0_cont_theme[,3]))
    }
    
    custom0_cont_theme <- custom0_cont_theme %>%
      mutate(theme_mal = round(rowMeans(dplyr::select(., contains("mal_")), na.rm=T),3),
             theme_mob = round(rowMeans(dplyr::select(., contains("mob_")),na.rm=T),3),
             theme_wash = round(rowMeans(dplyr::select(., contains("washing")), na.rm=T),3),
             theme_pop = round(rowMeans(dplyr::select(., contains("pop_")), na.rm=T),3))
    
    custom0_cont_theme <- data.frame(lapply(custom0_cont_theme, function(x) {gsub("NaN", NA, x)})) # setting NaNs to NA
    custom0_cont_theme[,-c(1:2)] <- data.frame(lapply(custom0_cont_theme[,-c(1:2)], function(x) as.numeric(as.character(x))))
    
    custom0_cont_theme <- custom0_cont_theme %>%
      mutate(overall_custom_score = round(rowMeans(dplyr::select(., contains("theme_")), na.rm = TRUE),3))
    
    custom0_cont_theme[,-c(1:2)] <- data.frame(lapply(custom0_cont_theme[,-c(1:2)], function(x) as.numeric(as.character(x))))
    
    # custom1 <- custom0_cont_theme %>%
    #   # dplyr::select(country, starts_with("theme"), starts_with("overall")) %>%
    #   group_by(country) %>%
    #   summarize_if(is.numeric, percent_rank) %>%
    #   # mutate_if(is.numeric, round, digits=3) %>%
    #   ungroup() %>%
    #   dplyr::select(-country)
    
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
    #   dplyr::select(-starts_with("households_", ))
    
    custom0_latest <- custom0_latest %>%
      dplyr::select(-starts_with("households_", ))
    
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
      dplyr::select(country, admin_level_1, date_max)
    
    custom_tot <- merge(custom_tot, test, by=c("country", "admin_level_1"), all.x=T)
    custom_tot <- custom_tot %>%
      mutate(date_max = ifelse(date == date_max, date, NA),
             overall_custom_score_country = ifelse(!is.na(date_max), overall_custom_score_country, NA))
    
    # custom_tot$date_max <- as.Date(as.character(custom_tot$date_max))
    
    custom_final <- custom_tot %>%
      dplyr::select(country, admin_level_1, year, date, month, year, starts_with("theme_"), ends_with("_country"))
    
    
  })
  
  output$surgo_map <- renderLeaflet({
    
  
    if(input$in_sources_osrc == "Surgo Foundation"){
      
      scores_sf <- vul_scores()
      
      # I created a palette for the legend so we can have the very high column display first even though it's the last quantile
      # done to match surgo dashboard
      
      pal <- colorQuantile(palette = "YlOrRd", domain = scores_sf$pal, n = 5) 
      pal_leg <- colorQuantile(palette = "YlOrRd", domain = scores_sf$pal, n = 5, rev=T) 
      labs <- c('Very High', "High", "Moderate", "Low", "Very Low")
      
      if (input$in_splevel_osrc == "Within countries") {
        pop <- paste(paste0('<strong>', scores_sf$name_1, ", ",scores_sf$country), '</strong>', hr(),
                     paste0('<strong>', "Overall CCVI: ",scores_sf$ccvi_compared_to_same_country), '</strong>', br(),
                     paste0("Socioeconomic: ", '<strong>',scores_sf$theme_1_compared_to_same_country), '</strong>', br(),
                     paste0("Population Density: ", '<strong>',scores_sf$theme_2_compared_to_same_country), '</strong>', br(),
                     paste0("Transport Availability and Housing: ", '<strong>',scores_sf$theme_3_compared_to_same_country), '</strong>', br(),
                     paste0("Epidemiological: ", '<strong>',scores_sf$theme_4_compared_to_same_country), '</strong>', br(),
                     paste0("Health System: ", '<strong>',scores_sf$theme_5_compared_to_same_country), '</strong>', br(),
                     paste0("Fragility: ", '<strong>',scores_sf$theme_6_compared_to_same_country), '</strong>', br(),
                     paste0("Age: ", '<strong>',scores_sf$theme_7_compared_to_same_country), '</strong>', br())
      } else {
        pop <- paste(paste0('<strong>', scores_sf$name_1, ", ",scores_sf$country), '</strong>', hr(),
                     paste0('<strong>', "Overall CCVI: ",scores_sf$ccvi_compared_to_continent), '</strong>', br(),
                     paste0("Socioeconomic: ", '<strong>',scores_sf$theme_1_compared_to_continent), '</strong>', br(),
                     paste0("Population Density: ", '<strong>',scores_sf$theme_2_compared_to_continent), '</strong>', br(),
                     paste0("Transport Availability and Housing: ", '<strong>',scores_sf$theme_3_compared_to_continent), '</strong>', br(),
                     paste0("Epidemiological: ", '<strong>',scores_sf$theme_4_compared_to_continent), '</strong>', br(),
                     paste0("Health System: ", '<strong>',scores_sf$theme_5_compared_to_continent), '</strong>', br(),
                     paste0("Fragility: ", '<strong>',scores_sf$theme_6_compared_to_continent), '</strong>', br(),
                     paste0("Age: ", '<strong>',scores_sf$theme_7_compared_to_continent), '</strong>', br())
      }
      
      sf <- st_as_sf(scores_sf)
      
      sf %>%
        st_transform(crs = "+init=epsg:4326") %>%
        leaflet(width = "100%") %>%
        addMapPane(name = "background", zIndex = 410) %>%
        addMapPane(name = "polygons", zIndex = 420) %>%
        addMapPane(name = "labels", zIndex = 430) %>%
        addProviderTiles(provider = "CartoDB.Positron",
                         options = pathOptions(pane = "background")) %>%
        addPolygons(popup = pop,
                    stroke = TRUE,
                    weight = .85,
                    smoothFactor = 0,
                    color= "lightgray",
                    opacity = 1,
                    fillColor = ~ pal(pal),
                    fillOpacity = 0.7,
                    options = pathOptions(pane = "polygons")) %>%
        addLegend("bottomright", pal = pal_leg, values = ~pal, 
                  title = "Surgo COVID-19<br>Vulnerability Index", 
                  labFormat = function(type, cuts, p) {
                    paste0(labs)
                  }, opacity = .7) %>%
        addProviderTiles(providers$CartoDB.PositronOnlyLabels,
                         options = pathOptions(pane = "labels"))
    }
    # else{
    #   custom_final <-  custom_final()
    #   
    #   s1_country <- s1 %>%
    #     filter(country == input$ctry)
    #   
    #   custom_final <- data.frame(lapply(custom_final, function(x) {gsub("NaN", NA, x)})) # setting NaNs to NA
    #   custom_final[,-c(1:5)] <- data.frame(lapply(custom_final[,-c(1:5)], function(x) as.numeric(as.character(x))))
    #   
    #   ## long version - to get descriptions
    #   custom_final_long <- custom_final %>%
    #     group_by(country, admin_level_1, date, month, year) %>%
    #     pivot_longer(cols=c(starts_with("mal_"), starts_with("mob_"), starts_with("pop_"),
    #                         contains("washing"), starts_with("overall"), starts_with("theme")),
    #                  names_to = "variable", values_to = "value") %>%
    #     mutate(value = ifelse(grepl("NaN", value), NA, value)) %>%
    #     mutate(value = as.numeric(value)) %>%
    #     ungroup() 
    #   
    #   custom_final_long <- custom_final_long %>% # removes all NA values ... idk what to do about this
    #     filter(!is.na(value)) %>%
    #     group_by(country, admin_level_1, variable) %>%
    #     slice(which.max(as.Date(date))) 
    #   
    #   testing <- custom_final_long %>%
    #     mutate(value = round(value, 3)) %>% # change
    #     mutate(month_name = month.abb[as.numeric(month)]) %>%
    #     mutate(desc = capitalize(variable)) %>%
    #     mutate(desc = gsub("_", " ", desc)) %>%
    #     mutate(desc = gsub("country", "", desc)) %>%
    #     mutate(desc = gsub("Pop perc", "Population -", desc)) %>%
    #     mutate(desc = gsub("Mob", "Mobility -", desc)) %>%
    #     mutate(desc = gsub("Population living" , "Hand-washing - Population living", desc)) %>%
    #     mutate(desc = gsub("^Mal" , "Malaria -", desc)) %>% 
    #     mutate(pop = paste0(desc, " : ", '<strong>', value, '</strong>', " (", month_name, " ", year, ")")) %>%
    #     mutate(pop = gsub("  :" , " :", pop))
    #   
    #   desc <- testing %>% 
    #     dplyr::select(-value, -desc, -month_name) %>%
    #     mutate(variable = paste0(variable, "_desc")) %>%
    #     pivot_wider(names_from = variable, values_from = pop) %>%
    #     dplyr::select(-starts_with("overall"), -starts_with("theme"))
    #   
    #   desc_long <- desc %>%
    #     group_by(country, admin_level_1, date, month, year) %>%
    #     pivot_longer(cols=c(starts_with("mal_"), starts_with("mob_"), starts_with("pop_"),
    #                         contains("washing"), starts_with("overall"), starts_with("theme")),
    #                  names_to = "variable", values_to = "value") %>%
    #     mutate(value = ifelse(grepl("NaN", value), NA, value)) %>%
    #     ungroup()
    #   
    #   desc_test <- desc_long %>%
    #     filter(!is.na(value)) %>%
    #     group_by(country, admin_level_1, variable) %>%
    #     dplyr::select(-c(date, month, year)) %>%
    #     pivot_wider(names_from = variable, values_from = value)
    #   
    #   custom_final <- custom_final %>%  
    #     group_by(country, admin_level_1) %>%
    #     summarize_if(is.numeric, mean, na.rm=T)
    #   
    #   custom_final <- data.frame(lapply(custom_final, function(x) {gsub("NaN", NA, x)})) # setting NaNs to NA
    #   custom_final[,-c(1:5)] <- data.frame(lapply(custom_final[,-c(1:5)], function(x) as.numeric(as.character(x))))
    #   
    #   # custom_final <- cbind(custom_final, desc[,-c(1:5)])
    #   custom_final_desc <- merge(custom_final, desc_test, all=T, by = c("country", "admin_level_1"))
    # 
    #   # adding descriptions prior to this country-filtered merge 
    #   vars_1 <- names(custom_final_desc)[grepl("_desc", names(custom_final_desc))]
    # 
    #   for (i in 1:length(vars_1)){
    #     custom_final_desc[,vars_1[i]] <-  ifelse(is.na(custom_final_desc[,vars_1[i]]),
    #                                              paste0(levels(as.factor(sub("\\ :.*", "", custom_final_desc[,vars_1[i]])))[1],  ' : <strong> NA </strong>'),
    #                                              custom_final_desc[,vars_1[i]])
    #   }
    # 
    #   scores_custom <- merge(custom_final_desc, s1_country, all.y=T, by = c("country", "admin_level_1"))
    #   scores_custom <- scores_custom %>%
    #     mutate(overall_custom_score_country = round(overall_custom_score_country, 3)) # change
    #   
    #   scores_custom_long <- merge(custom_final_long, s1_country, all.y=T, by = c("country", "admin_level_1"))
    #   scores_custom_long <- scores_custom_long %>%
    #     mutate(value = round(value, 3)) # for some reason rounding to 2 makes 3 digits appear in plot
    #   
    #   sf_custom <- st_as_sf(scores_custom) 
    #   sf_custom_long <- st_as_sf(scores_custom_long)
    #   
    #   pal_test <- colorQuantile(palette = "YlOrRd", domain = 0:1, n = 5, rev=T) 
    #   pal <- colorQuantile(palette = "YlOrRd", domain = sf_custom$overall_custom_score_country, n = 5) 
    #   pal_leg <- colorQuantile(palette = "YlOrRd", domain = sf_custom$overall_custom_score_country, n = 5, rev=T) 
    #   labs <- c('Very High', "High", "Moderate", "Low", "Very Low")
    #   pop <- paste(paste0('<strong>', sf_custom$admin_level_1, ", ", sf_custom$country), '</strong>', hr(),
    #                paste0('<strong>', "Overall Score: ", sf_custom$overall_custom_score_country), '</strong>', br())
    #   
    #   vars_1 <- names(scores_custom)[grepl("_desc", names(scores_custom))]
    # 
    #   for (i in 1:length(vars_1)){
    #     scores_custom[,vars_1[i]] <-  ifelse(is.na(scores_custom[,vars_1[i]]),
    #                                          paste0(levels(as.factor(sub("\\ :.*", "", scores_custom[,vars_1[i]])))[1],  ' : <strong> NA </strong>'),
    #                                          scores_custom[,vars_1[i]])
    #   }
    #   
    #   vars_test1 <- paste0(input$in_indicator_inctry, "_country_desc")
    #   
    #   for (i in 1:length(vars_test1)){
    #     pop <- paste0(pop, scores_custom[,vars_test1[i]], br())
    #   }
    # 
    #   if(length(unique(sf_custom$overall_custom_score_country)) == 1){
    #     sf_custom %>%
    #       st_transform(crs = "+init=epsg:4326") %>%
    #       leaflet(width = "100%") %>%
    #       addMapPane(name = "background", zIndex = 410) %>%
    #       addMapPane(name = "polygons", zIndex = 420) %>%
    #       addMapPane(name = "labels", zIndex = 430) %>%
    #       addProviderTiles(provider = "CartoDB.Positron",
    #                        options = pathOptions(pane = "background"))%>%
    #       addPolygons(popup= pop,
    #                   stroke = TRUE,
    #                   weight = .85,
    #                   smoothFactor = 0,
    #                   color= "lightgray",
    #                   opacity = 1,
    #                   fillColor = "#FFFFB2",
    #                   fillOpacity = 0.65,
    #                   options = pathOptions(pane = "polygons")) %>% 
    #       addLegend("bottomright", pal = pal_test, values = 0:1, 
    #                 title = "Custom COVID-19<br>Vulnerability Index", 
    #                 labFormat = function(type, cuts, p) {
    #                   paste0(labs)
    #                 }, opacity = .7) %>%
    #       addProviderTiles(providers$CartoDB.PositronOnlyLabels,
    #                        options = pathOptions(pane = "labels"))
    #   } else{
    #     sf_custom %>%
    #       st_transform(crs = "+init=epsg:4326") %>%
    #       leaflet(width = "100%") %>%
    #       addMapPane(name = "background", zIndex = 410) %>%
    #       addMapPane(name = "polygons", zIndex = 420) %>%
    #       addMapPane(name = "labels", zIndex = 430) %>%
    #       addProviderTiles(provider = "CartoDB.Positron",
    #                        options = pathOptions(pane = "background"))%>%
    #       addPolygons(popup= pop,
    #                   stroke = TRUE,
    #                   weight = .85,
    #                   smoothFactor = 0,
    #                   color= "lightgray",
    #                   opacity = 1,
    #                   fillColor = ~ pal(overall_custom_score_country),
    #                   fillOpacity = 0.65,
    #                   options = pathOptions(pane = "polygons")) %>% 
    #       addLegend("bottomright", pal = pal_leg, values = ~overall_custom_score_country, 
    #                 title = "Custom COVID-19<br>Vulnerability Index", 
    #                 labFormat = function(type, cuts, p) {
    #                   paste0(labs)
    #                 }, opacity = .7) %>%
    #       addProviderTiles(providers$CartoDB.PositronOnlyLabels,
    #                        options = pathOptions(pane = "labels"))
    #   }      
    #   }
    else{
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(19, 4, zoom =3)
    }
  })
  
  output$custom_scores_inctry <- renderLeaflet({
    
      custom_final <-  custom_final()
      
      req(input$in_indicator_inctry_custom) # prevent pre-load error message

      s1_country <- s1 %>%
        filter(country == input$in_ctry_covid_inctry) # change

      custom_final <- data.frame(lapply(custom_final, function(x) {gsub("NaN", NA, x)})) # setting NaNs to NA
      custom_final[,-c(1:5)] <- data.frame(lapply(custom_final[,-c(1:5)], function(x) as.numeric(as.character(x))))

      ## long version - to get descriptions
      custom_final_long <- custom_final %>%
        group_by(country, admin_level_1, date, month, year) %>%
        pivot_longer(cols=c(starts_with("mal_"), starts_with("mob_"), starts_with("pop_"),
                            contains("washing"), starts_with("overall"), starts_with("theme")),
                     names_to = "variable", values_to = "value") %>%
        mutate(value = ifelse(grepl("NaN", value), NA, value)) %>%
        mutate(value = as.numeric(value)) %>%
        ungroup()

      custom_final_long <- custom_final_long %>% # removes all NA values ... idk what to do about this
        filter(!is.na(value)) %>%
        group_by(country, admin_level_1, variable) %>%
        slice(which.max(as.Date(date)))

      testing <- custom_final_long %>%
        mutate(value = round(value, 3)) %>% # change
        mutate(month_name = month.abb[as.numeric(month)]) %>%
        mutate(desc = capitalize(variable)) %>%
        mutate(desc = gsub("_", " ", desc)) %>%
        mutate(desc = gsub("country", "", desc)) %>%
        mutate(desc = gsub("Pop perc", "Population -", desc)) %>%
        mutate(desc = gsub("Mob", "Mobility -", desc)) %>%
        mutate(desc = gsub("Population living" , "Hand-washing - Population living", desc)) %>%
        mutate(desc = gsub("^Mal" , "Malaria -", desc)) %>%
        mutate(desc = gsub(" 20 $" , "", desc)) %>%
        mutate(desc = gsub(" cont $" , "", desc)) %>%
        mutate(desc = gsub("health workers" , "Health workers", desc)) %>%
        mutate(desc = gsub("new consultation all cause" , "Annual new consultations", desc)) %>%
        mutate(pop = ifelse(grepl('^Malaria', desc), paste0(desc, " : ", '<strong>', value, '</strong>'), 
                            paste0(desc, " : ", '<strong>', value, '</strong>', " (", month_name, " ", year, ")"))) %>% # AZ change
        mutate(pop = gsub("  :" , " :", pop))

      desc <- testing %>%
        dplyr::select(-value, -desc, -month_name) %>%
        mutate(variable = paste0(variable, "_desc")) %>%
        pivot_wider(names_from = variable, values_from = pop) %>%
        dplyr::select(-starts_with("overall"), -starts_with("theme"))

      desc_long <- desc %>%
        group_by(country, admin_level_1, date, month, year) %>%
        pivot_longer(cols=c(starts_with("mal_"), starts_with("mob_"), starts_with("pop_"),
                            contains("washing"), starts_with("overall"), starts_with("theme")),
                     names_to = "variable", values_to = "value") %>%
        mutate(value = ifelse(grepl("NaN", value), NA, value)) %>%
        ungroup()

      desc_test <- desc_long %>%
        filter(!is.na(value)) %>%
        group_by(country, admin_level_1, variable) %>%
        dplyr::select(-c(date, month, year)) %>%
        pivot_wider(names_from = variable, values_from = value)

      custom_final <- custom_final %>%
        group_by(country, admin_level_1) %>%
        summarize_if(is.numeric, mean, na.rm=T)

      custom_final <- data.frame(lapply(custom_final, function(x) {gsub("NaN", NA, x)})) # setting NaNs to NA
      custom_final[,-c(1:5)] <- data.frame(lapply(custom_final[,-c(1:5)], function(x) as.numeric(as.character(x))))

      # custom_final <- cbind(custom_final, desc[,-c(1:5)])
      custom_final_desc <- merge(custom_final, desc_test, all=T, by = c("country", "admin_level_1"))

      # adding descriptions prior to this country-filtered merge
      vars_1 <- names(custom_final_desc)[grepl("_desc", names(custom_final_desc))]

      for (i in 1:length(vars_1)){
        custom_final_desc[,vars_1[i]] <-  ifelse(is.na(custom_final_desc[,vars_1[i]]),
                                                 paste0(levels(as.factor(sub("\\ :.*", "", custom_final_desc[,vars_1[i]])))[1],  ' : <strong> NA </strong>'),
                                                 custom_final_desc[,vars_1[i]])
      }

      scores_custom <- merge(custom_final_desc, s1_country, all.y=T, by = c("country", "admin_level_1"))
      
      scores_custom <- scores_custom %>%
        mutate(overall_custom_score_country = round(overall_custom_score_country, 3))  # change

      if(sum(grepl("mal_", vars_1))>0){
        for (i in 1:length(vars_1)) {
          scores_custom$test <-  ifelse(is.na(scores_custom[,vars_1[i]]),
                                        paste0(levels(as.factor(sub("\\ :.*", "", custom_final_desc[,vars_1[i]])))[1],  ' : <strong> NA </strong>'),
                                        scores_custom[,vars_1[i]])

          scores_custom$test2 <- ifelse(grepl("^mal_", vars_1[i]), ifelse(grepl("_20_", vars_1[i]), " (2020)",
                                 ifelse(scores_custom$country %in% custom_mal_ctry_18, " (2018)", " (2019)")),
                                "")
          
          scores_custom[,vars_1[i]] <- paste(scores_custom$test, scores_custom$test2)
          
        }
      }
      
      scores_custom <- scores_custom %>%
        mutate(overall_custom_score_country = round(overall_custom_score_country, 3)) # change

      scores_custom_long <- merge(custom_final_long, s1_country, all.y=T, by = c("country", "admin_level_1"))
      scores_custom_long <- scores_custom_long %>%
        mutate(value = round(value, 3)) # for some reason rounding to 2 makes 3 digits appear in plot

      sf_custom <- st_as_sf(scores_custom)
      sf_custom_long <- st_as_sf(scores_custom_long)

      pal_test <- colorQuantile(palette = "YlOrRd", domain = 0:1, n = 5, rev=T)
      pal <- colorQuantile(palette = "YlOrRd", domain = sf_custom$overall_custom_score_country, n = 5)
      pal_leg <- colorQuantile(palette = "YlOrRd", domain = sf_custom$overall_custom_score_country, n = 5, rev=T)
      labs <- c('Very High', "High", "Moderate", "Low", "Very Low")
      pop <- paste(paste0('<strong>', sf_custom$admin_level_1, ", ", sf_custom$country), '</strong>', hr(),
                   paste0('<strong>', "Overall Score: ", sf_custom$overall_custom_score_country), '</strong>', br())

      vars_1 <- names(scores_custom)[grepl("_desc", names(scores_custom))]

      for (i in 1:length(vars_1)){
        scores_custom[,vars_1[i]] <-  ifelse(is.na(scores_custom[,vars_1[i]]),
                                             paste0(levels(as.factor(sub("\\ :.*", "", scores_custom[,vars_1[i]])))[1],  ' : <strong> NA </strong>'),
                                             scores_custom[,vars_1[i]])
      }

      vars_test1 <- paste0(input$in_indicator_inctry_custom, "_country_desc")

      for (i in 1:length(vars_test1)){
        pop <- paste0(pop, scores_custom[,vars_test1[i]], br())
      }

      if(length(unique(sf_custom$overall_custom_score_country)) == 1){
        if(is.na(sf_custom$overall_custom_score_country)){
          sf_custom %>%
            st_transform(crs = "+init=epsg:4326") %>%
            leaflet(width = "100%") %>%
            addMapPane(name = "background", zIndex = 410) %>%
            addMapPane(name = "polygons", zIndex = 420) %>%
            addMapPane(name = "labels", zIndex = 430) %>%
            addProviderTiles(provider = "CartoDB.Positron",
                             options = pathOptions(pane = "background"))%>%
            addPolygons(popup= pop,
                        stroke = TRUE,
                        weight = .85,
                        smoothFactor = 0,
                        color= "lightgray",
                        opacity = 1,
                        fillColor = "darkgray",
                        fillOpacity = 0.8,
                        options = pathOptions(pane = "polygons")) %>%
            addLegend("bottomright", pal = pal_test, values = 0:1,
                      title = "Custom COVID-19<br>Vulnerability Index",
                      labFormat = function(type, cuts, p) {
                        paste0(labs)
                      }, opacity = .7) %>%
            addProviderTiles(providers$CartoDB.PositronOnlyLabels,
                             options = pathOptions(pane = "labels"))
        } else {
          sf_custom %>%
            st_transform(crs = "+init=epsg:4326") %>%
            leaflet(width = "100%") %>%
            addMapPane(name = "background", zIndex = 410) %>%
            addMapPane(name = "polygons", zIndex = 420) %>%
            addMapPane(name = "labels", zIndex = 430) %>%
            addProviderTiles(provider = "CartoDB.Positron",
                             options = pathOptions(pane = "background"))%>%
            addPolygons(popup= pop,
                        stroke = TRUE,
                        weight = .85,
                        smoothFactor = 0,
                        color= "lightgray",
                        opacity = 1,
                        fillColor = "#FFFFB2",
                        fillOpacity = 0.65,
                        options = pathOptions(pane = "polygons")) %>%
            addLegend("bottomright", pal = pal_test, values = 0:1,
                      title = "Custom COVID-19<br>Vulnerability Index",
                      labFormat = function(type, cuts, p) {
                        paste0(labs)
                      }, opacity = .7) %>%
            addProviderTiles(providers$CartoDB.PositronOnlyLabels,
                             options = pathOptions(pane = "labels"))
          
        }
        
      } else{
        sf_custom %>%
          st_transform(crs = "+init=epsg:4326") %>%
          leaflet(width = "100%") %>%
          addMapPane(name = "background", zIndex = 410) %>%
          addMapPane(name = "polygons", zIndex = 420) %>%
          addMapPane(name = "labels", zIndex = 430) %>%
          addProviderTiles(provider = "CartoDB.Positron",
                           options = pathOptions(pane = "background"))%>%
          addPolygons(popup= pop,
                      stroke = TRUE,
                      weight = .85,
                      smoothFactor = 0,
                      color= "lightgray",
                      opacity = 1,
                      fillColor = ~ pal(overall_custom_score_country),
                      fillOpacity = 0.65,
                      options = pathOptions(pane = "polygons")) %>%
          addLegend("bottomright", pal = pal_leg, values = ~overall_custom_score_country,
                    title = "Custom COVID-19<br>Vulnerability Index",
                    labFormat = function(type, cuts, p) {
                      paste0(labs)
                    }, opacity = .7) %>%
          addProviderTiles(providers$CartoDB.PositronOnlyLabels,
                           options = pathOptions(pane = "labels"))
      }
  })
  
} # end of server