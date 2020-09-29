server <- function(input, output, session){
  
  
  #------------------------------------------------------------------------------------------------
  # INFO TAB
  #------------------------------------------------------------------------------------------------
  
  observeEvent(input$in_go_info, {
    
    if(input$in_go_info %in% "about") {
      
      output$out_info <- renderUI({includeMarkdown("info_files/about.Rmd")})
      
    } else if(input$in_go_info %in% "start") {
      
      output$out_info <- renderUI(NULL)
      
    } else  if(input$in_go_info %in% "malaria"){
      
      output$out_info <- renderUI({includeMarkdown("info_files/malaria_data.Rmd")})
    } else if(input$in_go_info  %in% "covid"){
      output$out_info <- renderUI({includeMarkdown("info_files/covid_data.Rmd")})
    } else if(input$in_go_info %in% "methods"){
      output$out_info <- renderUI({includeMarkdown("info_files/methods.Rmd")})
    } else {
      output$out_info <- renderUI(NULL)
    }
    
    
    
  })
  
  
  
  # --------------------------------------------------------------------------------
  #
  # MALARIA TAB
  #
  # --------------------------------------------------------------------------------
  
  screen_dim <- reactive({
    return(as.numeric(input$dimension))
  })
  
  snapshot_region <- reactive({
    return(input$in_region_snapshot)
  })
  
  
  ## MALARIA SNAPSHOT TAB PANEL 
  
  ## Malaria snapshot: dynamic main panel UI  --------------------------------------
  
  output$out_snapshot_panel <- renderUI({
    
    myregion <- snapshot_region()
    
    ### multiple countries view ----------
    
    if (snapshot_region() %in% region_list) {
      
      fluidPage(style = "margin-left:20px;",
                
        fluidRow(column(6, # offset = 0.75,
                        HTML("Where have malaria-related indicators been increasing or decreasing as compared to the selected baseline year?<br/>
                             <i>(select indicator to map from the sidebar panel)</i><br/>&nbsp")),
                 column(6, style = 'padding-left:30px;',
                        HTML("[Google mobility map placeholder]")
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
    
    ### one country view ------------------
      
    } else {
      
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
                        HTML("Placeholder for mobility data plot(map?)"))),
        fluidRow(column(6, htmlOutput("out_txt_maptitle_snapshot")),
                 column(6, style = 'padding-left:30px;', htmlOutput("out_txt_mobilitymaptitle_snapshot"))),
        fluidRow(column(6, style = 'padding-right:20px;', leafletOutput("out_map_snapshot")),
                 column(6, style = 'padding-left:30px;',leafletOutput("out_mobilitymap_snapshot"))),
        br(),
        br(),
        # hr(),
        fluidRow(column(12, style = "padding-bottom:2px", 
                        HTML("<br/>Which region observed the largest decrease/increase in each malaria indicators?"))),
        fluidRow(column(12, style = "padding-bottom:5px", htmlOutput("out_title_malranking_snapshot"))),
        fluidRow(column(12, plotOutput(outputId = "out_adm1malranking_snapshot"))))
    }
  })

  
  


  ## Malaria snapshot: text and titles output ---------------------------------------------------
  
  ### map title
  output$out_txt_maptitle_snapshot <- renderUI({
    txt <- paste("<h5><b>Percentage changes for ", indicator_labels[[input$in_indicator_snapshot]],
                 " in ", input$in_yr0m0_snap, "-", input$in_yr0mf_snap, " 2020 ",
                 "(from ", input$in_yr0_snap, ")</b></h5>",sep = "")
    HTML(txt)
  })
  
  
  output$out_title_malranking_snapshot <- renderUI({
    txt <- paste("<h5><b>Percentage changes for all indicators ",
                 " in ", input$in_yr0m0_snap, "-", input$in_yr0mf_snap, " 2020 ",
                 "(from ", input$in_yr0_snap, ")</b></h5>",sep = "")
    HTML(txt)
  })
  
  
  output$out_txt_mobilitymaptitle_snapshot <- renderUI({
    txt <- paste("<h5><b>Mean changes across Google's mobility categories ",
                 " in ", input$in_yr0m0_snap, "-", input$in_yr0mf_snap, " 2020 ",
                 "(from baseline: Jan 3 - Feb 6, 2020)</b></h5>",sep = "")
    HTML(txt)
  })
  
  
  ## Malaria snapshot: graphs and maps outputs -------------------------------------------------------
  
  ### Malaria snapshot: map for indicator percentage changes
  
  #/ shapefile input data to the map changes based on inputs
  get_mymap <- reactive({
    
    # data frame and shapefile selection (whether country-level or subnational-level)
    if (input$in_region_snapshot %in% region_list) {
      mapdat <- xctry; sdat <- s0 
    } else {
      mapdat <- xprov; sdat <- s1
    }
    
    # use covid deaths if indicator is malaria deaths and set legend appropriately
    if (input$in_indicator_snapshot %in% "malaria_deaths"){
      covid_str <- "covid_deaths"
      covid_legend <- "COVID-19 Cumulative Deaths"
    } else {
      covid_str <- "covid_cases"
      covid_legend <- "COVID-19 Cumulative Cases"
    }
    
    xmap <- filter_region_snap(mapdat, input$in_region_snapshot) %>%
      filter(count_type %in% input$in_yaxs_snap) %>%
      get_shpdat_snap(., input$in_indicator_snapshot, sdat, input$in_yr0_snap,
                      input$in_yr0m0_snap, input$in_yr0mf_snap, covid_str)
    
    minval <- min(-100, min(xmap$delta, na.rm = TRUE), na.rm = TRUE)
    maxval <- max(100, max(xmap$delta, na.rm = TRUE), na.rm = TRUE)
    
    map_pal <- colorBin(delta_map_clr, domain = xmap$delta,
                    bins = c(minval, -50, -25, -10, 0, 10, 25, 50, maxval))
    
    pts_covid <- subset(xmap, !(is.na(covid_cumu_scaled)) | (covid_cumu_scaled > 0))
    
    view0 <- get_latlon0(input$in_region_snapshot, xmap)
    
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
    
    return(mymap)
  })
  
  
  
  output$out_map_snapshot <- renderLeaflet({get_mymap()})
  
  
  ### Malaria snapshot: map for mobility
  
  
  get_mobilitymap <- reactive({
    
    m0 <- which(month.abb %in% input$in_yr0m0_snap)
    mf <- which(month.abb %in% input$in_yr0mf_snap)
    
    # data frame and shapefile selection (whether country-level or subnational-level)
    if (input$in_region_snapshot %in% region_list) {
      mobdat <- xmob %>% filter(admin_level_1 %in% "") 
      sdat <- s0; covdat <- xctry 
    } else {
      mobdat <- xmob %>% filter(!(admin_level_1 %in% ""))
      sdat <- s1; covdat <- xprov
    }
    
    # use covid deaths if indicator is malaria deaths and set legend appropriately
    if (input$in_indicator_snapshot %in% "malaria_deaths"){
      covid_str <- "covid_deaths"
      covid_legend <- "COVID-19 Cumulative Deaths"
    } else {
      covid_str <- "covid_cases"
      covid_legend <- "COVID-19 Cumulative Cases"
    }
    
    mobdat <- filter_region_snap(mobdat, input$in_region_snapshot) %>%
      ungroup() %>%
      filter((month >= m0) & (month <=  mf) & (year == 2020)) %>%
      select(-c("date", "month", "year")) %>%
      group_by(country, admin_level_1) %>%
      summarise_all(mean, na.rm = TRUE) %>%
      ungroup() %>% rowwise() %>%
      mutate(allbuthome = mean(c(grocery_and_pharmacy, retail_and_recreation, transit_stations, workplaces, parks)))
      
    
    covdat <- filter_region_snap(covdat, input$in_region_snapshot) %>%
      ungroup() %>%
      filter((variable %in% covid_str) & (month >= m0) & (month <= mf) & (year == 2020)) %>%
      filter(count_type %in% input$in_yaxs_snap) %>%
      select(-c(contains("group"), "count_type", "info", "info_txt")) %>%
      distinct()
    
    if("admin_level_1" %in% colnames(covdat)){
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
    
    if(input$in_yaxs_snap %in% "value_rate"){
      min_covid <- 1; max_covid <- 10
      new_value <- rescale(cmdat$covid_cumu, to = c(min_covid, max_covid))
      new_value[(cmdat$covid_cumu == 0) | (is.na(cmdat$covid_cumu))] <- NA
      if (str_detect(input$in_indicator_snapshot, "deaths")) {
        csuff2 <- "per 1M people"
      } else if (str_detect(input$in_indicator_snapshot, "anc|tpr", negate = TRUE)) {
        csuff2 <- "per 1K people"
      } else {
        csuff2 <- NULL
      }
    } else {
      new_value <- log(cmdat$covid_cumu)
      new_value[is.infinite(new_value)] <- NA
      csuff2 <- NULL
    }
    cmdat$covid_cumu_scaled <- new_value
    
    if( covid_str %in% "covid_deaths") {
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
  })

  
  
  
  
  output$out_mobilitymap_snapshot <- renderLeaflet(get_mobilitymap())
  
  



  ## Multi-country indicator ranking barplot -------------

  output$out_malranking_snapshot <- renderPlot({

    xbar0 <- filter_region_snap(xctry, input$in_region_snapshot) %>%
      filter(count_type %in% input$in_yaxs_snap)

    xbar <- NULL
    for (i in mal_vars) {
      xi <-  get_shpdat_snap(xbar0, i, s0, input$in_yr0_snap, input$in_yr0m0_snap, input$in_yr0mf_snap, "covid_cases") %>%
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

    print(ggbar0)

  })




  # output$out_donut_snapshot <- renderPlot({
  #   plot(runif(10), runif(10))
  # })


  ## One country national level title ---------------------
  output$out_txt_adm0title_snapshot <- renderUI({
    txt <- paste("<h4>", input$in_region_snapshot, " - National Level</h4><br/>", sep = "")
    HTML(txt)
  })

  ## One country subnational level title ---------------------
  output$out_txt_adm1title_snapshot <- renderUI({
    txt <- paste("<h4>", input$in_region_snapshot, " - Subnational Level</h4><br/>", sep = "")
    HTML(txt)
  })

  ## One country title for national-level indicator ranking plot  ---------------------
  output$out_plottitle_adm0indicator_snapshot <- renderUI({
    txt <- paste("<h5><b>Percentage changes in ", input$in_yr0m0_snap, "-", input$in_yr0mf_snap,
                 " 2020 (from ", input$in_yr0_snap, ")</b></h5>", sep = "")
    HTML(txt)
  })

  ## One country title for national-level reporting rate plot ---------------------------
  output$out_plottitle_adm0reporting_snapshot <- renderUI({
    txt <- paste("<h5><b>Average monthly reporting rate between ", input$in_yr0m0_snap, "-",
                 input$in_yr0mf_snap, "</b></h5>", sep = "")
    HTML(txt)
  })



  ## One country national-level indicator plot

  output$out_adm0indicator_snapshot <- renderPlot({
    
    if(input$in_region_snapshot %in% region_list){
      
      return(NULL)
      
    } else {
      xbar1 <- filter_region_snap(xctry, input$in_region_snapshot) %>%
        filter(count_type %in% input$in_yaxs_snap)
      
      xb1 <- data.frame()
      for(i in mal_vars){
        bi <- as.data.frame(get_shpdat_snap(xbar1, i, s0, input$in_yr0_snap,
                                            input$in_yr0m0_snap, input$in_yr0mf_snap, "covid_cases"))
        if(nrow(bi) > 0) {
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
        
        
        ggbar1 <- ggplot(xb1, aes(x = var_label, y = delta)) +
          geom_bar(aes(fill = delta_sign), stat = "identity") +
          coord_flip() +
          theme_few(12) +
          scale_fill_manual(values = snap_bar_clr, drop = FALSE) +
          scale_y_continuous(labels = percent) +
          xlab("") + ylab("Percentage change") +
          theme(legend.position = "none")
        
        
      } else {
        ggbar1 <- ggplot(data = data.frame(x = c(0,1), y = c(0,1)), aes(x = x, y = y)) +
          geom_text(aes(x = 0.5, y = 0.5, label = "Data unavailable")) +
          theme_few(12) + xlab("") + ylab("")
      }
      
      print(ggbar1)
    }

     
  })


  ## One country national-level reporting rate plot

  output$out_adm0reporting_snapshot <- renderPlot({
    
    gg_rr <- get_rrplot_snap(dat = xctry, m0 = input$in_yr0m0_snap, mf = input$in_yr0mf_snap,
                             yr0 = input$in_yr0_snap, region = input$in_region_snapshot, 
                             value_type = input$in_yaxs_snap)
    
    print(gg_rr)

    # m0 <- which(month.abb %in% input$in_yr0m0_snap)
    # mf <- which(month.abb %in% input$in_yr0mf_snap)
    # 
    # tt0 <- seq(as.Date(paste(2020, "-", m0, "-01", sep = "")), 
    #            as.Date(paste(2020, "-", mf, "-01", sep = "")), by = "1 month")
    # ttf <- seq(as.Date(paste(input$in_yr0_snap, "-", m0, "-01", sep = "")), 
    #            as.Date(paste(input$in_yr0_snap, "-", mf, "-01", sep = "")), by = "1 month")
    # 
    # 
    # xrr1 <- filter_region_snap(xctry, input$in_region_snapshot) %>%
    #   filter(count_type %in% input$in_yaxs_snap) %>%
    #   filter(str_detect(variable, "reports")) %>%
    #   filter( date %in% c(tt0, ttf) ) %>%
    #   select(year, variable, info) %>%
    #   mutate(info = as.numeric(as.character(info))) %>%
    #   group_by(variable, year) %>%
    #   summarise(rr_average = mean(info, na.rm = TRUE)) %>%
    #   mutate(variable = factor(variable, levels = rev(c("reports_rate__allcause_cases", "reports_rate__confirmed_cases", "reports_rate__anc1_visit")),
    #                            labels = rev(c("All Cause Consultations", "Confirmed Cases", "ANC (1st) Visit"))))
    # 
    # rrtitle <- paste("Average monthly reporting rates between\n", input$in_yr0m0_snap, "-", input$in_yr0mf_snap, " 2020")
    # rrmax <- max(max(xrr1$rr_average), 1) 
    # 
    # fill_values <- setNames(c("#9e9ac8", "#ffffff"), c("2020", as.character(input$in_yr0_snap)))
    # clr_values <- setNames(c("#9e9ac8", "#54278f"), c("2020", as.character(input$in_yr0_snap)))
    # 
    # 
    # gg1_adm0report <- ggplot(xrr1, aes(x = rr_average, y = variable, 
    #                                    group = as.factor(year), fill = as.factor(year), colour = as.factor(year))) + 
    #   geom_bar(stat = "identity", position = position_dodge()) +
    #   geom_text(aes(label = label_percent(accuracy = 0.1)(rr_average), group = as.factor(year)), 
    #             position = position_dodge(width = 1), hjust = 1.2, color = "black") + 
    #   geom_vline(xintercept = 1, linetype = "dotted") +
    #   scale_x_continuous(labels = percent) +
    #   theme_few(12) + xlab("Reporting Rate") + ylab("") +
    #   # theme(legend.position = "none")  + 
    #   scale_fill_manual(values = fill_values) + scale_colour_manual(values = clr_values) +
    #   theme(legend.title = element_blank(), legend.position = "top")

    print(gg_rr)

  })



  ## Subnational snapshot map ----------------------------------
  
  

  # output$out_adm1map_snapshot <- renderLeaflet({get_mymap()})
  # output$out_adm1map_snapshot <- renderLeaflet({
  # 
  #   xmap1 <- filter_region_snap(xprov, input$in_region_snapshot) %>%
  #     filter(count_type %in% input$in_yaxs_snap)
  # 
  #   if (input$in_region_snapshot %in% "malaria_deaths"){
  #     covid_str <- "covid_deaths"
  #     covid_legend <- "COVID-19 Cumulative Deaths"
  #   } else {
  #     covid_str <- "covid_cases"
  #     covid_legend <- "COVID-19 Cumulative Cases"
  #   }
  #   covid_str <- if_else(input$in_region_snapshot %in% "malaria_deaths", "covid_deaths", "covid_cases")
  # 
  #   mdat1 <- get_shpdat_snap(xmap1, input$in_indicator_snapshot, s1, input$in_yr0_snap,
  #                            input$in_yr0m0_snap, input$in_yr0mf_snap, covid_str)
  # 
  #   minval1 <- min(-100, min(mdat1$delta, na.rm = TRUE), na.rm = TRUE)
  #   maxval1 <- max(100, max(mdat1$delta, na.rm = TRUE), na.rm = TRUE)
  # 
  #   pal1  <- colorBin(delta_map_clr, domain = mdat1$delta,
  #                             bins = c(minval1, -50, -25, -10, 0, 10, 25, 50, maxval1))
  # 
  #   mdat1_covid <- subset(mdat1, !(is.na(covid_cumu_scaled)) | (covid_cumu_scaled > 0))
  # 
  #   poly_ctr <- subset(s0, country %in% input$in_region_snapshot)
  # 
  #   leaflet(mdat1) %>%
  #     setView(poly_ctr$lon_ctr, poly_ctr$lat_ctr, zoom = 5) %>%
  #     addProviderTiles("CartoDB.Positron")  %>%
  #     addPolygons(color = "#444444", weight = 1, fillOpacity = 0.8,
  #                 fillColor = ~ pal1(delta),
  #                 popup = ~htmlEscape(txt_poly)) 
  #     addCircleMarkers(lng = mdat1_covid$lon_ctr, lat =  mdat1_covid$lat_ctr,
  #                      radius = ~ (covid_cumu_scaled),
  #                      # color = "#ffca05", fill = "#ffca05",
  #                      color = "#a65628", fill = "#a65628",
  #                      fillOpacity = 0.8,
  #                      popup = htmlEscape( mdat1_covid$txt_cov)) %>%
  #     addLegendCustom("bottomleft", colors = "#a65628",
  #                     sizes = 15,
  #                     labels = covid_legend,
  #                     shapes = "circle",
  #                     borders = "#a65628",
  #                     opacity = 0.96) %>%
  #     addLegend("bottomleft", pal = pal1, values = ~ delta,
  #               opacity = 0.8, labFormat = labelFormat(between = " to "),
  #               title = "% Change")
  #   
  #     
  # })
  # 
  # 
  # ## Subnational indicator ranking snapshot plot ----------------
  # 


  output$out_adm1malranking_snapshot <- renderPlot({

    xbar1 <- filter_region_snap(xprov, input$in_region_snapshot) %>%
      filter(count_type %in% input$in_yaxs_snap)

    xb1 <- NULL
    for (i in mal_vars) {
      covid_str <- if_else(mal_vars %in% "malaria_deaths", "covid_deaths", "covid_cases")
      x1i <-  get_shpdat_snap(xbar1, i, s1, input$in_yr0_snap, input$in_yr0m0_snap, input$in_yr0mf_snap, covid_str) %>%
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
             delta = delta/100)

    rank_order <- setNames(as.character(xb1$admin_level_1), xb1$delta_order)


    ggbar1 <- ggplot(xb1, aes(x = delta_order, y = delta, fill = delta_sign)) +
      geom_hline(yintercept = 0) + geom_col() +
      coord_flip() +
      theme_few(12) + xlab("") + ylab("Percentage change") +
      facet_wrap(~variable, scales = "free", drop = TRUE, nrow = 1) +
      scale_x_discrete(labels = rank_order) +
      scale_y_continuous(labels = percent) +
      scale_fill_manual(values = snap_bar_clr) +
      theme(legend.position = "none")

    print(ggbar1)

  })
  
  
  
  ## MALARIA PERCENT TS -----------------------------------------------------------------------
  
  ## Malaria percent ts: dynamic input selector ------------------------------------------------
  
  
  output$out_plottitle_pcnt <- renderUI({
    
    if(input$in_indicator_pcnt %in% "all"){
      txt_indi <- "all malaria indicators"
    } else {
      txt_indi <- tolower(as.vector(indicator_labels[input$in_indicator_pcnt]))
    }
    
    txt <- paste("<h5><b>Monthly difference in ", txt_indi, " from ", input$in_yr0_snap, "</b></h5><br/>&nbsp", sep = "")
    HTML(txt)
  })
  
  observeEvent(input$in_aggr_pcnt, ignoreNULL = FALSE, {
    
    if (input$in_aggr_pcnt %in% "admin0") {
      updateSelectInput(session, inputId = "in_region_pcnt", 
                        label = "Region/Country", 
                        choices = c("All Countries", "Northern Hemisphere", 
                                    "Southern Hemisphere", "Western Africa", 
                                    "Eastern Africa", "Central/Southern Africa",  
                                    "Southeast Asia", "----------------------",
                                    as.character((unique(xctry$country)))),
                        selected = "All Countries")
    } else {
      updateSelectInput(session, inputId = "in_region_pcnt",
                        label = "Country", choices = cc_list)
    }
  })
  
  
  get_adm1name_pcnt <- reactive({
    adm1dat <- subset(xprov, country %in% input$in_region_pcnt)
    return(as.character(unique(adm1dat$admin_level_1)))
  })
  
  # Populate admin level 1 choices or return NULL
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
  
  
  # observe({
  #   
  #   req(input$in_adm1_pcnt)    
  #   if ( ((input$in_aggr_pcnt %in% "admin0") & (input$in_region_pcnt %in% cc_list)) | 
  #        ((input$in_aggr_pcnt %in% "admin1") & !(input$in_adm1_pcnt %in% "View All")) ) {
  #     updateSelectInput(session, inputId = "in_indicator_pcnt", label = "Indicator",
  #                       choices = c("All Indicators" = "all"))
  #   } else {
  #     updateSelectInput(session, inputId = "in_indicator_pcnt", label = "Indicator",
  #                       choices = c("All Cause Consultations" = "allcause_cases", 
  #                                   "Tested Cases" = "tested_cases", 
  #                                   "Malaria Confirmed Cases" = "confirmed_cases",
  #                                   "Test Positivity Ratio" = "tpr", 
  #                                   "Malaria Severe Cases" = "severe_cases", 
  #                                   "Malaria Deaths" = "malaria_deaths", 
  #                                   "ANC (1st) Visit" = "anc1_visit"))
  #   }
  # })
  
  
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
                                      "Test Positivity Ratio" = "tpr", 
                                      "Malaria Severe Cases" = "severe_cases", 
                                      "Malaria Deaths" = "malaria_deaths", 
                                      "ANC (1st) Visit" = "anc1_visit"))
      }
    } else {
      updateSelectInput(session, inputId = "in_indicator_pcnt", label = "Indicator",
                        choices = c("All Cause Consultations" = "allcause_cases", 
                                    "Tested Cases" = "tested_cases", 
                                    "Malaria Confirmed Cases" = "confirmed_cases",
                                    "Test Positivity Ratio" = "tpr", 
                                    "Malaria Severe Cases" = "severe_cases", 
                                    "Malaria Deaths" = "malaria_deaths", 
                                    "ANC (1st) Visit" = "anc1_visit"))
    }
    
    
  })
  
  
  ## Malaria percent ts: data selector ---------------------------------------------
  
  output$out_plot_pcnt <- renderPlot({
    
    if (input$in_aggr_pcnt %in% "admin0") {
      pdat0 <- xctry
    } else {
      pdat0 <- xprov
    }
    
    
    pdat1 <- extract_countries(pdat0, input$in_region_pcnt) %>%
      filter(count_type %in% input$in_epiunit_pcnt)
    
    if(input$in_aggr_pcnt %in% "admin1"){
      pdat1 <- pdat1 %>% filter(country %in% input$in_region_pcnt)
    }
    
    
    if( (input$in_aggr_pcnt %in% "admin1") & (input$in_indicator_pcnt %in% "all") ) {
      pdat1 <- subset(pdat1, admin_level_1 %in% input$in_adm1_pcnt)
    } 


    if(input$in_indicator_pcnt %in% "all"){
      pdat <- data.frame()
      for (j in mal_vars) {
          pj <- get_difference(dat = pdat1, var_name = j, m0 = 1, mf = mo_now, yr0 = input$in_yr0_snap, as_total = FALSE) %>%
            select(-c(starts_with("count_type"), starts_with("value")))
          if(nrow(pj) > 0){
            pj$variable <- indicator_labels[[j]]
            pdat <- rbind(pdat, pj)
          }
       }
      } else {
        pdat <- get_difference(dat = pdat1, var_name = input$in_indicator_pcnt, m0 = 1, mf = mo_now, yr0 = input$in_yr0_snap, as_total = FALSE)
        pdat$variable <- indicator_labels[[input$in_indicator_pcnt]]
      }




    pdat <- pdat %>%
      mutate(boxgroup = variable,
             delta = delta/100)
    
    # Covid data
    # [..........]
    
    # if (input$in_indicator_pcnt %in% "all") {
    #   wrap_var <- enquo(boxgroup)
    #   ncols <- 4
    # } else {
    #   ncols <- 5
    #   if(input$in_region_pcnt %in% "admin0"){
    #     wrap_var <- enquo(country)
    #   } else {
    #     wrap_var <- enquo(admin_level_1)
    #   }
    # }
    
    
    gg_pcnt <- ggplot(pdat, aes(x  = month, y = delta)) +
      geom_col(aes(fill = colorgroup)) + 
      scale_y_continuous(labels = percent) +
      scale_fill_manual(values = snap_bar_clr) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      theme_few(12) + ylab("Percent Change") + xlab("2020") +
      theme(legend.position = "none") +
      scale_x_continuous(limits = c(0, 12), breaks = seq(1, 12, by = 2), labels = month.abb[seq(1, 12, by = 2)]) 
    
    if( !(input$in_indicator_pcnt %in% "all") & (input$in_aggr_pcnt %in% "admin0") ) {
      gg_pcnt <- gg_pcnt + facet_wrap( ~ country, ncol = 5, scales = "free_x")
    } else if ( !(input$in_indicator_pcnt %in% "all") & (input$in_aggr_pcnt %in% "admin1")) {
      gg_pcnt <- gg_pcnt + facet_wrap ( ~ admin_level_1, ncol = 5, scales = "free_x")
    } else {
      gg_pcnt <- gg_pcnt + facet_wrap ( ~ boxgroup, ncol = 4, scales = "free_x")
    }

      

    print(gg_pcnt)
  })
  
  
  
  ## MALARIA VALUE TS -----------------------------------------------------------------------
  
  
  get_adm1name_ts <- reactive({
    adm1dat <- subset(xprov, country %in% input$in_region_ts)
    return(as.character(unique(adm1dat$admin_level_1)))
  })
  
  observeEvent(input$in_aggr_ts, ignoreNULL = FALSE, {
    
    if (input$in_aggr_ts %in% "admin0") {
      updateSelectInput(session, inputId = "in_region_ts", 
                        label = "Region/Country", 
                        choices = c("All Countries", "Northern Hemisphere", 
                                    "Southern Hemisphere", "Western Africa", 
                                    "Eastern Africa", "Central/Southern Africa",  
                                    "Southeast Asia", "----------------------",
                                    as.character((unique(xctry$country)))),
                        selected = "All Countries")
    } else {
      updateSelectInput(session, inputId = "in_region_ts",
                        label = "Country", choices = cc_list)
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
                                      "Test Positivity Ratio" = "tpr", 
                                      "Malaria Severe Cases" = "severe_cases", 
                                      "Malaria Deaths" = "malaria_deaths", 
                                      "ANC (1st) Visit" = "anc1_visit"))
      }
    } else {
      updateSelectInput(session, inputId = "in_indicator_ts", label = "Indicator",
                        choices = c("All Cause Consultations" = "allcause_cases", 
                                    "Tested Cases" = "tested_cases", 
                                    "Malaria Confirmed Cases" = "confirmed_cases",
                                    "Test Positivity Ratio" = "tpr", 
                                    "Malaria Severe Cases" = "severe_cases", 
                                    "Malaria Deaths" = "malaria_deaths", 
                                    "ANC (1st) Visit" = "anc1_visit"))
    }
    
 
  })
  
  
  
  get_dat_malts <- reactive({

    if (input$in_aggr_ts %in% "admin0") {
      tsdat <- xctry
    } else {
      tsdat <- xprov}

    tsdat1 <- extract_countries(tsdat, input$in_region_ts) %>%
      filter(count_type %in% input$in_epiunit_ts)
    
    if (input$in_aggr_ts %in% "admin0") {
      if (input$in_region_ts %in% region_list) {
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
    
    if(!is.null(tsdat1)){
      # Exclude forecasts if requested
      if(input$in_radio_include_ts == "None"){
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
        title_ts <- tsdat1$country[1]
      } else {
        title_ts <- paste(tsdat1$admin_level_1[1], ", ", tsdat1$country[1], sep = "")
      }
    } else {
      if(input$in_aggr_ts %in% "admin0") {
        title_ts <- paste(input$in_region_ts, ": ", as.vector(indicator_labels[input$in_indicator_ts]), sep = "")
      } else {
        title_ts <- paste(tsdat1$admin_level_1[1], ", ", tsdat1$country[1], ": ", as.vector(indicator_labels[input$in_indicator_ts]), sep = "")
      }
    }


    return(list(dat = tsdat1, ribbon = tsribbon, ylabel = ylabel, gtitle = title_ts))
  })



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
    
    
    if(input$in_aggr_ts %in% "admin0") {
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
    if(!(input$in_indicator_ts %in% "all")){
      if(narea > 15) {
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

  

  
  
  
  
  
  # --------------------------------------------------------------------------------
  #
  # COVID TAB
  #
  # --------------------------------------------------------------------------------
  
  
  # Create 3rd input selection in covid-in-country tab
  
  observeEvent(input$in_adminlevel_inctry, ignoreNULL = FALSE,{
    
    if(input$in_adminlevel_inctry %in% "Level 1"){
      
      output$out_filter_l3_inctry <- renderUI(
        checkboxGroupInput(inputId = "in_indicator_inctry", 
                           label = "Select indicators to include:",
                           choices = c("> 60 yrs population density", "30 - 60 yrs population density", 
                                       "20 - 30 yrs population density", "10 - 20 yrs population density",
                                       " 5 - 10 yrs population density", " 0 - 5 yrs population density",
                                       "Mobility (Google)", "Handwashing (Survey)",
                                       "Health workers per population",
                                       "Annual new consultations (fever) per 10K people"))
      )
      
    } else if(input$in_adminlevel_inctry %in% "Level 2"){
      
      output$out_filter_l3_inctry <- renderUI(
        checkboxGroupInput(inputId = "in_indicator_inctry", 
                           label = "Select indicators to include:",
                           choices = c(">60 population density", "30 - 60 population density", 
                                       "20 - 30 population density", "10 - 20 population density",
                                       " 5 - 10 population density", " 0 - 5 population density",
                                       "Health workers per population",
                                       "Annual new consultations (fever) per 10K people"))
      )
      
    } else {
      output$out_filter_l3_inctry <- renderUI(NULL)
    }
    
  })
  
  
  
  # Create 3rd input selection in covid-general tab
  
  observeEvent(input$in_adminlevel_gen, ignoreNULL = FALSE,{
    
    if(input$in_adminlevel_gen %in% "Level 1"){
      
      output$out_filter_l3_gen <- renderUI(
        checkboxGroupInput(inputId = "in_indicator_inctry", 
                           label = "Select indicators to include:",
                           choices = c("> 60 yrs population density", "30 - 60 yrs population density", 
                                       "20 - 30 yrs population density", "10 - 20 yrs population density",
                                       " 5 - 10 yrs population density", " 0 - 5 yrs population density",
                                       "Mobility (Google)", "Handwashing (Survey)"))
      )
      
    } else if(input$in_adminlevel_gen %in% "Level 2"){
      
      output$out_filter_l3_gen <- renderUI(
        checkboxGroupInput(inputId = "in_indicator_gen", 
                           label = "Select indicators to include:",
                           choices = c(">60 population density", "30 - 60 population density", 
                                       "20 - 30 population density", "10 - 20 population density",
                                       " 5 - 10 population density", " 0 - 5 population density"))
      )
      
    } else {
      output$out_filter_l3_gen <- renderUI(NULL)
    }
    
  })
  
  
  
  
  # Create 2nd input selection in covid-other sources tab
  
  observeEvent(input$in_sources_osrc, ignoreNULL = FALSE,{
    
    if(input$in_sources_osrc %in% "Surgo Foundation"){
      
      output$out_filter_l2_osrc <- renderUI(
        
        selectInput(inputId = "in_splevel_osrc", label = "Select spatial ranking level",
                    choices = c("Between countries", "Within countries"))) 
    } else {
      
      output$out_filter_l2_osrc<- renderUI(NULL)
    }
    
  })
  
  
  

  # Plot COVID - In country -----------------------------------------------------------------------
  
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
      addPolygons(color = "#444444", weight = 1, # smoothFactor = 0.5,
                  fillOpacity = 1,
                  fillColor = ~colorQuantile("YlOrRd", domain = spop_adm1$popden_60_years_and_over, n = 5)(popden_60_years_and_over)) 
    
  )
  
  output$out_pop40_inctry <- renderLeaflet(
    
    leaflet(spop_adm1) %>%
      setView(19, 4, zoom =4)  %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = "#444444", weight = 1, # smoothFactor = 0.5,
                  fillOpacity = 1,
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
  
  

  # Plot COVID - General --------------------------------------------------------------------------

  
  output$out_vmap_gen <- renderLeaflet(
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(19, 4, zoom =3) 
    
  )
  
  
  output$out_pop60_gen <- renderLeaflet(
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(19, 4, zoom =3) 
    
  )
  
  output$out_pop40_gen <- renderLeaflet(
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(19, 4, zoom =3) 
    
  )
  
  output$out_mobile_gen <- renderLeaflet(
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(19, 4, zoom =3) 
    
  )
  
  output$out_wash_gen <- renderLeaflet(
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(19, 4, zoom =3) 
    
  )
  
  
  





# --------------------------------------------------------------------------------
#
# Rainfall Tab
#
# --------------------------------------------------------------------------------


# observeEvent(input$in_totcumu_rf_adm0, ignoreNULL = FALSE, {
#   
#   if(input$in_totcumu_rf_adm0 %in% "Accumulated"){
#     
#     output$out_acc_period_rf_adm0 <-  renderUI(selectInput(inputId = "in_acc_period_adm0", label = "Accumulated Period",
#                                                            choices = c("Jan - Dec", "Aug - July (S. Hemisphere only)"),
#                                                            selected = c("Jan-Dec")))
#   } else {
#     output$out_acc_period_rf_adm0 <-  renderUI(NULL)
#   }
#   
# })
  
  observeEvent(input$in_aggr_rf, ignoreNULL = FALSE, {
    if (input$in_aggr_rf %in% "admin0") {
      updateSelectInput(session, inputId = "in_region_rf",
                        label = "Region/Country:",
                        choices = c("All Countries", "Northern Hemisphere", 
                                    "Southern Hemisphere", "Western Africa", 
                                    "Eastern Africa", "Central/Southern Africa",  
                                    "Southeast Asia", "----------------------",
                                    as.character((unique(xrf_adm0$country)))))
    } else {
      updateSelectInput(session, inputId = "in_region_rf",
                        label = "Country:",
                        choices = as.character((unique(xrf$country))))
      
    }
  })
  
  
 
  
  get_dat_rf <- reactive({
    
    rfdat_adm_level <- input$in_aggr_rf
    rfdat_ctry <- input$in_region_rf
    rfdat_indicator <- input$in_indicator_rf_adm0
    rfdat_rftype <- input$in_rftype_rf
    rfdat_count_type <- input$in_count_type
    
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
    
    if(dat_rf$rf_type %in% "acc_ssn") {
      grf <- ggplot(dat_rf$dat, aes(x = month_order_ssn, y = value)) +
        scale_x_continuous(breaks = seq(1, 12, by = 2), labels = month.abb[c(8:12, 1:7)[seq(1, 12, by = 2)]]) +
        ylab("Cumulative Rainfall (mm)")
    }  else {
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
  
  
  
  # get_dat_rf<- reactive({
  #   
  #   y <- switch(input$in_region_rf_adm0,
  #                     "All Countries" = xrf_adm0  %>%  mutate(country = factor(country, levels = ctry_bylat)) ,
  #                     "Northern Hemisphere" = subset(xrf_adm0, country %in% ctry_nhem) %>% mutate(country = factor(country, levels = ctry_nhem)), 
  #                     "Southern Hemisphere" = subset(xrf_adm0, country %in% ctry_shem)  %>% mutate(country = factor(country, levels = ctry_shem)),
  #                     "Western Africa" = subset(xrf_adm0, country %in% ctry_wafr)  %>% mutate(country = factor(country, levels = ctry_wafr)), 
  #                     "Eastern Africa" = subset(xrf_adm0, country %in% ctry_eafr)  %>% mutate(country = factor(country, levels = ctry_eafr)), 
  #                     "Central/Southern Africa" = subset(xrf_adm0, country %in% ctry_csafr)  %>% mutate(country = factor(country, levels = ctry_csafr)),  
  #                     "Southeast Asia" = subset(xrf_adm0, country %in% ctry_sea)
  #                     )
  #   
  #   return(y)
  #   
  # })
  # 
  # output$out_rfplot_adm0 <- renderPlot({
  #   
  #   dat_rf0 <- get_dat_rf()
  #   
  #   # grf0 <- ggplot(dat_rf0, aes(x = month)) +
  #   #   geom_line(data = subset(dat_rf0, !(year %in% c(2019, 2020))),
  #   #             aes(y = rf, group = as.factor(year)), colour = "grey80") +
  #   #   geom_line(data = subset(dat_rf0, (year %in% c(2019, 2020))),
  #   #             aes(y = rf, group = as.factor(year), colour = as.factor(year)), size = 1.1, alpha = 0.8) +
  #   #   geom_line(data = subset(dat_rf0, (year == 2015)),
  #   #             aes(y = rf_ltm), colour = "black", linetype = "dashed", size = 0.8) +
  #   #   geom_line(data = subset(dat_rf0, year == 2020), 
  #   #             aes(y = new_confirmed_rate), colour = "green", size = 0.8, alpha = 0.6) +
  #   #   facet_wrap(~ country, scales = "free_y") +
  #   #   geom_line(data = subset(dat_rf0, year == 2019), 
  #   #             aes(y = new_confirmed_rate), colour = "#993404", size = 0.8, alpha = 0.6) +
  #   #   facet_wrap(~ country, scales = "free_y") +
  #   #   scale_colour_manual(values = c("blue", "red")) +
  #   #   theme_minimal() +
  #   #   scale_x_continuous(breaks = seq(1, 12, by = 2), labels = month.abb[seq(1, 12, by = 2)]) +
  #   #   ylab("Monthly Rainfall (mm)") + xlab("") +
  #   #   theme(legend.position = "top", legend.title = element_blank()) 
  #   # 
  #   
  #   grf0 <- ggplot(dat_rf0 , aes(x = month)) +
  #     geom_line(aes(y = rf, group = as.factor(year), colour = as.factor(colour_rf)) )+
  #     geom_line(data = subset(dat_rf0 , year == 2015),
  #               aes( y = rf_ltm),  colour = "black", linetype = "dashed", size = 0.8) +
  #     geom_line(data = subset(dat_rf0 , year == c(2019, 2020)), 
  #               aes(y = new_confirmed_rate, group = year, colour = colour_mal), alpha = 0.6, size = 0.8) +
  #     facet_wrap(~ country, scales = "free_y") +
  #     theme_minimal(16) +
  #     # scale_x_continuous(breaks = seq(1, 12, by = 2), labels = month.abb[seq(1, 12, by = 2)]) +
  #     ylab("Monthly Rainfall (mm)") + xlab("") +
  #     scale_colour_manual(values = c("blue", "red", "#993404" ,"green", "grey80")) +
  #     theme(legend.position = "top", legend.title = element_blank()) 
  #     
  #   
  #   # print(grf0)
  #     
  # })
  
  
  
  
} # end of server