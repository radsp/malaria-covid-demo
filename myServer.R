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
  
  # Toggle button to hide/show sidebar ---------------------------------------------
  
  # Malaria - national tab
  observeEvent(input$toggle_side_mal_adm0, {
    shinyjs::toggle(id = "sidebar_mal_adm0")
  })
  
  # Malaria - subnational tab
  observeEvent(input$toggle_side_mal_adm1, {
    shinyjs::toggle(id = "sidebar_mal_adm1")
  })
  
  
  # Generate select input based on the first selection ---------------------------
  
  # Create second input selection in the malaria national tab
  
  observeEvent(input$in_plot_type_mal_adm0, ignoreNULL = FALSE, {
    
    if(input$in_plot_type_mal_adm0 %in% "Country"){
      
      output$out_filter_l2_mal_adm0 <-  renderUI(selectInput(inputId = "in_country_mal_adm0", label = "Select Country:",
                                                             choices = c(sort(as.character(unique(xctry$country)))),
                                                             selected = (sort(as.character(unique(xctry$country))))[1]))
      
    } else if(input$in_plot_type_mal_adm0  %in% "Indicator") {
      output$out_filter_l2_mal_adm0 <- renderUI(selectInput(inputId = "in_indicator_mal_adm0", label = "Select Indicator:",
                                                            choices = c("All Cause Consultations", "Tested Cases", "Malaria Confirmed Cases",
                                                                        "Test Positivity Ratio", "Malaria Severe Cases", "Malaria Deaths", "ANC (1st) Visit"),
                                                            selected = "All Cause Consultation"))
    } else {
      output$out_filter_l2_mal_adm0 <-  renderUI(NULL)
    }
    
  })
  
  
  # Create 3rd input selection in the malaria national tab
  
  observeEvent(input$in_plot_type_mal_adm0, ignoreNULL = FALSE, {
    
    if(input$in_plot_type_mal_adm0 %in% "Indicator"){
      
      output$out_filter_l3_mal_adm0 <-  renderUI(selectInput(inputId = "in_region_mal_adm0", label = "Select Geographical Region:",
                                                             choices = c("All countries", "Northern Hemisphere", "Southern Hemisphere", 
                                                                         "Western Africa", "Eastern Africa", "Central/Southern Africa"),
                                                             selected = "All countries"))
    } else {
      output$out_filter_l3_mal_adm0 <-  renderUI(NULL)
    }
    
  })
  
  
  
  
  # Filter data frame based on selection -----------------------------------------
  
  # Get data for country level
    
    
    get_dat_adm0 <- reactive({
      
      if(input$in_plot_type_mal_adm0 %in% "Country"){
      
        a <- subset(xctry, country %in% input$in_country_mal_adm0)
        # b <- subset(xctryExc, country %in% input$in_country_mal_adm0 )

    } else if(input$in_plot_type_mal_adm0 %in% "Indicator") {
        
            if(input$in_region_mal_adm0 %in% "Northern Hemisphere"){
              a <- subset(xctry, (boxgroup %in% input$in_indicator_mal_adm0) & (country %in% ctry_nhem))
              # b <- subset(xctryExc, (boxgroup %in% input$in_indicator_mal_adm0) & (country %in% ctry_nhem))
            } else if(input$in_region_mal_adm0 %in% "Southern Hemisphere"){
              a <- subset(xctry, (boxgroup %in% input$in_indicator_mal_adm0) & (country %in% ctry_shem))
              # b <- subset(xctryExc,  (boxgroup %in% input$in_indicator_mal_adm0) &(country %in% ctry_shem))
            } else if(input$in_region_mal_adm0 %in% "Western Africa"){
              a <- subset(xctry,  (boxgroup %in% input$in_indicator_mal_adm0) & (country %in% ctry_wafr))
              # b <- subset(xctryExc, (boxgroup %in% input$in_indicator_mal_adm0) & country %in% ctry_wafr)
            } else if(input$in_region_mal_adm0 %in% "Eastern Africa"){
              a <- subset(xctry, (boxgroup %in% input$in_indicator_mal_adm0) & (country %in% ctry_eafr))
              # b <- subset(xctryExc, (boxgroup %in% input$in_indicator_mal_adm0) & country %in% ctry_eafr)
            }else if(input$in_region_mal_adm0 %in% "Central/Southern Africa"){
              a <- subset(xctry, (boxgroup %in% input$in_indicator_mal_adm0) & (country %in% ctry_csafr))
              # b <- subset(xctryExc, (boxgroup %in% input$in_indicator_mal_adm0) & (country %in% ctry_csafr))
            } else {
              a <- subset(xctry, boxgroup %in% input$in_indicator_mal_adm0)
              # b <- subset(xctryExc, boxgroup %in% input$in_indicator_mal_adm0)
            }
      
      a <- a %>% mutate(country = factor(country, levels = unique(country)))
      # b <- b %>% mutate(country = factor(country, levels = unique(country)))

      } else {

        a <- b <- NULL
      }
      
      
      delta <- NULL


    if( !(is.null(a)) ){
      
      # Exclude forecasts if requested
      if(input$in_radio_include_adm0 == "None"){
        a <- subset(a, !(combogroup %in% c("COVID-19 Forecast", "Report Rate")) )
      } else if (input$in_radio_include_adm0 == "COVID-19 Forecasts") {
        a <- subset(a, !(combogroup %in% c("Report Rate")))
      } else {
        a <- subset(a, !(combogroup %in% c("COVID-19 Forecast")))
      }
   
      
      if(input$in_yaxs_mal_adm0 %in% "rate"){
        a <- subset(a, count_type %in% "value_rate")
        ylabel <- "Cases per 1K people"
      } else {
        a <- subset(a, count_type %in% "value")
        ylabel <- "Cases"
        
      }
      
      delta <- get_delta(input$in_yr0_mal_adm0, input$in_yr0m0_mal_adm0, input$in_yr0mf_mal_adm0, a)
      delta$text_hover <- paste("Difference between ", input$in_yr0m0_mal_adm0, "-", input$in_yr0mf_mal_adm0, " ", 2020,
                                " and ", input$in_yr0m0_mal_adm0, "-", input$in_yr0mf_mal_adm0, " ", input$in_yr0_mal_adm0, sep = "")
      
      a <- a %>% filter( !((variable %in% "reports_rate__confirmed_cases") & (year < 2020)) ) %>%
        filter( !((variable %in% "reports_rate__allcause_cases") & (year <  2020)) ) %>%
        filter( !((variable %in% "reports_rate__anc1_visit") & (year <  2020)) )

    }
      
      m0 <- input$in_yr0m0_mal_adm0
      mf <- input$in_yr0mf_mal_adm0
      yf <- input$in_yr0_mal_adm0
      
      b <- get_ribbon_data(m0, mf, yf, a)
      
      # max4text <- xctry %>%
      #   group_by(country, boxgroup) %>%
      #   summarise(ypos = max(value,  na.rm = TRUE),
      #             xpos = 11.5)
      
      # delta <- merge(delta, max4text, by = c("country", "boxgroup"), all.x = TRUE, all.y = FALSE)
      
      
    
    return(list(dat = a, ribbon = b, plot_view = input$in_plot_type_mal_adm0, delta = delta, ylabel = ylabel))
  })
  
 
  
  # Plot malaria national ---------------------------------------------------------
  
  
  observeEvent(input$dimension,{
    
    output$out_plot_mal_adm0 <- renderPlotly({
      
      dat_adm0 <- get_dat_adm0()
      
      
      
      if(dat_adm0[["plot_view"]] == "Country"){
        
        
        
        g1 <- ggplot(dat_adm0[["dat"]], aes(x = month)) +
          geom_line(aes(x = month, y = value, colour = combogroup, # text = line_label,
                        linetype = combogroup, alpha = combogroup, group = interaction(mygroup, combogroup)), size = 0.5) +
          geom_point(aes(x = month, y = value, colour = combogroup, # text = point_label,
                         shape = combogroup, size = combogroup, alpha = combogroup, group = interaction(mygroup, combogroup))) +
          geom_ribbon(data = dat_adm0$ribbon, 
                      aes(x = month, ymin = minval, ymax = maxval, fill = combogroup), colour = NA, alpha = 0.1) +
          facet_wrap( ~ boxgroup, scales = "free", ncol = 4) +
          xlab("") + 
          ylab(dat_adm0[["ylabel"]]) +
          ggtitle(as.character(dat_adm0$dat$country[1])) +
          scale_x_continuous(breaks = seq(from = 1, to = 12, by = 2), labels = month.abb[seq(from = 1, to = 12, by = 2)]) +
          # geom_text(data = dat_adm0$delta, aes(x = 11, y = 0, label = delta_text, hjust = 1, vjust = -1, text = text_hover), size = 6, colour = "grey20", alpha = 0.75) +
          theme_few(12) +
          scale_alpha_manual("", values = alpha_values, drop = FALSE) +
          scale_linetype_manual("", values = linetype_values, drop = FALSE) +
          scale_shape_manual(values = shape_values, drop = FALSE) +
          scale_size_manual(values = size_values, drop = FALSE) +
          scale_colour_manual("", values = clr_values, drop = FALSE) +
          scale_fill_manual("", values = clr_values, drop = FALSE) +
          # scale_fill_manual(values = clrset, drop = FALSE) +
          # guides(fill = FALSE,
          #        colour = guide_legend(override.aes = list(colour = c(NA, NA, NA, NA, NA, NA, "#a65628"),
          #                              shape = NA), order = 1)) +
          theme(legend.title = element_blank(), legend.position = "none")
        
        
        
      } else if(dat_adm0[["plot_view"]] == "Indicator"){
        
        g1 <- ggplot(dat_adm0$dat) +
          geom_line(aes(x = month, y = value, colour = combogroup, # text = line_label,
                        linetype = combogroup, alpha = combogroup, group = interaction(mygroup, combogroup))) +
          geom_point(aes(x = month, y = value, colour = combogroup, # text = point_label,
                         shape = combogroup, size = combogroup, alpha = combogroup, group = interaction(mygroup, combogroup))) +
          geom_ribbon(data = dat_adm0$ribbon, 
                      aes(x = month, ymin = minval, ymax = maxval, fill = combogroup), colour = NA, alpha = 0.1) +
          facet_wrap( ~ country, scales = "free") +
          # geom_text(data = dat_adm0$delta, aes(x = 11, y = 0, label = delta_text, hjust = 1, vjust = -1, text = text_hover), size = 6, colour = "grey20", alpha = 0.75) +
          xlab("") + 
          ylab(dat_adm0$ylabel) +
          ggtitle(as.character(input$in_indicator_mal_adm0)) +
          scale_x_continuous(breaks = seq(from = 1, to = 12, by = 2), labels = month.abb[seq(from = 1, to = 12, by = 2)]) +
          theme_few(12) +
          scale_alpha_manual("", values = alpha_values, drop = FALSE) +
          scale_linetype_manual("", values = linetype_values, drop = FALSE) +
          scale_shape_manual(values = shape_values, drop = FALSE) +
          scale_size_manual(values = size_values, drop = FALSE) +
          scale_colour_manual("", values = clr_values, drop = FALSE) +
          scale_fill_manual("", values = clr_values, drop = FALSE) +
          # scale_fill_manual(values = clrset, drop = FALSE) +
          # guides(fill = FALSE,
          #        colour = guide_legend(
          #        override.aes = list(colour = c(NA, NA, NA, NA, NA, NA, "#a65628"),
          #                               shape = NA), order = 1)) +
          theme(legend.title = element_blank(), legend.position = "none")
      }
      
      ggplotly(g1, # tooltip = "text", 
               width = (0.75 * as.numeric(input$dimension[1])),
               height = as.numeric(0.7*input$dimension[2])
               )
      
      
      
    })
    
    
  })
  
  
  # MALARIA - SUBNATIONAL ---------------------------------------------------------
  

  
  # Generate select input based on the first selection ---------------------------
  
  # Create second input selection in the malaria subnational tab
  
  observeEvent(input$in_plot_type_mal_adm1, ignoreNULL = FALSE, {
    
    if(input$in_plot_type_mal_adm1 %in% "Admin. Level 1"){
      
      output$out_filter_l2_mal_adm1 <-  renderUI(selectInput(inputId = "in_adm1_mal_adm1", label = "Admin. Level 1",
                                                             choices = c(sort(as.character(unique(subset(xprov, country %in% input$in_country_mal_adm1)[, "admin_level_1"])))),
                                                             selected = c(sort(as.character(unique(subset(xprov, country %in% input$in_country_mal_adm1)[, "admin_level_1"]))))))
      
    } else if(input$in_plot_type_mal_adm1  %in% "Indicator") {
      output$out_filter_l2_mal_adm1 <- renderUI(selectInput(inputId = "in_indicator_mal_adm0", label = "Select Indicator:",
                                                            choices = c("All Cause Consultation", "Malaria Confirmed Cases",
                                                                        "Malaria Test Positivity Rate", "Severe Malaria Cases",
                                                                        "Malaria Deaths", "ANC Visit"),
                                                            selected = "All Cause Consultation"))
    } else {
      output$out_filter_l2_mal_adm1 <-  renderUI(NULL)
    }
    
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


observeEvent(input$in_totcumu_rf_adm0, ignoreNULL = FALSE, {
  
  if(input$in_totcumu_rf_adm0 %in% "Accumulated"){
    
    output$out_acc_period_rf_adm0 <-  renderUI(selectInput(inputId = "in_acc_period_adm0", label = "Accumulated Period",
                                                           choices = c("Jan - Dec", "Aug - July (S. Hemisphere only)"),
                                                           selected = c("Jan-Dec")))
  } else {
    output$out_acc_period_rf_adm0 <-  renderUI(NULL)
  }
  
})
  
  
  get_dat_rf<- reactive({
    
    y <- switch(input$in_region_rf_adm0,
                      "All Countries" = xrf_adm0  %>%  mutate(country = factor(country, levels = ctry_bylat)) ,
                      "Northern Hemisphere" = subset(xrf_adm0, country %in% ctry_nhem) %>% mutate(country = factor(country, levels = ctry_nhem)), 
                      "Southern Hemisphere" = subset(xrf_adm0, country %in% ctry_shem)  %>% mutate(country = factor(country, levels = ctry_shem)),
                      "Western Africa" = subset(xrf_adm0, country %in% ctry_wafr)  %>% mutate(country = factor(country, levels = ctry_wafr)), 
                      "Eastern Africa" = subset(xrf_adm0, country %in% ctry_eafr)  %>% mutate(country = factor(country, levels = ctry_eafr)), 
                      "Central/Southern Africa" = subset(xrf_adm0, country %in% ctry_csafr)  %>% mutate(country = factor(country, levels = ctry_csafr)),  
                      "Southeast Asia" = subset(xrf_adm0, country %in% ctry_sea)
                      )
    
    return(y)
    
  })
  
  output$out_rfplot_adm0 <- renderPlot({
    
    dat_rf0 <- get_dat_rf()
    
    # grf0 <- ggplot(dat_rf0, aes(x = month)) +
    #   geom_line(data = subset(dat_rf0, !(year %in% c(2019, 2020))),
    #             aes(y = rf, group = as.factor(year)), colour = "grey80") +
    #   geom_line(data = subset(dat_rf0, (year %in% c(2019, 2020))),
    #             aes(y = rf, group = as.factor(year), colour = as.factor(year)), size = 1.1, alpha = 0.8) +
    #   geom_line(data = subset(dat_rf0, (year == 2015)),
    #             aes(y = rf_ltm), colour = "black", linetype = "dashed", size = 0.8) +
    #   geom_line(data = subset(dat_rf0, year == 2020), 
    #             aes(y = new_confirmed_rate), colour = "green", size = 0.8, alpha = 0.6) +
    #   facet_wrap(~ country, scales = "free_y") +
    #   geom_line(data = subset(dat_rf0, year == 2019), 
    #             aes(y = new_confirmed_rate), colour = "#993404", size = 0.8, alpha = 0.6) +
    #   facet_wrap(~ country, scales = "free_y") +
    #   scale_colour_manual(values = c("blue", "red")) +
    #   theme_minimal() +
    #   scale_x_continuous(breaks = seq(1, 12, by = 2), labels = month.abb[seq(1, 12, by = 2)]) +
    #   ylab("Monthly Rainfall (mm)") + xlab("") +
    #   theme(legend.position = "top", legend.title = element_blank()) 
    # 
    
    grf0 <- ggplot(dat_rf0 , aes(x = month)) +
      geom_line(aes(y = rf, group = as.factor(year), colour = as.factor(colour_rf)) )+
      geom_line(data = subset(dat_rf0 , year == 2015),
                aes( y = rf_ltm),  colour = "black", linetype = "dashed", size = 0.8) +
      geom_line(data = subset(dat_rf0 , year == c(2019, 2020)), 
                aes(y = new_confirmed_rate, group = year, colour = colour_mal), alpha = 0.6, size = 0.8) +
      facet_wrap(~ country, scales = "free_y") +
      theme_minimal(16) +
      # scale_x_continuous(breaks = seq(1, 12, by = 2), labels = month.abb[seq(1, 12, by = 2)]) +
      ylab("Monthly Rainfall (mm)") + xlab("") +
      scale_colour_manual(values = c("blue", "red", "#993404" ,"green", "grey80")) +
      theme(legend.position = "top", legend.title = element_blank()) 
      
    
    print(grf0)
      
  })
  
  
  
  
} # end of server
