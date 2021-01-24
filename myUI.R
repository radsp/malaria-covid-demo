
#--------------------------------------------------------------------------------------------------------------------------------
# Define malaria tab
#--------------------------------------------------------------------------------------------------------------------------------

tab_malaria <- 
  navbarMenu("Malaria Impact", 
             tabPanel("Snapshot",
                      useShinyjs(),
                      br(),
                      sidebarLayout(
                        sidebarPanel(width = 2, 
                                     selectInput(inputId = "in_region_snapshot", label = "Region/Country:",
                                                 choices = c("All Countries", "Northern Hemisphere", 
                                                             "Southern Hemisphere", "Western Africa", 
                                                             "Eastern Africa", "Central/Southern Africa",  
                                                             "Southeast Asia", "----------------------",
                                                             as.character((unique(xctry$country)))),
                                                 selected = "All Countries"),
                                     selectInput(inputId = "in_indicator_snapshot", label = "Select indicator to map:",
                                                 choices = c("All Cause Consultations" = "allcause_cases", 
                                                             "Tested Cases" = "tested_cases", 
                                                             "Malaria Confirmed Cases" = "confirmed_cases",
                                                             "Test Positivity Rate" = "tpr", 
                                                             "Malaria Severe Cases" = "severe_cases", 
                                                             "Malaria Deaths" = "malaria_deaths", 
                                                             "ANC (1st) Visit" = "anc1_visit"), 
                                                 selected = "allcause_cases"),
                                     prettyRadioButtons(inputId = "in_yaxs_snap",
                                                        label = HTML("Indicator Unit:"), choices = c("Cases per 1K people" = "value_rate", "Cases" = "value"),
                                                        inline = FALSE, status = "default"),
                                     hr(),
                                     h5(HTML("Baseline Period for Comparison")),
                                     br(),
                                     selectInput(inputId = "in_yr0_snap", label = "Select Year or Historical Mean:", choices = c(rev(2016:2019), "Historical mean"), 
                                                 selected = "2019"),
                                     fluidRow(column(6,
                                                     selectInput(inputId = "in_yr0m0_snap", label = "Start Month:", choices = month.abb, selected = "Jan")),
                                              column(6,
                                                     selectInput(inputId = "in_yr0mf_snap", label = "End Month:", choices = month.abb, selected = "Mar")))
                        ),
                        mainPanel(width = 10, setBackgroundColor("white"),
                          uiOutput(outputId = "out_snapshot_panel")
                        ))),
             
             tabPanel("Changes In Indicator",
                      tags$style(type = "text/css", "#out_plot_pcnt {height: calc(100vh - 150px) !important;}"),
                      br(),
                      sidebarLayout(
                        sidebarPanel(width = 2,
                                     selectInput(inputId = "in_aggr_pcnt", label = "Spatial Aggregation", 
                                                        choices = c("National Level" = "admin0", "Sub-National Level" = "admin1"),
                                                        selected = "admin0"),
                                     selectInput(inputId = "in_region_pcnt", label = "Region/Country", 
                                                 choices = c("All Countries", "Northern Hemisphere", 
                                                             "Southern Hemisphere", "Western Africa", 
                                                             "Eastern Africa", "Central/Southern Africa",  
                                                             "Southeast Asia", "----------------------",
                                                             as.character((unique(xctry$country)))),
                                                 selected = "All Countries"),
                                     uiOutput("out_filter_adm1_pcnt"),
                                     selectInput(inputId = "in_indicator_pcnt", label = "Indicator",
                                                 choices = c("All Cause Consultations" = "allcause_cases", 
                                                             "Tested Cases" = "tested_cases", 
                                                             "Malaria Confirmed Cases" = "confirmed_cases",
                                                             "Test Positivity Rate" = "tpr", 
                                                             "Malaria Severe Cases" = "severe_cases", 
                                                             "Malaria Deaths" = "malaria_deaths", 
                                                             "ANC (1st) Visit" = "anc1_visit"),
                                                 selected = "allcause_cases"),
                                     prettyRadioButtons(inputId = "in_epiunit_pcnt",
                                                        label = HTML("Indicator unit:"), choices = c("Cases per 1,000 population*" = "value_rate", "Cases" = "value"),
                                                        inline = FALSE, status = "default", selected = "value_rate"),
                                     prettyCheckboxGroup(inputId = "in_overlay_pcnt", label = HTML("Include:"),
                                                        choices = c("Reporting Rate" = "reports_rate", "Mobility" = "mob_"), inline = FALSE,
                                                        status = "default"),
                                     hr(),
                                     h5(HTML("Baseline year for comparison")),
                                     br(),
                                     selectInput(inputId = "in_yr0_pcnt", label = "Select year to compare:", choices = c(rev(2016:2019), "Historical Mean"), 
                                                 selected = "2019")
                        ),
                        mainPanel(
                          fluidRow(style = 'padding-left:10px;',
                                   column(10, 
                                          "How do the changes in malaria indicators (as compared to the selected baseline) vary by month? Do any decreases/increases coincide with COVID-19 dynamics?")),
                          br(),
                          fluidRow(style = 'padding-left:30px;', htmlOutput("out_plottitle_pcnt")), 
                          fluidRow(style = 'padding-left:10px;',
                                   plotlyOutput("out_plot_pcnt", width = "auto"), setBackgroundColor("white")))
                      )),
             
             tabPanel("Historical time series",
                      fluidPage( theme = "bootstrap_simplex.css",
                                 
                                 tags$head(tags$script('var dimension = [0, 0];$(document).on("shiny:connected", function(e) {
                                      dimension[0] = window.innerWidth; dimension[1] = window.innerHeight; Shiny.onInputChange("dimension", dimension);});
                                      $(window).resize(function(e) {dimension[0] = window.innerWidth; dimension[1] = window.innerHeight;
                                      Shiny.onInputChange("dimension", dimension); });')),
                                 
                                 br(),
                                 
                                 sidebarLayout(
                                   sidebarPanel(width = 2,
                                                selectInput(inputId = "in_aggr_ts", label = "Spatial Aggregation", 
                                                            choices = c("National Level" = "admin0", "Sub-National Level" = "admin1"),
                                                            selected = "admin0"),
                                                selectInput(inputId = "in_region_ts", label = "Region/Country", 
                                                            choices = c("All Countries", "Northern Hemisphere", 
                                                                        "Southern Hemisphere", "Western Africa", 
                                                                        "Eastern Africa", "Central/Southern Africa",  
                                                                        "Southeast Asia", "----------------------",
                                                                        as.character((unique(xctry$country)))),
                                                            selected = "All Countries"),
                                                uiOutput("out_filter_adm1_ts"),
                                                selectInput(inputId = "in_indicator_ts", label = "Indicator",
                                                            choices = c("All Cause Consultations" = "allcause_cases", 
                                                                        "Tested Cases" = "tested_cases", 
                                                                        "Malaria Confirmed Cases" = "confirmed_cases",
                                                                        "Test Positivity Rate" = "tpr", 
                                                                        "Malaria Severe Cases" = "severe_cases", 
                                                                        "Malaria Deaths" = "malaria_deaths", 
                                                                        "ANC (1st) Visit" = "anc1_visit"),
                                                            selected = "allcause_cases"),
                                                prettyRadioButtons(inputId = "in_epiunit_ts",
                                                                   label = HTML("Indicator unit:"), choices = c("Cases per 1K" = "value_rate", "Cases" = "value"),
                                                                   inline = FALSE, status = "default", selected = "value_rate"),
                                                prettyRadioButtons(inputId = "in_radio_include_ts", label = HTML("Include:"),
                                                                   choices = c("COVID-19 Forecasts", "Reporting Rate", "None"), inline = FALSE,
                                                                   fill = FALSE, status = "default", selected = "None"),
                                                hr(),
                                                h5(HTML("<b>Compare 2020 Data</b>")),
                                                br(),
                                                selectInput(inputId = "in_yr0_ts", label = "Select year to compare:", choices = c(rev(2016:2019), "Historical mean"), 
                                                            selected = "Historical mean"),
                                                fluidRow(column(6,
                                                                selectInput(inputId = "in_yr0m0_ts", label = "Start Month:", choices = month.abb, selected = "Jan")),
                                                         column(6,
                                                                selectInput(inputId = "in_yr0mf_ts", label = "End Month:", choices = month.abb, selected = "Mar")))
                                   ),
                                   mainPanel( setBackgroundColor("white"),
                                     fluidRow(
                                         column(10, style = 'padding-left:10px;', p(HTML("KEY ANALYTICAL QUESTIONS:")),
                                                tags$ol(
                                                  tags$li("How do malaria indicators reported in 2020 - during COVID-19 pandemic - appear to compare to previous years and/or historical mean?"),
                                                  tags$li("By how much do the indicators in 2020 exceed or below the level of previous years?"),
                                                  tags$li("Do any changes in malaria indicators coincide with COVID19 dynamics?"),
                                                  tags$li("What is the forecasted COVID-19 trajectory and how does the timing potentially coincide with malaria season?")))
                                     ),
                                     br(),
                                     fluidRow(style = 'padding-left:10px;', plotlyOutput("out_plot_ts", width = "auto")))
                                 )
                                 
                                 

                        
                      )
               
             ),
             
             
             # Rainfall tab panel -------------------------------------------------------------------------------------
             tabPanel("Rainfall", # theme = "bootstrap_simplex.css",
                      
                      tags$style(type = "text/css", "#out_rfplot_adm0 {height: calc(100vh - 150px) !important;}"),
                      
                      fluidPage(
                        
                        fluidRow(
                          column(2, p(HTML("<b>Rainfall: National Level</b>"))),
                          column(8, p(HTML("KEY ANALYTICAL QUESTIONS:")),
                                 tags$ol(
                                   tags$li("Is 2020 rainfall below/above normal (or compared to last year level)?"),
                                   tags$li("Are rainy seasons in 2020 starting earlier than normal (or 2019)"),
                                   tags$li("Has any shift in rainfall pattern appeared to have been followed by changes in malaria indicators (between 2019 and 2020)")
                                 ),
                                 column(2))),
                        br(),
                        fluidRow(
                          sidebarLayout(
                            sidebarPanel(width = 2,
                                         selectInput(inputId = "in_aggr_rf", label = "Spatial aggregation level:",
                                                     choices = c("National Level" = "admin0", "Sub-National Level" = "admin1"),
                                                     selected = "country"),
                                         selectInput(inputId = "in_region_rf", label = "Region/Country:",
                                                     choices = c("All Countries", "Northern Hemisphere", 
                                                                 "Southern Hemisphere", "Western Africa", 
                                                                 "Eastern Africa", "Central/Southern Africa",  
                                                                 "Southeast Asia", "----------------------",
                                                                 as.character((unique(xrf_adm0$country))))),
                                         
                                         prettyRadioButtons(inputId = "in_rftype_rf", label = "Display rainfall as:",
                                                            choices = c("Monthly Total" = "monthly", 
                                                                        "Cumulative (Jan-Dec)" = "acc_cy", 
                                                                        "Cumulative (Aug-July) (S.Hemisphere)" = "acc_ssn"),
                                                            selected = "monthly",
                                                            inline = FALSE, status = "default"),
                                         hr(),
                                         h6(HTML("<b>Epidemiological Indicator</b>")),
                                         br(),
                                         selectInput(inputId = "in_indicator_rf_adm0", label = "Indicator:",
                                                     choices = c("All Cause Consultations" = "allcause_cases", 
                                                                 "Tested Cases" = "tested_cases", 
                                                                 "Confirmed Cases" = "confirmed_cases",
                                                                 "Test Positivity Rate" = "tpr", 
                                                                 "Severe Cases" = "severe_cases", 
                                                                 "Malaria Deaths" = "malaria_deaths", 
                                                                 "ANC (1st) Visit" = "anc1_visit",
                                                                 "---------------------" = NA,
                                                                 "COVID-19 Cases" = "covid_cases", "COVID-19 Deaths" = "covid_deaths")),
                                         # selectInput(inputId = "in_year_rf_adm0", label = "Year", 
                                         #             choices = rev(2016:2020), selected = 2020),
                                         prettyRadioButtons(inputId = "in_count_type",
                                                            label = HTML("Display indicator as:"), 
                                                            choices = c("Cases per 1000 population" = "value_rate", "Cases" = "value"),
                                                            inline = FALSE, status = "default")
                            ), # End of sidebarPanel - RF National
                            mainPanel(width = 8, plotOutput("out_rfplot_adm0"))
                          )
                        )
                        
                        
                      ) # End of fluid page - RF National
                      
                      
             ) # End of tab Panel - RF National
             
             )




#--------------------------------------------------------------------------------------------------------------------------------
# Define COVID tab
#--------------------------------------------------------------------------------------------------------------------------------

tab_covid <- navbarMenu("COVID-19 Vulnerability",
                        tabPanel("In-Country Calculator", theme = "bootstrap_simplex.css",
                                 fluidPage(
                                   # fluidRow(
                                   #   column(2, p(tags$b("COVID-19 Vulnerability : In-Country Calculator"))),
                                   #   column(8, p(HTML("KEY ANALYTICAL QUESTIONS:")),
                                   #          tags$ol(
                                   #            tags$li("Which geographic area is most vulnerable to COVID-19 based on selected indicators?"),
                                   #            tags$li("How does the individual indicator vary within the country?")
                                   #          )),
                                   #   column(2)),
                                    # ),
                                   br(),
                                     
                                     sidebarLayout(
                                       sidebarPanel(width = 2,
                                                    selectInput(inputId = "in_ctry_covid_inctry", label = "Country:",
                                                                choices = c('Angola','Benin','Burkina Faso','Cambodia','Cameroon',
                                                                            "Cote D'Ivoire",'DR Congo','Ethiopia',
                                                                            'Ghana','Guinea','Liberia','Madagascar','Malawi',
                                                                            'Mali','Mozambique','Myanmar','Niger','Nigeria',
                                                                            'Rwanda','Senegal','Sierra Leone','Tanzania (Mainland)',
                                                                            'Tanzania (Zanzibar)','Thailand','Uganda','Zambia','Zimbabwe')),
                                                    # selectInput(inputId = "in_adminlevel_inctry", label = "Administrative Level:",
                                                    #             choices = c("Level 1", "Level 2"),
                                                    #             selected = "Level 1"),
                                                    checkboxGroupInput(inputId = "in_indicator_inctry_custom", 
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
                                                                                   "Handwashing: Population living in households where a place for handwashing was observed (%)"="population_with_a_place_for_handwashing_was_observed"
                                                                                   # "Malaria: Number of health workers"="mal_health_workers",
                                                                                   # "Malaria: Annual new consultations (fever)"="mal_new_consultation_all_cause" # temporarily removing malaria indicators 
                                                                       ),
                                                                       selected = "pop_perc_65_years_and_over"),
                                                    
                                                    uiOutput(outputId = "out_filter_l3_inctry")
                                       ), 
                                       
                                       ## Main panel -----------------------------------------------------------------------------
                                       mainPanel(
                                         fluidRow(
                                           h2("Customizable COVID-19 Vulnerability Rank Map")), br(),
                                         fluidRow(
                                         column(8, p(HTML("KEY ANALYTICAL QUESTIONS:")),
                                                         tags$ol(
                                                           tags$li("Which geographic area is most vulnerable to COVID-19 based on selected indicators?"),
                                                           tags$li("How does the individual indicator vary within the country?")
                                                         ))),
                                         br(),
                                         # HTML("<h4><b>COVID-19 Vulnerability Rank Map</b></h4>
                                         #                 <h6>Currently a placeholder - risk ranking here obtained from Cooper/Smith as an example</h6>"),
                                         fluidRow(
                                             p("The custom vulnerability scores are calculated following the", (a("Surgo Foundation's COVID-19 
                                           Community Vulnerability Index methodology", href="https://covid-static-assets.s3.amazonaws.com/Africa+CCVI+methodology.pdf")),
                                               ", using a stepwise ranking procedure. First, indicator values for the most recently available data are
                                           grouped by country and converted into percentiles. These indicators are then aggregated to themes 
                                           (i.e. mobility indicators are aggregated to the mobility theme, handwashing data indicators 
                                           are aggregated to the handwashing theme, etc.). The indicators 
                                           and themes are both weighted equally when calculating the overall score, following Surgo’s 
                                           methodology. The custom vulnerability scores deviate from Surgo’s methodology through some key 
                                           features: the custom scores use the most recently available data for each of the indicators 
                                           while Surgo appears to use both recent data and historical data. The Surgo Foundation's scores calculate cross-country
                                           comparisons, which we have opted not to include. The custom scores are also 
                                           determined by user selected variables; they are recalculated each time the user’s selections 
                                           change, whereas the Surgo scores are static since there is no option for the user to select 
                                           indicators. The risk factors included for the custom scores are population age structure, 
                                           mobility, and hand washing. We obtained population age structure data from ", 
                                               (a("World Population", href="https://www.worldpop.org/geodata/listing?id=65")),  
                                               ", mobility data from ", (a("Google COVID-19 Community Mobility Reports", href="https://www.google.com/covid19/mobility/")), 
                                               ", and hand washing data from the ", 
                                               (a("Demographic Health Survey.", href="http://api.dhsprogram.com/#/index.html")), br()),
                                             h6(tags$strong("Click on the plot below to view the scores for your selected indicator(s) in
                                            each region.")),
                                           
                                        leafletOutput("custom_scores_inctry")
                                         ),
                                         hr(),
                                         h4(HTML("<b>Indicators</b>
                                                 <h6>Currently a placeholder - risk ranking here obtained from Cooper/Smith as an example</h6>")),
                                         fluidRow(
                                           box( title = h4("> 60 yrs population density"), collapsible = TRUE, leafletOutput("out_pop60_inctry") ),
                                           box( title = h4("40 - 60 yrs population density"), collapsible = TRUE, leafletOutput("out_pop40_inctry")),
                                           box( title = h4("Mobility"), collapsible = TRUE, leafletOutput("out_mobile_inctry") ),
                                           box( title = h4("Handwashing"), collapsible = TRUE, leafletOutput("out_wash_inctry") )
                                         )
                                       )
                                       
                                     )
                                     
                                   )
                                   
                                 
                                 
                                 
                                 ), 
                        
                        # tabPanel("General Calculator",
                        #          fluidPage(
                        #            sidebarLayout(
                        #              sidebarPanel(width = 2,
                        #                           selectInput(inputId = "in_ctry_covid_gen", label = "Country:",
                        #                                       choices = c("Benin", "Guinea", "...")),
                        #                           selectInput(inputId = "in_adminlevel_gen", label = "Administrative Level:",
                        #                                       choices = c("Level 1", "Level 2"),
                        #                                       selected = "Level 1"),
                        #                           uiOutput(outputId = "out_filter_l3_gen")
                        #              ),
                        # 
                        #              mainPanel(
                        #                h1("COVID-19 Vulnerability Rank Map"),
                        #                fluidRow(
                        #                  box(title = h4("COVID-19 Vulnerability"), collapsible = TRUE, leafletOutput("out_vmap_gen"))
                        #                ),
                        #                hr(),
                        #                h1("Indicators"),
                        #                fluidRow(
                        #                  box( title = h4(">60 population density"), collapsible = TRUE, leafletOutput("out_pop60_gen") ),
                        #                  box( title = h4("40 - 60 population density"), collapsible = TRUE, leafletOutput("out_pop40_gen")),
                        #                  box( title = h4("Mobility"), collapsible = TRUE, leafletOutput("out_mobile_gen") ),
                        #                  box( title = h4("Handwashing"), collapsible = TRUE, leafletOutput("out_wash_gen") )
                        #                )
                        #              )
                        # 
                        #            )
                        #          )
                        # 
                        #          ),

                        tabPanel("Other Sources",
                                 fluidPage(
                                   sidebarLayout(
                                     sidebarPanel(width = 2,

                                                  selectInput(inputId = "in_sources_osrc", label = "Select source:",
                                                              choices = c("Cooper/Smith", "Surgo Foundation"),
                                                              selected = "Surgo Foundation"),
                                                  selectInput(inputId = "ctry",
                                                              label = "Country:",
                                                              choices = c("Angola", "Benin", "Burkina Faso", "Cameroon","DR Congo", 
                                                                          "Ethiopia", "Guinea", "Ghana", "Liberia", "Mozambique", "Niger", 
                                                                          "Nigeria", "Rwanda", "Senegal","Uganda","Zambia", "Zimbabwe")
                                                              ),
                                                  uiOutput("out_filter_l2_osrc"),
                                                  # conditionalPanel(
                                                  #   condition = "input.in_sources_osrc == 'Custom Scores'",
                                                  # checkboxGroupInput(inputId = "in_indicator_inctry",
                                                  #                    label = "Select indicators to include:",
                                                  #                    choices = c("Population: 65 yrs and older (%)"="pop_perc_65_years_and_over",
                                                  #                                "Population: 45 - 64 yrs (%)"="pop_perc_45_to_64_years"  ,
                                                  #                                "Population: 25 - 44 yrs"="pop_perc_25_to_44_years",
                                                  #                                "Population: 15 - 24 years (%)"="pop_perc_15_to_24_years",
                                                  #                                "Population: 5 - 14 yrs (%)"="pop_perc_5_to_14_years",
                                                  #                                "Population 0 - 4 yrs (%)"="pop_perc_0_to_4_years",
                                                  #                                "Mobility: Grocery & Pharmacy"="mob_grocery_and_pharmacy",
                                                  #                                "Mobility: Parks"="mob_parks",
                                                  #                                "Mobility: Residential"="mob_residential",
                                                  #                                "Mobility: Retail & Recreation"="mob_retail_and_recreation",
                                                  #                                "Mobility: Transit Stations"="mob_transit_stations",
                                                  #                                "Mobility: Workplaces"="mob_workplaces",
                                                  #                                "Handwashing: Population living in households with access to soap and water (%)"="population_living_in_households_with_a_basic_handwashing_facility_with_soap_and_water_available",
                                                  #                                "Handwashing: Population living in households where a place for handwashing was observed (%)"="population_with_a_place_for_handwashing_was_observed"
                                                  #                                # "Malaria: Number of health workers"="mal_health_workers",
                                                  #                                # "Malaria: Annual new consultations (fever)"="mal_new_consultation_all_cause" # temporarily removing malaria indicators
                                                  #                                ),
                                                  #                    selected = "pop_perc_65_years_and_over")),
                                                  conditionalPanel(
                                                    condition = "input.in_sources_osrc == 'Surgo Foundation'",
                                                    uiOutput("out_surgo_bt_wi")
                                                  )
                                                  ),
                                     mainPanel(
                                       conditionalPanel(
                                         condition = "input.in_sources_osrc == 'Surgo Foundation'",
                                       h2("Surgo COVID-19 Vulnerability Rank Map"),
                                       p("Scores within the Surgo Foundation portion of this page are from the CCOVID-19
                                         Community Vulnerability Index (CCVI), which the Surgo Foundation created to inform 
                                         coronavirus planning and response efforts. These scores are percentiles, ranging from 0 to 1,
                                         representing the following seven themes: socioeconomic status, population density,
                                         access to housing & transportation, epidemiological factors, health system factors,
                                         fragility, and age.", br()),
                                       tags$i("More information regarding the Cooper/Smith scores will be added as their data
                                            is incorporated into this app. "), br()),
                                         h6(tags$strong("Click on the plot below to view information on all of the CCVI scores for
                                            each region.")), 
                                     conditionalPanel(
                                       condition = "input.in_sources_osrc == 'Cooper/Smith'",
                                       h2("Cooper/Smith COVID-19 Vulnerability Rank Map"),
                                      tags$i("Cooper/Smith Scores are not currently available within the Malaria-COVID App. ")),
                                       # conditionalPanel(
                                       #   condition = "input.in_sources_osrc == 'Custom Scores'",
                                       #   h2("Customizable COVID-19 Vulnerability Rank Map"),
                                       #   p("The custom vulnerability scores are calculated following the", (a("Surgo Foundation's COVID-19 
                                       #     Community Vulnerability Index methodology", href="https://covid-static-assets.s3.amazonaws.com/Africa+CCVI+methodology.pdf")),
                                       #     ", using a stepwise ranking procedure. First, indicator values for the most recently available data are
                                       #     grouped by country and converted into percentiles. These indicators are then aggregated to themes 
                                       #     (i.e. mobility indicators are aggregated to the mobility theme, handwashing data indicators 
                                       #     are aggregated to the handwashing theme, etc.). The indicators 
                                       #     and themes are both weighted equally when calculating the overall score, following Surgo’s 
                                       #     methodology. The custom vulnerability scores deviate from Surgo’s methodology through some key 
                                       #     features: the custom scores use the most recently available data for each of the indicators 
                                       #     while Surgo appears to use both recent data and historical data. The Surgo Foundation's scores calculate cross-country
                                       #     comparisons, which we have opted not to include. The custom scores are also 
                                       #     determined by user selected variables; they are recalculated each time the user’s selections 
                                       #     change, whereas the Surgo scores are static since there is no option for the user to select 
                                       #     indicators. The risk factors included for the custom scores are population age structure, 
                                       #     mobility, and hand washing. We obtained population age structure data from ", 
                                       #     (a("World Population", href="https://www.worldpop.org/geodata/listing?id=65")),  
                                       #     ", mobility data from ", (a("Google COVID-19 Community Mobility Reports", href="https://www.google.com/covid19/mobility/")), 
                                       #     ", and hand washing data from the ", 
                                       #     (a("Demographic Health Survey.", href="http://api.dhsprogram.com/#/index.html")), br()),
                                       #   h6(tags$strong("Click on the plot below to view the scores for your selected indicator(s) in
                                       #      each region."))),
                                       leafletOutput(outputId = "surgo_map")
                                     )
                                     
                                   )
                                 )
                                 )
                        )




#-------------------------------------------------------------------------------------
# Define Info tab
#-------------------------------------------------------------------------------------


tab_info <- tabPanel("Info",

                     navlistPanel(well = FALSE, widths = c(2, 10),
                                  tabPanel("Getting Started",
                                           tags$head(tags$style('.card {
                                                   width: 250px;
                                                   height: 320px;
                                                 clear: both;
                                                 /* Add shadows to create the "card" effect */
                                                 box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
                                                 transition: 0.3s;
                                                 }
                                                 /* On mouse-over, add a deeper shadow */
                                                 .card:hover {
                                                 box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2);
                                                 }
                                                 /* Add some padding inside the card container */
                                                 .container {
                                                 width: 250px;
                                                 padding: 2px 16px;
                                                 }
                                                .center {
                                                  display: block;
                                                  margin-left: auto;
                                                  margin-right: auto;
                                                  width: 80%;
                                                  }')),
                                           fluidPage(
                                             fluidRow(
                                               column(width = 10,
                                                      HTML("<p>Population-level and health systems data is a powerful resource for understanding how viruses are spreading and which communities are at risk. 
                                                            However, interpreting that information is challenging.  Health authorities from some of the PMI focus countries have already developed their own 
                                                            dashboards, the <b>COVID-19 and Malaria Monitoring and Vulnerability Tool in M-DIVE</b> offers a means for PMI and national malaria control programs to 
                                                            visualize existing data in an integrated way to inform high level COVID-19 and malaria related <b>questions</b> such as:</p>
                                                            <div style='text-indent: 1em;'><i>To what extent are changes to treatment-seeking patterns or 
                                                            intervention campaigns (or both) are having an effect on malaria burden?</i></div>
                                                            <br><p>Visualizations in this dashboard can provide <b>insights</b> but are <b>not meant to empirically answer</b>
                                                            key analytical questions related to the impact of COVID-19 and vulnerability. Please see About section to learn more on the dashboard's objectives
                                                            and limitations. List of data used in this dashboard and the description can be found in the Data Description section. </p>"), 
                                                      br())),
                                             hr(),
                                             fluidRow(column(width = 10, HTML("<h2>Visualizations</h2><br>
                                                                 <p>The dashboard consists of two main tabs, Malaria Impact and COVID-19 Vulnerability, that can be accessed from the top navigation bar. 
                                                                 Each tab consists of visualizations described below."))),
                                             fluidRow(
                                               column(width = 12, #style='border-right: 1px solid #d9d9d9',
                                                      HTML("<h4 style='color:#d9230f;'><b>Malaria Impact</b></h4>"),
                                                      br(),
                                                      flowLayout(
                                                        card(.id = "card_snap", .img = "snapshot_tab.png",
                                                             .ctitle = "Snapshot", 
                                                             .cdescr = "National and subnational overview of changes in malaria indicators",
                                                             .actBttn = actionButton(inputId = "go2snap", label = "GO")),
                                                        card(.id = "card_pcnt", .img = "change_tab.jpeg",
                                                             .ctitle = "Changes In Indicator", 
                                                             .cdescr = "Time series of percentage changes in malaria-associated indicators, percent changes in mobility, covid cases",
                                                             .actBttn = actionButton(inputId = "go2pcnt", label = "GO")),
                                                        card(.id = "card_ts", .img = "historical_tab.png",
                                                             .ctitle = "Historical Time Series", 
                                                             .cdescr = "Time series of malaria-associated indicators, covid cases and forecasts",
                                                             .actBttn = actionButton(inputId = "go2ts", label = "GO")),
                                                        card(.id = "card_rf", .img = "rainfall_tab.png",
                                                             .ctitle = "Rainfall", 
                                                             .cdescr = "Compare monthly patterns/trends between malaria indicators and rainfall.",
                                                             .actBttn = actionButton(inputId = "go2rf", label = "GO")),
                                                        cellArgs = list( style = "width: auto; height: auto; margin: 5px;")))),
                                             br(),br(),
                                             fluidRow(
                                               column(width = 12, 
                                                      HTML("<h4 style='color:#d9230f;'><b>COVID-19 Vulnerability</b></h4>"),
                                                      br(),
                                                      flowLayout(
                                                        card(.id = "card_custom", .img = "custom_tab.png",
                                                             .ctitle = "Vulnerability Custom Scoring", 
                                                             .cdescr = "Calculate COVID-19 vulnerability index based on demographics, mobility, access to handwashing, number of health workers, etc.",
                                                             .actBttn = actionButton(inputId = "go2custom", label = "GO")),
                                                        card(.id = "card_others", .img = "surgo_tab.jpeg",
                                                             .ctitle = "Other Sources", 
                                                             .cdescr = "COVID-19 vulnerability index developed by Surgo Foundation",
                                                             .actBttn = actionButton(inputId = "go2surgo", label = "GO")),
                                                        cellArgs = list( style = "width: auto; height: auto; margin: 5px;"))))
                                             
                                             
                                             
                                             
                                           )),
                                  
                                  tabPanel("About",
                                           fluidPage(style = "margin-left:40px;",
                                                     includeMarkdown("info_files/about.Rmd"))
                                  ),
                                  tabPanel("Data Description",
                                           fluidPage(style = "margin-left:40px;",
                                                     h1("Data Description"),
                                                     br(),
                                                     tabsetPanel(id = "mytabset",
                                                                 
                                                                 tabPanel("Malaria", 
                                                                          fluidRow(column(width = 10,
                                                                                          includeMarkdown("info_files/malaria_data.Rmd")))),
                                                                 tabPanel("COVID-19 Data and Indicators",
                                                                          fluidRow(column(width = 10,
                                                                                          includeMarkdown("info_files/covid_data.Rmd") %>%
                                                                                            stringr::str_replace("<table>", '<table class=\"table\">') %>%
                                                                                            HTML()))
                                                                 ),
                                                                 tabPanel("Rainfall", 
                                                                          fluidRow(column(width = 10, 
                                                                                          includeMarkdown("info_files/rainfall_data.RMD")))
                                                                 )))),
                                  tabPanel("Methods",
                                           fluidPage(style = "margin-left:40px;", includeMarkdown("info_files/methods.Rmd")))
                     )
                         
                       
                     )
                       

                    
            


# --------------------------------------------------------------------------------------------------------------------------------
# Assemble the UI
#--------------------------------------------------------------------------------------------------------------------------------
ui <- navbarPage(title = HTML("COVID-19 and Malaria Tracking and Vulnerability Tool &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
                  id = "top_page", 
                  theme = "bootstrap_simplex.css",
                 # tags$head(includeCSS("www/bootstrap_simplex.css")),
                   
                   # PANEL 1 - INFO
                   tab_info,
                   
                   # PANEL 2
                   tab_malaria,
                   
                   # PANEL 3 - COVID
                   tab_covid #,
                   
                   # # PANEL 4 - CLIMATE(?)
                   # tab_rf
                   
                   
)