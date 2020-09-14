
#--------------------------------------------------------------------------------------------------------------------------------
# Define malaria tab
#--------------------------------------------------------------------------------------------------------------------------------

tab_malaria <- 
  navbarMenu("Malaria Impact", 
             tabPanel("National Level",
                      
                      fluidPage(theme = "bootstrap_simplex.css",
                        
                        tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
                        
                        # tags$style(type = "text/css", "#out_plot_mal_adm0 {height: calc(100vh - 150px) !important;}"),
                        
                        
                        useShinyjs(),
                        
                        # fluidRow(
                          # # column(2, HTML("<B>Malaria Tracker: National Level</B>")),
                          # column(8, p(HTML("KEY ANALYTICAL QUESTIONS:")),
                          #         tags$ol(
                          #           tags$li("How do malaria indicators reported in 2020 - during COVID-19 pandemic - appear to compare to previous years and/or historical mean?"),
                          #           tags$li("By how much do the indicators in 2020 exceed or below the level of previous years?"),
                          #           tags$li("Do any changes in malaria indicators coincide with COVID19 dynamics?"),
                          #           tags$li("What is the forecasted COVID-19 trajectory and how does the timing potentially coincide with malaria season?")
                          #         )
                          #  
                          # ),
                          # column(2)
                        # ),
                        
                        br(),
                        actionButton(inputId = "toggle_side_mal_adm0", label = "  Selector", icon = icon("angle-double-down"), 
                                     style = "color: #000000; background: #fcfcfc; border: #fcfcfc"),
                        br(),
                        sidebarLayout(
                          div(id = "sidebar_mal_adm0", 
                              sidebarPanel(width = 2,
                                           h5(HTML("<b>Plot Options</b>")), 
                                           br(),
                                           selectInput(inputId = "in_plot_type_mal_adm0", label = "View plots by:",
                                                       choices = c("Country", "Indicator"), selected = "Country"),
                                           uiOutput(outputId = "out_filter_l2_mal_adm0"),
                                           uiOutput(outputId = "out_filter_l3_mal_adm0" ),
                                           prettyRadioButtons(inputId = "in_yaxs_mal_adm0",
                                                              label = HTML("Y-Axis:"), choices = c("Cases per 1000 population" = "rate", "Cases" = "count"),
                                                              inline = FALSE, status = "default"),
                                           prettyRadioButtons(inputId = "in_radio_include_adm0", label = HTML("Include:"),
                                                      choices = c("COVID-19 Forecasts", "Reporting Rate", "None"), inline = FALSE,
                                                      fill = FALSE, status = "default", selected = "None"),
                                           hr(),
                                           h5(HTML("<b>Calculate 2020 Data Difference</b>")),
                                           br(),
                                           selectInput(inputId = "in_yr0_mal_adm0", label = "Select year to compare:", choices = c(rev(2016:2019), "Historical mean"), 
                                                       selected = "Historical mean"),
                                           fluidRow(column(6,
                                                           selectInput(inputId = "in_yr0m0_mal_adm0", label = "Start Month:", choices = month.abb, selected = "Jan")),
                                                    column(6,
                                                           selectInput(inputId = "in_yr0mf_mal_adm0", label = "End Month:", choices = month.abb, selected = "Mar")))
                                           )),
                           mainPanel(setBackgroundColor("white"),
                                     fluidRow(
                                       # column(2, HTML("<B>Malaria Impact: National Level</B>")),
                                       column(10, p(HTML("KEY ANALYTICAL QUESTIONS:")),
                                              tags$ol(
                                                tags$li("How do malaria indicators reported in 2020 - during COVID-19 pandemic - appear to compare to previous years and/or historical mean?"),
                                                tags$li("By how much do the indicators in 2020 exceed or below the level of previous years?"),
                                                tags$li("Do any changes in malaria indicators coincide with COVID19 dynamics?"),
                                                tags$li("What is the forecasted COVID-19 trajectory and how does the timing potentially coincide with malaria season?")
                                              )
                                              
                                       )
                                     ),
                                     # fluidRow(
                                     #   column(6),
                                     #   column(3,
                                     #          prettyRadioButtons(inputId = "in_yaxs_mal_adm0",
                                     #                               label = HTML("<b>Y-Axis:</b>"), choices = c("Cases per 1000 population" = "rate", "Cases" = "count"),
                                     #                             inline = TRUE, status = "default")),
                                     #   column(3, 
                                     #          prettyRadioButtons(inputId = "in_radio_include_adm0", label = HTML("<b>Include:</b>"), 
                                     #            choices = c("COVID-19 Forecasts", "Reporting Rate", "None"), inline = TRUE, 
                                     #            fill = FALSE, status = "default",
                                     #          ))
                                     #   ),
                                    fluidRow(plotlyOutput("out_plot_mal_adm0", width = "auto")))
                        ))),
             
             
             # Subnational panel --------------------------------------------------------------------------------
             
             tabPanel("Sub-national Level",
                      fluidPage(
                        fluidRow(
                          column(2, HTML("<B>Malaria Impact: Sub-national Level</B>")),
                          column(8, p(HTML("KEY ANALYTICAL QUESTIONS:")),
                                 tags$ol(
                                   tags$li("How do malaria indicators in 2020 - during COVID-19 pandemic - compared to previous years and/or mean?"),
                                   tags$li("By how much do the indicators in 2020 exceed or below the level of previous years?"),
                                   tags$li("Do any changes in malaria indicators coincides (or lags) with COVID19 dynamics?"),
                                   tags$li("For key malaria indicators, which geographic areas have seen changes this year compared to previous years and/or the historical mean?")
                                 )),
                          column(2)),
                        br(),
                        actionButton(inputId = "toggle_side_mal_adm1", label = "  Selector", icon = icon("angle-double-down"), 
                                     style = "color: #000000; background: #fcfcfc; border: #fcfcfc"),
                        br(),
                        sidebarLayout(
                          div(id = "sidebar_mal_adm1", 
                              sidebarPanel(width = 2,
                                           h5(HTML("<b>Plot Options</b>")), 
                                           br(),
                                           selectInput(inputId = "in_country_mal_adm1", label = "Country:",
                                                       choices = c(sort(as.character(unique(xctry$country)))),
                                                       selected = (sort(as.character(unique(xctry$country))))[1]),
                                           selectInput(inputId = "in_plot_type_mal_adm1", label = "View plots by:",
                                                       choices = c("Admin. Level 1", "Indicator"), selected = "Admin. Level 1"),
                                           uiOutput(outputId = "out_filter_l2_mal_adm1"),
                                           uiOutput(outputId = "out_filter_l3_mal_adm1" ),
                                           prettyRadioButtons(inputId = "in_yaxs_mal_adm1",
                                                              label = HTML("Y-Axis:"), choices = c("Cases per 1000 population" = "rate", "Cases" = "count"),
                                                              inline = FALSE, status = "default", selected = "rate"),
                                           prettyRadioButtons(inputId = "in_radio_include_adm1", label = HTML("Include:"),
                                                              choices = c("Reporting Rate", "None"), inline = FALSE,
                                                              fill = FALSE, status = "default", selected = "None"),
                                           hr(),
                                           h5(HTML("<b>Calculate 2020 Data Difference</b>")),
                                           br(),
                                           selectInput(inputId = "in_yr0_mal_adm1", label = "Select year to compare:", choices = c(rev(2016:2019), "Historical mean"), selected = "Historical mean"),
                                           fluidRow(column(6,
                                                           selectInput(inputId = "in_yr0m0_mal_adm1", label = "Start Month:", choices = month.abb, selected = "Jan")),
                                                    column(6,
                                                           selectInput(inputId = "in_yr0mf_mal_adm1", label = "End Month:", choices = month.abb, selected = "Mar")))
                              )),
                          mainPanel(setBackgroundColor("white"),
                                    fluidRow(plotlyOutput("out_plot_mal_adm1", width = "auto")))
                        )))
             )




#--------------------------------------------------------------------------------------------------------------------------------
# Define info tab
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
                                                                choices = c("Benin", "Guinea", "...")),
                                                    selectInput(inputId = "in_adminlevel_inctry", label = "Administrative Level:",
                                                                choices = c("Level 1", "Level 2"),
                                                                selected = "Level 1"),
                                                    uiOutput(outputId = "out_filter_l3_inctry")
                                       ), 
                                       
                                       ## Main panel -----------------------------------------------------------------------------
                                       mainPanel(
                                         fluidRow(
                                         column(8, p(HTML("KEY ANALYTICAL QUESTIONS:")),
                                                         tags$ol(
                                                           tags$li("Which geographic area is most vulnerable to COVID-19 based on selected indicators?"),
                                                           tags$li("How does the individual indicator vary within the country?")
                                                         ))),
                                         br(),
                                         HTML("<h4><b>COVID-19 Vulnerability Rank Map</b></h4>
                                                         <h6>Currently a placeholder - risk ranking here obtained from Cooper/Smith as an example</h6>"),
                                         fluidRow(
                                           box(collapsible = TRUE, leafletOutput("out_vmap_inctry"))
                                         ),
                                         hr(),
                                         h4(HTML("<b>Indicators</b>")),
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
                        
                        tabPanel("General Calculator",
                                 fluidPage(
                                   sidebarLayout(
                                     sidebarPanel(width = 2,
                                                  selectInput(inputId = "in_ctry_covid_gen", label = "Country:",
                                                              choices = c("Benin", "Guinea", "...")),
                                                  selectInput(inputId = "in_adminlevel_gen", label = "Administrative Level:",
                                                              choices = c("Level 1", "Level 2"),
                                                              selected = "Level 1"),
                                                  uiOutput(outputId = "out_filter_l3_gen")
                                     ), 
                                     
                                     mainPanel(
                                       h1("COVID-19 Vulnerability Rank Map"),
                                       fluidRow(
                                         box(title = h4("COVID-19 Vulnerability"), collapsible = TRUE, leafletOutput("out_vmap_gen"))
                                       ),
                                       hr(),
                                       h1("Indicators"),
                                       fluidRow(
                                         box( title = h4(">60 population density"), collapsible = TRUE, leafletOutput("out_pop60_gen") ),
                                         box( title = h4("40 - 60 population density"), collapsible = TRUE, leafletOutput("out_pop40_gen")),
                                         box( title = h4("Mobility"), collapsible = TRUE, leafletOutput("out_mobile_gen") ),
                                         box( title = h4("Handwashing"), collapsible = TRUE, leafletOutput("out_wash_gen") )
                                       )
                                     )
                                     
                                   )
                                 )
                                 
                                 ), 
                        
                        tabPanel("Other Sources",
                                 fluidPage(
                                   sidebarLayout(
                                     sidebarPanel(width = 2,
                                                  selectInput(inputId = "in_sources_osrc", label = "Select source:",
                                                              choices = c("Smith/Cooper", "Surgo Foundation")),

                                                  uiOutput(outputId = "out_filter_l2_osrc")
                                     ), 
                                     
                                     mainPanel(
                                       h1("COVID-19 Vulnerability Rank Map"),
                                       fluidRow(
                                         box(title = h4("COVID-19 Vulnerability"), collapsible = TRUE, leafletOutput("out_vmap_osrc"))
                                       )
                                     )
                                     
                                   )
                                 )
                                 )
                        
                        )





#-------------------------------------------------------------------------------------
# Define Info tab
#-------------------------------------------------------------------------------------


tab_info <- tabPanel("Info",
                      fluidPage(
                       column(2, 
                             tags$style(HTML("
                                  .btn-danger{
                                      color:  #424242;
                                      border: 2px #fff solid;
                                      background-color: #f7f7f7;
                                  }
                                  .btn-danger:hover {
                                      color: #424242;
                                      background-color: #c6c6c6;
                                  }
                                  .btn-danger.active, .btn-danger:active, .open > .dropdown-toggle.btn-danger {
                                      color: red;
                                      background-color: #f7f7f7;
                                      border-color: #fff;
                                  }
                                  .btn-danger:active:hover, .btn-danger.active:hover, .open > .dropdown-toggle.btn-danger:hover,
                                  .btn-danger:active:focus, .btn-danger.active:focus, .open > .dropdown-toggle.btn-danger:focus,
                                  .btn-danger:active.focus, .btn-dnager.active.focus, .open > .dropdown-toggle.btn-danger.focus {
                                      color: red;
                                      background-color: #2ecc71;
                                      border-color: #fff;
                                  }
                                  .btn-danger.disabled:hover, .btn-danger[disabled]:hover, fieldset[disabled] .btn-danger:hover,
                                  .btn-danger.disabled:focus, .btn-danger[disabled]:focus, fieldset[disabled] .btn-danger:focus,
                                  .btn-danger.disabled.focus, .btn-danger[disabled].focus, fieldset[disabled] .btn-danger.focus {
                                      color: #2ecc71;
                                      background-color: #2ecc71;
                                      border-color: #fff;
                                  }
                                  .btn-danger{
                                      background-image: linear-gradient(#f7f7f7, #ededed 6%, #e3e3e3);
                                      background-repeat: no-repeat;
                                      -webkit-filter: none;
                                      filter: none;
                                      border: 1px solid #fff;
                                  }
                                  .btn-danger:hover{
                                      background-image: linear-gradient(#bcbcbc, #b2b2b2 6%, #9f9f9f);
                                      background-repeat: no-repeat;
                                      border: 1px solid #fff;
                                  }
                                  .btn-danger:focus{
                                      color: red;
                                      background-image: linear-gradient(#f7f7f7, #ededed 6%, #e3e3e3);
                                      background-repeat: no-repeat;
                                      -webkit-filter: none;
                                      filter: none;
                                      border: 1px solid #fff;
                                  }
                                  btn-danger:active{
                                      color: red;
                                      background-image: linear-gradient(#f7f7f7, #ededed 6%, #e3e3e3);
                                      background-repeat: no-repeat;
                                      -webkit-filter: none;
                                      filter: none;
                                      border: 1px solid #fff;
                                  }
                                  .btn-danger .badge {
                                      color: #d9230f;
                                      background-color: #ffffff;
                                  }
                               ")),
                               
                              br(),
                              radioGroupButtons(inputId = "in_go_info", label = NULL, status = 'danger',
                                                       choices = c("About &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" = 'about',
                                                                   "Getting Started  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" = "start",
                                                                   "Malaria &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" = 'malaria',
                                                                   "COVID-19 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" = 'covid'),
                                                       direction = "vertical")
                              
                              ## hr(),
                              # actionButton(inputId = "in_about", label = "About", style = "color: #5c5c5c; background: #f2f2f2; border: 2px white solid; text-align: left;"),
                              ## hr(),

                              # actionButton(inputId = "in_start", label = "Getting Started", style = "color: #5c5c5c; background: #f2f2f2; border: 2px white solid; text-align: left;"),
                              ## hr(),

                              # actionButton(inputId = "in_mal", label = "Malaria Data", style = "color: #5c5c5c; background: #f2f2f2; border: 2px white solid; text-align: left;"),
                              ## hr(),

                              # actionButton(inputId = "in_cov", label = "Covid Data", style = "color: #5c5c5c; background: #f2f2f2; border: 2px white solid; text-align: left;")
                              # hr()
                              
                              ),
                       column(10, 
                              uiOutput("out_info"))
                     )
                     
                         
                       
                     )
                       

                     
                     
                     
#--------------------------------------------------------------------------------------------------------------------------------
# Define rainfall tab
#--------------------------------------------------------------------------------------------------------------------------------


tab_rf <- 
  navbarMenu("Rainfall",
             
             tabPanel("National Level", # theme = "bootstrap_simplex.css",
                      
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
                                                                        "Cumulative (Aug-July)" = "acc_ssn"),
                                                            selected = "Monthly Total",
                                                            inline = FALSE, status = "default"),
                                         hr(),
                                         h6(HTML("<b>Epidemiological Indicator</b>")),
                                         br(),
                                         selectInput(inputId = "in_indicator_rf_adm0", label = "Indicator:",
                                                     choices = c("All Cause Consultations" = "allcause_cases", 
                                                                 "Tested Cases" = "tested_cases", 
                                                                 "Malaria Confirmed Cases" = "confirmed_cases",
                                                                 "Test Positivity Ratio" = "tpr", 
                                                                 "Malaria Severe Cases" = "severe_cases", 
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
                      
                      
                      ), # End of tab Panel - RF National
             
             tabPanel("Sub-national Level", theme = "bootstrap_simplex.css"),
             
             tabPanel("Compare with Malaria Indicator", theme = "bootstrap_simplex.css",
                      
                      fluidPage(
                        
                        fluidRow(column(2, p(HTML("<b>Rainfall: Compare With Malaria Indicator</b>"))),
                                 column(8, p(HTML("KEY ANALYTICAL QUESTIONS:")),
                                        tags$ol(
                                          tags$li("Q1"),
                                          tags$li("Q2")
                                        ),
                                        column(2))
                          
                        ), # End of 1st fluidRow
                        
                        
                        fluidRow(
                          
                          sidebarLayout(
                            
                            sidebarPanel(width = 2,
                                         selectInput(inputId = "in_adminlevel_rfcomp", 
                                                     label = "Select Administrative Level",
                                                     choices = c("National", "Sub-national (Admin. Level 1)"), 
                                                     selected = "National"),
                                         selectInput(inputId = "in_adm0_rfcomp",
                                                     label = "Select Country:",
                                                     choices = c(sort(as.character(unique(xctry$country)))),
                                                     selected = "Benin"),
                                         
                                         uiOutput(outputId = "out_adm1_menu_rfcomp"),
                                         
                                          selectInput(inputId = "in_indicator_rfcomp",
                                                       label = "Select Malaria Indicator",
                                                       choices = c("All Cause Consultation", "Malaria Confirmed Cases",
                                                                      "Malaria Test Positivity Rate", "Severe Malaria Cases",
                                                                       "Malaria Deaths", "ANC Visit", "COVID-19 Cases"),
                                                        selected = "Malaria Confirmed Cases"),
                                         
                                         prettyRadioButtons(inputId = "in_yrs_rfcomp",
                                                            label = "Select years to highlight (up to 5)", 
                                                            choices = 2016:2020,
                                                            inline = FALSE, status = "default")
                                         
                                         ),
                            
                            mainPanel(
                              br(),
                              fluidRow(column(6, "Malaria Indicator plot here"),
                                       column(6, "Rainfall plot here"))
                            )

                            
                          )
                          
                          
                        ),
                        
                      ) # End of Fluid Page
                      
                      ) # End of tab Panel - RF Compare
)
            


# --------------------------------------------------------------------------------------------------------------------------------
# Assemble the UI
#--------------------------------------------------------------------------------------------------------------------------------
ui <- navbarPage(title = HTML("COVID-19 and Malaria Tracking and Vulnerability Tool &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
                   
                  theme = "bootstrap_simplex.css",
                 # tags$head(includeCSS("www/bootstrap_simplex.css")),
                   
                   # PANEL 1 - INFO
                   tab_info,
                   
                   # PANEL 2
                   tab_malaria,
                   
                   # PANEL 3 - COVID
                   tab_covid,
                   
                   # PANEL 4 - CLIMATE(?)
                   tab_rf
                   
                   
)