# modify in-country calculator for out of country viewers

# this can be used at line 416 in order to replace the "general calculator" tab

observeEvent(input$in_ctry_covid_inctry, ignoreNULL = FALSE, {
  if (input$in_adminlevel_inctry %in% "Level 1" & input$in_adminlevel_inctry %!in% user_filtered_countries) {
    updateSelectInput(session,
                      inputId = "in_indicator_inctry",
                      choices = c(
                        ">60 population density", "30 - 60 population density",
                        "20 - 30 population density", "10 - 20 population density",
                        " 5 - 10 population density", " 0 - 5 population density",
                        "Mobility (Google)", "Handwashing (Survey)"
                      )
                      
    )
  }
  
  else if(input$in_adminlevel_inctry %!in% user_filtered_countries & input$in_adminlevel_inctry %in% "Level 1") { 
    updateSelectInput(session,
                      inputId = "in_indicator_inctry", 
                      choices =c(
                        ">60 population density", "30 - 60 population density",
                        "20 - 30 population density", "10 - 20 population density",
                        " 5 - 10 population density", " 0 - 5 population density",
                      )
    )}
  
  
})

# to add ULF to any country selection group, use this code
observe({
  updateSelectInput(session, "input_id", 
                    selected = (sort(as.character(unique(xctry$country))))[1], 
                    choices=user_filtered_countries
  )})