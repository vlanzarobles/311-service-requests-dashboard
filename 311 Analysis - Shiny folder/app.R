
# Shiny app: Analysis of the City of Chicago 311 Service Requests 
# April 2024
# Violeta Lanza Robles

# 1.Load packages

    library(tidyverse)
    library(sf)
    library(ggplot2)
    library(dplyr)
    library(shiny)
    library(shinyFeedback)
    library(plotly)

    
# 2.Paths and file names to be changed to reproduce the results

    # Path to Shiny folder where the data used in the app are stored:
    
    wards_file_name <- "chicago_wards_data.csv"
    education_file_name <- "education_data.csv"
    race_file_name <- "race_data.csv"
    poverty_file_name <- "poverty_data.csv"
    streetpotholes_file_name <- "Pothole in Street Complaint.csv"
    rodents_file_name <- "Rodent BaitingRat Complaint.csv"
    
    # Shapefiles:
    
    # Shapefiles path for the Chicago wards, relative to the Shapefile folder path:
    wards_shapefile_relative_path <- "geo_export_60924955-2a68-41a6-86c1-7dbcdded901b.shp"

# 3.Shiny app
    
    
    ui <- fluidPage(
      tags$br(),
      titlePanel("311 Service Requests: Are We Serving Equitably?"),
      tags$br(),
      tabsetPanel(
        tabPanel("Sociodemographic context",
                 navlistPanel("Choose information to display:",
                              tabPanel("Educational Attainment",
                                       column(width = 1),
                                       column(width = 11,
                                                tags$br(),
                                                selectInput(inputId = "education_selection",
                                                            label = "Choose a variable:",
                                                            choices = c("less_than_high_school_graduate", "high_school_graduate", 
                                                                        "some_college_or_associates_degree",
                                                                        "bachelors_degree_or_higher")),
                                                 selectInput(inputId = "zipcode_selection_education",
                                                             label = "Choose a ZIP code:",
                                                             choices = NULL),
                                                tableOutput("education_zipcode_table"),
                                                tags$br(),
                                                verbatimTextOutput("education_title_output"),
                                                plotlyOutput("education_plot"),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                verbatimTextOutput("education_caption_output"),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br()
                                         ) # End of column of width 11
                              ), # End of tabPanel "Education",
                              tabPanel("Race",
                                       column(width = 1),
                                       column(width = 11,
                                                tags$br(), 
                                                selectInput(inputId = "race_selection",
                                                            label = "Choose a variable:",
                                                            choices = c("white", "black", "american_indian", "asian", 
                                                                        "pacific_islander", "two_or_more", "other")), 
                                                selectInput(inputId = "zipcode_selection_race",
                                                            label = "Choose a ZIP code:",
                                                            choices = NULL),
                                                tableOutput("race_zipcode_table"),
                                                tags$br(),
                                                verbatimTextOutput("race_title_output"),
                                                plotlyOutput("race_plot"),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                verbatimTextOutput("race_caption_output"),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br()
                                         ) # End of column of width 11
                              ), #End of tabpanel "Race",
                              tabPanel("Poverty Status",
                                       column(width = 1),
                                       column(width = 11,
                                                tags$br(),
                                                selectInput(inputId = "poverty_selection",
                                                            label = "Choose a variable:",
                                                            choices = c("income_below_poverty_level", "income_above_poverty_level")), 
                                                selectInput(inputId = "zipcode_selection_poverty",
                                                            label = "Choose a ZIP code:",
                                                            choices = NULL),
                                                tableOutput("poverty_zipcode_table"),
                                                tags$br(),
                                                verbatimTextOutput("poverty_title_output"),
                                                plotlyOutput("poverty_plot"),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                verbatimTextOutput("poverty_caption_output"),
                                                tags$br(),
                                                tags$br(),
                                                tags$br(),
                                                tags$br()
                                         ) # End of column of width 11
                              ) #End of tabpanel "Poverty"
                 )
        ),
        tabPanel("Analysis of 311 Service Requests",
                 navlistPanel("Choose information to display:",
                              tabPanel("Summary of 311 Service Requests",
                                       column(width = 1),
                                       column(width = 11,
                                             tags$br(),
                                             selectInput(inputId = "sort_variable",
                                                         label = "Choose a variable to order service request types in descending order",
                                                         choices = c("owner_department", "number_of_requests", "percentage_of_total_requests", "percentage_completed", "mean_processing_time_days")),
                                             tags$br(),
                                             verbatimTextOutput("summary_table_title_output"),
                                             tags$br(),
                                             tableOutput("summary_table")
                                              ), # End of column of width 11
                              ), # End of tabPanel "General 311 Service Requests information"
                              tabPanel("Street Pothole complaints",
                                       tabsetPanel(
                                         tabPanel("Number of requests",
                                                  tags$br(),
                                                  tags$br(),
                                                  fluidRow(
                                                    column(width = 1),
                                                    column(width = 11,
                                                           sliderInput(inputId = "number_streetpotholes_range",
                                                                       label = "Choose a range for the number of requests received:",
                                                                       value = c(0,100), min = 1, max = 10))
                                                          ), # End of column of width 11
                                                  fluidRow(
                                                    column(width = 1),
                                                    column(width = 11,
                                                           verbatimTextOutput("number_streetpotholes_title_output"),
                                                           plotlyOutput("number_streetpotholes_plot"),
                                                           tags$br(),
                                                           tags$br(),
                                                           tags$br(),
                                                           tags$br(),
                                                           tags$br(),
                                                           tags$br(),
                                                           verbatimTextOutput("number_streetpotholes_caption_output")
                                                    ) # End of column of width 11
                                                  ), # End of fluidRow
                                                  fluidRow(
                                                    column(width = 1),
                                                    column(width = 11,
                                                           tags$br(),
                                                           tableOutput("table_number_streetpotholes")
                                                    ) # End of column of width 11
                                                  ) # End of fluidRow
                                         ), # End of tabPanel "Number of requests"
                                         tabPanel("Average processing days",
                                                  tags$br(),
                                                  tags$br(),
                                                  fluidRow(
                                                    column(width = 1),
                                                    column(width = 11,
                                                           sliderInput(inputId = "time_streetpotholes_range",
                                                                       label = "Choose a range for the average days to close a complaint:",
                                                                       value = c(0,10), min = 1, max = 10))
                                                           ), # End of column of width 11
                                                  tags$br(),
                                                  fluidRow(
                                                    column(width = 1),
                                                    column(width = 11,
                                                           verbatimTextOutput("average_time_streetpotholes_title_output"),
                                                           plotlyOutput("average_time_streetpotholes_plot")
                                                    )
                                                          ), # End of column of width 11
                                                  fluidRow(
                                                    column(width = 1),
                                                    column(width = 11,
                                                           tags$br(),
                                                           tags$br(),
                                                           tags$br(),
                                                           tags$br(),
                                                           tags$br(),
                                                           verbatimTextOutput("average_time_streetpotholes_caption_output")
                                                          )  # End of column of width 11
                                                  )
                                         ),
                                         tags$br(),
                                         fluidRow(
                                           column(width = 1),
                                           column(width = 11,
                                                  tags$br(),
                                                  tableOutput("table_average_time_streetpotholes")
                                                  ) # End of column of width 11
                                         ) # End of fluidRow
                                       ) # End of tabPanel "Average processing days"
                                      ),
                              tabPanel("Rodent complaints",
                                       tabsetPanel(
                                         tabPanel("Number of requests",
                                                  tags$br(),
                                                  tags$br(),
                                                  fluidRow(
                                                    column(width = 1),
                                                    column(width = 11,
                                                           sliderInput(inputId = "number_rodents_range",
                                                                       label = "Choose a range for the number of requests received:",
                                                                       value = c(0,100), min = 1, max = 10))
                                                          ), # End of column of width 11
                                                  fluidRow(
                                                    column(width = 1),
                                                    column(width = 11,
                                                           verbatimTextOutput("number_rodents_title_output"),
                                                           plotlyOutput("number_rodents_plot"),
                                                           tags$br(),
                                                           tags$br(),
                                                           tags$br(),
                                                           tags$br(),
                                                           tags$br(),
                                                           tags$br(),
                                                           verbatimTextOutput("number_rodents_caption_output")
                                                    ) # End of column of width 11
                                                  ), # End of fluidRow
                                                  fluidRow(
                                                    column(width = 1),
                                                    column(width = 11,
                                                           tags$br(),
                                                           tableOutput("table_number_rodents")
                                                    ) # End of column of width 11
                                                  ) # End of fluidRow
                                         ), # End of tabPanel "Number of requests"
                                         tabPanel("Average processing days",
                                                  tags$br(),
                                                  tags$br(),
                                                  fluidRow(
                                                    column(width = 1),
                                                    column(width = 11,
                                                           sliderInput(inputId = "time_rodents_range",
                                                                       label = "Choose a range for the average days to close a complaint:",
                                                                       value = c(0,10), min = 1, max = 10, step = 1))
                                                          ), # End of column of width 11
                                                  tags$br(),
                                                  fluidRow(
                                                    column(width = 1),
                                                    column(width = 11,
                                                           verbatimTextOutput("average_time_rodents_title_output"),
                                                           plotlyOutput("average_time_rodents_plot")
                                                          ) # End of column of width 11
                                                  ),
                                                  fluidRow(
                                                    column(width = 1),
                                                    column(width = 11,
                                                           tags$br(),
                                                           tags$br(),
                                                           tags$br(),
                                                           tags$br(),
                                                           tags$br(),
                                                           verbatimTextOutput("average_time_rodents_caption_output")
                                                           ) # End of column of width 11
                                                  )
                                         ),
                                         tags$br(),
                                         fluidRow(
                                           column(width = 1),
                                           column(width = 11,
                                                  tags$br(),
                                                  tableOutput("table_average_time_rodents")
                                           ) # End of column of width 11
                                         ) # End of fluidRow
                                       ) # End of tabPanel "Average processing days"
                              )
                 )
        )
      )
    )
    
    server <- function(input, output, session) {
      
      
      
      # Function to import a csv as a spatial dataframe, with the correct CRS:
      import_csv <- function(file_path){
        
        # Set the correct CRS for all the maps, which is the CRS in the Chicago wards shapefile:
        chicago_wards <- st_read(paste0(wards_shapefile_relative_path))
        crs_correct <- st_crs(chicago_wards)
        
        dataframe <- read.csv(paste0(file_path))
        dataframe <- st_as_sf(dataframe, wkt = "WKT")
        dataframe <- st_set_crs(dataframe, crs_correct)
        
        return(dataframe)
        
      }
      
      # Import file for Chicago wards:
      chicago_wards_data <- import_csv(wards_file_name)
      
      
      # Sociodemographic information tab:
      
          # General function to create the sociodemographic plots: 
      
                  generate_sociodemographic_plot <- function(data, input_selection, zipcode_selection) {
                    
                    filtered_data <- reactive({
                      data |> filter(zip_codes == zipcode_selection)
                    })
                    
                    plot <- ggplotly(
                      ggplot() +
                        geom_sf(data = data, aes(fill = !!sym(input_selection), text = paste("ZIP Code:", zip_codes, "<br>")), size = 0.05) +
                        geom_sf(data = filtered_data(), aes(text = paste("ZIP Code:", zip_codes, "<br>", paste0(input_selection, ": "), !!sym(input_selection))), fill = "Red") + 
                        scale_fill_distiller(palette = "Blues", direction = 1) + 
                        theme_minimal() +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
                              axis.text.y = element_text(size = 10))) |>
                      layout(height = 562.5, width = 843.75) |>
                      config(tooltip = list(align = "left"))
                    
                    return(plot)
                  }
                  
          # General function to create sociodemographic tables:
                  
                  generate_zipcode_table <- function(dataframe, input_zipcode_selection, table_title){
                    table_data <- st_drop_geometry(dataframe) |> filter(zip_codes == input_zipcode_selection) |> select(-1, -last_col()) 
                    pivot_longer(table_data, cols = everything(), names_to = table_title, values_to = "% population")
                  }
      

          # Education tab:
    
                education_data <- import_csv(education_file_name)
                
                observe({
                  zipcodes_list_education <- unique(education_data$zip_codes)
                  updateSelectInput(session, "zipcode_selection_education",
                                    choices = zipcodes_list_education)
                })

                output$education_plot <- renderPlotly({
                  generate_sociodemographic_plot(education_data, input$education_selection, input$zipcode_selection_education)
                })
                
                output$education_title_output <- renderText({"Educational attainment (% population in category)."})
                
                output$education_zipcode_table <- renderTable({
                  generate_zipcode_table(education_data, input$zipcode_selection_education, "Educational attainment")
                })
                
                output$education_caption_output <- renderText({"Source: American Community Survey 2018-2022 5-Year Data. Table B23006"})
          
          # Race tab:
          
                race_data <- import_csv(race_file_name)
                
                observe({
                  zipcodes_list_race <- unique(race_data$zip_codes)
                  updateSelectInput(session, "zipcode_selection_race",
                                    choices = zipcodes_list_race)
                })
                
                output$race_plot <- renderPlotly({
                  generate_sociodemographic_plot(race_data, input$race_selection, input$zipcode_selection_race)
                })
                
                output$race_title_output <- renderText({"Racial composition (% population in category)."})
                
                output$race_zipcode_table <- renderTable({
                  generate_zipcode_table(race_data, input$zipcode_selection_race, "Racial composition")
                })
                
                
                output$race_caption_output <- renderText({"Source: American Community Survey 2018-2022 5-Year Data. Table B02001"})
          
          #Poverty tab:
          
                poverty_data <- import_csv(poverty_file_name)
                
                observe({
                  zipcodes_list_poverty <- unique(poverty_data$zip_codes)
                  updateSelectInput(session, "zipcode_selection_poverty",
                                    choices = zipcodes_list_poverty)
                })
                
                output$poverty_plot <- renderPlotly({
                  generate_sociodemographic_plot(poverty_data, input$poverty_selection, input$zipcode_selection_poverty)
                })
                
                output$poverty_title_output <- renderText({"Poverty status (% population in category)."})
                
                output$poverty_zipcode_table <- renderTable({
                  generate_zipcode_table(poverty_data, input$zipcode_selection_poverty, "Poverty status")
                })
                
                output$poverty_caption_output <- renderText({"Source: American Community Survey 2018-2022 5-Year Data. Table B17001"})
              
                
      # 311 Service Requests tab:
                
            
            # General 311 Service Requests information tab:
                
                output$summary_table_title_output <- renderText({"Summary of 311 Service Requests. Q1 2024"})
                
                output$summary_table <- renderTable({
                  summary_table <- read.csv(paste0("summary_table.csv")) |>
                    select(owner_department, everything()) |>
                    arrange(desc(!!sym(input$sort_variable)), owner_department)
                  
                  summary_table              
                })
                 
                
            # Street potholes tab:
                
                streetpotholes_data <- import_csv(streetpotholes_file_name)
                
                    # Number of cases tab:
                
                            observe({
                              # Calculate the rounded minimum and maximum values
                              min_val <- floor(min(streetpotholes_data$number_cases))
                              max_val <- ceiling(max(streetpotholes_data$number_cases))
                              
                              # Update the slider input with the rounded values
                              updateSliderInput(session, "number_streetpotholes_range", 
                                                value = c(min_val, max_val),
                                                min = min_val, 
                                                max = max_val)
                            })            
                            
                            number_streetpotholes_data <- reactive({
                              streetpotholes_data |>
                              filter(number_cases >= as.numeric(input$number_streetpotholes_range[1]), 
                                     number_cases <= as.numeric(input$number_streetpotholes_range[2]))
                          })
                            
                            output$number_streetpotholes_plot <- renderPlotly({
                              ggplotly(
                                ggplot() +
                                  geom_sf(data = chicago_wards_data, fill = "transparent", color = "grey", size = 0.05) +
                                  geom_sf(data = number_streetpotholes_data(), aes(fill = number_cases,
                                                                                   text = paste("Ward:", ward, "<br>")))  +
                                  scale_fill_distiller(palette = "Blues", direction = 1) + 
                                  theme_minimal() 
                              ) |>
                                layout(height = 500, width = 750)
                            })
                            
                            
                            output$number_streetpotholes_title_output <- renderText({"Number of Street Pothole complaints. Q1 2024"})
                            
                            output$number_streetpotholes_caption_output <- renderText({"Source: City of Chicago Data Portal."})
                            
                            output$table_number_streetpotholes <- renderTable({
                              st_drop_geometry(number_streetpotholes_data()) |> select(ward, number_cases)
                            })
                  
                    # Average processing time:
                            
                            observe({
                              # Calculate the rounded minimum and maximum values
                              min_val <- floor(min(streetpotholes_data$mean_processing_time_days))
                              max_val <- ceiling(max(streetpotholes_data$mean_processing_time_days))
                              
                              # Update the slider input with the rounded values
                              updateSliderInput(session, "time_streetpotholes_range", 
                                                value = c(min_val, max_val),
                                                min = min_val, 
                                                max = max_val)
                            })
                            
                            average_time_streetpotholes_data <- reactive({
                              streetpotholes_data |>
                                filter(mean_processing_time_days >= as.numeric(input$time_streetpotholes_range[1]), 
                                       mean_processing_time_days <= as.numeric(input$time_streetpotholes_range[2]))
                            })
                            
                            output$average_time_streetpotholes_plot <- renderPlotly({
                              ggplotly(
                                ggplot() +
                                  geom_sf(data = chicago_wards_data, fill = "transparent", color = "grey", size = 0.05) +
                                  geom_sf(data = average_time_streetpotholes_data(), aes(fill = mean_processing_time_days,
                                                                                         text = paste("Ward:", ward, "<br>")))  +
                                  scale_fill_distiller(palette = "Blues", direction = 1) + 
                                  theme_minimal() 
                              ) |>
                                layout(height = 500, width = 750)
                            })
                            
                            output$average_time_streetpotholes_title_output <- renderText({"Average processing time (days) for street pothole complaints. Q1 2024"})
                            
                            output$average_time_streetpotholes_caption_output <- renderText({"Source: City of Chicago Data Portal."})
                            
                            output$table_average_time_streetpotholes <- renderTable({
                              st_drop_geometry(average_time_streetpotholes_data()) |> select(ward, mean_processing_time_days)
                            })
                            
              # Rodent complaints tab:
              
              rodents_data <- import_csv(rodents_file_name)
              
                    # Number of cases tab:
              
                          observe({
                            # Calculate the rounded minimum and maximum values
                            min_val <- floor(min(rodents_data$number_cases))
                            max_val <- ceiling(max(rodents_data$number_cases))
                            
                            # Update the slider input with the rounded values
                            updateSliderInput(session, "number_rodents_range", 
                                              value = c(min_val, max_val),
                                              min = min_val, 
                                              max = max_val)
                          })            
                          
                          number_rodents_data <- reactive({
                            rodents_data |>
                              filter(number_cases >= as.numeric(input$number_rodents_range[1]), 
                                     number_cases <= as.numeric(input$number_rodents_range[2]))
                          })
                          
                          output$number_rodents_plot <- renderPlotly({
                            ggplotly(
                              ggplot() +
                                geom_sf(data = chicago_wards_data, fill = "transparent", color = "grey", size = 0.05) +
                                geom_sf(data = number_rodents_data(), aes(fill = number_cases, text = paste("Ward:", ward, "<br>")))  +
                                scale_fill_distiller(palette = "Blues", direction = 1) + 
                                theme_minimal() 
                            ) |>
                              layout(height = 500, width = 750)
                          })
                          
                          output$number_rodents_title_output <- renderText({"Number of Rodent complaints. Q1 2024"})
                          
                          output$number_rodents_caption_output <- renderText({"Source: City of Chicago Data Portal."})
                          
                          output$table_number_rodents <- renderTable({
                            st_drop_geometry(number_rodents_data()) |> select(ward, number_cases)
                          })
                          
                    # Average processing time:
                    
                        observe({
                          # Calculate the rounded minimum and maximum values
                          min_val <- floor(min(rodents_data$mean_processing_time_days))
                          max_val <- ceiling(max(rodents_data$mean_processing_time_days))
                          
                          # Update the slider input with the rounded values
                          updateSliderInput(session, "time_rodents_range", 
                                            value = c(min_val, max_val),
                                            min = min_val, 
                                            max = max_val)
                        })
                        
                        average_time_rodents_data <- reactive({
                          rodents_data |>
                            filter(mean_processing_time_days >= as.numeric(input$time_rodents_range[1]), 
                                   mean_processing_time_days <= as.numeric(input$time_rodents_range[2]))
                        })
                        
                        output$average_time_rodents_plot <- renderPlotly({
                          ggplotly(
                            ggplot() +
                              geom_sf(data = chicago_wards_data, fill = "transparent", color = "grey", size = 0.05) +
                              geom_sf(data = average_time_rodents_data(), aes(fill = mean_processing_time_days,
                                                                              text = paste("Ward:", ward, "<br>")))  +
                              scale_fill_distiller(palette = "Blues", direction = 1) + 
                              theme_minimal() 
                          ) |>
                            layout(height = 500, width = 750)
                        })
                        
                        output$average_time_rodents_title_output <- renderText({"Average processing time (days) for rodent complaints. Q1 2024"})
                        
                        output$average_time_rodents_caption_output <- renderText({"Source: City of Chicago Data Portal."})
                        
                        output$table_average_time_rodents <- renderTable({
                          st_drop_geometry(average_time_rodents_data()) |> select(ward, mean_processing_time_days)
                        })
                
    }
    
    
    
    shinyApp(ui = ui, server = server)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    