
# Analysis of the City of Chicago 311 Service Requests 
# April 2024
# Violeta Lanza Robles

# 1.Load packages

    library(ggplot2)
    library(sf)
    library(RSocrata)
    library(dplyr)
    library(tidyverse)


# 2.Paths and dates to be changed to reproduce the results

    # Path to project folder:
    general_path <- "/Users/cesarlanzasuarez/Library/Mobile Documents/com~apple~CloudDocs/Todo lo demás/Coding/GitHub/City of Chicago 311 Service Requests Dashboard/311-service-requests-dashboard/"
    
        # Data downloaded from the internet:
        downloaded_data_path <- paste0(general_path, "Data/Downloaded data/")
    
            # Shapefiles:
            
                # Shapefile folder:
                shapefiles_general_path <- paste0(downloaded_data_path,"Shapefiles/")
                
                # Shapefiles paths, relative to the Shapefile folder path:
                wards_shapefile_relative_path <- "Boundaries - Wards (2023-)_20240328/geo_export_60924955-2a68-41a6-86c1-7dbcdded901b.shp"
                zipcodes_shapefile_relative_path <- "Boundaries - ZIP Codes/geo_export_9cc5cb5a-fc1b-48b3-b630-72ece45f1549.shp"
                education_shapefile_relative_path <- "acs2022_5yr_B23006_86000US60610/acs2022_5yr_B23006_86000US60610.shp"
                race_shapefile_relative_path <- "acs2022_5yr_B02001_86000US60610/acs2022_5yr_B02001_86000US60610.shp"
                poverty_shapefile_relative_path <- "acs2022_5yr_B17001_86000US60610/acs2022_5yr_B17001_86000US60610.shp"
        
    # Processed data (processed data not used for the Shiny app):
    processed_data_path <- "/Users/cesarlanzasuarez/Library/Mobile Documents/com~apple~CloudDocs/Todo lo demás/Coding/GitHub/City of Chicago - Final Project/Data/Processed data (not for the Shiny app)/"
    
    # Datasets used by the Shiny app:
    shiny_data_path <- "/Users/cesarlanzasuarez/Library/Mobile Documents/com~apple~CloudDocs/Todo lo demás/Coding/GitHub/City of Chicago - Final Project/Shiny folder/"

    # Set the initial and end dates to filter for the date of creation in the 311 service requests:
    start_date <- "2024-01-01"
    today_date <- Sys.Date()
    
# 3.Import the Chicago wards shapefiles
    
    # Ward level:
    
        # Import shapefile:
        chicago_wards <- st_read(paste0(shapefiles_general_path, wards_shapefile_relative_path))
        
        # Keep only the columns with the ward ID and the geometry:
        chicago_wards <- chicago_wards[, c("ward", "geometry")]
    
        # Preliminary ward visualization:
        ggplot() +
          geom_sf(data = chicago_wards)
        
        # Save the wards csv into the Shiny folder:
        if (!file.exists(paste0(shiny_data_path, "chicago_wards_data.csv"))) {
          # Write the sf object to CSV if the file doesn't exist
          st_write(chicago_wards, paste0(shiny_data_path, "chicago_wards_data.csv"), layer_options = "geometry=AS_WKT")
        } else {
          print(paste0("File already exists: ", paste0(shiny_data_path, "chicago_wards_data.csv")))
        }  

# 4.Process the census data
        
      # Function to process each census shapefile individually:
          
          process_census_data <- function(census_topic){
            
            # Import the Chicago ZIP codes shapefile from the Open Data Portal:
                  # Obtain the list of zipcodes that are in Chicago:
                  # Import shapefile for the Chicago ZIP codes in the Chicago Data Portal:
                  chicago_zipcodes <- st_read(paste0(shapefiles_general_path, zipcodes_shapefile_relative_path))

                  # According to American Community Survey, ZIP code 60666 is not in Chicago. On the contrary, 60018 is in Chicago, it contains O'Hare
                  # Manually add 60018 Ordered list of the unique values of the zipcodes in the chicago_zipcodes shapefile:
                  relevant_zipcodes <- c(unique(chicago_zipcodes$zip), "60018")
                  relevant_zipcodes <- sort(unique(relevant_zipcodes))
            
              # Create dictionary to link every census topic to the relative path to its corresponding shapefile:
              shapefile_paths_dictionary <- list(
              'education' = education_shapefile_relative_path,
              'race' = race_shapefile_relative_path,
              'poverty' = poverty_shapefile_relative_path)  
              
              # Correspondence between the column names in the ACS data and names that are easier to interpret:
              column_names_dictionary <- list(
                'B23006001' = 'total_population',
                'B23006002' = 'less_than_high_school_graduate',
                'B23006009' = 'high_school_graduate',
                'B23006016' = 'some_college_or_associates_degree',
                'B23006023' = 'bachelors_degree_or_higher',
                'B02001001' = 'total_population',
                'B02001002' = 'white',
                'B02001003' = 'black',
                'B02001004' = 'american_indian',
                'B02001005' = 'asian',
                'B02001006' = 'pacific_islander',
                'B02001007' = 'other',
                'B02001008' = 'two_or_more',
                'B17001001' = 'total_population',
                'B17001002' = 'income_below_poverty_level',
                'B17001031' = 'income_above_poverty_level',
                'name' = 'zip_codes')
            
              # Import the census shapefiles by zipcode:
              zipcodes_shapefile <- st_read(paste0(shapefiles_general_path, shapefile_paths_dictionary[[census_topic]]))
              
              # Rename columns to names that are easier to interpret:
              for (old_name in names(zipcodes_shapefile)) {
                if (old_name %in% names(column_names_dictionary)) {
                  new_name <- column_names_dictionary[[old_name]]
                  names(zipcodes_shapefile)[names(zipcodes_shapefile) == old_name] <- new_name
                }
              }
              new_column_names <- unlist(column_names_dictionary)
              columns_to_keep <- c(new_column_names, "zip_codes")
              zipcodes_shapefile <- zipcodes_shapefile[, intersect(names(zipcodes_shapefile), columns_to_keep)]
              
              # Calculate the percentage of the total population, per category:
              
                  # Remove the geometry column from the sf dataframe:
                  zipcodes_shapefile_without_geometry <- st_drop_geometry(zipcodes_shapefile)
                  
                  # Calculate the percentages per category:
                  zipcodes_shapefile_without_geometry <- zipcodes_shapefile_without_geometry[, -which(names(zipcodes_shapefile_without_geometry) %in% c("total_population", "zip_codes"))] / zipcodes_shapefile_without_geometry$total_population * 100
                  
                  # Create a new sf dataframe with the geometry column from the original sf dataframe:
                  zipcodes_shapefile_with_geometry <- st_sf(zipcodes_shapefile_without_geometry, geometry = zipcodes_shapefile$geometry)
                  
                  # Add the zip_codes and total population columns:
                  zipcodes_shapefile_with_geometry$zip_codes <- zipcodes_shapefile$zip_codes
                  zipcodes_shapefile_with_geometry$total_population <- zipcodes_shapefile$total_population
              
                  # Reorder the column names in order to see the zip_code column first:
                  zipcodes_shapefile_with_geometry <- zipcodes_shapefile_with_geometry |>
                    select(zip_codes, everything())
                  
                  # Filter to keep only the zipcodes that are considered to be within the City of Chicago boundaries:
                  zipcodes_shapefile_with_geometry <- zipcodes_shapefile_with_geometry |> filter(zip_codes %in% relevant_zipcodes)
                  
                  # Rename the resulting dataframe, and drop the total_population column:
                  dataframe <- zipcodes_shapefile_with_geometry |> 
                    select(zip_codes, everything())
                  
                  # Set the correct CRS:
                  crs_correct <- st_crs(chicago_zipcodes)
                  dataframe <- st_set_crs(dataframe, crs_correct)
                  
              # Save the file into the Shiny app folder:
              if (!file.exists(paste0(shiny_data_path, paste0(census_topic, "_data.csv")))) {
                # Write the sf object to CSV if the file doesn't exist
                st_write(dataframe, paste0(shiny_data_path, paste0(census_topic, "_data.csv")), layer_options = "geometry=AS_WKT")
              } else {
                print(paste0("File already exists: ", paste0(shiny_data_path, paste0(census_topic, "_data.csv"))))
              }  
              
              return(dataframe)
              
          }
          
          # Use the process_census_data function to create the three census dataframes:
          education_data <- process_census_data('education')
          race_data <- process_census_data('race')
          poverty_data <- process_census_data('poverty')
          
          
      # Combine the three census dataframes to create a dataframe with all census data:
          all_census_data <- cbind(education_data, race_data, poverty_data)|> 
            select(-c(zip_codes.1, zip_codes.2, geometry.1, geometry.2)) |> 
            select(zip_codes, everything())
          
          # Save the file:
          if (!file.exists(paste0(shiny_data_path, "all_census_data.csv"))) {
            # Write the sf object to CSV if the file doesn't exist
            st_write(all_census_data, paste0(shiny_data_path, "all_census_data.csv"), layer_options = "geometry=AS_WKT")
          } else {
            print(paste0("File already exists: ", paste0(shiny_data_path, "all_census_data.csv")))
          }

# 5.Process data on 311 Service Requests from the City of Chicago Open Data Portal

    # Download all 311 data (not filtering for any specific request type) with the Chicago Data Portal API:
    
        # If the  file is already saved on my directory, don't download it. If it isn't saved on my directory, download it:
        if (!file.exists(paste0(downloaded_data_path, "service_data.csv"))) {
          
          # Download data with API: all 311 requests data between January 1sr 2024 and March 31st 2024
          service_data <- read.socrata("https://data.cityofchicago.org/resource/v6vf-nfxy.json?$where=created_date >= '2024-01-01T00:00:00' AND created_date < '2024-03-31T00:00:00'",
                                       app_token = "Iy7muIkJJriErnnu432st5UYT",
                                       email     = "vlanzarobles@uchicago.edu",
                                       password  = "HiDataPortal!2")
          
          # Process the downloaded data:
          service_data <- service_data |>
            # Remove all the duplicated requests:
            filter(duplicate == "FALSE") |>
            # Remove all the rows originated either through "Mass Entry" or "Generated In House": 
            filter(origin != "Generated In House" & origin != "Mass Entry") |>
            # Add a column with the processing time taken to close each request:
            mutate(processing_time_days = as.numeric(difftime(closed_date, created_date, units = "days"))) |>
            # Convert the dates columns into date format:
            mutate(created_date = as.Date(created_date),
                   last_modified_date = as.Date(last_modified_date),
                   closed_date = as.Date(closed_date)) |>
            # Remove all canceled requests:
            filter(status != "Canceled") |>
            # Remove all requests without a ward:
            filter(!is.na(ward)) |>
            # Remove columns from the 311 requests data that are irrelevant:
            select(sr_number, sr_type, status, created_date, closed_date, owner_department, processing_time_days, ward)
          
          # Save the service data:
          write.csv(service_data, paste0(downloaded_data_path, "service_data.csv"), row.names = FALSE)
          
        } else {
          
          # State that the file already exists:
          cat("File already exists:", paste0(downloaded_data_path, "service_data.csv"))
          
          # Import the overall service data:
          service_data <- read.csv(paste0(downloaded_data_path, "service_data.csv"))
        }
          
    # Summarize the data:
              
          # List of all request types in the 311 dataset:
          request_types <- sort(unique(service_data$sr_type))
          request_types

          # Create a summary table: number of requests, % completed requests, mean processing time, by request type and department responsible:
              
              summary_table <- service_data |> 
                group_by(sr_type, owner_department) |> 
                summarise(n_requests = n(),
                          percentage_n_requests = (n()/nrow(service_data)*100),
                          percentage_completed = mean(status == "Completed", na.rm = TRUE) * 100,
                          mean_processing_time_days = mean(processing_time_days, na.rm = TRUE)) |>
                rename(request_type = sr_type,
                       number_of_requests = n_requests,
                       percentage_of_total_requests = percentage_n_requests)
              
              # Save the summary table to the Shiny folder:
              if (!file.exists(paste0(shiny_data_path,"summary_table.csv"))){
                write.csv(summary_table, paste0(shiny_data_path, "summary_table.csv"), row.names = FALSE)
              } else {
                cat("File already exists:", paste0(shiny_data_path,"summary_table.csv"))
              }
              
          
          # Function to obtain the total number of requests, percentage of open
          # cases, and average processing time (for closed cases),
          # filtering by request type and by date:
          
          process_requests_data <- function(request_type, initial_date, end_date){
            # Warning message: if the end date is earlier than the initial date, warn the user:
            if (end_date < initial_date) {
              warning("The end date should be later than or equal to the start date")
            } else{
              
              # Convert date inputs into date formats:
              initial_date <- as.Date(initial_date)
              end_date <- as.Date(end_date)
              
              # Filter the service data for the required service type and dates:
              dataframe <- service_data |>
                filter(sr_type == request_type & 
                         created_date >= initial_date & 
                         (closed_date <= end_date | is.na(closed_date))) 
              
              # Percentage of completed cases by ward:
              requests_by_ward <- dataframe |>
                group_by(ward) |>
                summarise(number_cases = n(),
                          percentage_completed = mean(status == "Completed", na.rm = TRUE) * 100, 
                          mean_processing_time_days = mean(processing_time_days, na.rm = TRUE)) 
              
              # Merge the dataframe with requests by ward with the wards geodataframe:
              requests_by_ward$ward <- as.numeric(requests_by_ward$ward)
              requests_by_ward <- left_join(chicago_wards, requests_by_ward, by = "ward")
              
              final_requests_path <- paste0(shiny_data_path,paste0(gsub("/", "", request_type), ".csv"))
              if (!file.exists(final_requests_path)) {
                # Write the sf object to CSV if the file doesn't exist
                st_write(requests_by_ward, final_requests_path, layer_options = "geometry=AS_WKT")
              } else {
                print(paste0("File already exists: ", final_requests_path))
              }
              
              return(requests_by_ward)
            }
          }
          
          # Use function process_requests_data to create dataframe with only 311 requests of type: "Pothole in Street Complaint"
          streetpotholes_data <- process_requests_data("Pothole in Street Complaint", start_date, today_date)
          
          # Use function process_requests_data to create dataframe with only 311 requests of type: "Rodent Baiting/Rat Complaint"
          rodents_data <- process_requests_data("Rodent Baiting/Rat Complaint", start_date, today_date)
          
      







