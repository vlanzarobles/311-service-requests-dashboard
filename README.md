
# Analysis of City of Chicago 311 Service Requests
## April 2024

Violeta Lanza Robles


**Date uploaded to GitHub**

24th April 2024


**Project summary**

This projects explores differences in the City of Chicago's average time to process 311 Service Requests across wards. Two specific request types are analyzed: street pothole complaints and rodent complaints. Requests between January 1st 2024 and March 31st 2024 are analyzed. The analysis is displayed in a Shiny dashboard, which contains a tab with the 311 Service Data analysis and another tab displaying three types of sociodemographic information (education, race, and poverty status) across ZIP codes. The source of the sociodemographic data is the 2018-2022 American Community Survey 5-year estimates.


**Notes on reproducibility**

The R scripts should be run in the following order:
1) data_processing.R - contains code for data download and processing
2) 311 Analysis - Shiny folder > app.R. In this script, all paths are relative to the 311 Analysis - Shiny folder path.

In all the scripts, section 2 of the code includes the paths to folders and files used in the analysis. Individuals seeking to reproduce the analysis should change these paths to their own. The rest of the script does not need to be edited.


**Links to the datasets**

City of Chicago 311 Service Requests dataset: https://data.cityofchicago.org/Service-Requests/311-Service-Requests/v6vf-nfxy/about_data

Ward boundaries: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Wards-2023-/p293-wvbd/about_data

Census data by ZIP code and its corresponding shapefiles:
- Education: https://censusreporter.org/data/table/?table=B23006&geo_ids=860%7C16000US1714000
- Race: https://censusreporter.org/data/table/?table=B02001&geo_ids=860%7C16000US1714000#valueType%7Cestimate
- Poverty: https://censusreporter.org/data/table/?table=B17001&geo_ids=860%7C16000US1714000#


**Notes on large files**

The shapefiles and the 311 Service Requests dataset are very large, I have committed them to Github and also to Google Drive: https://drive.google.com/drive/u/0/folders/1-ssAE_1ZNdEvqF_J8NiQ7CJmFHoK1URR


**Link to Shiny app**

https://vlanzarobles.shinyapps.io/shiny_folder/


**Version of RStudio used**

Version 2023.12.1+402 (2023.12.1+402)


**Packages used**

All in their latest version:

RSocrata
tidyverse
sf
ggplot2
dplyr
shiny
shinyFeedback
plotly










