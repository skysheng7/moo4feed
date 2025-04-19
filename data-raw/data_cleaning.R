## code to prepare `data_cleaning` dataset goes here

#usethis::use_data(data_cleaning, overwrite = TRUE)

# load the libraries needed
library(moo4feed)

################################################################################
###################### data loading & processing ###############################
################################################################################
# load in example raw data
extdata_path <- system.file("extdata", package = "moo4feed")

fileNames.f <- list.files(path = extdata_path, full.names = TRUE,
                          recursive = TRUE,pattern ="^VR.*\\.DAT$")
fileNames.w <- list.files(path = extdata_path, full.names = TRUE,
                          recursive = TRUE,pattern ="^VW.*\\.DAT$")
fileNames.f <- sort(fileNames.f)
fileNames.w <- sort(fileNames.w)

################################################################################
################# customized processing for this study  ########################
######### Daylight Saving Change (this code is for in North America) ###########
################################################################################
# this part involves processing the data that is customized for this study only
# please change this based on your study

# Fetch daylight saving dates for year 2020 and 2021, in Pacific Time
dst_df <- dst_switch_day(years = c(2020,2021), tz = "America/Vancouver")
