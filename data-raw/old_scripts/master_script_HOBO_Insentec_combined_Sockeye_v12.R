###################################################################################################
###################################################################################################
###                                                                                             ###
### Title: HOBO_Insentec_combined_Sockeye                                                       ###
### Author: Kehan (Sky) Sheng, Borbala Foris                                                    ###
### Email: skysheng7@gmail.com                                                                  ###
### Association: UBC                                                                            ###
### Date: August 13 2020 - November 18, 2021                                                    ###
### Location: Vancouver, BC, Canada                                                             ###
###                                                                                             ###
### Description:  This code is written to be run on super computer clusters. This code uses HOBO###
###               data to analyze lying and standing information of cows                        ###
###               on trial. It will also analyze lying synchrony. This code also uses Insentec  ###
###               data to analyze feeding and drinking behaviours. And analyze the replacment   ###
###               happened at feed bins.                                                        ###
### Source: 		  (1) Sky Sheng: HOBO_Lying_Standing_Analysis_R_Code_SS_KR 200623 BLANK_Version2###
###                   .R                                                                        ###
###               (1) Borbala Foris: Insentec_code.R                                            ###
###                                                                                             ###
###################################################################################################
###################################################################################################

###################################################################################################
########################################## Load Packages ##########################################
###################################################################################################

# Load all packages from library
library(lubridate)
library(plyr)
library(ggplot2)
library(pdftools)
#library(tidyverse)

###################################################################################################
##################### Build a list to store all analysis in different months ######################
###################################################################################################
# build a master list that holds all the data collected, including both HOBO and Insentec
# Structure of the list:
#
#                                  Social_character_project master list ----------
#                                 /                             \       \         \
#                                /                               \       \         \
#                           [[1]]HOBO                   [[2]]Insentec [[3]]Milking [[4]]THI 
#                              |                                   |       Machine
#                              |                                   |
#               [1] HOBO warning (table)                          [1] Feeding and drinking analysis (list)
#               [2] cleaned_HOBO_raw_data_in_minutes              [2] 90 minutes and 3hour time interval after feed is added (list)
#                   (table)                                            
#               [3] lying_standing_summary_by_date                [3] Insentec warning (table)
#                   (table)                                       [4] Bins with number of visits daily (list)
#               [4] lying_data_for_feeding_conflicts              [5] number of visits for each bin for each cow (list)
#                   (table)                                       [6] number of bins visited by each cow (list)
#               [5] paired lying bout                             [7] long feed duration (list)
#                   (a list of matrixes by date)                  [8] long water duration (list)
#               [6] paired lying total time                       [9] double_detection_1cow_2bins (list)
#                   (a list of matrixes by date)                  [10] double_detection_1bin_2cows (list)
#               [7] paired lying avergae duration                 [11] eating and lying conflict (list)
#                   (a list of matrixes by date)                  [12] negative duration bin (list)
#               [8] lying_together_analysis_by_cow (table)        [13] negative intake bin (list)
#                                                                 [14] large feed intake in one bout (list)
#               [9] duration_for_each_bout (table)                [15] large water intake in one bout (list)
#                                                                 [16] large feed intake in short time (list)
#               [10] standing_time_with_milking_excluded (table)  [17] large water intake in short time (list)
#                                                                 [18] which cows are present each second 
#                                                                      for feed (table)
#                                                                 [19] which bins are occupied each second 
#                                                                      for feed  (table)
#                                                                 [20] how much feed left each bin (table)
#                                                                 [21] which cows are present each second 
#                                                                      for water (table)
#                                                                 [22] which bins are occupied each second 
#                                                                      for water  (table)
#                                                                 [23] how much water left each bin (table)
#                                                                 [24] Feeding/drinking at the same time_bout (list)
#                                                                 [25] Feeding/drinking at the same time_total time (list)
#                                                                 [26] Feeding/drinking at the same time_average duration (list)
#                                                                 [27] Feeding/drinking neighbour_bout (list)
#                                                                 [28] Feeding/drinking neighbour_total time (list)
#                                                                 [29] Feeding/drinking neighbor_average duration (list)
#                                                                 [30] Replacement behaviour by date (list)
#                                                                 [31] all feed water bins occupied (table)
#                                                                 [32] average number of feeding buddies (list)
#                                                                 [33] Cleaned_feeding_original_data (list)
#                                                                 [34] Cleaned_drinking_original_data (list)
#                                                                 [35] Cleaned_combined_original_data (list)
#                                                                 [36] non_nutritive_visits (list)
#                                                                 [37] visited_but_no_feed_record (list)
#                                                                 [38] visited_but_no_feed_freq (list)
#                                                                 [39] culled_cow_table (table)
#                                                                 [40] warning_days (table)
#                                                                 [41] Drinking replacement behaviour by date (list)
#                                                                 [42] empty_bin_time_detail (list)
#                                                                 [43] bin_empty_total_time_summary

# list on the root
Social_character_project <- list()
# create HOBO and Insentec list
HOBO <- list()
Insentec <- list()
bins_visit_num <- list()
visit_per_bin_per_cow <- list()
bin_num_visit_per_cow <- list()
long_feed_dur_list <- list()
long_wat_dur_list <- list()
double_bin_detection_list <- list()
double_cow_detection_list <- list()
eating_lying_conflict_list <- list()
negative_dur_list <- list()
negative_intake_list <- list()
large_feed_intake_in_one_bout <- list()
large_water_intake_in_one_bout <- list()
large_feed_intake_in_short_time <- list()
large_water_intake_in_short_time <- list()


###################################################################################################
#################################### Super Computer Set Directory #################################
###################################################################################################
# HOBO: set input and output directory
#HOBO_input_dir <- "../data/round2"
#output_dir <- "../results/round2"
HOBO_input_dir <- "C:/Users/skysheng/OneDrive - The University Of British Columbia/University of British Columbia/Research/PhD Project/Lameness one year trial/Data cleaning/step2_variable_generation"
output_dir <- "C:/Users/skysheng/OneDrive - The University Of British Columbia/University of British Columbia/Research/PhD Project/Lameness one year trial/Analysis/Dominance_competition_density/total_feeding_visits/results"

# Because compiling 1 year data at the same time sequentically takes too long, 
# I broke data into smaller chunks and submit an array job on super computer.
# Array jobs are created based on index, the code below grabs the index and put into this R script
pbs_job_index <- commandArgs(trailingOnly=TRUE)
pbs_job_index2 <- as.integer(pbs_job_index)

# Insentec: set input directory
#fileNames.f <- list.files(path = "../data/round2/feed",full.names = TRUE,recursive = TRUE,pattern =".DAT")
#fileNames.w <- list.files(path = "../data/round2/water",full.names = TRUE,recursive = TRUE,pattern =".DAT")
# fileNames.f <- list.files(path = "C:/Users/skysheng/OneDrive - UBC/University of British Columbia/Research/Master's Project/Lameness one year trial/Super Computer Analysis/Previous data cleaning and running/insentec_round2/data/feed",full.names = TRUE,recursive = TRUE,pattern =".DAT")
# fileNames.w <- list.files(path = "C:/Users/skysheng/OneDrive - UBC/University of British Columbia/Research/Master's Project/Lameness one year trial/Super Computer Analysis/Previous data cleaning and running/insentec_round2/data/water",full.names = TRUE,recursive = TRUE,pattern =".DAT")

fileNames.f <- sort(fileNames.f) # sort the order of the files 
fileNames.w <- sort(fileNames.w) # Get the file names

# break down the full 1 year data list into smaller chunks (16 days each), and process in an array
# get the current small chunk based on array index
if (pbs_job_index2 < 20) {
  fileNames.f <- fileNames.f[((pbs_job_index2 * 15)+1):((pbs_job_index2+ 1)*15)]
  fileNames.w <- fileNames.w[((pbs_job_index2 * 15)+1):((pbs_job_index2+ 1)*15)]
} else {
  fileNames.f <- fileNames.f[((pbs_job_index2 * 15)+1):length(fileNames.f)]
  fileNames.w <- fileNames.w[((pbs_job_index2 * 15)+1):length(fileNames.w)]
}


# milk data: read in
load("../data/9_month/HOBO/milk/clean_milking_data_full_colName.Rda")



###################################################################################################
################################ local computer test use only #####################################
###################################################################################################

HOBO_input_dir <- "C:/Users/skysheng/OneDrive - The University Of British Columbia/University of British Columbia/Research/Master's Project/Lameness one year trial/test/Test Result/2_day_test"
output_dir <- "C:/Users/skysheng/OneDrive - The University Of British Columbia/University of British Columbia/Research/Master's Project/Lameness one year trial/test/Test Result/2_day_test"
fileNames.f = list.files(path = "C:/Users/skysheng/OneDrive - The University Of British Columbia/University of British Columbia/Research/Master's Project/Lameness one year trial/test/Test data/2_day_test_Insentec/Feed",full.names = TRUE,recursive = TRUE,pattern =".DAT")
fileNames.w = list.files(path = "C:/Users/skysheng/OneDrive - The University Of British Columbia/University of British Columbia/Research/Master's Project/Lameness one year trial/test/Test data/2_day_test_Insentec/Water",full.names = TRUE,recursive = TRUE,pattern =".DAT")
fileNames.f <- sort(fileNames.f) # sort the order of the files 
fileNames.w <- sort(fileNames.w) # Get the file names
# load milking data
load("C:/Users/skysheng/OneDrive - The University Of British Columbia/University of British Columbia/Research/Master's Project/Lameness one year trial/Parlor Data/clean data/clean_milking_data_full_colName.Rda")
setwd("C:/Users/skysheng/OneDrive - The University Of British Columbia/University of British Columbia/Research/Master's Project/Lameness one year trial/Code/Combined")


###################################################################################################
##################################### Daylight Saving Change ######################################
###################################################################################################

#Import daylight saving time change csv
daylight_saving_table <- read.csv("../data/daylight_saving_csv.csv")
#daylight_saving_table <- read.csv("daylight_saving_csv.csv")
colnames(daylight_saving_table) <- c("Year", "Spring", "Fall")
daylight_saving_table$Year <- as.character(daylight_saving_table$Year)
daylight_saving_table$Spring <- ymd(paste(daylight_saving_table$Year, daylight_saving_table$Spring, sep = "-"), tz="America/Los_Angeles")
daylight_saving_table$Fall <- ymd(paste(daylight_saving_table$Year, daylight_saving_table$Fall, sep = "-"), tz="America/Los_Angeles")
daylight_saving_table$Year <- as.integer(daylight_saving_table$Year)
daylight_saving_table <- daylight_saving_table[order(daylight_saving_table$Year),]
daylight_saving_table$Spring_nextDay <- daylight_saving_table$Spring + days(1)
daylight_saving_table$Fall_nextDay <- daylight_saving_table$Fall + days(1)



###################################################################################################
################################### Days to Be Discarded ##########################################
###################################################################################################
date <- c("2021-02-03", "2021-02-04", "2021-02-05", "2021-02-06", "2021-02-07", "2021-02-08", "2021-02-09", "2021-02-10", "2021-03-06", "2021-03-11", "2021-03-13", "2021-03-17", "2021-03-22",  "2021-04-27", "2021-04-28", "2021-05-02", "2021-05-03")
Red_warning <- c("Human present disturbance", "Human present disturbance", "Insentec break down", "Insentec break down", "Insentec break down", "bin 5 & 6 down", "bin 5 & 6 down", "bin 3, 4, 5 & 6 down", "Insentec compressor was not working, and Cow 6062 lost transponder before 8AM", "Insentec disturbed in the morning", "Insentec aren't opening for cows, manually turned to all open in the morning", "Water bin broken down", "Insentec compressor down", "Feed composition change, no feed access during night", "Feed composition change, no feed access during night", "Missing data for half a day", "Missing data for half a day")
days_to_be_discarded <- data.frame(date, Red_warning)
days_to_be_discarded$date <- ymd(days_to_be_discarded$date, tz="America/Los_Angeles")

orange_date <- c("2021-02-11", "2021-02-12", "2021-02-16", "2021-02-17", "2021-02-18", "2021-03-16", "2021-03-23", "2021-04-08", "2021-04-09", "2021-04-10", "2021-04-11", "2021-04-15", "2021-05-06", "2021-05-07", "2021-05-08", "2021-05-09", "2021-05-10", "2021-05-11", "2021-05-12", "2021-05-13", "2021-05-17", "2021-06-27", "2021-06-28", "2021-06-29", "2021-06-30")
orange_warning <- c("Power Outage from 17:30 - 18:20; extreme cold weather", "extreme cold weather, bins not closing properly", "Cow 5120 lost both tages, registered as 1111", "Cow 5120 lost both tages, registered as 1111", "Morning only:Cow 5120 lost both tages, registered as 1111", "No access to feed for several hours in the afternoon due to hoof trimming", "Compressor down again", "cow 7064 was switched to 0", "cow 7064 was switched to 0", "cow 5096 was removed for half a day due to difficulty turning around in the parlor", "cow 5096 was removed for half a day due to difficulty turning around in the parlor", "Brush crew people were in the pen for significnat amount of time", "water bin 104 was temperorily closed", "water bin 104 was temperorily closed", "water bin 104 was temperorily closed", "water bin 104 was temperorily closed", "water bin 104 was temperorily closed", "water bin 104 was temperorily closed", "water bin 104 was temperorily closed", "water bin 104 was temperorily closed", "Cows escaped to the pasture from around 9pm to 10:30 pm; bin 9 left closed for some part of the day", "Water bins kept all open starting 5:00PM due to heat wave", "Water bins kept all open due to heat wave", "Water bins kept all open due to heat wave", "Water bins kept all open due to heat wave")
orange_date_sheet <- data.frame(orange_date, orange_warning)
colnames(orange_date_sheet) <- c("date", "orange_warning")
orange_date_sheet$date <- ymd(orange_date_sheet$date, tz="America/Los_Angeles")

warning_days <- merge(days_to_be_discarded, orange_date_sheet, all = TRUE)
warning_days[is.na(warning_days)] <- ""

###################################################################################################
###                                                                                             ###
### Chapter 1: Lying & Standing analysis with HOBO                                              ###
### Description: This first part of the code interpret HOBO data and summarized the bout,       ###
###              duration, total time for lying and standing behavior of each cow on each day.  ###
###              On all logger take on day, any data points collected before 5 PM are deleted.  ###
###              on all logger take off day, any data points collected after 3 PM are deleted.  ###
###              This code also analyzes lying synchrony. The total bouts, total time and       ###
###              average duration of two cows lying together.                                   ###
###                                                                                             ###
###################################################################################################



###################################################################################################
##################################### Import All CSV Data Files ###################################
###################################################################################################

#list all files
#cur_folder <- paste(HOBO_input_dir, "/July 13- July 27_csv", sep = "")
# Create a list of all files' name from the current folder
#list_file = list.files(path=cur_folder, pattern="*.csv", full.names=TRUE)
list_file = list.files(path=HOBO_input_dir, pattern="*.csv", full.names=TRUE)


# build a master sheet for warning massages
HOBO_warning <- data.frame(list_file)
colnames(HOBO_warning) <- c("File names")
HOBO_warning$Shifted_data_cloud <- ""
HOBO_warning$Wrong_y_cut_off <- ""
HOBO_warning$Logger_lying_on_floor <- ""
HOBO_warning$Wrong_file_name_format <- ""
HOBO_warning$Wrong_data_start_date <- ""
HOBO_warning$Wrong_data_end_date <- ""
HOBO_warning$Empty_dataSheet <- ""


# Create an empty data frame as the master data frame
# read in the first csv file, and then clear all rows to create an empty master data frame
temp <- read.csv(list_file[1], header = TRUE, skip = 1)
temp <- temp[-c(1, 5:ncol(temp))]  # delete columns that are not useful
temp <- setNames(temp, c("dateTime", "y", "z"))  # change column names
#Below is creating a column in the data sheet for each section of the data file name ("Cohort#_Pen#_Week#_Cow.csv")
temp$Cow = "1"  # add a new column stating the ID number, 1 is just a random place holder
temp$dateTime <- mdy_hms(temp$dateTime,tz="America/Los_Angeles")  # standardize the date and time
temp <- temp[, c(4, 1, 2, 3)]  # reorder the column names to in this order "Cow, dateTime, y, z"
master <- temp[-c(1:nrow(temp)), ]

straight_line_validation <- master
deleted_straight_line_record <- master


# Create a function called modFile that will take a list of files' names as varaible, make modifications
# to the csv files, and then merge it into the master data sheet which have all information of
# all cows
modFile <- function(fileList) {  # take a list variable as input argument, example input: list_file
  # Iterate through the list of files, make modification on each file and merge into a big master sheet
  
  # create a folder called "Results" to store all the output pdf and csv files
  # create a pdf file to store the histogram that will be generated for current cohort and pen
  pdfPath = paste(output_dir, "/HOBO_histogram.pdf", sep = "")
  pdf(file=pdfPath)
  
  
  # read in all datasheets in the folder
  for (i in 1:length(fileList)){
    #print(i)
    
    # extract and trim whitespace from the file name
    filename = trimws(fileList[i], which = "both")
    # filename = trimws(list_file[i], which = "both")
    filename = gsub(" ", "", filename)
    filename = chartr(" ", "_", filename)  # replace " " from filename with "_"
    filename = chartr(".", "_", filename)  # replace "." from filename with "_"
    filename = chartr("/", "_", filename)  # replace "/" from filename with "_"
    filename = chartr("-", "_", filename)  # replace "-" from filename with "_"
    filename_list = strsplit(filename, "_")  # split the filename string by "_"
    
    
    # extract animal ID
    id <- filename_list[[1]][length(filename_list[[1]])-3]
    start_d <- filename_list[[1]][length(filename_list[[1]])-2]
    end_d <- filename_list[[1]][length(filename_list[[1]])-1]
    
    # print out the current file that it's being processed
    print(paste("Cow", id, start_d, end_d, sep =" "))
    
    
    ### Test 1 ### test if all cow ID get extracted correctly
    #The length of the Cows used in this example are between 3 and 4 numbers long (111 or 2222),
    #adjust as needed for length of animal ID
    if (nchar(id) >= 5 | nchar(id) <= 2) {
      HOBO_warning$Wrong_file_name_format[i] <- "YES"
      cat("Sorry, the ID for animal recorded in file: ", fileList[i], " is likely to be wrong. ",
          "Please check the format of csv file name and make appropriate corrections.\n")
    }
    
    
    # register start and end date
    start_dateTime <- filename_list[[1]][length(filename_list[[1]])-2]
    end_dateTime <- filename_list[[1]][length(filename_list[[1]])-1]
    # change abbreviation of month name to full month name. Otherwise, R could not parse to date format
    # change start dateTime's month format
    if ((tolower(substring(trimws(start_dateTime), 1, 3)) == "jan") & (tolower(substring(trimws(start_dateTime), 4, 4)) != "u")) {
      start_dateTime <- paste("January", substring(trimws(start_dateTime), 4, nchar(start_dateTime)), sep = "")
    } else if ((tolower(substring(trimws(start_dateTime), 1, 3)) == "feb") & (tolower(substring(trimws(start_dateTime), 4, 4)) != "r")) {
      start_dateTime <- paste("February", substring(trimws(start_dateTime), 4, nchar(start_dateTime)), sep = "")
    } else if ((tolower(substring(trimws(start_dateTime), 1, 3)) == "mar") & (tolower(substring(trimws(start_dateTime), 4, 4)) != "c")) {
      start_dateTime <- paste("March", substring(trimws(start_dateTime), 4, nchar(start_dateTime)), sep = "")
    } else if ((tolower(substring(trimws(start_dateTime), 1, 3)) == "apr") & (tolower(substring(trimws(start_dateTime), 4, 4)) != "i")) {
      start_dateTime <- paste("April", substring(trimws(start_dateTime), 4, nchar(start_dateTime)), sep = "")
    } else if ((tolower(substring(trimws(start_dateTime), 1, 4)) == "apri") & (tolower(substring(trimws(start_dateTime), 5, 5)) != "l")) {
      start_dateTime <- paste("April", substring(trimws(start_dateTime), 5, nchar(start_dateTime)), sep = "")
    } else if ((tolower(substring(trimws(start_dateTime), 1, 3)) == "jun") & (tolower(substring(trimws(start_dateTime), 4, 4)) != "e")) {
      start_dateTime <- paste("June", substring(trimws(start_dateTime), 4, nchar(start_dateTime)), sep = "")
    } else if ((tolower(substring(trimws(start_dateTime), 1, 3)) == "jul") & (tolower(substring(trimws(start_dateTime), 4, 4)) != "y")) {
      start_dateTime <- paste("July", substring(trimws(start_dateTime), 4, nchar(start_dateTime)), sep = "")
    } else if (tolower(substring(trimws(start_dateTime), 1, 5)) == "augst") {
      start_dateTime <- paste("August", substring(trimws(start_dateTime), 6, nchar(start_dateTime)), sep = "")
    } else if ((tolower(substring(trimws(start_dateTime), 1, 3)) == "aug") & (tolower(substring(trimws(start_dateTime), 4, 4)) != "u")) {
      start_dateTime <- paste("August", substring(trimws(start_dateTime), 4, nchar(start_dateTime)), sep = "")
    } else if (tolower(substring(trimws(start_dateTime), 1, 4)) == "sept" & (tolower(substring(trimws(start_dateTime), 5, 5)) != "e")){
      start_dateTime <- paste("September", substring(trimws(start_dateTime), 5, nchar(start_dateTime)), sep = "")
    } else if (tolower(substring(trimws(start_dateTime), 1, 3)) == "oct" & (tolower(substring(trimws(start_dateTime), 4, 4)) != "o")){
      start_dateTime <- paste("October", substring(trimws(start_dateTime), 4, nchar(start_dateTime)), sep = "")
    } else if (tolower(substring(trimws(start_dateTime), 1, 3)) == "nov" & (tolower(substring(trimws(start_dateTime), 4, 4)) != "e")){
      start_dateTime <- paste("November", substring(trimws(start_dateTime), 4, nchar(start_dateTime)), sep = "")
    } else if (tolower(substring(trimws(start_dateTime), 1, 3)) == "dec" & (tolower(substring(trimws(start_dateTime), 4, 4)) != "e")){
      start_dateTime <- paste("December", substring(trimws(start_dateTime), 4, nchar(start_dateTime)), sep = "")
    }
    # change end dataTime's month format
    if ((tolower(substring(trimws(end_dateTime), 1, 3)) == "jan") & (tolower(substring(trimws(end_dateTime), 4, 4)) != "u")) {
      end_dateTime <- paste("January", substring(trimws(end_dateTime), 4, nchar(end_dateTime)), sep = "")
    } else if ((tolower(substring(trimws(end_dateTime), 1, 3)) == "feb") & (tolower(substring(trimws(end_dateTime), 4, 4)) != "r")) {
      end_dateTime <- paste("February", substring(trimws(end_dateTime), 4, nchar(end_dateTime)), sep = "")
    } else if ((tolower(substring(trimws(end_dateTime), 1, 3)) == "mar") & (tolower(substring(trimws(end_dateTime), 4, 4)) != "c")) {
      end_dateTime <- paste("March", substring(trimws(end_dateTime), 4, nchar(end_dateTime)), sep = "")
    } else if ((tolower(substring(trimws(end_dateTime), 1, 3)) == "apr") & (tolower(substring(trimws(end_dateTime), 4, 4)) != "i")) {
      end_dateTime <- paste("April", substring(trimws(end_dateTime), 4, nchar(end_dateTime)), sep = "")
    } else if ((tolower(substring(trimws(end_dateTime), 1, 4)) == "apri") & (tolower(substring(trimws(end_dateTime), 5, 5)) != "l")) {
      end_dateTime <- paste("April", substring(trimws(end_dateTime), 5, nchar(end_dateTime)), sep = "")
    } else if ((tolower(substring(trimws(end_dateTime), 1, 3)) == "jun") & (tolower(substring(trimws(end_dateTime), 4, 4)) != "e")) {
      end_dateTime <- paste("June", substring(trimws(end_dateTime), 4, nchar(end_dateTime)), sep = "")
    } else if ((tolower(substring(trimws(end_dateTime), 1, 3)) == "jul") & (tolower(substring(trimws(end_dateTime), 4, 4)) != "y")) {
      end_dateTime <- paste("July", substring(trimws(end_dateTime), 4, nchar(end_dateTime)), sep = "")
    } else if (tolower(substring(trimws(end_dateTime), 1, 5)) == "augst") {
      end_dateTime <- paste("August", substring(trimws(end_dateTime), 6, nchar(end_dateTime)), sep = "")
    } else if (tolower(substring(trimws(end_dateTime), 1, 3)) == "aug" & (tolower(substring(trimws(end_dateTime), 4, 4)) != "u")) {
      end_dateTime <- paste("August", substring(trimws(end_dateTime), 4, nchar(end_dateTime)), sep = "")
    } else if (tolower(substring(trimws(end_dateTime), 1, 4)) == "sept" & (tolower(substring(trimws(end_dateTime), 5, 5)) != "e")){
      end_dateTime <- paste("September", substring(trimws(end_dateTime), 5, nchar(end_dateTime)), sep = "")
    } else if (tolower(substring(trimws(end_dateTime), 1, 3)) == "oct" & (tolower(substring(trimws(end_dateTime), 4, 4)) != "o")){
      end_dateTime <- paste("October", substring(trimws(end_dateTime), 4, nchar(end_dateTime)), sep = "")
    } else if (tolower(substring(trimws(end_dateTime), 1, 3)) == "nov" & (tolower(substring(trimws(end_dateTime), 4, 4)) != "e")){
      end_dateTime <- paste("November", substring(trimws(end_dateTime), 4, nchar(end_dateTime)), sep = "")
    } else if (tolower(substring(trimws(end_dateTime), 1, 3)) == "dec" & (tolower(substring(trimws(end_dateTime), 4, 4)) != "e")){
      end_dateTime <- paste("December", substring(trimws(end_dateTime), 4, nchar(end_dateTime)), sep = "")
    }
    
    
    # read in the csv file to R, skip the first row, and identify that there is a header
    temp <- read.csv(fileList[i], header = TRUE, skip = 1)
    # temp <- read.csv(list_file[i], header = TRUE, skip = 1)
    
    # test if it is an empty datasheet
    if (nrow(temp) < 50) {
      HOBO_warning$Empty_dataSheet[i] <- "YES"
    } else { # if it is not an empty datasheet
      temp <- temp[-c(1, 5:ncol(temp))]  # delete columns that are not useful
      temp <- setNames(temp, c("dateTime", "y", "z"))  # change column names
      temp$Cow = id  # add a new column stating the ID number of current cow
      temp <- na.omit(temp)
      temp$dateTime <- mdy_hms(temp$dateTime,tz="America/Los_Angeles")  # standarize the date and time
      temp <- temp[, c(4, 1, 2, 3)]  # reorder the column names to in this order "Cow, dateTime, y, z"
      
    
      
      ##################################### Daylight saving change ################################
      cur_year <- year(temp$dateTime[1])
      cur_year2 <- as.integer(cur_year)
      cur_year_line <- daylight_saving_table[which(daylight_saving_table$Year == cur_year2),]
      cur_month <- as.integer(month(temp$dateTime[1]))
      # determine if current period is in the spring or fall
      if (cur_month >= 8) {  # this is fall
        daylight_change_date <- cur_year_line$Fall[1]
        daylight_change_next_date <- cur_year_line$Fall_nextDay[1]
        daylight_change_time <- ymd_hms(paste(as.character(daylight_change_date), "02:00:00", sep = " "), tz = "America/Los_Angeles")
        daylight_change_time2 <- ymd_hms(paste(as.character(daylight_change_date), "03:00:00", sep = " "), tz = "America/Los_Angeles")
        
        # determine if current data sheet contains daylight saving dates
        contain_daylight_change <- temp[which(temp$dateTime == daylight_change_time), ]
        if (nrow(contain_daylight_change) >  0) {
          temp <- temp[which((temp$dateTime <= daylight_change_time) | (temp$dateTime > daylight_change_time2)), ]
          before_change <- temp[which(temp$dateTime <= daylight_change_time), ]
          after_change <- temp[which(temp$dateTime > daylight_change_time2), ]
          after_change$dateTime <- after_change$dateTime - hours(1)
          temp <- rbind(before_change, after_change)
        }
        
        
      } else { # this is spring
        daylight_change_date <- cur_year_line$Spring[1]
        daylight_change_next_date <- cur_year_line$Spring_nextDay[1]
        daylight_change_time <- ymd_hms(paste(as.character(daylight_change_date), "00:59:00", sep = " "), tz = "America/Los_Angeles")
        daylight_change_time2 <- ymd_hms(paste(as.character(daylight_change_date), "03:00:00", sep = " "), tz = "America/Los_Angeles")
        
        # determine if current data sheet contains daylight saving dates
        contain_daylight_change <- temp[which(temp$dateTime == daylight_change_time), ]
        if (nrow(contain_daylight_change) >  0) {
          before_change <- temp[which(temp$dateTime <= daylight_change_time), ]
          after_change <- temp[which(temp$dateTime >= daylight_change_time2), ]
          
          
          # In Linux system on super computer cluster, all time between 2am-3am on 2021-03-14 is converted to be 1am-2am automatically 
          # because 2am-3am does not exist in reality on that day. To handle this problem: 
          # we deleted all the time between 2020-03-14 1am-3am
          
          #middle_change <- temp[is.na(temp$dateTime), ]
          #middle_change$dateTime[1] <- daylight_change_time2
          #for (a in 2:nrow(middle_change)) {
          #  middle_change$dateTime[a] <- middle_change$dateTime[a-1] + minutes(1)
          #}
          after_change$dateTime <- after_change$dateTime + hours(1)
          #temp <- rbind(before_change, middle_change)
          #temp <- rbind(temp, after_change)
          
          temp <- rbind(before_change, after_change)
        }
      }
      
      
      
      # Cut time for logger change at start and end dates
      # on logger on day, any data > 5pm will be true reading
      # on logger off day, any data > 3pm will need be deleted
      date_range <- paste(substring(temp$dateTime[1], 1, 4), start_dateTime, "-", substring(temp$dateTime[nrow(temp)], 1, 4), end_dateTime, sep = "")
      start_dateTime <- paste(substring(temp$dateTime[1], 1, 4), start_dateTime, " 17:00:00", sep = "")
      start_dateTime <- ymd_hms(start_dateTime, tz="America/Los_Angeles")
      end_dateTime <- paste(substring(temp$dateTime[nrow(temp)], 1, 4), end_dateTime, " 15:00:00", sep = "")
      end_dateTime <- ymd_hms(end_dateTime, tz="America/Los_Angeles")
      # cut logger setup and takeoff time
      temp <- temp[which((temp$dateTime >= start_dateTime) & (temp$dateTime <= end_dateTime)), ]
      
      
      ##################################### Manual Data Cleaning ##################################
      # Some HOBO files have errors that can not be detected by code, here we manually clean those
      # HOBO files
      
      # File 1: 5133_july27_august10 & 5133_july27_july31
      if ((id == "5133") & (start_d == "july27") & (end_d == "august10")) {
        temp <- temp[which((temp$dateTime >= ymd_hms("2020-07-31 06:00:00", tz="America/Los_Angeles"))), ]
      } else if ((id == "5133") & (start_d == "july27") & (end_d == "july31")) {
        temp <- temp[which(temp$dateTime <= ymd_hms("2020-07-31 01:00:00", tz="America/Los_Angeles")), ]
        
        # File 2: 7018_Sept21_oct6 & 7018_October5_October20
      }else if ((id == "7018") & (start_d == "Sept21") & (end_d == "oct6")) {
        temp <- temp[which(temp$dateTime <= ymd_hms("2020-10-06 15:00:00", tz="America/Los_Angeles")), ]
      } else if ((id == "7018") & (start_d == "October5") & (end_d == "October20")) {
        temp <- temp[which(temp$dateTime >= ymd_hms("2020-10-06 16:00:00", tz="America/Los_Angeles")), ]
        
      # File 3: 7022_October5_October20 & 7022_Sept21_oct6
      } else if ((id == "7022") & (start_d == "October5") & (end_d == "October20")) {
        temp <- temp[which(temp$dateTime >= ymd_hms("2020-10-06 16:00:00", tz="America/Los_Angeles")), ]
      } else if ((id == "7022") & (start_d == "Sept21") & (end_d == "oct6")) {
        temp <- temp[which(temp$dateTime <= ymd_hms("2020-10-06 15:00:00", tz="America/Los_Angeles")), ]
      
      # File 4: 6005_October5_October20
      } else if ((id == "6005") & (start_d == "October5") & (end_d == "October20")) {
        temp <- temp[which(temp$dateTime <= ymd_hms("2020-10-08 13:00:00", tz="America/Los_Angeles")), ]
      } 
      
      
      ##################################### Automatic Data Cleaning ###############################
      # test again if it is an empty datasheet
      if (nrow(temp) < 50) {
        HOBO_warning$Empty_dataSheet[i] <- "YES"
      } else {
        ### Test 2 ### Test for straight line reading (y and z value not changing for an extended period of time), which
        # indicates the logger is probably not on a living cow. Instaed, it's more possible to be on the floor or a table
        # We will detect y and z value straight line reading at the same time.
        # Longer than 5 hours of value not changing will be visible to human eye when reading the HOBO plot,
        # and it's likely this is due to HOBO accidently fall off from the cow
        window_size <- 60 * 7 # if value not change much in about 5 hours
        step_size <- 1
        sd_cutoff <- 0.07 # Based on observation, the cutoff point for standard deviation is 0.07, anything below
        y_sd_cutoff <- 0.1
        # 0.07 is straight line reading
        # Question & problem with this curoff value:
        # (1) majority of stright line get correctly detected, but some fraction (~30 minutes) did not get detected
        #     If I increase this cutoff value, other non-straight reading get incorrectly deleted
        # (2) solution: <1> do nothing, just detect which file contain straight line reading, manually check later
        # <2> keep things in this way
        # <3> if straight line reading is detected, delete the whole day
        temp$y_z <- (temp$y) - (temp$z)
        temp$SD <- NA
        temp$y_SD <- NA
        temp$to_delete <- 0
        # iterate through the current datasheet to find straight line reading
        for (e in 1:(nrow(temp) - window_size + 1)) {
          cur_sd <- sd(temp$y_z[e:(e + window_size - 1)])
          temp$SD[e] <- cur_sd
          temp$y_SD[e] <- sd(temp$y[e:(e + window_size - 1)])
        }
        
        straight_temp <- temp
        straight_line_validation <- rbind(straight_line_validation, straight_temp)
        
        # iterate through the datasheet again and delete those straight line reading points
        if (nrow(temp[which(temp$SD <= sd_cutoff),]) > 0 ) { # if there is straight line reading
          
          u = 1
          while (u <= (nrow(temp) - window_size + 1)) {
            # if standard deviation is less than the cut off point
            if ((temp$SD[u] <= sd_cutoff) & (temp$y_SD[u] <= y_sd_cutoff)) {
              temp$to_delete[u] = 1 # delete this row
              
              # if this is the last row with standard deviation calculated
              if (u == (nrow(temp) - window_size + 1)) {
                temp$to_delete[(u+1):nrow(temp)] <- 1
              } else { # if this is not the last row
                # if the next row's SD is more than the cut off point
                if ((temp$SD[u+1] > sd_cutoff) | (temp$y_SD[u+1] > y_sd_cutoff)) {
                  temp$to_delete[(u + 1):(u + window_size-1)] <- 1 # delete the next 5 hours of data
                  u = u + window_size - 1 # move the curser to 5 hours later
                }
                # if the next row's SD is less than the cut off value still, just jump to the next row as normal
              }
            }
            # we will do nothing if current SD is more than the curoff value
            
            # jump to the next row
            u = u + 1
          }
        }
        
        ### extra processing 1:  for the last 7 hours in each file
        # if more than half of the time in the last 5 hours need to be deleted, delete the entire 5 hours in the end
        # Bacause based on observation, this usually is just some residual of straight line reading.
        if (sum(temp$to_delete[(nrow(temp)-window_size +1): nrow(temp)]) > (window_size/2)) {
          temp$to_delete[(nrow(temp)-window_size +1): nrow(temp)] <- 1
        }
        ### extra processing 2: for the first 7 hours in each file. 
        # if more than half of the time in the first 5 hours need to be deleted, delete the entire 5 hours in the end
        # Bacause based on observation, this usually is just some residual of straight line reading.
        if (sum(temp$to_delete[1: window_size]) > (window_size/2)) {
          temp$to_delete[1: window_size] <- 1
        }
        ### extra processing 3: delete short bouts, for example, 0 is toKeep, 1 is toDelete, if 11111100111111, correct the 00 in the middle to be 11
        # because this is likely due to cutoff point exception errors. Sometimes even it's straight line reading, 
        # y/z value still have a sudden huge jump causing the machine to miss deleting those several short bouts
        temp$bout <- 0
        temp$dur <- 0
        temp$bout_end <- -1 #this is used to mark down it's the last minute of current bout
        temp$bout_end[nrow(temp)]<- 1
        # mark duration of each bout
        for (r in 1:nrow(temp)) {
          if (r == 1) {
            temp$bout[r] <- 1
            temp$dur[r] <- 1
          } else {
            if (temp$to_delete[r] != temp$to_delete[r-1]){
              temp$bout[r] <- temp$bout[r-1] + 1
              temp$dur[r] <- 1
              temp$bout_end[r-1]<- 1
            } else {
              temp$bout[r] <- temp$bout[r-1]
              temp$dur[r] <-temp$dur[r-1] + 1
            }
          }
        }
        
        bout_end_table <- temp[which(temp$bout_end == 1), ]
        if (nrow(bout_end_table)>= 3) { # this special extra processing only could be applied when bout >= 3
          bout_end_table$new_to_delete <- NA
          for (t in 2:(nrow(bout_end_table) - 1)) {
            box_size <- 9 * 60 # 9 hour is the thereshold
            if ((bout_end_table$to_delete[t-1] == bout_end_table$to_delete[t+1]) & (bout_end_table$to_delete[t] != bout_end_table$to_delete[t-1])) {
              if (bout_end_table$dur[t] <= box_size) {
                if (bout_end_table$to_delete[t] == 1) {
                  bout_end_table$new_to_delete[t] <- 0
                } else {
                  bout_end_table$new_to_delete[t] <- 1
                }
              }
            }
          }
          
          # filter out and correct short bout
          bout_end_table2 <- na.omit(bout_end_table)
          bout_end_table2$dateTime <- NULL
          bout_end_table2$y <- NULL
          bout_end_table2$z <- NULL
          bout_end_table2$y_z <- NULL
          bout_end_table2$SD <- NULL
          bout_end_table2$y_SD <- NULL
          bout_end_table2$dur <- NULL
          bout_end_table2$bout_end <- NULL
          temp <- merge(temp, bout_end_table2, all = TRUE)
          for (p in 1:nrow(temp)) {
            if (!is.na(temp$new_to_delete[p])) {
              temp$to_delete[p] <- temp$new_to_delete[p]
            }
          }
          
        }
        # delete helping columns
        temp$bout <- NULL
        temp$dur <- NULL
        temp$bout_end <- NULL
        temp$new_to_delete <- NULL
        
        
        # record date that has straight line reading to the warning sheet
        to_delete_sheet <- temp[which(temp$to_delete == 1), ]
        if (nrow(to_delete_sheet) > 0) {
          to_delete_sheet$date <- date(to_delete_sheet$dateTime)
          straight_line_date <- sort(unique(to_delete_sheet$date))
          straight_line_date_str <- paste(unlist(straight_line_date), collapse="; ")
          HOBO_warning$Logger_lying_on_floor[i] <- paste("YES", straight_line_date_str, sep = ": ") # register on HOBO warning files
        }
        deleted_straight_line_record <- rbind(deleted_straight_line_record, to_delete_sheet)
        
        ##################################### Manual Data Cleaning ##################################
        # Some HOBO files were wrongly marked as need to be deleted as a whole file due to exception could not be
        # handled by this code, here we manually change it to not delete and be kept as a whole file. 
        
        # File 1: 6005_September7-September21
        if ((id == "6005") & (start_d == "September7") & (end_d == "September21")) {
          temp <- temp
          HOBO_warning$Logger_lying_on_floor[i] <- "YES, but this file contains rows that should not be deleted after manual checking, so this file was kept as a whole, no deletion."
        } else {
          
        ##################################### Automatic Data Cleaning ################################## 
          # for all the other files, conduct automatic cleaning and delete the straight line readings
          # delete straight line reading
          temp <- temp[which(temp$to_delete == 0), ]
        }
        
        # delete 3 columns added for testing
        temp$y_z <- NULL
        temp$SD <- NULL
        temp$y_SD <- NULL
        temp$to_delete <- NULL
        
        
        if (nrow(temp) < 50) {
          HOBO_warning$Empty_dataSheet[i] <- "YES"
        } else {
          # sort out the datasheet
          temp <- temp[order(temp$dateTime),]
          # mark down wrong HOBO data start dates
          if (temp$dateTime[1] != start_dateTime) {
            HOBO_warning$Wrong_data_start_date[i] <- paste("YES, ", as.character(temp$dateTime[1]), sep = "")
          }
          if (temp$dateTime[nrow(temp)] != end_dateTime) {
            HOBO_warning$Wrong_data_end_date[i] <- paste("YES, ", as.character(temp$dateTime[nrow(temp)]), sep = "")
          }
          
          
          ### Test 3 ### Test for shifted data clouds (if the logger is correctly orientated, or if it's set up upside down.)
          # usually we expect to see two data cloud around 0 and -1. However, if the logger is placed upside down,
          #  we would be expecting to see data cloud near 0 and 1. We should not have more than 35% of the data points above 0.5
          flip_check <- temp[which(temp$y > 0.5),]
          above_0.5 <- nrow(flip_check)/nrow(temp)
          above_0.5_cutoff <- 0.2
          if (above_0.5 >= above_0.5_cutoff) {
            HOBO_warning$Shifted_data_cloud[i] <- "YES"
            print(paste("File: ", substring(fileList[i], 3, nchar(fileList[i])), " has data clouds shifted upwards too far!", sep = ""))
            # if there is shifted data cloud for this file, it will be fixed by flipping it again
            
            temp$y = 3.2 - temp$y  # flip the data cloud if it's shifted data clouds
            temp$z = temp$z + 3.2
            
          } else { # only proceed if the data is orientated as expected
            
            # Add 3.2 to both y and z value. Animal that is standing will be assigned as 1, and lying as 0.
            # Cutting off point is 2.55 for y value (Ledgerwood et al., 2010)
            temp$y = temp$y + 3.2
            temp$z = temp$z + 3.2
          } 
          
          # generate a histogram to see the distribution of y, z.
          par(mfrow = c(1, 2))  # create a 1x2 graph to store at maximum 2 plots side by side
          hist(temp$y, xlim = range(1:5), breaks=seq(0, 8, by=0.1), main= paste("Cow", id, " ", date_range, sep = ""),
               xlab="y value")
          hist(temp$z, xlim = range(1:5), breaks=seq(0, 8, by=0.1), main= paste("Cow", id, " ", date_range, sep = ""),
               xlab="z value")
          
          
          
          ### Test 4 ### check if the cutoff point for y and z value is where it's supposed to be.
          # cutoff point for y is 2.55, for z is 3.025. If the distribution is way off, the cow might need to
          # be excluded from the trail as well as the file folder.
          # Since we only record standing and lying, not left/right side lying here, we only check y value cutoff point
          cutoff_test <- temp$y
          hist_list <- hist(cutoff_test, breaks = seq(-10, 10, by = 0.05), plot = FALSE)
          # get density distribution of y value at each bin on a historgam
          hist_table <- data.frame(hist_list[[1]][1:(length(hist_list[[1]])-1)], hist_list[[2]], hist_list[[3]])
          # (length(hist_list[[1]])-1)] is done to make sure each column have same number of rows
          colnames(hist_table) <- c("breaks", "counts", "density")
          hist_table$'percentage(%)' <- round((hist_table$counts/sum(hist_table$counts))*100, digits = 3)
          below_2.55 <- sum(hist_table[which(hist_table$breaks <= 2.55),]$`percentage(%)`)
          percentage_at_2.55 <- hist_table[which(round(hist_table$breaks, digits = 2) == 2.55),]$`percentage(%)`[1]
          #print(percentage_at_2.55)
          
          
          # print(paste("cutoff test: percentage below 2.55 is:", below_2.55, ". Percentage at 2.55 is:", percentage_at_2.55))
          # we say cutoff point for y value is off, if there is higher than 5.2% of data points at y = 2.55,
          # or the percentage of data below 2.55 is below 24% or above 76%
          ### there are 3 types of situations when it's likely to be a wrong y distribution 
          
          ### handle special occasion files first
          # File 1: 7146_Sept21-Sept25.csv
          if ((id == "7146") & (start_d == "Sept21") & (end_d == "Sept25")) {
            # do nothing
          } else if ((percentage_at_2.55 >= 5.2) | (below_2.55 >= 76) | (below_2.55 <= 24)) {
            HOBO_warning$Wrong_y_cut_off[i] <- "YES"
            print(paste("File: ", substring(fileList[i], 3, nchar(fileList[i])), " y cutoff point might be off!", sep = ""))
          } else if ((percentage_at_2.55 >= 1.1) & (below_2.55 >= 64)) {
            HOBO_warning$Wrong_y_cut_off[i] <- "YES"
            print(paste("File: ", substring(fileList[i], 3, nchar(fileList[i])), " y cutoff point might be off!", sep = ""))
          } else if ((percentage_at_2.55 >= 1.1) & (below_2.55 <= 46)) {
            HOBO_warning$Wrong_y_cut_off[i] <- "YES"
            print(paste("File: ", substring(fileList[i], 3, nchar(fileList[i])), " y cutoff point might be off!", sep = ""))
          } 
          
          
          if (HOBO_warning$Wrong_y_cut_off[i] != "YES") {
            # add new columns
            # get the time difference between current row and the row above
            temp_column <- temp$dateTime
            temp_column <- temp_column[1:(length(temp_column)-1)]
            temp_column2 <- c(temp_column[1], temp_column)
            temp$previous_dateTime <- temp_column2
            temp$interval_dur <- as.duration(temp$dateTime - temp$previous_dateTime)
            temp$time_duration_str <- as.character(temp$interval_dur)
            temp$time_interval_from_above <- trimws(tolower(substring(temp$time_duration_str, 1, 3)))
            temp$previous_dateTime <- NULL
            temp$interval_dur <- NULL
            temp$time_duration_str<- NULL
            
            
            # get the time difference between current row and 2 rows above
            temp_column <- temp$dateTime
            temp_column <- temp_column[1:(length(temp_column)-2)]
            temp_column2 <- c(temp_column[1:2], temp_column)
            temp$previous_dateTime <- temp_column2
            temp$interval_dur <- as.duration(temp$dateTime - temp$previous_dateTime)
            temp$time_duration_str <- as.character(temp$interval_dur)
            temp$time_interval_from_2_rows_above <- trimws(tolower(substring(temp$time_duration_str, 1, 4)))
            temp$previous_dateTime <- NULL
            temp$interval_dur <- NULL
            temp$time_duration_str<- NULL
            
            
            
            # only proceed to read the file if all tests are passed
            ### Handle special occasion
            ### File 1: 7039_July27_August10.csv : this HOBO was attached correctly in the first place, but gradually shifted upwards, so it's unfixable,
            ###         delete this file
            if ((id == "7039") & (start_d == "July27") & (end_d == "August10")) {
              # do nothing
              
            ### File 2: 6005_September7-September21.csv : this HOBO was set to record every 5 minutes instead of 1 minute, delete this file completely
            ###
            } else if ((id == "6005") & (start_d == "September7") & (end_d == "September21")) {
              # do nothing
              
            }else {
              master <- rbind(master, temp)
            }
            
          }
        }
      }
    }
  }
  
  # only export those with warning massages to the HOBO warning file
  HOBO_warning$total <- paste(HOBO_warning$Shifted_data_cloud, HOBO_warning$Wrong_y_cut_off, HOBO_warning$Logger_lying_on_floor, HOBO_warning$Wrong_file_name_format, HOBO_warning$Wrong_data_start_date, HOBO_warning$Wrong_data_end_date, HOBO_warning$Empty_dataSheet)
  HOBO_warning$total <- trimws(HOBO_warning$total)
  HOBO_warning <- HOBO_warning[which(nchar(HOBO_warning$total) > 1), ]
  HOBO_warning$total <- NULL
  #export the HOBO_warning file for now to save the changes. Will read in afterwards
  export_file <- paste(output_dir, "/HOBO_warning_messages_files.csv", sep = "")
  write.csv(HOBO_warning, export_file, row.names = FALSE)
  
  
  export_file <- paste(output_dir, "/straight_line_validation.csv", sep = "")
  write.csv(straight_line_validation,  export_file, row.names = FALSE)
  export_file <- paste(output_dir, "/deleted_straight_line.csv", sep = "")
  write.csv(deleted_straight_line_record,  export_file, row.names = FALSE)
  deleted_straight_line_record
  
  dev.off() # close the pdf file
  # make sure that the master sheet is the variable that get returned
  return(master)
}



# import csv files under test data set
master = modFile(list_file)

# update the HOBO_warning datasheet that were modified in the Function
export_file <- paste(output_dir, "/HOBO_warning_messages_files.csv", sep = "")
HOBO_warning <- read.csv(export_file, header = TRUE)
HOBO_warning[is.na(HOBO_warning)] <- ""


# Add a separate column stating date and hour
master$date = date(master$dateTime)
master$hour = hour(master$dateTime)
# Change the data type of Cow into numeric for easier comparison in the future
master$Cow = as.numeric(master$Cow)
# Delete rows with NA (missing value) in them
master <- na.omit(master)


###################################################################################################
############# Error Checking & Date Exclusion & Cow Exclusion Based on Histogram  #################
###################################################################################################

# Before conducting any analysis, check if the y & z data distribution is within the rational range,
# all data distribution should usually be binomial, with a clear cutoff point in between. The ideal
# cutoff point for y is 2.55, for z is 3.025. If the distribution is way off, the cow might need to
# be excluded from the trail as well as the file folder.
# During the "Import all CSV data files" step above, we have already created pdf files storing the
# histogram of y and z value for every cow


# test if there is overlapping for the same cow between different files imported
test_duplicate <- master[order(master$Cow, master$dateTime), ]
test_duplicate <- test_duplicate[, 1:2]
duplicate_table <- test_duplicate[duplicated(test_duplicate),]
# if there is overlap and duplication
if (nrow(duplicate_table) > 0) {
  me1 <- "Please check your files, there are overlapping period of logger reading between different files belonging to the same cow!"
  #winDialog(type = "ok", me1)
  print(me1)
} else {
  # order the sheet based on cow and date
  master<- master[order(master$Cow, master$dateTime), ]
  print("Congratulations! No duplicated data sheet!")
}


# Take a look at the logger book and disgard days when there are human disturbance or other recording errors
#master <- merge(master, days_to_be_discarded, all = TRUE)
#master <- master[is.na(master$Red_warning), ]
#master$Red_warning <- NULL
#master <- master[, c(2, 3, 4, 5, 6, 7, 1, 8)]

# record first day and last day of all files in current folder
master <- master[order(master$Cow, master$dateTime), ]
folder_start <- master$date[1]
folder_end <- master$date[nrow(master)]
HOBO_date_period <- paste(folder_start, "_", folder_end, sep = "")



###################################################################################################
########################################## Standing VS Lying ######################################
###################################################################################################

# Assign if the cow is standing or lying. If y value < 2.55 then it is standing (1), otherwise,
# the cow is lying.
master_stand <- master[which(master$y<2.55), ]
master_lyi <- master[which(master$y >= 2.55),]
master_stand$ST = 1
master_lyi$ST = 0
m5 <- merge(master_stand, master_lyi, all = TRUE)
master <- m5
master<- master[order(master$Cow, master$dateTime), ]


# This function is created to count the bout and duration of each behavior
bout_duration <- function(master) {
  # sort the datasheet based on Cow and dateTime
  master <- master[order(master$Cow, master$dateTime), ]
  # Clear any pre-existing columns' data
  master$bout = NULL
  master$duration = NULL
  master$isOne = NULL
  # Record the bout and duration of standing and lying
  w = 1  # clear w as 1
  master$bout = 0  # create a new column
  master$duration = 0  # create a new column
  totalRow = nrow(master)
  for (w in 1:totalRow) {
    if (w == 1) {  # if it is the first row
      master$bout[w] = 1  #  set bout as 1
      master$duration[w] = 1  # set duration as 1
    }
    
    else {  # if it is not the first row
      if (master$Cow[w] != master$Cow[w - 1]) {  # if current cow ID is different from the cow
        # ID above
        master$bout[w] = master$bout[w - 1] + 1  # whatever behavior it is, record as a new bout
        master$duration[w] = 1  # reset duration to 1
      }
      
      else {  # if this is the same cow to read
        if (master$ST[w] != master$ST[w - 1]) {  # if the cow changed behavior
          master$bout[w] = master$bout[w - 1] + 1  # record as a new bout
          master$duration[w] = 1 # reset duration to 1
        }
        else if (master$time_interval_from_above[w] != "60s") { # if this is the same cow, but there is a time gap between this row and row above
          master$bout[w] = master$bout[w - 1] + 1  # record as a new bout
          master$duration[w] = 1 # reset duration to 1
        }
        else {  # if this is the same cow did not change behavior, or date, and the time gap is 60s
          master$bout[w] = master$bout[w - 1]  # copy the bout number like the line above
          master$duration[w] = master$duration[w - 1] + 1 # duration + 1
        }
      }
    }
  }
  
  return(master)  # make sure the master sheet get returned
}

# This function "lessOne" find bouts of duration less than or equal to 1 minute and mark it down
lessOne <- function(master) {
  master$isOne = 0  # create a new column. If the total duration of the bout is <= 1, isOne = 1
  # if the duration of the bout is > 1, isOne = 0
  totalRow = nrow(master)
  for (w in 1:totalRow) {
    if (w != totalRow) {  # if it is not the last row in the data sheet, look forward
      
      if (master$bout[w] != master$bout[w + 1]) {  # if the behavior changed compared to the next minute
        if (master$duration[w] <= 1) {  # if this is the last minute of observation for current bout and
          # the duration is less than or equal to 1
          master$isOne[w] = 1  # mark down that the duration is 1 minute
        }
        else {  # if this is the last minute of observation for current bout but the durition is > 1
          # do not mark down
        }
      }
      
      else {  # if the behavior remain the same at next minute
        # do not mark down
      }
    }
    
    else {  # if it is the last row in the data sheet, look backward
      if (master$bout[w] != master$bout[w - 1]) {  # if the current behavior changed from 1 to 0 or 0
        # to 1 compared to previous minute
        master$isOne[w] = 1  # mark down that the duration is 1 minute
      }
      else {  # if the current behavior remain unchanged compared to previous minute
        # do not mark down
        
      }
    }
  }
  
  return(master)  # make sure that the master sheet get returned
}

#### LONG COMPUTATION TIME WARNING!! The following function takes a long time to run!
# Count the bout and duration of current master sheet
master <- bout_duration(master)


# find bouts of duration less than or equal to 1 minute and mark it down
master <- lessOne(master)



###################################################################################################
#################################### Handling Error Reading #######################################
###################################################################################################

# Error handling: Behaviors that has the duration <= 1 minute will be considered
# reading error (Ledgerwood et al., 2010. + Zobel et al., 2015). All bouts that is of the duration
# <= 1 minute will be corrected.


# This function "lookForward" is created to find the next consecutive behavior with duration > 1
# minute from current row of observation. Make current behavior (duration < = 1) the same as the
# next behavior with a duration > 1
lookForward <- function(master, cur) {
  # sort the datasheet based on Cow and dateTime
  master <- master[order(master$Cow, master$dateTime), ]
  j = 1  # j is used to count which row we are at on the data sheet during for loop
  totalRow = nrow(master)
  cur1 = cur + 1  # the row below current row
  for (j in cur1:totalRow) {  # look forward pass current row
    if (master$isOne[j] == 0) {  # find the next bout with duration > 1 minute
      master$ST[cur] = master$ST[j]  # assign the current ST the same as the next consecutive behavior
      # with duration > 1
      break  # jump out of the loop
    }
  }
  return(master) # make sure that the data sheet "master" get returned before break
}


### Step 1: Cleaning Special Occasion ###
# Clean out the special occasion when the first observation for a cow happens to be a <=1 minute
# single observation. Looking forward, find the next consecutive observation and assign current
# standing/lying score to be the same as the next consecutive observation (duration > 1).
w = 1  # clear w as 1
totalRow = nrow(master)
# sort the datasheet based on cohort, week, pen, Cow and dateTime
master <- master[order(master$Cow, master$dateTime), ]
for (w in 1:totalRow) {
  # Situation 1: if duration of currernt bout is > 1 minute, skip
  if (master$isOne[w] == 0) {
    # do nothing
  }
  # If duration of current bout is <= 1 minute
  else {
    
    # Situation 2: if duration of current bout is <=1 minute and it's the first observation of the cow
    if (w == 1) {  # if it is the first row in the entire data sheet
      master <- lookForward(master, w)  # Look forward and copy next behavior with duration > 1
    }
    # Situation 3: if it is not the first row in the data sheet, but it is the first observation of the cow
    else if ((w > 1) & (master$Cow[w] != master$Cow[w - 1])) {
      master <- lookForward(master, w)  # Look forward and copy next behavior with duration > 1
    }
    # Situation 4: if it is not the first row in the data sheet, but it is the first observation after a time gap
    else if ((w > 1) & master$time_interval_from_above[w] != "60s") {
      master <- lookForward(master, w)  # Look forward and copy next behavior with duration > 1
    }
    # Situation 5: If duration of current bout is <= 1 minute and it's the second observation of the cow
    else if (w == 2) {# if it is the second row in the entire data sheet
      master <- lookForward(master, w)  # Look forward and copy next behavior with duration > 1
    }
    # Situation 6: if it is not the second row in the entire data sheet, but it is the second observation of the cow
    else if ((w > 2) & (master$Cow[w] == master$Cow[w - 1]) & (master$Cow[w] != master$Cow[w - 2])) {
      master <- lookForward(master, w)  # Look forward and copy next behavior with duration > 1
    }
    # Situation 7: if it is not the first row in the data sheet, but it is the second observation after a time gap
    else if ((w > 2) & (master$time_interval_from_above[w] == "60s") & (master$time_interval_from_2_rows_above[w] != "120s")) {
      master <- lookForward(master, w)  # Look forward and copy next behavior with duration > 1
    }
  }
}
# The code above fix the first 2 observations of each cow to handle situation when the first sequence of
# observation is like this: 10111111 or 101011111



### Step 2: Cleaning General Occasion ###
# count the bout and duration of standing and lying again based on updated ST value
#### LONG COMPUTATION TIME WARNING!! The following function takes a long time to run!
master <- bout_duration(master)
# find bouts of duration less than or equal to 1 minute and mark it down
master <- lessOne(master)

w = 1  # clear w as 1
totalRow = nrow(master)
# sort the datasheet based on cohort, week, pen, Cow and dateTime
master <- master[order(master$Cow, master$dateTime), ]
for (w in 1:totalRow) {
  # Situation 1: if duration of currernt bout is > 1 minute, skip
  if (master$isOne[w] == 0) {
    # do nothing
  }
  # Situation 2: If duration of current bout is <= 1 minute, keep looking for previous behavior that
  #              has a duration that is > 1 minute, and set the current bout the same as that
  else {
    j = 1  # clear j as 1
    for (j in 1:100) {  # set the max boundary of previous behaviors to look backward for as 100.
      # It is almost impossible that we will have to look back for 100 lines
      # untill a previous behavior with duration > 1 minute is met. The loop will
      # be jumped out of before 100 times of iteration. 100 is just a number that
      # get randomly assigned here.
      if ((w > j) & (master$isOne[w - j] == 0)) {  # if a previous behavior with duration > 1 minute is found
        master$ST[w] = master$ST[w-j]  # assign the current ST the same as that previous behavior
        #with duration > 1
        
        # It is important to realize that it is impossible to keep going up and looking for previous
        # behavior with a duration > 1 minute to an extend that we reach to the behavior records of
        # a different cow above current cow. This is because in "step 1 cleaning special occasion"
        # we have already made sure that the first two lines of observation or the first bout for
        # each cow has a duartion > 1 minute.
        
        break  # jump out of the loop
      }
    }
  }
}
# The cleaning code above should handle all the single bout observation for us.


# Test 4: test if there is any bout with duration <= 1 minute left.
# count the bout and duration of standing and lying again based on updated ST value
#### LONG COMPUTATION TIME WARNING!! The following function takes a long time to run!
master <- bout_duration(master)
# find bouts of duration less than or equal to 1 minute and mark it down
master <- lessOne(master)
if (sum(master$isOne) > 0) {
  print("Sorry, there are still some bouts with duration <= 1 minute left.")
} else {
  print("Congratulations! All bouts with duration <=1 are cleaned up!")
}

# delete isOne columns
master$isOne = NULL



###################################################################################################
####################################### Standing & Lying Summary ##################################
########################################### Summary by Date #######################################
###################################################################################################

# create a copy of the master data sheet for future analysis
master2 <- master


# Count bout and duration
# Clear any pre-existing columns' data, and calculate bout and duration for lying & standing
master2$bout_ST = master2$bout  # change the name of the column
master2$duration_ST = master2$duration  # change the name of the column
master2$bout = NULL  # delete old column
master2$duration = NULL  # delete old column


### Step 1:Standing and lying time ###
# (1) Standing Time (y-axis) #
standing_table <- master2[ which(master2$ST == 1), ]  # only keep standing observations
standing_sum <- count(standing_table, vars=c("Cow", "date")) #change the column titles as needed for own data
standing_sum$'standing_time(seconds)' <- standing_sum$freq  # rename column
standing_sum$'standing_time(seconds)' <- standing_sum$'standing_time(seconds)' * 60 # transfer minute to seconds
standing_sum$freq = NULL  # delete old column
# (2) Lying Time (z-axis) #
lying_table <- master2[ which(master2$ST == 0), ]  # only keep lying observations
lying_sum <- count(lying_table, vars=c("Cow", "date")) #change the column titles as needed for own data
lying_sum$'lying_time(seconds)' <- lying_sum$freq  # rename column
lying_sum$'lying_time(seconds)' <- lying_sum$'lying_time(seconds)' * 60
lying_sum$freq = NULL  # delete old column


### Step 2:Standing and lying BOUT & DURATION ###
# Get each bout_ST duration. This first step will split the bout by date
ST_bout <- count(master2, vars=c("Cow", "date", "bout_ST", "ST"))  #change the column titles as needed for own data
ST_bout$dur <- ST_bout$freq  # rename column as dur, dur = duration
ST_bout$freq = NULL  # delete old column
# To avoid spliting bout by date, run the following code. For example, if cow 1 started lying
# at 11PM on Day 1 and stopped at 1AM on Day2, this should be considered one bout, instead of 2
w = 1  # clear w as 1
totalRow = nrow(ST_bout) - 1  # since we will be comparing current row and next row, this is
# to avoid out of boundary exceptions
# sort the ST_bout datasheet
ST_bout <- ST_bout[order(ST_bout$Cow, ST_bout$date, ST_bout$bout_ST), ]
for (w in 1:totalRow) {
  if (ST_bout$bout_ST[w] == ST_bout$bout_ST[w +1]) {
    # if the same bout number appeared twice, it must be separated by date
    # It is important to note that same bout number appear twice must happen to the same Cow
    # The algorithm we used above made sure that if it is a different cow, it will start a new bout
    ST_bout$dur[w] = ST_bout$dur[w] +ST_bout$dur[w+1]  # If the same bout number show up in two
    # consecutive days, merge their duration as one
    ST_bout$dur[w + 1] = ST_bout$dur[w]  # updated the duration of current bout for the following date
  }
  else {
    # if it is different bout number, do nothing
  }
}


# Test 6: test if there is any bout with a duration <= 1. After all the data cleaning above, there
#         should not be any bout with duration <=1 left.
w = 1  # clear w as 1
totalRow = nrow(ST_bout)
for (w in 1:totalRow) {
  if (ST_bout$dur[w] <= 1) {
    #print(w)
    print(paste("Sorry, there are still some single bouts left. The duration of these kind of bout is <= 1.",
                " Please check what went wrong.", sep = ""))
  }
}


# (1) Standing bout
stand <- ST_bout[ which(ST_bout$ST == 1), ]  # only keep the standing record
stand_bout_final <- count(stand, vars=c("Cow", "date")) #change the column titles as needed for own data
stand_bout_final$standing_bout <- stand_bout_final$freq  # rename column
stand_bout_final$freq = NULL  # delete old column
# (2) Lying bout
lying <- ST_bout[ which(ST_bout$ST == 0), ]  # only keep the lying record
lying_bout_final <- count(lying, vars=c("Cow", "date")) #change the column titles as needed for own data
lying_bout_final$lying_bout <- lying_bout_final$freq  # rename column
lying_bout_final$freq = NULL  # delete old column
# (3) Standing duration on average
stand <- ST_bout[ which(ST_bout$ST == 1), ]  # only keep the standing record
stand_dur_final <- aggregate(stand[, "dur"], list(stand$Cow, stand$date), mean) #change the portion after"$" as needed for own data column titles
# change column names
stand_dur_final <- setNames(stand_dur_final, c("Cow", "date", "average_standing_duration/bout(seconds)"))  #change the column titles as needed for own data
stand_dur_final$'average_standing_duration/bout(seconds)' <- stand_dur_final$'average_standing_duration/bout(seconds)' * 60 # transfer minutes to seconds
# (4) Lying duration on average
lying <- ST_bout[ which(ST_bout$ST == 0), ]  # only keep the lying record
lying_dur_final <- aggregate(lying[, "dur"], list(lying$Cow, lying$date), mean) #change the portion after"$" as needed for own data column titles
# change column names
lying_dur_final <- setNames(lying_dur_final, c("Cow", "date", "average_lying_duration/bout(seconds)")) #change the column titles as needed for own data
lying_dur_final$'average_lying_duration/bout(seconds)' <- lying_dur_final$'average_lying_duration/bout(seconds)'* 60 # transfer minutes to seconds


### Step 4: Merge all data table together as one ###
HOBO_final_summary <- join_all(list(standing_sum, stand_bout_final, stand_dur_final, lying_sum, lying_bout_final,
                                    lying_dur_final), by = c("Cow", "date")) #change the column titles as needed for own data
# replace all the NA with 0
HOBO_final_summary[is.na(HOBO_final_summary)] = 0
# calculate the total time with the lying and standing combined
HOBO_final_summary$'total(seconds)' = HOBO_final_summary$standing_time + HOBO_final_summary$lying_time
final <-HOBO_final_summary



###################################################################################################
######################################### Generate boxplot ########################################
###################################################################################################
temp <- final
temp$percent <- temp$`lying_time(seconds)`/temp$`total(seconds)`
want <- temp[which(temp$percent < 0.3 | temp$percent > 0.7),]
want2 <- want[which(want$`total(seconds)` == 86400),]
temp2 <- temp[which(temp$`total(seconds)` == 86400),]
cow_ave <- aggregate(temp2[, "percent"], list(temp2$Cow), mean)
colnames(cow_ave) <- c("Cow", "average_lying_percentage")


# create a pdf file to store the histogram that will be generated for current cohort and pen
pdfPath = paste(output_dir, "/lying_standing_boxplot.pdf", sep = "")
pdf(file=pdfPath)
# make a boxplot, label outliers
boxout=boxplot(cow_ave$average_lying_percentage)$out
outname=as.character(cow_ave$Cow)
outname[(cow_ave$average_lying_percentage %in% boxout)==FALSE]="\n"
ggplot(cow_ave, aes(x = 1, y = average_lying_percentage))+geom_boxplot(outlier.size=4, outlier.colour="green")+geom_text(aes(label=outname),na.rm=TRUE,nudge_y=0.01)
# make a histogram, bining outliers with a different colour
ggplot(data=cow_ave, aes(average_lying_percentage)) + geom_histogram(breaks=seq(0.15, 0.8, by=0.05), col = "black", fill = "cornflowerblue")
dev.off() # close the pdf file



###################################################################################################
########################### Prepare for lying & feeding conflicts Analysis ########################
###################################################################################################
# create a copy of the master sheet to prepare for lying & feeding conflicts analysis
master3 <- master
master3$Start <- master3$dateTime
master3$End <- master3$dateTime
master3$keep_start <- 0
master3$keep_end <- 0


# mark down start and end time of each bout
for (h in 1:nrow(master3) ) {
  
  #if this is the start of a bout, mark down start time
  if (master3$duration[h] == 1) {
    master3$keep_start[h] <- 1
  }
  
  # if this is the end of the entire datasheet, mark down end time
  if (h == nrow(master3)) {
    master3$keep_end[h] <- 1
  }
  else if (master3$bout[h] != master3$bout[h+1]) { # if this is the end of a bout, mark down end time
    master3$keep_end[h] <- 1
  }
  
}


# get a list of start time for each bout
start_table <- master3[which(master3$keep_start == 1),]
start_table$End <- NULL
start_table$keep_start <- NULL
start_table$keep_end <- NULL
start_table <- start_table[, -c(2:8, 11) ]


# get a list of end time for each bout
end_table <- master3[which(master3$keep_end == 1),]
end_table$Start <- NULL
end_table$keep_start <- NULL
end_table$keep_end <- NULL
end_table <- end_table[, -c(2:8) ]


# merge the end and start time for each bout
start_end_table <- merge(start_table, end_table)
lying_start_end_table <- start_end_table[which(start_end_table$ST == 0), ]
lying_start_end_table <- lying_start_end_table[, -c(2:3)]
lying_start_end_table <- lying_start_end_table[, c(1, 2, 4, 3)]
colnames(lying_start_end_table) <- c("Cow", "Start", "End", "Lying_Duration(minute)")
lying_start_end_table$'Lying_Duration(seconds)' <- lying_start_end_table$`Lying_Duration(minute)` * 60
lying_start_end_table$`Lying_Duration(minute)` <- NULL


# Get a table about duration of each standing/lying bouts
# lying duration for each bout
lying_each_bout <- start_end_table[which(start_end_table$ST == 0), ]
lying_each_bout <- lying_each_bout[, -c(2:3)]
lying_each_bout <- lying_each_bout[, c(1, 2, 4, 3)]
colnames(lying_each_bout) <- c("Cow", "Start", "End", "Duration(minute)")
lying_each_bout$'Duration(seconds)' <- lying_each_bout$`Duration(minute)` * 60
lying_each_bout$`Duration(minute)` <- NULL
lying_each_bout$Behaviour <- "lying"
# standing duration for each bout
standing_each_bout <- start_end_table[which(start_end_table$ST == 1), ]
standing_each_bout <- standing_each_bout[, -c(2:3)]
standing_each_bout <- standing_each_bout[, c(1, 2, 4, 3)]
colnames(standing_each_bout) <- c("Cow", "Start", "End", "Duration(minute)")
standing_each_bout$'Duration(seconds)' <- standing_each_bout$`Duration(minute)` * 60
standing_each_bout$`Duration(minute)` <- NULL
standing_each_bout$Behaviour <- "standing"
# lying & standing merge
dur_each_bout <- rbind(lying_each_bout, standing_each_bout)
dur_each_bout <- dur_each_bout[order(dur_each_bout$Cow, dur_each_bout$Start),]




###################################################################################################
################################## Lying Synchrony Preperation ####################################
###################################################################################################
# delete all columns that are not useful for lying synch analysis
master_lying_synch <- master[, -c(3:6, 8, 10, 11)]
# only keep the lying info
master_lying_synch <- master_lying_synch[which(master_lying_synch$ST == 0),]


# seperate each sheet grouped by cowID, all seperated sheets go into a list
master_list <- list()
cow_list <- sort(unique(master_lying_synch$Cow))
for (i in 1:length(cow_list)) {
  cur_cow <- cow_list[i]
  master_list[[i]] <- master_lying_synch[which(master_lying_synch$Cow == cur_cow),]
  master_list[[i]]$Cow = NULL # delete this column
  colnames(master_list[[i]]) <- c("dateTime", "date", cur_cow)
  master_list[[i]][, 3] <- 1
}


# merge all seperate sheet in the master_list into one gaint sheet
# create an empty datasheet
master_lying_synch2 <- master_list[[1]]
for (j in 2:length(master_list)) {
  master_lying_synch2 <- merge(master_lying_synch2, master_list[[j]], all = TRUE)
}
master_lying_synch2[is.na(master_lying_synch2)] <- 0 # replace NA with 0
master_lying_synch2 <- master_lying_synch2[order(master_lying_synch2$dateTime), ] # sort the datasheet
master_lying_synch2$total_cow <- rowSums(master_lying_synch2[, 3:ncol(master_lying_synch2)], na.rm = TRUE)
# we only care about the minutes when more than 1 cow is lying
master_lying_synch3 <- master_lying_synch2[which(master_lying_synch2$total_cow > 1),]


# split the master sheet by date
date_list <- sort(unique(master$date))
master_lying_synch4 <- list()
for (i in 1:length(date_list)) {
  cur_date <- date_list[i]
  master_lying_synch4[[i]] <- master_lying_synch3[which(master_lying_synch3$date == cur_date),]
  names(master_lying_synch4)[i] <- as.character(date_list[i])
}



###################################################################################################
################################## Lying Synchrony Analysis #######################################
###################################################################################################
#### LONG COMPUTATION TIME WARNING!! The following chunk of code takes a long time to run!

# get a list of all cow's ID and create a Cow X Cow empty matrix
cow_list <- sort(unique(master$Cow))
cow_num <- length(cow_list)
empty_matrix <- matrix(0, cow_num, cow_num)
colnames(empty_matrix) <- c(cow_list)
rownames(empty_matrix) <- c(cow_list)


# list the result sheets we want to get
paired_lying_bout <- list()
paired_lying_total_time <- list()
paired_lying_average_dur <- list()


# create a function to calculate bout and duration
lying_synch_bout_dur <- function(cur_worksheet) {
  cur_worksheet <- cur_worksheet[order(cur_worksheet$dateTime),] # sort based on time
  # clear any
  cur_worksheet$bout <- 0
  cur_worksheet$duration <- 0
  total_row <- nrow(cur_worksheet)
  
  for (w in 1:total_row) {
    
    # if this is the first row
    if (w == 1) {
      cur_worksheet$bout[w] = 1 # set bout to be 1
      cur_worksheet$duration[w] = 1 # set duration to be 1
    }
    else { # if this is not the first row
      time_interval <- cur_worksheet$dateTime[w] %--% cur_worksheet$dateTime[w-1]
      time_dur <- as.duration(time_interval)
      time_dur_str <- as.character(time_dur)
      time_difference <- trimws(tolower(substring(time_dur_str, 2, 4)))
      
      # if the time gap between current row and the row above is not 60s
      if (time_difference != "60s") {
        cur_worksheet$bout[w] <- cur_worksheet$bout[w-1] + 1 # bout number + 1
        cur_worksheet$duration[w] <- 1 # duration reset to 1
      } else { # if the time gap is 1s
        cur_worksheet$bout[w] <- cur_worksheet$bout[w-1] # bout number does not change
        cur_worksheet$duration[w] <- cur_worksheet$duration[w-1] + 1  # duration + 1
      }
      
    }
  }
  
  return(cur_worksheet) # make sure the datasheet get returned
}



# iterate through each date
for (i in 1:length(date_list)) {
  cur_date <- as.character(date_list[i])
  cur_master_sheet <- master_lying_synch4[[cur_date]]
  
  
  # create matrix to store result
  paired_lying_bout[[i]] <- empty_matrix
  paired_lying_total_time[[i]] <- empty_matrix
  paired_lying_average_dur[[i]] <- empty_matrix
  # rename the sheet
  names(paired_lying_bout)[i] <- cur_date
  names(paired_lying_total_time)[i] <- cur_date
  names(paired_lying_average_dur)[i] <- cur_date
  
  
  
  # iterate through all cows
  for (k in 1:(cow_num-1)) {
    start_index <- k+2 # the column index of the current cow on the master_lying_synch4 datasheet
    matrix_row_index <- k  # the index of cow on the row of result matrix
    
    # pair each cow up with the cow at her right side
    for (h in (k+1):cow_num) {
      end_index <- h+2 # the column index of the paired other cow on the master_lying_synch4 datasheet
      matrix_col_index <- h # the index of cow on the result matrix
      
      #print(paste(k, h))
      
      # lying together
      # get a datasheet with only this two cows' information
      cur_pair <- cur_master_sheet[, c(1, start_index, end_index)]
      cur_pair$total <- rowSums(cur_pair[, 2:3], na.rm = TRUE)
      cur_pair2 <- cur_pair[which(cur_pair$total > 1),]
      
      
      # if the two cow ever lie down together
      if (nrow(cur_pair2) > 0) {
        # calculate bout and duration
        cur_pair2 <- lying_synch_bout_dur(cur_pair2)
        total_lying_time <- nrow(cur_pair2)
        total_lying_bout <- max(cur_pair2$bout)
        average_lying_dur <- total_lying_time/total_lying_bout
        
        # record result to the matrix
        paired_lying_bout[[i]][matrix_row_index, matrix_col_index] <- total_lying_bout
        paired_lying_total_time[[i]][matrix_row_index, matrix_col_index] <- total_lying_time * 60 # transfer minute to seconds
        paired_lying_average_dur[[i]][matrix_row_index, matrix_col_index] <- average_lying_dur * 60 # transfer minute to seconds
        paired_lying_bout[[i]][matrix_col_index, matrix_row_index] <- total_lying_bout
        paired_lying_total_time[[i]][matrix_col_index, matrix_row_index] <- total_lying_time * 60 # transfer minute to seconds
        paired_lying_average_dur[[i]][matrix_col_index, matrix_row_index] <- average_lying_dur * 60 # transfer minute to seconds
        
        
      } else {
        # do nothing if two never lie down together because 0 is default
      }
    }
  }
}


###################################################################################################
############################## Lying Synchrony grouped by cow #####################################
###################################################################################################
cow_table <- data.frame(cow_list)
date_table <- data.frame(date_list)
lying_synch_by_cow <- merge(cow_table, date_table, all = TRUE)
colnames(lying_synch_by_cow) <- c("Cow", "date")
lying_synch_by_cow$'lying_together_time(seconds)' <- 0
lying_synch_by_cow$lying_together_bout <- 0
lying_synch_by_cow$'lying_together_average_duration/bout(seconds)' <- 0
lying_synch_by_cow <- lying_synch_by_cow[order(lying_synch_by_cow$Cow, lying_synch_by_cow$date),]


#iterate through the list
date_num <- length(paired_lying_total_time)
for (i in 1:date_num) {
  total_cow_num <- ncol(paired_lying_total_time[[i]])
  row_start_index <- i
  
  # iterate through each column in the matrix
  for(j in 1:total_cow_num) {
    row_actual_index <- row_start_index + (date_num*(j - 1))
    total_together_time <- sum(paired_lying_total_time[[i]][, j])
    total_together_bout <- sum(paired_lying_bout[[i]][, j])
    average_together_dur <- mean(paired_lying_average_dur[[i]][, j])
    
    lying_synch_by_cow$`lying_together_time(seconds)`[row_actual_index] <- total_together_time
    lying_synch_by_cow$lying_together_bout[row_actual_index] <- total_together_bout
    lying_synch_by_cow$`lying_together_average_duration/bout(seconds)`[row_actual_index] <- average_together_dur
  }
}


###################################################################################################
##################################### HOBO Result Storage #########################################
###################################################################################################

# load existing social_character_project data set if there is one
# Create a list of all files' name from the current folder
list_file2 = list.files(path=output_dir, pattern="*.Rda", full.names=TRUE)
if (length(list_file2) > 0 ) {
  for (n in 1:length(list_file2)) {
    cur_file <- list_file2[n]
    cur_file_str <- substring(cur_file, nchar(cur_file)-36, nchar(cur_file))
    if (cur_file_str == "social_character_project_all_data.Rda") {
      out_file <- paste(output_dir, "/social_character_project_all_data.Rda", sep = "")
      load(out_file)
      
    }
  }
}


# record all the HOBO lying and standing result data sheet after cleaning
avi_index <- 1# record the next available index to write a datasheet into the list
if (length(Social_character_project) ==0) { # if this is the first time we record HOBO result
  # sheet 1: HOBO warning
  HOBO[[avi_index]] <- HOBO_warning
  names(HOBO)[avi_index] <- "HOBO_warning"
  # sheet 2: all cleaned out raw data recorded in HOBO in minutes
  HOBO[[avi_index + 1]] <- master
  names(HOBO)[avi_index + 1] <- "cleaned_HOBO_raw_data_in_minutes"
  # sheet 3: lying and standing behavior summary and analysis for all cows grouped by date
  HOBO[[avi_index + 2]] <- final
  names(HOBO)[avi_index + 2] <- "lying_standing_summary_by_date"
  # sheet 4: lying data sheet prepared for lying & eating conflicts analysis
  HOBO[[avi_index + 3]] <- lying_start_end_table
  names(HOBO)[avi_index + 3] <- "lying_data_for_feeding_conflicts"
  # sheet 5: total number of bouts when 2 cows are lying together
  HOBO[[avi_index + 4]] <- paired_lying_bout
  names(HOBO)[avi_index + 4] <- "paired lying bout"
  # sheet 6: total amount of time when 2 cows are lying together
  HOBO[[avi_index + 5]] <- paired_lying_total_time
  names(HOBO)[avi_index + 5] <- "paired lying total time"
  # sheet 7: average duration of bout when 2 cows are lying together
  HOBO[[avi_index + 6]] <- paired_lying_average_dur
  names(HOBO)[avi_index + 6] <- "paired lying avergae duration"
  # sheet 8: total lying together time, bouts, and average duration grouped by cow
  HOBO[[avi_index + 7]] <- lying_synch_by_cow
  names(HOBO)[avi_index + 7] <- "lying_together_analysis_by_cow"
  # sheet 9: duration for each standing and lying bout
  HOBO[[avi_index + 8]] <- dur_each_bout
  names(HOBO)[avi_index + 8] <- "duration_for_each_bout"
  
  
  # add the HOBO list to social_character_project
  Social_character_project[[1]] <- HOBO
  names(Social_character_project)[1] <- "HOBO"
  
} else { # if this is not the first time
  # sheet 1: HOBO warning
  Social_character_project[["HOBO"]][[avi_index]] <- rbind(Social_character_project[["HOBO"]][[avi_index]], HOBO_warning)
  # sheet 2: all cleaned out raw data recorded in HOBO in minutes
  Social_character_project[["HOBO"]][[avi_index + 1]] <- rbind(Social_character_project[["HOBO"]][[avi_index + 1]], master)
  # sheet 3: lying and standing behavior summary and analysis for all cows grouped by date
  Social_character_project[["HOBO"]][[avi_index + 2]] <- rbind(Social_character_project[["HOBO"]][[avi_index + 2]], final)
  # sheet 4: lying data sheet prepared for lying & eating conflicts analysis
  Social_character_project[["HOBO"]][[avi_index + 3]] <- rbind(Social_character_project[["HOBO"]][[avi_index + 3]], lying_start_end_table)
  # sheet 5: total number of bouts when 2 cows are lying together
  Social_character_project[["HOBO"]][[avi_index + 4]] <- append(Social_character_project[["HOBO"]][[avi_index + 4]], paired_lying_bout)
  # sheet 6: total amount of time when 2 cows are lying together
  Social_character_project[["HOBO"]][[avi_index + 5]] <- append(Social_character_project[["HOBO"]][[avi_index + 5]], paired_lying_total_time)
  # sheet 7: average duration of bout when 2 cows are lying together
  Social_character_project[["HOBO"]][[avi_index + 6]] <- append(Social_character_project[["HOBO"]][[avi_index + 6]], paired_lying_average_dur)
  # sheet 8: total lying together time, bouts, and average duration grouped by cow
  Social_character_project[["HOBO"]][[avi_index + 7]] <- rbind(Social_character_project[["HOBO"]][[avi_index + 7]], lying_synch_by_cow)
  # sheet 9: duration for each standing and lying bout
  Social_character_project[["HOBO"]][[avi_index + 8]] <- rbind(Social_character_project[["HOBO"]][[avi_index + 8]], dur_each_bout)
  
}


# output the social_character_project as a RDA file
out_file <- paste(output_dir, "/social_character_project_all_data.Rda", sep = "")
save(Social_character_project, file = out_file)


###################################################################################################
###                                                                                             ###
### Chapter 2: Feeding & Drinking Behaviour analysis                                            ###
### Description: This code runs all dates on trial. It uses Insentec data to analyze daily feed ###
###               and drinking intake. It generates warning massages of which bins need to be   ###
###               cleaned.                                                                      ###
###                                                                                             ###
###################################################################################################


###################################################################################################
##################### Feeding & Drinking Analysis from Insentec Data ##############################
###################################################################################################
# deal with when there are different number of feeding data files and drinking data files
# get a list of dates that has feeding information
feed_name <- data.frame(fileNames.f)
colnames(feed_name) <- c("Feed_dir")
feed_name$Feed_dir_mod <- trimws(feed_name$Feed_dir, which = "both")
feed_name$Feed_dir_mod <- chartr("/", "_", feed_name$Feed_dir_mod)
feed_name$Feed_dir_mod <- chartr(" ", "_", feed_name$Feed_dir_mod)
feed_name$Feed_dir_mod <- chartr(".", "_", feed_name$Feed_dir_mod)
feed_name$date <- ""
for (i in 1:nrow(feed_name)) {
  temp_list <- strsplit(feed_name$Feed_dir_mod[i], "_")  # split the filename string by "_"
  # extract date
  feed_name$date[i] = substring(temp_list[[1]][length(temp_list[[1]])-1], 3) # extract the date information of the file
}
# get a list of dates that has drinking information
wat_name <- data.frame(fileNames.w)
colnames(wat_name) <- c("Drink_dir")
wat_name$Drink_dir_mod <- trimws(wat_name$Drink_dir, which = "both")
wat_name$Drink_dir_mod <- chartr("/", "_", wat_name$Drink_dir_mod)
wat_name$Drink_dir_mod <- chartr(" ", "_", wat_name$Drink_dir_mod)
wat_name$Drink_dir_mod <- chartr(".", "_", wat_name$Drink_dir_mod)
wat_name$date <- ""
for (i in 1:nrow(wat_name)) {
  temp_list <- strsplit(wat_name$Drink_dir_mod[i], "_")  # split the filename string by "_"
  # extract date
  wat_name$date[i] = substring(temp_list[[1]][length(temp_list[[1]])-1], 3) # extract the date information of the file
}
# compare water and feeding sheet
compare_sheet <- merge(feed_name, wat_name, all = TRUE)
compare_sheet <- compare_sheet[order(compare_sheet$date),]
compare_sheet2 <- na.omit(compare_sheet)
fileNames.f <- compare_sheet2$Feed_dir
fileNames.w <- compare_sheet2$Drink_dir
# we only retain the dates when both drinking and feeding data are available at the same time


# set up string variables for start and end date
if(length(fileNames.f) > 1) {
  start_date <- substring(as.character(fileNames.f[1]),(nchar(as.character(fileNames.f[1]))-9),(nchar(as.character(fileNames.f[1]))-4))
  start_date <- as.POSIXct(start_date,format="%y%m%d")
  end_date <- substring(as.character(fileNames.f[length(fileNames.f)]),(nchar(as.character(fileNames.f[length(fileNames.f)]))-9), (nchar(as.character(fileNames.f[length(fileNames.f)]))-4))
  end_date <- as.POSIXct(end_date,format="%y%m%d")
  date_range <- paste(start_date, "_", end_date, "_", sep = "")
} else if (length(fileNames.f) == 1){
  start_date <- substring(as.character(fileNames.f[1]), (nchar(as.character(fileNames.f[1]))-9), (nchar(as.character(fileNames.f[1]))-4))
  start_date <- as.POSIXct(start_date,format="%y%m%d")
  end_date <- ""
  date_range <- paste(start_date, "_", sep = "")
} else {
  start_date <- ""
  end_date <- ""
  date_range <- ""
  print("Warning: You read in 0 files!")
}


# Colnames in the feed and water bin log files, change based on your own file format
coln=c("Transponder","Cow","Bin","Start","End","Duration","Startweight","Endweight","Comment","Intake","Intake2","X1","X2","X3","X4")
coln.wat=c("Transponder","Cow","Bin","Start","End","Duration","Startweight","Endweight","Intake")

#Check your bins of interest: Feed bins: 1-30, water bins:1-5 (all)
#Get the feeder, drinker and combined data into a list
len = length(fileNames.f)
all.fed=list()
all.wat=list()
all.comb=list()


for(i in 1:len)
{
  ##### Feed bins #####
  feeder=read.table(as.character(fileNames.f[i]),header=F,sep=",")
  date=substring(as.character(fileNames.f[i]),nchar(as.character(fileNames.f[i]))-9,nchar(as.character(fileNames.f[i]))-4) #this gets the date from the file name
  date2 <- ymd(date, tz="America/Los_Angeles")
  date = as.character(ymd(date, tz="America/Los_Angeles"))
  colnames(feeder)=coln
  #get rid of calibration entries, cow's that are 0, and only keep bins from 1-30
  # get rid of cow 1111 because this is the cow ID Wali used to test, it's not a real cow
  tak.out=which((feeder$Transponder==0) | (feeder$Cow ==0) | (feeder$Cow ==1111) | (feeder$Bin > 30) | (feeder$Bin< 1)) 
  if(length(tak.out)!=0)fed.1=feeder[-tak.out,c(1:8,10)]#get the same columns as for water data -here if you need the comment column you have to tweak the code a bit
  if(length(tak.out)==0)fed.1=feeder[,c(1:8,10)]
  
  ###### Water bins #####
  water=read.table(as.character(fileNames.w[i]),header=F,sep=",")
  colnames(water)=coln.wat
  #get rid of calibration entries, cow's that are 0, and only keep bins from 1-5
  # get rid of cow 1111 because this is the cow ID Wali used to test, it's not a real cow
  tak.out=which((water$Transponder==0) | (water$Cow ==0) | (water$Cow ==1111) | (water$Bin > 5) | (water$Bin< 1))
  if(length(tak.out)!=0)wat.1=water[-tak.out,1:9]
  if(length(tak.out)==0)wat.1=water[,1:9]
  # Water bins have to be renamed to enable combination with feed bin data
  wat.1[which(wat.1$Bin==1),3]=101
  wat.1[which(wat.1$Bin==2),3]=102
  wat.1[which(wat.1$Bin==3),3]=103
  wat.1[which(wat.1$Bin==4),3]=104
  wat.1[which(wat.1$Bin==5),3]=105
  
  
  
  #Feeder, drinker and combined data, HAVE TO ADJUST BIN NUMBERS ACCORDING TO GROUP OF INTEREST
  all.fed[[i]]=na.omit(fed.1)
  all.wat[[i]]=wat.1[which(wat.1$Bin>100),]
  
  
  # trim the start and end time format
  all.fed[[i]]$Start <- trimws(all.fed[[i]]$Start, which = "both")
  all.fed[[i]]$End <- trimws(all.fed[[i]]$End, which = "both")
  
  
  ##################################### Daylight saving change ################################
  # daylight saving change date in the spring, time went from 2am to 3am directly
  # daylight saving change date in the fall, time went from 2am back to 1am 
  # get the current year
  cur_year <- as.integer(year(date2))
  cur_month <- as.integer(month(date2))
  cur_year_line <- daylight_saving_table[which(daylight_saving_table$Year == cur_year), ]
  
  # determine if current period is in the spring or fall
  # this is fall: delete 2am-3am and move all time after 3am by 1 hour earlier
  if (cur_month > 9) {  
    daylight_change_date <- cur_year_line$Fall[1]
    daylight_change_next_date <- cur_year_line$Fall_nextDay[1]
    daylight_change_time <- hms("2:00:00")
    daylight_change_time2 <- hms("3:00:00")
    
    # If current day is the day when daylight saving change happened
    if (date2 == daylight_change_date) {
      ### For Feeding
      temp <- all.fed[[i]]
      temp$Start2 <- hms(temp$Start)
      temp$End2 <- hms(temp$End)
      # delete all events with Start time or end time happened between 2-3am
      if (nrow(temp[which((temp$Start2 > daylight_change_time) & (temp$Start2 <= daylight_change_time2)), ]) > 0) {
        temp <- temp[-which((temp$Start2 > daylight_change_time) & (temp$Start2 <= daylight_change_time2)), ]
      } 
      if (nrow(temp[which((temp$End2 > daylight_change_time) & (temp$End2 <= daylight_change_time2)), ]) >0) {
        temp <- temp[-which((temp$End2 > daylight_change_time) & (temp$End2 <= daylight_change_time2)), ]
      }
      # handle start time change
      before_change <- temp[which(temp$Start2 <= daylight_change_time), ]
      after_change <- temp[which(temp$Start2 > daylight_change_time2), ]
      after_change$Start2 <- after_change$Start2 - hours(1)
      after_change$Start <- paste(as.character(hour(after_change$Start2)), as.character(minute(after_change$Start2)), as.character(second(after_change$Start2)), sep = ":")
      temp <- rbind(before_change, after_change)
      # handle end time change
      before_change <- temp[which(temp$End2 <= daylight_change_time), ]
      after_change <- temp[which(temp$End2 > daylight_change_time2), ]
      after_change$End2 <- after_change$End2 - hours(1)
      after_change$End <- paste(as.character(hour(after_change$End2)), as.character(minute(after_change$End2)), as.character(second(after_change$End2)), sep = ":")
      temp <- rbind(before_change, after_change)
      # delete helping columns
      temp$Start2 <- NULL
      temp$End2 <- NULL
      all.fed[[i]] <- temp
      
      
      ### For Drinking
      temp <- all.wat[[i]]
      temp$Start2 <- hms(temp$Start)
      temp$End2 <- hms(temp$End)
      # delete all events with Start time or end time happened between 2-3am
      if (nrow(temp[which((temp$Start2 > daylight_change_time) & (temp$Start2 <= daylight_change_time2)), ]) >0){
        temp <- temp[-which((temp$Start2 > daylight_change_time) & (temp$Start2 <= daylight_change_time2)), ]
      }
      if (nrow(temp[which((temp$End2 > daylight_change_time) & (temp$End2 <= daylight_change_time2)), ])>0) {
        temp <- temp[-which((temp$End2 > daylight_change_time) & (temp$End2 <= daylight_change_time2)), ]
      }
      # handle start time change
      before_change <- temp[which(temp$Start2 <= daylight_change_time), ]
      after_change <- temp[which(temp$Start2 > daylight_change_time2), ]
      after_change$Start2 <- after_change$Start2 - hours(1)
      after_change$Start <- paste(as.character(hour(after_change$Start2)), as.character(minute(after_change$Start2)), as.character(second(after_change$Start2)), sep = ":")
      temp <- rbind(before_change, after_change)
      # handle end time change
      before_change <- temp[which(temp$End2 <= daylight_change_time), ]
      after_change <- temp[which(temp$End2 > daylight_change_time2), ]
      after_change$End2 <- after_change$End2 - hours(1)
      after_change$End <- paste(as.character(hour(after_change$End2)), as.character(minute(after_change$End2)), as.character(second(after_change$End2)), sep = ":")
      temp <- rbind(before_change, after_change)
      # delete helping columns
      temp$Start2 <- NULL
      temp$End2 <- NULL
      all.wat[[i]] <- temp
      
    } 
    
    # this is spring: move all time after 2am to be 1 hour later
  } else { 
    daylight_change_date <- cur_year_line$Spring[1]
    daylight_change_next_date <- cur_year_line$Spring_nextDay[1]
    daylight_change_time <- hms("2:00:00")
    daylight_change_time2 <- hms("3:00:00")
    
    # If current day is the day when daylight saving change happened
    if (date2 == daylight_change_date) {
      ### For Feeding
      temp <- all.fed[[i]]
      temp$Start2 <- hms(temp$Start)
      temp$End2 <- hms(temp$End)
      # handle special occations when start time < 2am, but end time > 2am. delete all of those events
      if (nrow(temp[which((temp$Start2 <= daylight_change_time) & (temp$End2 > daylight_change_time)), ]) > 0 ){
        temp <- temp[-which((temp$Start2 <= daylight_change_time) & (temp$End2 > daylight_change_time)), ]
      }
      
      # handle start time change
      before_change <- temp[which(temp$Start2 <= daylight_change_time), ]
      after_change <- temp[which(temp$Start2 > daylight_change_time), ]
      after_change$Start2 <- after_change$Start2 + hours(1)
      after_change$Start <- paste(as.character(hour(after_change$Start2)), as.character(minute(after_change$Start2)), as.character(second(after_change$Start2)), sep = ":")
      temp <- rbind(before_change, after_change)
      # handle end time change
      before_change <- temp[which(temp$End2 <= daylight_change_time), ]
      after_change <- temp[which(temp$End2 > daylight_change_time), ]
      after_change$End2 <- after_change$End2 + hours(1)
      after_change$End <- paste(as.character(hour(after_change$End2)), as.character(minute(after_change$End2)), as.character(second(after_change$End2)), sep = ":")
      temp <- rbind(before_change, after_change)
      # delete helping columns
      temp$Start2 <- NULL
      temp$End2 <- NULL
      all.fed[[i]] <- temp
      
      ### For Drinking
      temp <- all.wat[[i]]
      temp$Start2 <- hms(temp$Start)
      temp$End2 <- hms(temp$End)
      # handle special occations when start time < 2am, but end time > 2am. delete all of those events
      if (nrow(temp[which((temp$Start2 <= daylight_change_time) & (temp$End2 > daylight_change_time)), ]) > 0 ){
        temp <- temp[-which((temp$Start2 <= daylight_change_time) & (temp$End2 > daylight_change_time)), ]
      }
      # handle start time change
      before_change <- temp[which(temp$Start2 <= daylight_change_time), ]
      after_change <- temp[which(temp$Start2 > daylight_change_time), ]
      after_change$Start2 <- after_change$Start2 + hours(1)
      after_change$Start <- paste(as.character(hour(after_change$Start2)), as.character(minute(after_change$Start2)), as.character(second(after_change$Start2)), sep = ":")
      temp <- rbind(before_change, after_change)
      # handle end time change
      before_change <- temp[which(temp$End2 <= daylight_change_time), ]
      after_change <- temp[which(temp$End2 > daylight_change_time), ]
      after_change$End2 <- after_change$End2 + hours(1)
      after_change$End <- paste(as.character(hour(after_change$End2)), as.character(minute(after_change$End2)), as.character(second(after_change$End2)), sep = ":")
      temp <- rbind(before_change, after_change)
      # delete helping columns
      temp$Start2 <- NULL
      temp$End2 <- NULL
      all.wat[[i]] <- temp
      
      
      # If current day is the next day after daylight saving change happened
    } else if (date2 == daylight_change_next_date) {
      
      # for Feeding
      temp <- all.fed[[i]]
      temp$Start2 <- as.integer(hour(hms(temp$Start)))
      temp$toDelete <- 0
      for (p in 1:(nrow(temp)-1)) {
        temp$toDelete[p] <- 1
        if ((temp$Start2[p] > 20) & (temp$Start2[p+1] < 5)) {
          break;
        }
      }
      temp2 <- temp[which(temp$toDelete == 0), ]
      temp2$toDelete <- NULL
      temp2$Start2 <- NULL
      all.fed[[i]] <- temp2
      
      
      # for Drinking
      temp <- all.wat[[i]]
      temp$Start2 <- as.integer(hour(hms(temp$Start)))
      temp$toDelete <- 0
      for (p in 1:(nrow(temp)-1)) {
        temp$toDelete[p] <- 1
        if ((temp$Start2[p] > 20) & (temp$Start2[p+1] < 5)) {
          break;
        }
      }
      temp2 <- temp[which(temp$toDelete == 0), ]
      temp2$toDelete <- NULL
      temp2$Start2 <- NULL
      all.wat[[i]] <- temp2
    }
  }
  
  
  
  # get combined data
  all.comb[[i]]=rbind(all.fed[[i]],all.wat[[i]])
  
  
  #Adjusting start and end times to make R recognize the date and time format
  all.fed[[i]]$Start=paste(rep(date,dim(all.fed[[i]])[1]),all.fed[[i]]$Start)
  all.fed[[i]]$Start=ymd_hms(all.fed[[i]]$Start, tz="America/Los_Angeles")
  all.fed[[i]]$End=paste(rep(date,dim(all.fed[[i]])[1]),all.fed[[i]]$End)
  all.fed[[i]]$End=ymd_hms(all.fed[[i]]$End, tz="America/Los_Angeles")
  
  all.wat[[i]]$Start=paste(rep(date,dim(all.wat[[i]])[1]),all.wat[[i]]$Start)
  all.wat[[i]]$Start=ymd_hms(all.wat[[i]]$Start, tz="America/Los_Angeles")
  all.wat[[i]]$End=paste(rep(date,dim(all.wat[[i]])[1]),all.wat[[i]]$End)
  all.wat[[i]]$End=ymd_hms(all.wat[[i]]$End, tz="America/Los_Angeles")
  
  all.comb[[i]]$Start=paste(rep(date,dim(all.comb[[i]])[1]),all.comb[[i]]$Start)
  all.comb[[i]]$Start=ymd_hms(all.comb[[i]]$Start, tz="America/Los_Angeles")
  all.comb[[i]]$End=paste(rep(date,dim(all.comb[[i]])[1]),all.comb[[i]]$End)
  all.comb[[i]]$End=ymd_hms(all.comb[[i]]$End, tz="America/Los_Angeles")
  
  #Adding dates as name
  names(all.fed)[i]=date
  names(all.wat)[i]=date
  names(all.comb)[i]=date
  
}


#### merge all data into 3 gaint master sheet ####
master_feeding <- all.fed[[1]][-c(1:nrow(all.fed[[1]])), ]
master_drinking <- all.wat[[1]][-c(1:nrow(all.wat[[1]])), ]
master_comb <- all.comb[[1]][-c(1:nrow(all.comb[[1]])), ]
for (e in 1:length(all.comb)){
  master_feeding <- rbind(master_feeding, all.fed[[e]])
  master_drinking <- rbind(master_drinking, all.wat[[e]])
  master_comb <- rbind(master_comb, all.comb[[e]])
}


# calculate the percentage of visits with a negative weight as start weight or end weight
master_subset <- master_comb
master_subset$date <- date(master_subset$Start)
master_subset <- master_subset[which((master_subset$date >= "2020-07-15") & (master_subset$date <= "2021-05-07")),]
negative_weight <- master_subset[which((master_subset$Startweight <0 ) | (master_subset$Endweight <0)),]
negative_weight_percent <- nrow(negative_weight)/nrow(master_subset)

###################################################################################################
####################################### Quality Check #############################################
###################################################################################################

# create a final warning csv datasheet to record all bins involved in double detection, 
# extended period of feeding/drinking (duration>2000), outliers, eating-lying conflicts
date_list <- names(all.fed)
time_interval_after_feed_added <- data.frame(date_list)
colnames(time_interval_after_feed_added) <- c("date")
Insentec_warning <- data.frame(date_list)
colnames(Insentec_warning) <- c("date") # rename column 
Insentec_warning$date <- ymd(Insentec_warning$date,tz="America/Los_Angeles")


# create some new columns for future recording
Insentec_warning$total_cow_number <- ""
Insentec_warning$missing_cow <- ""
Insentec_warning$long_feed_duration_bin <- ""  # error 1  # feeding duration > 2400s
Insentec_warning$long_water_duration_bin <- ""   # error 2  # drinking duration > 1800s
Insentec_warning$double_bin_detection_bin <- ""  # error 3
Insentec_warning$double_cow_detection_bin <- ""  # error 4
Insentec_warning$eating_lying_conflict_bin <- ""  # error 5
Insentec_warning$negative_duration_bin <- ""  # error 6  
Insentec_warning$negative_intake_bin <- ""  # error 7  # negative intake bins here only account for those bins that has negative intake with an absolute value of > 1
Insentec_warning$large_one_bout_feed_intake_bin <- ""  # error 8
Insentec_warning$large_one_bout_water_intake_bin <- ""  # error 9
Insentec_warning$large_feed_intake_in_short_time_bin <- ""  # error 10
Insentec_warning$large_water_intake_in_short_time_bin <- ""  # error 11
Insentec_warning$no_show_after_6pm_cows <- "" #error 12 # this cow never showed up again after 6pm, and register the last time this cow was seen
Insentec_warning$no_show_after_12pm_cows <- "" #error 13
Insentec_warning$no_visit_after_6pm_bins <- "" # error 14  # this bin never get visited again after 6pm, and register the last time the bin was visited
Insentec_warning$no_visit_after_12pm_bins <- "" # error 15
Insentec_warning$bins_not_visited_today <- "" # error 16 
Insentec_warning$feed_bins_with_low_visits_today <- "" # error 17  # daily visit < 10 times
Insentec_warning$cows_no_visit_to_feed_bin <- "" # error 18
Insentec_warning$cows_no_visit_to_water_bin <- "" # error 19
Insentec_warning$low_daily_feed_intake_cows <- "" # error 20 # daily feed intake < 35 kg # register what is the exact feed intake
Insentec_warning$high_daily_feed_intake_cows <- ""  # error 21  # daily feed intake > 75 kg # register what is the exact feed intake
Insentec_warning$low_daily_water_intake_cows <- "" # error 22 # daily water intake < 60 L # register what is the exact water intake
Insentec_warning$high_daily_water_intake_cows <- "" # error 23 # daily water intake > 180 L # register what is the exact water intake
Insentec_warning$feed_add_time_no_found <- "" # error 24


########################## Insentec warning: total number of cows #################################
Insentec_warning <- Insentec_warning[order(Insentec_warning$date),]
for (i in 1:length(all.comb)) {
  Insentec_warning$total_cow_number[i] <- length(unique(all.comb[[i]]$Cow))
}


############################# Error 1: long feeding duration ######################################
#Plot daily visit durations to check for outliers
#Bin staying open,visits of an hour or more (3600 sec) are very unlikely, many visits above 2400 sec are warning, especially for drinkers
# create a pdf file to store the boxplot that will be generated
pdfPath = paste(output_dir, "/", date_range, "feed_water_allDate_boxplot.pdf", sep = "")
pdf(file=pdfPath)
boxplot(master_feeding$Duration,main=paste(names(all.fed)[1], " to ", names(all.fed)[length(all.fed)], "Feeder",sep = "-"))
boxplot(master_drinking$Duration,main=paste(names(all.wat)[1], " to ", names(all.wat)[length(all.wat)], "Water",sep = "-"))
dev.off() # close the pdf file

# get a list of all the outliers
# outliers in the feed bin
outlier_values_fed <- boxplot.stats(master_feeding$Duration)$out
out_ind_fed <- which(master_feeding$Duration %in% c(outlier_values_fed))
outlier_table_fed <- master_feeding[out_ind_fed, ]
outlier_fed_percentage <- paste((round(nrow(outlier_table_fed)/nrow(master_feeding), digits = 4)*100), "%", sep = "") # get extreme outlier percentage
master_feeding2 <- master_feeding # do not delete outliers

# outliers in the water bin
outlier_values_wat <- boxplot.stats(master_drinking$Duration)$out
out_ind_wat <- which(master_drinking$Duration %in% c(outlier_values_wat))
outlier_table_wat <- master_drinking[out_ind_wat, ]
outlier_wat_percentage <- paste((round(nrow(outlier_table_wat)/nrow(master_drinking), digits = 4)*100), "%", sep = "")
master_drinking2 <- master_drinking

# get a new combined data sheet including all feeding and drinking data
master_comb2 <- rbind(master_feeding2, master_drinking2)
master_comb2$date <- date(master_comb2$Start)
master_comb2 <- master_comb2[order(master_comb2$date),]
all_date <- sort(unique(master_comb2$date))
Insentec_warning <- Insentec_warning[order(Insentec_warning$date),]

#get the rows with duration > 2000
for(u in 1: length(all.fed)){
  extended_fed <- all.fed[[u]][which(all.fed[[u]]$Duration > 2000),]
  cur_index <- length(long_feed_dur_list)+1
  long_feed_dur_list[[cur_index]] <- extended_fed
  names(long_feed_dur_list)[cur_index] <- names(all.fed)[u]
  extended_fed_bin <- sort(unique(extended_fed$Bin))
  extended_fed_bin_str <- paste(unlist(extended_fed_bin), collapse="; ")
  Insentec_warning$long_feed_duration_bin[u] <- extended_fed_bin_str#record it on warning message sheet
}



############################# Error 2: long drinking duration ######################################
#get the rows with duration > 1800s
for(u in 1: length(all.wat)){
  extended_wat <- all.wat[[u]][which(all.wat[[u]]$Duration > 1800),]
  cur_index <- length(long_wat_dur_list)+1
  long_wat_dur_list[[cur_index]] <- extended_wat
  names(long_wat_dur_list)[cur_index] <- names(all.wat)[u]
  extended_wat_bin <- sort(unique(extended_wat$Bin))
  extended_wat_bin_str <- paste(unlist(extended_wat_bin), collapse="; ")
  Insentec_warning$long_water_duration_bin <- extended_wat_bin_str#record it on warning message sheet
}


################# Error 3: double detection (same cow shows up at 2 bins) #########################
#Cows at two locations at the same time
# create an empty master sheet first
double_detection <- all.comb[[1]][-(1:nrow(all.comb[[1]])), ]
for(i in 1:length(all.comb))
{
  dat=all.comb[[i]] # 1 day
  cows=unique(dat$Cow)
  # create a new sheet recording each day's double detection
  daily_double_detection <- all.comb[[1]][-(1:nrow(all.comb[[1]])), ]
  
  for(j in 1: length(cows))
  {
    dat2=dat[which(dat$Cow==cows[j]),]#data for 1 cow
    dat2=dat2[order(dat2$Start),]#time ordered visits for the cow
    
    if (dim(dat2)[1] > 1) {
      for(k in 2:dim(dat2)[1]) 
      {
        if(dat2[k,4] < dat2[k-1,5]) {
          # add the double detection records to daily_double_detection first
          daily_double_detection <- rbind(daily_double_detection, dat2[(k-1):k,])
        }
      }
    }
  }
  
  cur_index <- length(double_bin_detection_list)+1
  double_bin_detection_list[[cur_index]] <- daily_double_detection
  names(double_bin_detection_list)[cur_index] <- names(all.comb)[i]
  
  # after iterating through all records of the cows, merge it with the master sheet into double_detection
  double_detection <- rbind(double_detection, daily_double_detection)
  # only the first bin in the double detection events are the bins that need to be cleaned. 
  if (nrow(daily_double_detection) > 0) { # only preceed if there is double detection for that day
    faulty_bin <- daily_double_detection
    faulty_bin$rowNum <- 0
    for (k in 1:nrow(faulty_bin)) {
      faulty_bin$rowNum[k] <- k
    }
    faulty_bin2 <- faulty_bin[which((faulty_bin$rowNum%%2) != 0),]
    # add bin number for double detections to the warning_massage datasheet
    double_detection_bin <- sort(unique(faulty_bin2$Bin))
    Insentec_warning$double_bin_detection_bin[i] <- paste(unlist(double_detection_bin), collapse="; ")
  }
  
  
}


################# Error 4: double detection (same bin registers 2 cows) ###########################
# different cows at the same bin at the same time
double_cow_detection <- all.comb[[1]][-(1:nrow(all.comb[[1]])), ]
for(i in 1:length(all.comb))
{
  dat=all.comb[[i]] # 1 day
  bins <- unique(dat$Bin)
  # create a new sheet recording each day's double detection
  daily_double_cow_detection <- all.comb[[1]][-(1:nrow(all.comb[[1]])), ]
  
  for(j in 1: length(bins))
  {
    dat2=dat[which(dat$Bin==bins[j]),]#data for 1 bin
    dat2=dat2[order(dat2$Start),]#time ordered visits for the cow
    
    if (dim(dat2)[1] > 1) {
      for(k in 2:dim(dat2)[1]) 
      {
        if(dat2[k,4] < dat2[k-1,5]) {
          # add the double detection records to daily_double_detection first
          daily_double_cow_detection <- rbind(daily_double_cow_detection, dat2[(k-1):k,])
        }
      }
    }
  }
  
  cur_index <- length(double_cow_detection_list)+1
  double_cow_detection_list[[cur_index]] <- daily_double_cow_detection
  names(double_cow_detection_list)[cur_index] <- names(all.comb)[i]
  
  # after iterating through all records of the cows, merge it with the master sheet into double_detection
  double_cow_detection <- rbind(double_cow_detection, daily_double_cow_detection)
  # add bin number for double detections to the warning_massage datasheet
  double_cow_detection_bin <- sort(unique(daily_double_cow_detection$Bin))
  Insentec_warning$double_cow_detection_bin[i] <- paste(unlist(double_cow_detection_bin), collapse="; ")
}



########################### Error 5: eating/drinking and lying conflict ####################################
lying_data <- Social_character_project[[1]][["lying_data_for_feeding_conflicts"]]
colnames(lying_data) <- c("Cow", "Start", "End", "Duration")
# transform the lying data to have the same format as the feeding and drinking data
# Here we register all the lying records to be having transponder, bin, startweight, endweight, and intake as NA
# in order to differentiate between eating, drinking and lying
lying_data$Transponder <- NA
lying_data$Bin <- NA
lying_data$Startweight <- NA
lying_data$Endweight <- NA
lying_data$Intake <- NA
lying_data <- lying_data[, c(5, 1, 6, 2, 3, 4, 7, 8, 9)]  # reorder the column names
lying_data$date <- date(lying_data$Start)
eating_lying_conflict <- all.comb[[1]][-(1:nrow(all.comb[[1]])), ]
for(i in 1:length(all.comb))
{
  print(i)
  dat=all.comb[[i]] # 1 day
  cur_date <- names(all.comb)[i]
  # get the lying data for current date
  cur_lying_data <- lying_data[which(lying_data$date == cur_date),]
  # transform the lying data to have the same format as the feeding and drinking data
  cur_lying_data$date <- NULL
  
  eating_lying_comb <- rbind(cur_lying_data, dat)
  cows=unique(eating_lying_comb$Cow)
  # create a new sheet recording each day's double detection
  daily_eating_lying_conflict <- all.comb[[1]][-(1:nrow(all.comb[[1]])), ]
  
  for(j in 1: length(cows))
  {
    eating_lying_comb2=eating_lying_comb[which(eating_lying_comb$Cow==cows[j]),]#eating_lying_comba for 1 cow
    eating_lying_comb2=eating_lying_comb2[order(eating_lying_comb2$Start),]#time ordered visits for the cow
    
    if (dim(eating_lying_comb2)[1] > 1) {
      for(k in 2:dim(eating_lying_comb2)[1]) 
      {
        if(eating_lying_comb2$Start[k] < eating_lying_comb2$End[k-1]) {
          # only register those in conflict with lying time 
          if (is.na(eating_lying_comb2$Transponder[k]) | is.na(eating_lying_comb2$Transponder[k-1])){
            # add the double detection records to daily_eating_lying_conflict first
            daily_eating_lying_conflict <- rbind(daily_eating_lying_conflict, eating_lying_comb2[(k-1):k,])
          }
          
        }
      }
    }
  }
  
  cur_index <- length(eating_lying_conflict_list)+1
  eating_lying_conflict_list[[cur_index]] <- daily_eating_lying_conflict
  names(eating_lying_conflict_list)[cur_index] <- names(all.comb)[i]
  
  # after iterating through all records of the cows, merge it with the master sheet into eating_lying_conflict
  eating_lying_conflict <- rbind(eating_lying_conflict, daily_eating_lying_conflict)
  # add bin number for double detections to the warning_massage datasheet
  eating_lying_conflict_b <- sort(unique(na.omit(daily_eating_lying_conflict$Bin)))
  Insentec_warning$eating_lying_conflict_bin[i] <- paste(unlist(eating_lying_conflict_b), collapse="; ")
  
}



################################# Error 6: Negative duration ######################################
################################## Error 7: Negative intake #######################################
all.fed2 <- all.fed
all.wat2 <- all.wat
all.comb2 <- all.comb

for(i in 1:length(all.comb2))
{
  print(i)
  dat=all.comb2[[i]] # 1 day
  
  ### error 6: Negative duration
  negative_duration<- dat[which(dat$Duration <0),] # take out negative duration records
  cur_index <- length(negative_dur_list)+1
  negative_dur_list[[cur_index]] <- negative_duration
  names(negative_dur_list)[cur_index] <- names(all.comb2)[i]
  negative_duration_bin <- sort(unique(negative_duration$Bin))
  Insentec_warning$negative_duration_bin[i] <- paste(unlist(negative_duration_bin), collapse="; ")
  
  ### error 7: negative intake
  negative_intake<- dat[which(dat$Intake <0),] # take out negative intake records
  negative_intake$abs_intake <- abs(negative_intake$Intake)
  negative_intake2 <- negative_intake[which(negative_intake$abs_intake > 1),] # only output those with negative intake more than - 1
  negative_intake2$abs_intake <- NULL
  cur_index_neg_intake <- length(negative_intake_list)+1
  negative_intake_list[[cur_index_neg_intake]] <- negative_intake2
  names(negative_intake_list)[cur_index_neg_intake] <- names(all.comb)[i]
  negative_intake_bin <- sort(unique(negative_intake2$Bin))
  Insentec_warning$negative_intake_bin[i] <- paste(unlist(negative_intake_bin), collapse="; ")
  
  ############################## delete negative duration and negative intake #####################
  all.fed2[[i]] <- all.fed2[[i]][which(all.fed2[[i]]$Duration >= 0 & all.fed2[[i]]$Intake >= 0),]
  all.wat2[[i]] <- all.wat2[[i]][which(all.wat2[[i]]$Duration >= 0 & all.wat2[[i]]$Intake >= 0),]
  all.comb2[[i]] <- all.comb2[[i]][which(all.comb2[[i]]$Duration >= 0 & all.comb2[[i]]$Intake >= 0),]
  master_feeding2 <- master_feeding2[which(master_feeding2$Duration >= 0 & master_feeding2$Intake >= 0),]
  master_drinking2 <- master_drinking2[which(master_drinking2$Duration >= 0 & master_drinking2$Intake >= 0),]
  master_comb2 <- master_comb2[which(master_comb2$Duration >= 0 & master_comb2$Intake >= 0),]
  
  ################################ Change negative start/end weight to be 0 #####################
  all.fed2[[i]][which(all.fed2[[i]]$Startweight < 0), c("Startweight")] <- 0
  all.fed2[[i]][which(all.fed2[[i]]$Endweight < 0), c("Endweight")] <- 0
  all.wat2[[i]][which(all.wat2[[i]]$Startweight < 0), c("Startweight")] <- 0
  all.wat2[[i]][which(all.wat2[[i]]$Endweight < 0), c("Endweight")] <- 0
  all.comb2[[i]][which(all.comb2[[i]]$Startweight < 0), c("Startweight")] <- 0
  all.comb2[[i]][which(all.comb2[[i]]$Endweight < 0), c("Endweight")] <- 0
  master_feeding2[which(master_feeding2$Startweight < 0), c("Startweight")] <- 0
  master_feeding2[which(master_feeding2$Endweight < 0), c("Endweight")] <- 0
  master_drinking2[which(master_drinking2$Startweight < 0), c("Startweight")] <- 0
  master_drinking2[which(master_drinking2$Endweight < 0), c("Endweight")] <- 0
  master_comb2[which(master_comb2$Startweight < 0), c("Startweight")] <- 0
  master_comb2[which(master_comb2$Endweight < 0), c("Endweight")] <- 0
  
  
  # add a new column for feeding and drinking rate 
  all.fed2[[i]]$rate <- all.fed2[[i]]$Intake/all.fed2[[i]]$Duration
  all.wat2[[i]]$rate <- all.wat2[[i]]$Intake/all.wat2[[i]]$Duration
  all.fed2[[i]][all.fed2[[i]] == Inf] <- 0  # when duration = 0, rate is Inf, fix that
  all.wat2[[i]][all.wat2[[i]] == Inf] <- 0
}

############################ Error 8: Large feed intake in one bout ###############################
for(i in 1:length(all.fed))
{
  dat=all.fed[[i]] # 1 day
  large_intake<- dat[which(dat$Intake > 8),] # intake > 8 kg are considered large intake in one bout
  cur_index_l_intake <- length(large_feed_intake_in_one_bout)+1
  large_feed_intake_in_one_bout[[cur_index_l_intake]] <- large_intake
  names(large_feed_intake_in_one_bout)[cur_index_l_intake] <- names(all.fed)[i]
  large_intake_bin <- sort(unique(large_intake$Bin))
  Insentec_warning$large_one_bout_feed_intake_bin[i] <- paste(unlist(large_intake_bin), collapse="; ")
}


############################ Error 9: Large water intake in one bout ###############################
for(i in 1:length(all.wat))
{
  dat=all.wat[[i]] # 1 day
  large_wat_intake<- dat[which(dat$Intake > 30),] # intake > 30L are considered large intake in one bout
  cur_index_w_intake <- length(large_water_intake_in_one_bout)+1
  large_water_intake_in_one_bout[[cur_index_w_intake]] <- large_wat_intake
  names(large_water_intake_in_one_bout)[cur_index_w_intake] <- names(all.wat)[i]
  large_wat_intake_bin <- sort(unique(large_wat_intake$Bin))
  Insentec_warning$large_one_bout_water_intake_bin[i] <- paste(unlist(large_wat_intake_bin), collapse="; ")
}


############################ Error 10: Large feed intake in short time ############################
for(i in 1:length(all.fed2))
{
  dat=all.fed2[[i]] # 1 day
  short_time_f<- dat[which(dat$Intake > 5 & dat$rate > 0.008),] # I run a boxplot to see outliers of feeding rate for 2 days, and the upper boundary was 0.008
  short_time_f_index <- length(large_feed_intake_in_short_time)+1
  large_feed_intake_in_short_time[[short_time_f_index]] <- short_time_f
  names(large_feed_intake_in_short_time)[short_time_f_index] <- names(all.fed2)[i]
  short_time_f_bin <- sort(unique(short_time_f$Bin))
  Insentec_warning$large_feed_intake_in_short_time_bin[i] <- paste(unlist(short_time_f_bin), collapse="; ")
}



########################## Error 11: Large water intake in short time #############################
for(i in 1:length(all.wat2))
{
  dat=all.wat2[[i]] # 1 day
  short_time_w<- dat[which(dat$Intake > 10 & dat$rate > 0.35),] # I run a boxplot to see outliers of drinking rate for 2 days, and the upper boundary was 0.347
  short_time_w_index <- length(large_water_intake_in_short_time)+1
  large_water_intake_in_short_time[[short_time_w_index]] <- short_time_w
  names(large_water_intake_in_short_time)[short_time_w_index] <- names(all.wat2)[i]
  short_time_w_bin <- sort(unique(short_time_w$Bin))
  Insentec_warning$large_water_intake_in_short_time_bin[i] <- paste(unlist(short_time_w_bin), collapse="; ")
}

############################## Error 12: Cow no show after 6pm ####################################
############################## Error 13: Cow no show after 12pm ###################################
############################ Error 14: bins no visits after 6pm ###################################
############################ Error 15: bins no visits after 12pm ###################################
for (i in 1:length(all.comb)){
  ####### Cows no show after 6pm and 12pm 
  cur_day <- all.comb[[i]]
  cur_day <- cur_day[order(cur_day$Cow, cur_day$End),] # sort it out by cow number and end time
  cur_day$last_seen <- 0
  cur_day$last_seen[nrow(cur_day)] <- 1
  # go through every row to find the last time a cow was seen 
  for (k in 1:(nrow(cur_day)-1)) {
    if (cur_day$Cow[k] != cur_day$Cow[k+1]){cur_day$last_seen[k] <- 1}
  }
  last_seen_table <- cur_day[which(cur_day$last_seen == 1),]
  last_seen_table2 <- last_seen_table[,c(2, 5)]
  after6pm<- ymd_hms(paste(names(all.comb)[i], "17:59:59"), tz="America/Los_Angeles")
  after12pm<- ymd_hms(paste(names(all.comb)[i], "11:59:59"), tz="America/Los_Angeles")
  
  # get a table of cows not show up after  6pm
  no_cow_after6 <- last_seen_table2[which(last_seen_table2$End < after6pm),]
  no_cow_after6$comb_string <- paste(no_cow_after6$Cow, as.character(no_cow_after6$End), sep = ", ")
  after6_warning <- sort(unique(no_cow_after6$comb_string))
  Insentec_warning$no_show_after_6pm_cows[i] <- paste(unlist(after6_warning), collapse="; ")
  
  # get a table of cows not show up after  12pm
  no_cow_after12 <- last_seen_table2[which(last_seen_table2$End < after12pm),]
  no_cow_after12$comb_string <- paste(no_cow_after12$Cow, as.character(no_cow_after12$End), sep = ", ")
  after12_warning <- sort(unique(no_cow_after12$comb_string))
  Insentec_warning$no_show_after_12pm_cows[i] <- paste(unlist(after12_warning), collapse="; ")
  
  
  
  ####### Bins no visits after 6pm and 12pm 
  cur_day_visit <- all.comb[[i]]
  cur_day_visit <- cur_day_visit[order(cur_day_visit$Bin, cur_day_visit$End),]
  cur_day_visit$last_seen <- 0
  cur_day_visit$last_seen[nrow(cur_day_visit)] <- 1
  # go through every row to find the last time a Bin was seen 
  for (k in 1:(nrow(cur_day_visit)-1)) {
    if (cur_day_visit$Bin[k] != cur_day_visit$Bin[k+1]){cur_day_visit$last_seen[k] <- 1}
  }
  last_seen_table <- cur_day_visit[which(cur_day_visit$last_seen == 1),]
  last_seen_table2 <- last_seen_table[,c(3, 5)]
  
  # get a table of Bins visits after  6pm
  no_Bin_after6 <- last_seen_table2[which(last_seen_table2$End < after6pm),]
  no_Bin_after6$comb_string <- paste(no_Bin_after6$Bin, as.character(no_Bin_after6$End), sep = ", ")
  after6_warning <- sort(unique(no_Bin_after6$comb_string))
  Insentec_warning$no_visit_after_6pm_bins[i] <- paste(unlist(after6_warning), collapse="; ")
  
  # get a table of Bins no visits after  12pm
  no_Bin_after12 <- last_seen_table2[which(last_seen_table2$End < after12pm),]
  no_Bin_after12$comb_string <- paste(no_Bin_after12$Bin, as.character(no_Bin_after12$End), sep = ", ")
  after12_warning <- sort(unique(no_Bin_after12$comb_string))
  Insentec_warning$no_visit_after_12pm_bins[i] <- paste(unlist(after12_warning), collapse="; ")
}



################################# Number of visits to each bin ####################################
######################### Number of visits to each bin for each cow ###############################
############################# Number of bins visited by each cow ##################################
############################### Error 16: Bins not visited today ##################################
############################# Error 17: Bins with low visits today ################################
############################# Error 18: cows didn't visit feed bins ###############################
############################ Error 19: cows didn't visit water bins ###############################
# create a table with all the bin numbers
feed_bin <- seq(1, 30, by = 1)
wat_bin <- seq(101, 105, by = 1)
total_bin <- append(feed_bin, wat_bin)
bin_list <- data.frame(total_bin)
colnames(bin_list) <- c("Bin")


for (i in 1:length(all.comb2)) {
  # number of visits to each bin on each day
  visit_each_bin <- count(all.comb2[[i]], vars=c("Bin"))
  colnames(visit_each_bin) <- c("Bin", "Visit_freq")
  visit_each_bin2 <- merge(bin_list, visit_each_bin, all = TRUE)
  visit_each_bin2[is.na(visit_each_bin2)] <- 0
  # add to the Insentec list
  bin_visit_index <- length(bins_visit_num)+1
  bins_visit_num[[bin_visit_index]] <- visit_each_bin2
  names(bins_visit_num)[bin_visit_index] <- names(all.comb2)[i]
  
  print("line998")
  
  # number of visits for each cow on each bin on each day
  cow_bin_visit <- count(all.comb2[[i]], vars=c("Cow","Bin"))
  colnames(cow_bin_visit) <- c("Cow" ,"Bin", "Visit_freq")
  # add to the Insentec list
  bin_cow_visit_index <- length(visit_per_bin_per_cow)+1
  visit_per_bin_per_cow[[bin_cow_visit_index]] <- cow_bin_visit
  names(visit_per_bin_per_cow)[bin_cow_visit_index] <- names(all.comb2)[i]
  
  
  # number of feed & water bins a cow visited each day
  cow_bin_visit_fed <- cow_bin_visit[which(cow_bin_visit$Bin < 100),]
  cow_bin_visit_wat <- cow_bin_visit[which(cow_bin_visit$Bin > 100),]
  num_bin_per_cow_fed <- count(cow_bin_visit_fed, vars=c("Cow"))
  colnames(num_bin_per_cow_fed) <- c("Cow", "num_of_feed_bins_visited")
  num_bin_per_cow_wat <- count(cow_bin_visit_wat, vars=c("Cow"))
  colnames(num_bin_per_cow_wat) <- c("Cow", "num_of_water_bins_visited")
  num_bin_per_cow_comb <- merge(num_bin_per_cow_fed, num_bin_per_cow_wat, all = TRUE)
  num_bin_per_cow_comb[is.na(num_bin_per_cow_comb)] <- 0
  num_bin_per_cow_comb$total_num_of_bins_visit <- num_bin_per_cow_comb$num_of_feed_bins_visited + num_bin_per_cow_comb$num_of_water_bins_visited
  # add to the Insentec list
  bin_each_cow_index <- length(bin_num_visit_per_cow)+1
  bin_num_visit_per_cow[[bin_each_cow_index]] <- num_bin_per_cow_comb
  names(bin_num_visit_per_cow)[bin_each_cow_index] <- names(all.comb2)[i]
  
  
  # Cows that did not visit water bin / feed bin
  # Missing cow, cow ont showing up neither at water nor feed bin
  if (nrow(num_bin_per_cow_comb) < 48) {
    Insentec_warning$missing_cow[i] <- "Yes"
  }
  # cows that no show at feed bin
  fed_no_show <- num_bin_per_cow_comb[which(num_bin_per_cow_comb$num_of_feed_bins_visited == 0),]
  fed_no_show_cow <- sort(unique(fed_no_show$Cow))
  Insentec_warning$cows_no_visit_to_feed_bin[i] <- paste(unlist(fed_no_show_cow), collapse="; ")
  # cows that no show at water bin
  wat_no_show <- num_bin_per_cow_comb[which(num_bin_per_cow_comb$num_of_water_bins_visited == 0),]
  wat_no_show_cow <- sort(unique(wat_no_show$Cow))
  Insentec_warning$cows_no_visit_to_water_bin[i] <- paste(unlist(wat_no_show_cow), collapse="; ")
  
  
  # bins not visited on each day
  no_visit <- visit_each_bin2[which(visit_each_bin2$Visit_freq == 0),]
  no_visit_bin <- sort(unique(no_visit$Bin))
  Insentec_warning$bins_not_visited_today[i] <- paste(unlist(no_visit_bin), collapse="; ")
  
  
  # feed bins with low visits on each day
  visit_each_bin3 <- visit_each_bin2[which(visit_each_bin2$Bin < 100),]
  low_visit <- visit_each_bin3[which(visit_each_bin3$Visit_freq < 10),]
  low_visit_bin <- sort(unique(low_visit$Bin))
  Insentec_warning$feed_bins_with_low_visits_today[i] <- paste(unlist(low_visit_bin), collapse="; ")
}


############################## Pinpoint the time when feed was added ##############################
time_interval_after_feed_added$morning_feed_add_start <- ymd_hms("2021-01-01 01:00:00", tz="America/Los_Angeles")
time_interval_after_feed_added$morning_90min_after_feed <- ymd_hms("2021-01-01 01:00:00", tz="America/Los_Angeles")
time_interval_after_feed_added$morning_2h_after_feed <- ymd_hms("2021-01-01 01:00:00", tz="America/Los_Angeles")
time_interval_after_feed_added$morning_3h_after_feed <- ymd_hms("2021-01-01 01:00:00", tz="America/Los_Angeles")
time_interval_after_feed_added$afternoon_feed_add_start <- ymd_hms("2021-01-01 01:00:00", tz="America/Los_Angeles")
time_interval_after_feed_added$afternoon_90min_after_feed <- ymd_hms("2021-01-01 01:00:00", tz="America/Los_Angeles")
time_interval_after_feed_added$afternoon_2h_after_feed <- ymd_hms("2021-01-01 01:00:00", tz="America/Los_Angeles")
time_interval_after_feed_added$afternoon_3h_after_feed <- ymd_hms("2021-01-01 01:00:00", tz="America/Los_Angeles")
time_interval_after_feed_added$morning_feed_delivery_no_found <- ""
time_interval_after_feed_added$afternoon_feed_delivery_no_found <- ""
# special delivery at noon
time_interval_after_feed_added$noon_feed_add_start <- ymd_hms("2021-01-01 01:00:00", tz="America/Los_Angeles")
time_interval_after_feed_added$noon_90min_after_feed <- ymd_hms("2021-01-01 01:00:00", tz="America/Los_Angeles")
time_interval_after_feed_added$noon_2h_after_feed <- ymd_hms("2021-01-01 01:00:00", tz="America/Los_Angeles")
time_interval_after_feed_added$noon_3h_after_feed <- ymd_hms("2021-01-01 01:00:00", tz="America/Los_Angeles")
time_interval_after_feed_added$noon_feed_delivery_found <- ""

for (i in 1:length(all.fed2)){
  cur_date <- as.character(date(all.fed2[[i]]$Start[1]))
  morning_feed_time <- ymd_hms(paste(cur_date, "00:01:01"), tz="America/Los_Angeles")
  afternoon_feed_time <- ymd_hms(paste(cur_date, "00:01:01"), tz="America/Los_Angeles")
  noon_feed_time <- ymd_hms(paste(cur_date, "00:01:01"), tz="America/Los_Angeles")
  
  # go through every day 
  cur_sheet <- all.fed2[[i]]
  cur_sheet <- cur_sheet[order(cur_sheet$Bin, cur_sheet$Start),] # sort by bin number and start time
  cur_sheet$feed_added <- 0
  
  # go through every row in the current sheet
  for (k in 2:nrow(cur_sheet)) {
    # if the bin number didn't change, but feed in the bin increased by more than 10 kg, then this is when feed get added
    if ((cur_sheet$Bin[k] == cur_sheet$Bin[k-1]) & ((cur_sheet$Startweight[k] - cur_sheet$Endweight[k-1]) > 10)) {
      cur_sheet$feed_added[k] <- 1
    }
  }
  
  feed_add_time <- cur_sheet[which(cur_sheet$feed_added == 1),]
  # create 2 empty default datasheet
  feed_add_morning <- feed_add_time[-c(1:nrow(feed_add_time)), ]
  feed_add_afternoon <- feed_add_time[-c(1:nrow(feed_add_time)), ]
  feed_add_morning <- feed_add_time[which(feed_add_time$Start < ymd_hms(paste(names(all.fed2)[i], "11:00:00"), tz="America/Los_Angeles")), ] 
  feed_add_morning <- feed_add_morning[order(feed_add_morning$Start),]
  # if there are feed add time in the afternoon after 11AM
  if (nrow(feed_add_time[which(feed_add_time$Start >= ymd_hms(paste(names(all.fed2)[i], "11:00:00"), tz="America/Los_Angeles")), ]) > 0 ) {
    # sometimes feed was added at noon. Handle those cases
    special_feed_add <- feed_add_time[which((feed_add_time$Start >= ymd_hms(paste(names(all.fed2)[i], "11:00:00" ), tz="America/Los_Angeles")) & (feed_add_time$Start <= ymd_hms(paste(names(all.fed2)[i], "14:00:00" ), tz="America/Los_Angeles"))), ]
    feed_add_afternoon <- feed_add_time[which(feed_add_time$Start > ymd_hms(paste(names(all.fed2)[i], "14:00:00"), tz="America/Los_Angeles")), ]
    feed_add_afternoon <- feed_add_afternoon[order(feed_add_afternoon$Start),]
    special_feed_add <- special_feed_add[order(special_feed_add$Start),]
  }
  
  #pick the earliest time when feed was added as the start time
  # morning feed add
  if (nrow(feed_add_morning) > 0) {
    morning_feed_time <- feed_add_morning$Start[1]
  } else {
    time_interval_after_feed_added$morning_feed_delivery_no_found[i] <- "YES"
    Insentec_warning$feed_add_time_no_found[i] <- "YES"
    
  }
  
  # afternoon feed add
  if (nrow(feed_add_afternoon) > 0) { # <1> there are feed added after 2pm
    afternoon_feed_time <- feed_add_afternoon$Start[1]
  }  else { # <2> no feed added at all after 11am. 
    time_interval_after_feed_added$afternoon_feed_delivery_no_found[i] <- "YES"
    Insentec_warning$feed_add_time_no_found[i] <- "YES"
    
  }
  
  # noon feed add
  if (nrow(special_feed_add) > 5) { # if there are more than 5 feed bin had more than 10kg of feed added at noon
    noon_feed_time <- special_feed_add$Start[1]
    time_interval_after_feed_added$noon_feed_delivery_found[i] <- "YES"
  } 
  
  # 90 minutes after feed was added
  ninty_morning_end <- morning_feed_time + hours(1) + minutes(30)
  ninty_afternoon_end <- afternoon_feed_time + hours(1) + minutes(30)
  ninty_noon_end <- noon_feed_time + hours(1) + minutes(30)
  
  # 2h after feed was added
  two_hour_morning_end <- morning_feed_time + hours(2)
  two_hour_afternoon_end <- afternoon_feed_time + hours(2)
  two_hour_noon_end <- noon_feed_time + hours(2)
  
  # 3 hours after feed was added
  three_hour_morning_end <- morning_feed_time + hours(3)
  three_hour_afternoon_end <- afternoon_feed_time + hours(3)
  three_hour_noon_end <- noon_feed_time + hours(3)
  
  # add everything to the datasheet
  time_interval_after_feed_added$morning_feed_add_start[i] <- morning_feed_time
  time_interval_after_feed_added$morning_90min_after_feed[i] <- ninty_morning_end
  time_interval_after_feed_added$morning_2h_after_feed[i] <- two_hour_morning_end
  time_interval_after_feed_added$morning_3h_after_feed[i] <- three_hour_morning_end
  time_interval_after_feed_added$afternoon_feed_add_start[i] <- afternoon_feed_time
  time_interval_after_feed_added$afternoon_90min_after_feed[i] <- ninty_afternoon_end
  time_interval_after_feed_added$afternoon_2h_after_feed[i] <- two_hour_afternoon_end
  time_interval_after_feed_added$afternoon_3h_after_feed[i] <- three_hour_afternoon_end
  time_interval_after_feed_added$noon_feed_add_start[i] <- noon_feed_time
  time_interval_after_feed_added$noon_90min_after_feed[i] <- ninty_noon_end
  time_interval_after_feed_added$noon_2h_after_feed[i] <- two_hour_noon_end
  time_interval_after_feed_added$noon_3h_after_feed[i] <- three_hour_noon_end
  
}

time_interval_after_feed_added$date <- ymd(time_interval_after_feed_added$date, tz="America/Los_Angeles")
# if feed delivery time no found, make it NA
time_interval_after_feed_added[which(time_interval_after_feed_added$morning_feed_delivery_no_found == "YES"), c("morning_feed_add_start", "morning_90min_after_feed", "morning_2h_after_feed", "morning_3h_after_feed")] <- NA
time_interval_after_feed_added[which(time_interval_after_feed_added$afternoon_feed_delivery_no_found == "YES"), c("afternoon_feed_add_start", "afternoon_90min_after_feed", "afternoon_2h_after_feed", "afternoon_3h_after_feed")] <- NA
time_interval_after_feed_added[which(time_interval_after_feed_added$noon_feed_delivery_found == ""), c("noon_feed_add_start", "noon_90min_after_feed", "noon_2h_after_feed", "noon_3h_after_feed")] <- NA


##################### Handle double detections and lying-eating conflicts #########################
# Logic: the idea behind this double detection handling is that we always keep the second bout in 
# double detections intact. We mark the end time of the first bout to the same as the start time of
# the second bout


# Handle lying and eating/drinking conflicts
lying_data <- Social_character_project[["HOBO"]][["lying_data_for_feeding_conflicts"]]
colnames(lying_data) <- c("Cow", "Start", "End", "Duration")
# transform the lying data to have the same format as the feeding and drinking data
# Here we register all the lying records to be having transponder, bin, startweight, endweight, and intake as NA
# in order to differentiate between eating, drinking and lying
lying_data$Transponder <- NA
lying_data$Bin <- NA
lying_data$Startweight <- NA
lying_data$Endweight <- NA
lying_data$Intake <- NA
lying_data <- lying_data[, c(5, 1, 6, 2, 3, 4, 7, 8, 9)]  # reorder the column names
lying_data$date <- date(lying_data$Start)
for(i in 1:length(all.comb2))
{
  cur_date <- names(all.comb2)[i]
  # get the lying data for current date
  cur_lying_data <- lying_data[which(lying_data$date == cur_date),]
  # transform the lying data to have the same format as the feeding and drinking data
  cur_lying_data$date <- NULL
  
  all.comb2[[i]] <- rbind(cur_lying_data, all.comb2[[i]])
  all.comb2[[i]] <- all.comb2[[i]][order(all.comb2[[i]]$Cow, all.comb2[[i]]$Start, all.comb2[[i]]$End),]
  for (k in 2: nrow(all.comb2[[i]])) {
    if ((all.comb2[[i]]$Cow[k] == all.comb2[[i]]$Cow[k-1]) & (all.comb2[[i]]$Start[k] < all.comb2[[i]]$End[k-1])) {
      all.comb2[[i]]$End[k-1] <- all.comb2[[i]]$Start[k] - seconds(1)
    }
  }
  
  # remove all the lying data
  all.comb2[[i]] <- na.omit(all.comb2[[i]])
}



# Handle double detection for when the same cow show up at different bins
for(i in 1:length(all.comb2)){
  # sort by cow, start and end time first
  all.comb2[[i]] <- all.comb2[[i]][order(all.comb2[[i]]$Cow, all.comb2[[i]]$Start, all.comb2[[i]]$End),]
  # iterate through the current day
  for (k in 2:nrow(all.comb2[[i]])) {
    # check when it's the same cow, if the start time of the next bout started earlier than the end time of previous bout
    if (( all.comb2[[i]]$Cow[k] == all.comb2[[i]]$Cow[k-1])& (all.comb2[[i]]$Start[k] < all.comb2[[i]]$End[k-1])) {
      all.comb2[[i]]$End[k-1] <- all.comb2[[i]]$Start[k] - seconds(1) # only change the end time of the first bout
    }
  }
}


# Handle double detection for when the same bin registers 2 different cows
for(i in 1:length(all.comb2)){
  # sort by bin, start and end time first
  all.comb2[[i]] <- all.comb2[[i]][order(all.comb2[[i]]$Bin, all.comb2[[i]]$Start, all.comb2[[i]]$End),]
  # iterate through the current day
  for (k in 2:nrow(all.comb2[[i]])) {
    # check when it's the same bin, if the start time of the next bout started earlier than the end time of previous bout
    if (( all.comb2[[i]]$Bin[k] == all.comb2[[i]]$Bin[k-1])& (all.comb2[[i]]$Start[k] < all.comb2[[i]]$End[k-1])) {
      all.comb2[[i]]$End[k-1] <- all.comb2[[i]]$Start[k] - seconds(1) # only change the end time of the first bout
    }
  }
}


# update the duration in all.comb2
for(i in 1:length(all.comb2)){
  all.comb2[[i]]$Duration <- all.comb2[[i]]$Start %--% all.comb2[[i]]$End
  all.comb2[[i]]$Duration <- seconds(as.duration(all.comb2[[i]]$Duration))
  all.comb2[[i]]$Duration <- trimws(as.character(all.comb2[[i]]$Duration), which = "both")
  all.comb2[[i]]$Duration <- substr(all.comb2[[i]]$Duration, 1, (nchar(all.comb2[[i]]$Duration)-1))
  all.comb2[[i]]$Duration <- as.numeric(all.comb2[[i]]$Duration)
}

# delete those with negative durations after cleaning
for (i in 1:length(all.comb2)) {
  all.comb2[[i]] <- all.comb2[[i]][which(all.comb2[[i]]$Duration > 0),]
}



#after all the double detections are cleaned up, update the changes to all.fed and all.wat
for (i in 1:length(all.comb2)) {
  cur_date <- names(all.comb2)[i]
  all.fed2[[cur_date]] <- all.comb2[[i]][which(all.comb2[[i]]$Bin < 100),]
  all.wat2[[cur_date]] <- all.comb2[[i]][which(all.comb2[[i]]$Bin > 100),]
}


# update gaint master datasheets
master_feeding3 <- all.fed2[[1]][-c(1:nrow(all.fed2[[1]])), ]
master_drinking3 <- all.wat2[[1]][-c(1:nrow(all.wat2[[1]])), ]
master_comb3 <- all.comb2[[1]][-c(1:nrow(all.comb2[[1]])), ]
for (e in 1:length(all.comb2)){
  master_feeding3 <- rbind(master_feeding3, all.fed2[[e]])
  master_drinking3 <- rbind(master_drinking3, all.wat2[[e]])
  master_comb3 <- rbind(master_comb3, all.comb2[[e]])
}
# add date
master_feeding3$date <- date(master_feeding3$Start)
master_drinking3$date <- date(master_drinking3$Start)
master_comb3$date <- date(master_comb3$Start)




###################################################################################################
################################ Feed & water summary #############################################
###################################################################################################
#### Feed intake, feeding time, number of visits ####
feeding_intake <- aggregate(master_feeding3[, "Intake"], list(master_feeding3$date, master_feeding3$Cow), sum)
colnames(feeding_intake)=c("date", "Cow","Feeding_Intake(kg)")
feeding_duration <- aggregate(master_feeding3[, "Duration"], list(master_feeding3$date, master_feeding3$Cow), sum)
colnames(feeding_duration)=c("date", "Cow","Feeding_Duration(s)")
feeding_visits <- count(master_feeding3, vars=c("date", "Cow"))
colnames(feeding_visits)=c("date", "Cow","Feeding_Visits")

#### Water intake, drinking time, number of visits ####
drinking_intake <- aggregate(master_drinking3[, "Intake"], list(master_drinking3$date, master_drinking3$Cow), sum)
colnames(drinking_intake)=c("date", "Cow","Drinking_Intake(kg)")
drinking_duration <- aggregate(master_drinking3[, "Duration"], list(master_drinking3$date, master_drinking3$Cow), sum)
colnames(drinking_duration)=c("date", "Cow","Drinking_Duration(s)")
drinking_visits <- count(master_drinking3, vars=c("date", "Cow"))
colnames(drinking_visits)=c("date", "Cow","Drinking_Visits")


################################## Error 20: Low feed intake ######################################
low_fed <- feeding_intake[which(feeding_intake$`Feeding_Intake(kg)` < 35), ]
if (nrow(low_fed) > 0) {
  low_fed$comb_str <- paste("Cow ", low_fed$Cow, ", ",low_fed$`Feeding_Intake(kg)`, "kg", sep = "")
  for (i in 1:nrow(Insentec_warning)) {
    cur_date <- Insentec_warning$date[i]
    cur_day_low_fed <- low_fed[which(low_fed$date == cur_date),]
    cur_day_low_fed_cow <- sort(unique(cur_day_low_fed$comb_str))
    Insentec_warning$low_daily_feed_intake_cows[i] <- paste(unlist(cur_day_low_fed_cow), collapse="; ")
  }
}


################################## Error 21: high feed intake #####################################
high_fed <- feeding_intake[which(feeding_intake$`Feeding_Intake(kg)` > 75), ]
if (nrow(high_fed) > 0) {
  high_fed$comb_str <- paste("Cow ", high_fed$Cow, ", ",high_fed$`Feeding_Intake(kg)`, "kg", sep = "")
  for (i in 1:nrow(Insentec_warning)) {
    cur_date <- Insentec_warning$date[i]
    cur_day_high_fed <- high_fed[which(high_fed$date == cur_date),]
    cur_day_high_fed_cow <- sort(unique(cur_day_high_fed$comb_str))
    Insentec_warning$high_daily_feed_intake_cows[i] <- paste(unlist(cur_day_high_fed_cow), collapse="; ")
  }
}


################################## Error 22: Low water intake #####################################
low_wat <- drinking_intake[which(drinking_intake$`Drinking_Intake(kg)` < 60),]
if (nrow(low_wat) > 0) {
  low_wat$comb_str <- paste("Cow ", low_wat$Cow, ", ",low_wat$`Drinking_Intake(kg)`, "kg", sep = "")
  for (i in 1:nrow(Insentec_warning)) {
    cur_date <- Insentec_warning$date[i]
    cur_day_low_wat <- low_wat[which(low_wat$date == cur_date),]
    cur_day_low_wat_cow <- sort(unique(cur_day_low_wat$comb_str))
    Insentec_warning$low_daily_water_intake_cows[i] <- paste(unlist(cur_day_low_wat_cow), collapse="; ")
  }
}

################################## Error 23: high water intake ####################################
high_wat <- drinking_intake[which(drinking_intake$`Drinking_Intake(kg)` > 180),]
if (nrow(high_wat) > 0) {
  high_wat$comb_str <- paste("Cow ", high_wat$Cow, ", ",high_wat$`Drinking_Intake(kg)`, "kg", sep = "")
  for (i in 1:nrow(Insentec_warning)) {
    cur_date <- Insentec_warning$date[i]
    cur_day_high_wat <- high_wat[which(high_wat$date == cur_date),]
    cur_day_high_wat_cow <- sort(unique(cur_day_high_wat$comb_str))
    Insentec_warning$high_daily_water_intake_cows[i] <- paste(unlist(cur_day_high_wat_cow), collapse="; ")
  }
}


###################################### Non-Nutritive Visits #######################################
# Non-nutritive visits refer to those when a cow visited a bin, but didn't eat anything, although
# there are feed left (feed > 0). If a cow visited a bin, but there is no feed left (feed <=0), then this is not non-nutritive visits
############################ visited but no feed record & frequency ###############################
# visit_but_no_feed refers to when a cow visited the bin, intake = 0 because there is no feed left
visited_but_no_feed_record <- list()
visited_but_no_feed_freq <- list()
non_nutritive_visits <- list()
for(i in 1:length(all.fed2)) {
  cur_date <- names(all.fed2)[i]
  cur_list <- all.fed2[[i]]
  
  # non-nutritive visits: Times when a cow made a visit to the bin, saw that there was more than 0.5kg of feed left,
  #and still didn't eat anything'
  sep_sheet <- cur_list[which((cur_list$Intake <= 0.5) & (cur_list$Startweight > 0.5)),]
  non_nutritive_sheet <- count(sep_sheet, vars=c("Cow"))
  colnames(non_nutritive_sheet) <- c("Cow", "number_of_non_nutritive_visits")
  cur_index <- length(non_nutritive_visits) + 1
  non_nutritive_visits[[cur_index]] <- non_nutritive_sheet
  names(non_nutritive_visits)[cur_index] <- as.character(cur_date)
  
  # visited but no feed frequency: Times when a cow made a visit to the bin, but there was less than 0.5kg of 
  #feed left, and the cow didn't eat anything
  no_feed <- cur_list[which((cur_list$Intake <= 0.5) & (cur_list$Startweight <= 0.5)),]
  non_feed_sheet <- count(no_feed, vars=c("Cow"))
  colnames(non_feed_sheet) <- c("Cow", "number_of_visits_when_no_feed")
  cur_index <- length(visited_but_no_feed_freq) + 1
  visited_but_no_feed_freq[[cur_index]] <- non_feed_sheet
  names(visited_but_no_feed_freq)[cur_index] <- as.character(cur_date)
  
  # visited but no feed records
  cur_index <- length(visited_but_no_feed_record) + 1
  visited_but_no_feed_record[[cur_index]] <- no_feed
  names(visited_but_no_feed_record)[cur_index] <- as.character(cur_date)
}


######################################## culled cow list ##########################################
cow_list <- c(2074, 4038, 5028, 5135, 6030, 6121, 7019)
lactation_num <- c(6, 4, 4, 4, 3, 2, 2)
culled_cow <- data.frame(cow_list, lactation_num)
colnames(culled_cow) <- c("Cow", "number_of_lactation")



### Final Output ###
Insentec_final_summary <- join_all(list(feeding_intake, feeding_duration, feeding_visits, drinking_intake, drinking_duration, drinking_visits), by = c("date", "Cow"))
Insentec_final_summary <- Insentec_final_summary[order(Insentec_final_summary$date, Insentec_final_summary$Cow),]
Insentec_final_summary[is.na(Insentec_final_summary)] <- 0 # remove NA with 0
# pop out a massage window to tell which bin needs to be cleaned.
m1 <- paste("Outlier percentage for feeding is: ", outlier_fed_percentage, ". Outlier percentage for drinking is: ", outlier_wat_percentage, ".")
#winDialog(type = "ok", m1)
print(m1)



###################################################################################################
###                                                                                             ###
### Chapter 3: Replacement behaiours and other social characteristics                           ###
### Description: This code runs all dates on trial. It uses Insentec data to analyze social     ###
###              behaviours including feeding replacement, feeding and drinking synchrony.      ###
###                                                                                             ###
###################################################################################################


###################################################################################################
############################ Feeding Synchrony Matrix Preparation #################################
###################################################################################################
master_feeding3 <- master_feeding3[order(master_feeding3$Start, master_feeding3$End),]
# mark down the earliest start time and latest end time for the entire master sheet
total_start <- min(master_feeding3$Start)
total_end <- max(master_feeding3$End)
dateTime_seq <- seq(total_start, total_end, by = "sec") # get a list of time by seconds


### MATRIX1: empty matrix preperation: CowID X Time for which cow is eating
# create a matrix where x axis contains cow ID, and y axis contains time (every seconds)
cow_list <- sort(unique(master_feeding3$Cow))
cow_num <- length(cow_list)
col_num <- cow_num + 1
row_num <- length(dateTime_seq)
feeding_synch_master_cow <- data.frame(matrix(0, row_num, col_num)) # create a matrix table: CowID X Time
colnames(feeding_synch_master_cow) <- c("Time", cow_list)
feeding_synch_master_cow$Time <- dateTime_seq


### MARTRIX2: empty matrix preperation: Time X CowID for which bin the cow is at
feeding_synch_master_bin <- feeding_synch_master_cow # create a matrix table: time X CowID


### MATRIX3: Time X Bin for how much feed is at each bin at each second
total_bin <- 30
bin_list <- seq(1, total_bin, by = 1)
bin_num = length(bin_list)
col_num <- bin_num + 1
row_num <- length(dateTime_seq)
feeding_synch_master_feed <- data.frame(matrix(NA, row_num, col_num))
colnames(feeding_synch_master_feed) <- c("Time", bin_list)
feeding_synch_master_feed$Time <- dateTime_seq


### Process MATRIX1 (feeding_synch_master_cow): Time X CowID for which cow is eating
### AND MARTRIX2 (feeding_synch_master_bin): Time X CowID for which bin the cow is at
### AND MATRIX3 (feeding_synch_master_feed): Time X Bin for how much feed is at each feed at each second
# go through the master_feeding3 datasheet, mark down a "1" on the time, if the cow is feeding at that second
for (o in 1:nrow(master_feeding3)) {
  cur_cow <- master_feeding3$Cow[o]
  index_cow <- match(cur_cow, cow_list)+1
  cur_start <- master_feeding3$Start[o]
  cur_end <- master_feeding3$End[o]
  cur_dur <- master_feeding3$Duration[o]
  cur_bin <- master_feeding3$Bin[o]
  index_bin <- match(cur_bin, bin_list) + 1
  start_weight <- master_feeding3$Startweight[o]
  end_weight <- master_feeding3$Endweight[o]
  start_row_number <- which(feeding_synch_master_cow$Time == cur_start)
  end_row_number <- which(feeding_synch_master_cow$Time == cur_end)
  weight_list <- round(seq(start_weight, end_weight, length.out = (end_row_number - start_row_number + 1)), digits = 1)
  
  
  # process matrix 1, time X CowID on cow
  feeding_synch_master_cow[(start_row_number:end_row_number) , index_cow] <- 1
  # process matrix 2, time X CowID on bin number
  feeding_synch_master_bin[(start_row_number:end_row_number) , index_cow] <- cur_bin
  # process matrix 3, time X Bin
  feeding_synch_master_feed[(start_row_number:end_row_number), index_bin] <- weight_list
}


# calculate how many cows are present eating at each second
feeding_synch_master_cow$total_cow_num <- rowSums(feeding_synch_master_cow[, 2:ncol(feeding_synch_master_cow)], na.rm = TRUE)
feeding_synch_master_cow$total_bin_occupied <- feeding_synch_master_cow$total_cow_num
feeding_synch_master_cow$empty_bin_num <- total_bin - feeding_synch_master_cow$total_bin_occupied


# delete the time when no cow is eating
records_to_keep <- which(feeding_synch_master_cow$total_cow_num > 0)
feeding_synch_master_cow2 <- feeding_synch_master_cow[records_to_keep, ]
feeding_synch_master_bin2 <- feeding_synch_master_bin[records_to_keep, ]
feeding_synch_master_feed2 <- feeding_synch_master_feed[records_to_keep, ]


# add date
feeding_synch_master_cow2$date <- date(feeding_synch_master_cow2$Time)
feeding_synch_master_bin2$date <- date(feeding_synch_master_bin2$Time)
feeding_synch_master_feed2$date <- date(feeding_synch_master_feed2$Time)


# Process the feeding_synch_master_feed2 sheet to make sure that every second has a feed amount recorded instead of NA
# set the first row of the datasheet to be the start weight of the first eating bout for each bin
for(i in 2:(ncol(feeding_synch_master_feed2)-1)) {
  # the first row do not have a start weight recorded
  if (is.na(feeding_synch_master_feed2[1, i])) {
    # keep going down current column untill you find the startweight of the first bout which is not NA
    for (j in 2:nrow(feeding_synch_master_feed2)) {
      if (!is.na(feeding_synch_master_feed2[j, i])) {
        feeding_synch_master_feed2[1, i] <- feeding_synch_master_feed2[j, i]
        break; # jump out of the for loop 
      }
    }
  }
}


### LONG COMPUTATION TIME WARNING!! The following chunk of loop takes a long time to run!
# took 11 hours to run for 2 weeks of data
# now the first row of each column has a startweight
# go through every column, every row, if there is NA value, copy the last not NA weight value above current row
# iterate through each column
for(i in 2:(ncol(feeding_synch_master_feed2)-1)) {
  # iterate through each row
  for (j in 2:nrow(feeding_synch_master_feed2)) {
    #print(paste(i, j))
    # if current cell has a NA value
    if (is.na(feeding_synch_master_feed2[j, i])) {
      # look backward (upward), to search for a not NA weight value
      #logically we should find our last not NA by looking at just 1 row above the current row
      feeding_synch_master_feed2[j, i] <- feeding_synch_master_feed2[j-1, i]
    }
  }
}
# add a new column calculating the total feed in all bins right now
feeding_synch_master_feed2$totalFeed <- rowSums(feeding_synch_master_feed2[, 2: (total_bin+1)], na.rm = TRUE)


# split the giant master sheet into multiple smaller ones, grouped by date
date_list <- sort(unique(feeding_synch_master_cow2$date))
feeding_synch_master_cow3 <- list()
feeding_synch_master_bin3 <- list()
feeding_synch_master_feed3 <- list()
for (y in 1:length(date_list)) {
  feeding_synch_master_cow3[[y]] <- feeding_synch_master_cow2[which(feeding_synch_master_cow2$date == date_list[y]),]
  feeding_synch_master_bin3[[y]] <- feeding_synch_master_bin2[which(feeding_synch_master_bin2$date == date_list[y]), ]
  feeding_synch_master_feed3[[y]] <- feeding_synch_master_feed2[which(feeding_synch_master_feed2$date == date_list[y]),]
  
  # rename the list name
  names(feeding_synch_master_cow3)[y] <- as.character(date_list[y])
  names(feeding_synch_master_bin3)[y] <- as.character(date_list[y])
  names(feeding_synch_master_feed3)[y] <- as.character(date_list[y])
}




###################################################################################################
######################## Drinking Synchrony Matrix Preparation ####################################
###################################################################################################

# mark down the earliest start time and latest end time for the entire master sheet
start_dateTimes <- master_drinking3$Start
end_dateTimes <- master_drinking3$End
total_start <- min(start_dateTimes)
total_end <- max(end_dateTimes)
dateTime_seq <- seq(total_start, total_end, by = "sec") # get a list of time by seconds


### MATRIX1: Time X CowID for which cow is drinking
# create a matrix where x axis contains cow ID, and y axis contains time (every seconds)
cow_list <- sort(unique(master_drinking3$Cow))
cow_num <- length(cow_list)
col_num <- cow_num + 1
row_num <- length(dateTime_seq)
drinking_synch_master_cow <- data.frame(matrix(0, row_num, col_num)) # create a matrix table: time X CowID
colnames(drinking_synch_master_cow) <- c("Time", cow_list)
drinking_synch_master_cow$Time <- dateTime_seq



# MARTRIX2: Time X CowID for which bin the cow is at
drinking_synch_master_bin <- drinking_synch_master_cow # create a matrix table: time X CowID


# MATRIX3: Time X Bin for how much water is at each bin at each second
total_drink_bin <- 5
bin_list <- seq(101, (101 + total_drink_bin -1), by = 1)
bin_num = length(bin_list)
col_num <- bin_num + 1
row_num <- length(dateTime_seq)
drinking_synch_master_wat<- data.frame(matrix(0, row_num, col_num))
colnames(drinking_synch_master_wat) <- c("Time", bin_list)
drinking_synch_master_wat$Time <- dateTime_seq


### Process MATRIX1 (drinking_synch_master_cow): Time X CowID for which cow is drinking
### AND MARTRIX2 (drinking_synch_master_bin): Time X CowID for which bin the cow is at
### AND MATRIX3 (drinking_synch_master_wat): Time X Bin for how much drink is at each drink at each second
# go through the master_drinking3 datasheet, mark down a "1" on the time, if the cow is drinking at that second
for (o in 1:nrow(master_drinking3)) {
  cur_cow <- master_drinking3$Cow[o]
  index_cow <- match(cur_cow, cow_list)+1
  cur_start <- master_drinking3$Start[o]
  cur_end <- master_drinking3$End[o]
  cur_dur <- master_drinking3$Duration[o]
  cur_bin <- master_drinking3$Bin[o]
  index_bin <- match(cur_bin, bin_list) + 1
  start_weight <- master_drinking3$Startweight[o]
  end_weight <- master_drinking3$Endweight[o]
  weight_list <- round(seq(start_weight, end_weight, length.out = (cur_dur + 1)), digits = 1)
  start_row_number <- which(drinking_synch_master_cow$Time == cur_start)
  end_row_number <- start_row_number + cur_dur
  
  
  # process matrix 1, time X CowID on cow
  drinking_synch_master_cow[(start_row_number:end_row_number) , index_cow] <- 1
  # process matrix 2, time X CowID on bin number
  drinking_synch_master_bin[(start_row_number:end_row_number) , index_cow] <- cur_bin
  # process matrix 3, time X Bin 
  drinking_synch_master_wat[(start_row_number:end_row_number), index_bin] <- weight_list
}


# calculate how many cows are present drinking at each second
drinking_synch_master_cow$total_cow_num <- rowSums(drinking_synch_master_cow[, 2:ncol(drinking_synch_master_cow)], na.rm = TRUE)
drinking_synch_master_cow$total_bin_occupied <- drinking_synch_master_cow$total_cow_num
drinking_synch_master_cow$empty_bin_num <- total_drink_bin - drinking_synch_master_cow$total_bin_occupied


# delete the time when no cow is drinking
records_to_keep <- which(drinking_synch_master_cow$total_cow_num > 0)
drinking_synch_master_cow2 <- drinking_synch_master_cow[records_to_keep, ]
drinking_synch_master_bin2 <- drinking_synch_master_bin[records_to_keep, ]
drinking_synch_master_wat2 <- drinking_synch_master_wat[records_to_keep, ]


# add date
drinking_synch_master_cow2$date <- date(drinking_synch_master_cow2$Time)
drinking_synch_master_bin2$date <- date(drinking_synch_master_bin2$Time)
drinking_synch_master_wat2$date <- date(drinking_synch_master_wat2$Time)


# split the gaint master sheet into multiple smaller ones, grouped by date
date_list <- sort(unique(drinking_synch_master_cow2$date))
drinking_synch_master_cow3 <- list()
drinking_synch_master_bin3 <- list()
drinking_synch_master_wat3 <- list()
for (y in 1:length(date_list)) {
  drinking_synch_master_cow3[[y]] <- drinking_synch_master_cow2[which(drinking_synch_master_cow2$date == date_list[y]),]
  drinking_synch_master_bin3[[y]] <- drinking_synch_master_bin2[which(drinking_synch_master_bin2$date == date_list[y]), ]
  drinking_synch_master_wat3[[y]] <- drinking_synch_master_wat2[which(drinking_synch_master_wat2$date == date_list[y]),]
  
  # rename the list name
  names(drinking_synch_master_cow3)[y] <- as.character(date_list[y])
  names(drinking_synch_master_bin3)[y] <- as.character(date_list[y])
  names(drinking_synch_master_wat3)[y] <- as.character(date_list[y])
  
}


###################################################################################################
############### Feeding & Drinking Combined Synchrony Matrix Preparation ##########################
###################################################################################################
total_bin <- (30 + 5)
master_feeding_drinking3 <- rbind(master_feeding3, master_drinking3)
master_feeding_drinking3 <- master_feeding_drinking3[order(master_feeding_drinking3$Start, master_feeding_drinking3$End),]
# mark down the earliest start time and latest end time for the entire master sheet
total_start <- min(master_feeding_drinking3$Start)
total_end <- max(master_feeding_drinking3$End)
dateTime_seq <- seq(total_start, total_end, by = "sec") # get a list of time by seconds


### MATRIX1: empty matrix preperation: CowID X Time for which cow is eating/drinking
# create a matrix where x axis contains cow ID, and y axis contains time (every seconds)
cow_list <- sort(unique(master_feeding_drinking3$Cow))
cow_num <- length(cow_list)
col_num <- cow_num + 1 
row_num <- length(dateTime_seq)
feed_drink_synch_master_cow <- data.frame(matrix(0, row_num, col_num)) # create a matrix table: CowID X Time
colnames(feed_drink_synch_master_cow) <- c("Time", cow_list)
feed_drink_synch_master_cow$Time <- dateTime_seq


### MARTRIX2: empty matrix preperation: Time X CowID for which bin the cow is at
feed_drink_synch_master_bin <- feed_drink_synch_master_cow # create a matrix table: time X CowID


### Process MATRIX1 (feed_drink_synch_master_cow): Time X CowID for which cow is eating/drinking
### AND MARTRIX2 (feed_drink_synch_master_bin): Time X CowID for which bin the cow is at
# go through the master_feeding_drinking3 datasheet, mark down a "1" on the time, if the cow is feeding at that second
for (o in 1:nrow(master_feeding_drinking3)) {
  cur_cow <- master_feeding_drinking3$Cow[o]
  index_cow <- match(cur_cow, cow_list)+1
  cur_start <- master_feeding_drinking3$Start[o]
  cur_end <- master_feeding_drinking3$End[o]
  cur_dur <- master_feeding_drinking3$Duration[o]
  cur_bin <- master_feeding_drinking3$Bin[o]
  start_row_number <- which(feed_drink_synch_master_cow$Time == cur_start)
  end_row_number <- which(feed_drink_synch_master_cow$Time == cur_end)
  
  # process matrix 1, time X CowID on cow
  feed_drink_synch_master_cow[(start_row_number:end_row_number) , index_cow] <- 1
  # process matrix 2, time X CowID on bin number
  feed_drink_synch_master_bin[(start_row_number:end_row_number) , index_cow] <- cur_bin
}


# calculate how many cows are present eating at each second
feed_drink_synch_master_cow$total_cow_num <- rowSums(feed_drink_synch_master_cow[, 2:ncol(feed_drink_synch_master_cow)], na.rm = TRUE)
feed_drink_synch_master_cow$total_bin_occupied <- feed_drink_synch_master_cow$total_cow_num
feed_drink_synch_master_cow$empty_bin_num <- total_bin - feed_drink_synch_master_cow$total_bin_occupied


# delete the time when no cow is eating
records_to_keep <- which(feed_drink_synch_master_cow$total_cow_num > 0)
feed_drink_synch_master_cow2 <- feed_drink_synch_master_cow[records_to_keep, ]
feed_drink_synch_master_bin2 <- feed_drink_synch_master_bin[records_to_keep, ]

# add date
feed_drink_synch_master_cow2$date <- date(feed_drink_synch_master_cow2$Time)
feed_drink_synch_master_bin2$date <- date(feed_drink_synch_master_bin2$Time)

# change the bin number from original bin number to new bin number.
# this is because feed bins were originaly from 1-30, water bins were from 101-105, 
# but water bins and feed bins were placed within each other. To assess when 
# 2 cows are eating/drinking together, we need to change the numbering of feed and 
# water bins to reflect neighboring using number. 
########################## Feed bin and water bin distribution ####################################
# facing water and feed bins, from left to right
# original bin number: 1   2   3   4   5   6  101 102  7   8   9   10  11  12  13  14  15  16  17  18 103 104  19  20  21  22  23  24  25  26  27  28  29  30 105
# feed(f) or water(w): f   f   f   f   f   f   w   w   f   f   f   f   f   f   f   f   f   f   f   f   w   w   f   f   f   f   f   f   f   f   f   f   f   f   w
# new bin number:     201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235

#update water bin number
feed_drink_synch_master_bin2[feed_drink_synch_master_bin2 == 101] <- 207
feed_drink_synch_master_bin2[feed_drink_synch_master_bin2 == 102] <- 208
feed_drink_synch_master_bin2[feed_drink_synch_master_bin2 == 103] <- 221
feed_drink_synch_master_bin2[feed_drink_synch_master_bin2 == 104] <- 222
feed_drink_synch_master_bin2[feed_drink_synch_master_bin2 == 105] <- 235

#update feed bin number
for (u in 1:30) {
  if (u <= 6) {
    feed_drink_synch_master_bin2[feed_drink_synch_master_bin2 == u] <- (u + 200)
  } else if (u <= 18) {
    feed_drink_synch_master_bin2[feed_drink_synch_master_bin2 == u] <- (u + 202)
  } else {
    feed_drink_synch_master_bin2[feed_drink_synch_master_bin2 == u] <- (u + 204)
  }
}


# split the giant master sheet into multiple smaller ones, grouped by date
date_list <- sort(unique(feed_drink_synch_master_cow2$date))
feed_drink_synch_master_cow3 <- list()
feed_drink_synch_master_bin3 <- list()
for (y in 1:length(date_list)) {
  feed_drink_synch_master_cow3[[y]] <- feed_drink_synch_master_cow2[which(feed_drink_synch_master_cow2$date == date_list[y]),]
  feed_drink_synch_master_bin3[[y]] <- feed_drink_synch_master_bin2[which(feed_drink_synch_master_bin2$date == date_list[y]), ]
  
  # rename the list name
  names(feed_drink_synch_master_cow3)[y] <- as.character(date_list[y])
  names(feed_drink_synch_master_bin3)[y] <- as.character(date_list[y])
}


###################################################################################################
######################## Feeding & Drinking Synchrony analysis ####################################
###################################################################################################
# get a list of all cows' ID, and create a Cow X Cow empty matrix
cow_list <- sort(unique(master_feeding_drinking3$Cow))
cow_num <- length(cow_list)
empty_matrix <- matrix(0, cow_num, cow_num)
colnames(empty_matrix) <- c(cow_list)
rownames(empty_matrix) <- c(cow_list)


# list the result sheets we want to get
paired_feeding_drinking_bout <- list() # number of times 2 cows feeding together
paired_feeding_drinking_total_time <- list() # total amount of time 2 cows are feeding together
paired_feeding_drinking_average_dur <- list() # average duration of 2 cows feeding together
neighbor_feeding_drinking_bout <- list() # number of times 2 cows are feeding neighbors
neighbor_feeding_drinking_total_time <- list() # total amount of time 2 cows are feeding neighbors
neighbor_feeding_drinking_average_dur <- list() # average duration of 2 cows are feeding neighbors


# create a function to calculate bout and duration
Insentec_bout_dur <- function(cur_worksheet) {
  cur_worksheet <- cur_worksheet[order(cur_worksheet$Time),] # sort based on time
  # clear any bout/duration
  cur_worksheet$bout <- 0
  cur_worksheet$duration <- 0
  total_row <- nrow(cur_worksheet)
  
  for (w in 1:total_row) {
    
    # if this is the first row
    if (w == 1) {
      cur_worksheet$bout[w] = 1 # set bout to be 1
      cur_worksheet$duration[w] = 1 # set duration to be 1
    }
    else { # if this is not the first row
      time_interval <- cur_worksheet$Time[w-1] %--% cur_worksheet$Time[w]
      time_dur <- as.duration(time_interval)
      time_dur_str <- tolower(as.character(time_dur))
      
      # if the time gap between current row and the row above is not 1s
      if (time_dur_str != "1s") {
        cur_worksheet$bout[w] <- cur_worksheet$bout[w-1] + 1 # bout number + 1
        cur_worksheet$duration[w] <- 1 # duration reset to 1
      } else { # if the time gap is 1s
        cur_worksheet$bout[w] <- cur_worksheet$bout[w-1] # bout number does not change
        cur_worksheet$duration[w] <- cur_worksheet$duration[w-1] + 1  # duration + 1
      }
      
    }
  }
  
  cur_worksheet <- cur_worksheet # make sure the datasheet get returned
}


### LONG COMPUTATION TIME WARNING!! The following chunk of for loop takes a long time to run!
# iterate through all dates
for (i in 1:length(date_list)) {
  # read in the current datasheet corresponding to the date
  cur_date <- as.character(date_list[i])
  cur_master_sheet <- feed_drink_synch_master_cow3[[cur_date]]
  cur_master_bin_sheet <- feed_drink_synch_master_bin3[[cur_date]]
  used_time <- which(cur_master_sheet$total_cow_num > 1)
  cur_master_sheet <- cur_master_sheet[used_time, ] 
  cur_master_bin_sheet <- cur_master_bin_sheet[used_time, ]
  # only when two cows are present, we can calculate paired eating
  
  
  # create matrix sheet to store result
  paired_feeding_drinking_bout[[i]] <- empty_matrix 
  paired_feeding_drinking_total_time[[i]] <- empty_matrix
  paired_feeding_drinking_average_dur[[i]] <- empty_matrix
  # change names
  names(paired_feeding_drinking_bout)[i] <- as.character(cur_date)
  names(paired_feeding_drinking_total_time)[i] <- as.character(cur_date)
  names(paired_feeding_drinking_average_dur)[i] <- as.character(cur_date)
  
  
  # create matrix sheet to store result
  neighbor_feeding_drinking_bout[[i]] <- empty_matrix 
  neighbor_feeding_drinking_total_time[[i]] <- empty_matrix 
  neighbor_feeding_drinking_average_dur[[i]] <- empty_matrix 
  # change names
  names(neighbor_feeding_drinking_bout)[i] <- as.character(cur_date)
  names(neighbor_feeding_drinking_total_time)[i] <- as.character(cur_date)
  names(neighbor_feeding_drinking_average_dur)[i] <- as.character(cur_date)
  
  
  # iterate through all cows
  for(k in 1:(cow_num-1)) {
    start_index <- k+1  # the index of the current cow on the cur_master_sheet
    matrix_row_index <- k # the index of cow on the row of result matrix
    
    # pair each cow up with the cow below her
    for (h in (k+1):cow_num) {
      end_index <- h+1 # the index of the paired other cow
      matrix_col_index <- h # the index of cow on the column of result matrix
      #print(paste(k, h))
      
      # <1> Simply feeding together
      # get a datasheet with only this two cows' information on current date
      cur_pair <- cur_master_sheet[, c(1, start_index, end_index)]
      cur_pair$total <- rowSums(cur_pair[, 2:3], na.rm = TRUE)
      cur_pair2 <- cur_pair[which(cur_pair$total > 1), ] 
      # if the paired cow ever eat together 
      if (nrow(cur_pair2) > 0) { 
        # calculate bout and duration of feeding together
        cur_pair2 <- Insentec_bout_dur(cur_pair2)
        total_feeding_time <- nrow(cur_pair2)
        total_bout <- max(cur_pair2$bout)
        average_feeding_dur <- total_feeding_time/total_bout
        # record result to the matrix
        paired_feeding_drinking_bout[[i]][matrix_row_index, matrix_col_index] <- total_bout
        paired_feeding_drinking_total_time[[i]][matrix_row_index, matrix_col_index] <- total_feeding_time
        paired_feeding_drinking_average_dur[[i]][matrix_row_index, matrix_col_index] <- average_feeding_dur
        
        
        # <2> Feeding Neighbors
        # get a datasheet with only this two cows' information
        cur_neighbor <- cur_master_bin_sheet[, c(1, start_index, end_index)]
        cur_neighbor <- cur_neighbor[which((cur_neighbor[3] != 0) & (cur_neighbor[2] != 0)),]
        cur_neighbor$bin_dif <- abs(cur_neighbor[3] - cur_neighbor[2]) # get the difference between bin number
        cur_neighbor2 <- cur_neighbor[which(cur_neighbor$bin_dif == 1), ]
        # if the paired neighbor ever eat together
        if (nrow(cur_neighbor2) > 0) {
          # calculate bout and duration of feeding neighbors
          cur_neighbor2 <- Insentec_bout_dur(cur_neighbor2)
          total_neighbor_time <- nrow(cur_neighbor2)
          total_neighbor_bout <- max(cur_neighbor2$bout)
          average_neighbor_dur <- total_neighbor_time/total_neighbor_bout
          # record result to the matrix
          neighbor_feeding_drinking_bout[[i]][matrix_row_index, matrix_col_index] <- total_neighbor_bout
          neighbor_feeding_drinking_total_time[[i]][matrix_row_index, matrix_col_index] <- total_neighbor_time
          neighbor_feeding_drinking_average_dur[[i]][matrix_row_index, matrix_col_index] <- average_neighbor_dur
        } else {
          # do nothing if the paired neighbor never eat together
        }
        
      }
      else { # if the paired cow never eat together
        # do nothing, because the matrix default value is 0
      }
    }
  }
  
}



###################################################################################################
######################################### Hunger Game #############################################
###################################################################################################

feeding_synch_master_feed_hunger <- feeding_synch_master_feed3
# iterate through each day
for (i in 1:length(feeding_synch_master_feed_hunger)) {
  feeding_synch_master_feed_hunger[[i]]$empty_bin_num <- 0
  for (k in 1:nrow(feeding_synch_master_feed_hunger[[i]])){
    feeding_synch_master_feed_hunger[[i]]$empty_bin_num[k] <- length(which(feeding_synch_master_feed_hunger[[i]][k, 2:31] <= 0.5))
  }
  
  feeding_synch_master_feed_hunger[[i]] <- feeding_synch_master_feed_hunger[[i]][which(feeding_synch_master_feed_hunger[[i]]$empty_bin_num > 0), ]
}

bin_empty_time_summary <- feeding_synch_master_feed3[[1]][-(1:nrow(feeding_synch_master_feed3[[1]])), 2:32]
bin_empty_time_summary <- bin_empty_time_summary[,c(31, 1:30)]
date_table <- data.frame(names(feeding_synch_master_feed3))
colnames(date_table) <- c("date")
bin_empty_time_summary <- merge(bin_empty_time_summary, date_table, all = TRUE)

for (i in 1:length(feeding_synch_master_feed_hunger)) {
  if (nrow(feeding_synch_master_feed_hunger[[i]]) >0){
    for (k in 2:31) {
      bin_empty_time_summary[i, k] <- length(which(feeding_synch_master_feed_hunger[[i]][, k] <= 0.5))
    }
  }
}
bin_empty_time_summary[is.na(bin_empty_time_summary)] <- 0
bin_empty_time_summary$date <- ymd(bin_empty_time_summary$date, tz="America/Los_Angeles")

###################################################################################################
#################################### Feeding Replacement ##########################################
###################################################################################################
# split master sheet based on bin number
master_feeding5 <- master_feeding3[order(master_feeding3$Bin, master_feeding3$Start, master_feeding3$End),]
master_feeding5 <- master_feeding5[, -c(1, 6:9)]
bin_list <- sort(unique(master_feeding5$Bin))
master_feeding5_bin_list <- list()
for (i in 1:length(bin_list)) {
  cur_bin <- bin_list[i]
  master_feeding5_bin_list[[i]] <- master_feeding5[which(master_feeding5$Bin == cur_bin),]
  # record the next start time for each row
  next_start_list <- master_feeding5_bin_list[[i]]$Start[2:nrow(master_feeding5_bin_list[[i]])]
  next_cow_list <- master_feeding5_bin_list[[i]]$Cow[2:nrow(master_feeding5_bin_list[[i]])]
  # delete the last row of the current sheet because there will not be possible to have another cow replace the last row
  master_feeding5_bin_list[[i]] <- master_feeding5_bin_list[[i]][-nrow( master_feeding5_bin_list[[i]]), ]
  # register next bout's CowID and start time
  master_feeding5_bin_list[[i]]$next_start <- next_start_list
  master_feeding5_bin_list[[i]]$next_cow <- next_cow_list
  # calculate time difference between the end of current bout and the start of the next
  time_interval <- master_feeding5_bin_list[[i]]$End %--% master_feeding5_bin_list[[i]]$next_start
  master_feeding5_bin_list[[i]]$time_dif <- as.duration(time_interval)
  replace_cutoff <- as.duration("26s") # set the curoff
  # only keep the replacement records
  master_feeding5_bin_list[[i]] <- master_feeding5_bin_list[[i]][
    which((master_feeding5_bin_list[[i]]$time_dif <= replace_cutoff) & 
            (master_feeding5_bin_list[[i]]$Cow != master_feeding5_bin_list[[i]]$next_cow)),]
  master_feeding5_bin_list[[i]] <- master_feeding5_bin_list[[i]][, -c(3,6)] # the time of replacement is changed to be the end time of the reactor cow event
  colnames(master_feeding5_bin_list[[i]]) <- c("Reactor_cow", "Bin", "Time", "date", "Actor_cow", "Bout_interval")
}


# process the replacement records
replacement_list <- master_feeding5_bin_list[[1]]
for (i in 2:length(master_feeding5_bin_list)) {
  replacement_list <- rbind(replacement_list, master_feeding5_bin_list[[i]])
}
replacement_list$available_feed_bin_num <- 0
replacement_list$total_feed_in_available_bin <- 0
replacement_list$neccessary_replacement <- "NO"
replacement_list$distance_to_nearst_unoccupied_bin <- 0 # this locate the nearst unoccupied bin from where the replacment happened. This includes water bins. the unit is "bin".
replacement_list$feed_in_nearst_unoccupied_bin <- 0 # this is the amount of feed left in the nearst unoccupied bin
replacement_list$distance_to_nearst_unoccupied_bin_with_feed <- 0 # this locate the nearst unoccupied bin with feed in it from where the replacment happened. This includes water bins. the unit is "bin"
replacement_list$was_replaced_earlier <- "NO"# this cow has been replaced by other cows before she replaced somebody else in the past 1 hour
replacement_list$frequency_of_being_replaced_earlier_today <- 0 # how many times was this actor cow replaced by others for today before current replacement event
replacement_list$frequency_of_being_replaced_earlier_1hour <- 0 # how many times was this actor cow replaced by others in 1 hour before current replacement
replacement_list$visited_empty_bins_earlier <- "NO" # this cow has visited some empty bins before she replaced somebody else in the past 1 hour
replacement_list$numbre_of_empty_bins_visited_1hour_before <- 0 # how many empty bins has this cow visited in the past 1 hour before current replacement event
replacement_list$actor_at_another_bin <- 0 # this is to mark down, at the end time of the reactor cow's feeding event, if the actor is eating/drinking from another bin. If that's the case, then the actor has an aliby to prove she is not the one pushing the reactor away. 
replacement_list$interval_from_actor_last_feeding_end_time <- NA # how many seconds ago did the actor cow end feeding in previous feed bout ?
replacement_list$actor_prev_bin_end_weight <- NA # what's the end weight in previous bin for actor cow?
replacement_list$latency_from_actor_being_replaced <- NA # the time from actor being replaced last time to current replacement event when she is an actor cow
replacement_list_by_date <- list()
date_list <- sort(unique(replacement_list$date))
for (h in 1:length(date_list)) {
  cur_date <- date_list[h]
  replacement_list_by_date[[h]] <- replacement_list[which(replacement_list$date == cur_date),]
  replacement_list_by_date[[h]] <- replacement_list_by_date[[h]][order(replacement_list_by_date[[h]]$Time),]
  names(replacement_list_by_date)[h] <- as.character(cur_date)
}


### LONG COMPUTATION TIME WARNING!! The following chunk of for loop takes a long time to run!
# record available bin number, total feed in all the other avaialble bin and decide if this is neccessary replacement
# iterate through each date
for (k in 1:length(replacement_list_by_date)) {
  # get the bin numbers at 200 level, after renumbering in previous steps
  all_bin <- seq(201, 235, by = 1)  # all bins (water and feed bin list, re-numbered bin sequence)
  wat_bin <- c(207, 208, 221, 222, 235) # water bin number
  feed_bin <- all_bin[-match(wat_bin, all_bin)] #feed bin number
  
  # get all datasheet for current date
  cur_date <- replacement_list_by_date[[k]]$date[1]
  cur_feeding_synch_master <- feeding_synch_master_cow3[[as.character(cur_date)]]
  cur_feeding_bin_master <- feeding_synch_master_bin3[[as.character(cur_date)]]
  cur_feeding_feed_master <- feeding_synch_master_feed3[[as.character(cur_date)]]
  colnames(cur_feeding_feed_master) <- c("Time", feed_bin, "date", "totalFeed") # change feed bin numbers to be 200-levek renumbered format
  cur_visited_but_no_feed <- visited_but_no_feed_record[[as.character(cur_date)]]
  cur_feeding_drinking_bin_master <- feed_drink_synch_master_bin3[[as.character(cur_date)]]
  
  # iterate through the current sheet and record available bin number, total feed in all the other avaialble bin, and neccessary replacement
  for (j in 1:nrow(replacement_list_by_date[[k]])) {
    #print(paste(k,j))
    cur_time <- ymd_hms(replacement_list_by_date[[k]]$Time[j], tz="America/Los_Angeles")
    cur_bin_num <- replacement_list_by_date[[k]]$Bin[j]
    #update feed bin number
    if (cur_bin_num <= 6) {
      cur_bin_num_renumber <-  (cur_bin_num + 200)
    } else if (cur_bin_num <= 18) {
      cur_bin_num_renumber <- (cur_bin_num + 202)
    } else if (cur_bin_num <= 30) {
      cur_bin_num_renumber <- (cur_bin_num + 204)
    }
    
    
    # check aliby for actor cow Step 1
    cur_actor <- replacement_list_by_date[[k]]$Actor_cow[j]
    actor_cow_occupied_bin <- cur_feeding_drinking_bin_master[which(cur_feeding_drinking_bin_master$Time == cur_time), ][1, as.character(cur_actor)]
    if (actor_cow_occupied_bin > 0) {
      replacement_list_by_date[[k]]$actor_at_another_bin[j] <- 1
    }
    
    # <2> Total available feed
    # occupied bin number list
    occupied_bin_list <- unique(unname(unlist(cur_feeding_bin_master[
      which(cur_feeding_bin_master$Time == cur_time), ][1, 2:(ncol(cur_feeding_bin_master) - 1)])))
    occupied_bin_list <- sort(occupied_bin_list[occupied_bin_list>0])
    # calculate the total amount of feed that is occupied by all cows
    occupied_feed <- 0
    cur_feed_row <- cur_feeding_feed_master[which(cur_feeding_feed_master$Time == cur_time), ][1, ]
    for (u in 1:length(occupied_bin_list)) {
      o_bin <- occupied_bin_list[u]
      index_bin <- o_bin + 1
      occupied_feed <- occupied_feed + cur_feed_row[1, index_bin]
    }
    # record total avaialble feed right now
    available_feed <- cur_feed_row$totalFeed[1] - occupied_feed
    replacement_list_by_date[[k]]$total_feed_in_available_bin[j] <- available_feed
    
    
    # <1> available bin number for current time
    replacement_list_by_date[[k]]$available_feed_bin_num[j] <- 30 - length(occupied_bin_list)
    
    # <3> Necessary replacement: if there is no other bin avaialble, or all available feed <= 0.5, then it's necessary replacement
    if ((replacement_list_by_date[[k]]$available_feed_bin_num[j] == 0) | (replacement_list_by_date[[k]]$total_feed_in_available_bin[j] <= 0.5)) {
      replacement_list_by_date[[k]]$neccessary_replacement[j] <- "YES"
    }
    
    # <4> distance to nearest unoccupied bin
    # <5> distance to nearest unoccupied bin with feed in it
    occupied_bin_list_renumber <- unique(unname(unlist(cur_feeding_drinking_bin_master[
      which(cur_feeding_drinking_bin_master$Time == cur_time), ][1, 2:(ncol(cur_feeding_drinking_bin_master) - 1)])))
    occupied_bin_list_renumber <- sort(occupied_bin_list_renumber[occupied_bin_list_renumber>0])
    wat_and_occupied_feed_bin <- sort(unique(append(wat_bin, occupied_bin_list_renumber))) # get the combination of all occupied feed bins and all water bins
    
    all_rest_bin <- all_bin[-match(wat_and_occupied_feed_bin, all_bin)] # this is a list of bins that are not occupied among the 30 bins
    # if there is unoccupied bins left
    if (length(all_rest_bin) > 0) {
      # get a list of unoccupied bins, record distance to the nearst unoccupied bin
      abs_distance <- abs(all_rest_bin - cur_bin_num_renumber) # absolute distance of the available bins from current bin
      nearst <- min(abs_distance)
      min_abs_distance_index <- match(nearst, abs_distance)
      closest_bin_number <- all_rest_bin[min_abs_distance_index] # find the bin number of the closest unoccupied bin near current bin
      feed_amount_in_closest_Bin <- cur_feed_row[1, c(as.character(closest_bin_number))] # feed amount in the cloest unoccupied bin
      replacement_list_by_date[[k]]$distance_to_nearst_unoccupied_bin[j] <- nearst
      replacement_list_by_date[[k]]$feed_in_nearst_unoccupied_bin[j] <- feed_amount_in_closest_Bin
      
      
      # get a list of unoccupied bins that has feed in it
      unoccupied_bin_with_feed_list <- c()
      for (d in 1:length(all_rest_bin)) {
        unoccupied_bin_num <- all_rest_bin[d]
        # check for NA
        if (!is.na(cur_feed_row[1, c(as.character(unoccupied_bin_num))])) {
          if (cur_feed_row[1, c(as.character(unoccupied_bin_num))] > 0.5) { # curoff for empty is set to be 0.5
            unoccupied_bin_with_feed_list <- append(unoccupied_bin_with_feed_list, unoccupied_bin_num)
          }
        }
      }
      
      
      # if there is any unoccupied bins with feed available
      if (length(unoccupied_bin_with_feed_list) > 0) {
        abs_distance_unoccupied_with_feed <- abs(unoccupied_bin_with_feed_list - cur_bin_num_renumber) # absolute distance of the available bins from current bin
        nearst_with_feed <- min(abs_distance_unoccupied_with_feed)
        replacement_list_by_date[[k]]$distance_to_nearst_unoccupied_bin_with_feed[j] <- nearst_with_feed
      } else{ # if there is no unoccupied bins with feed in it left, then set value to NA
        replacement_list_by_date[[k]]$distance_to_nearst_unoccupied_bin_with_feed[j] <- NA
      }
      
      
      
    } else { # if all bins are occupied, set the value to NA
      replacement_list_by_date[[k]]$distance_to_nearst_unoccupied_bin[j] <- NA
      replacement_list_by_date[[k]]$distance_to_nearst_unoccupied_bin_with_feed[j] <- NA
    }
    
    
    # <6> the actor cow was replaced by somebody else before she became the actor cow in current replacement event
    cur_actor <- replacement_list_by_date[[k]]$Actor_cow[j]
    prev_replacment <- replacement_list_by_date[[k]][which((replacement_list_by_date[[k]]$Reactor_cow == cur_actor) & (replacement_list_by_date[[k]]$Time < cur_time)),]
    prev_replacment <- prev_replacment[order(prev_replacment$Time),]
    # <7> frequency of this current actor cow being replaced earlier today before current replacement happened
    # <8> frequency of this current actor cow replaced in an hour before current replacement happened
    if (nrow(prev_replacment) > 0) {
      # <13> the time interval from the actor was replaced by someone else, to the current time when she became an actor cow
      prev_replace_time <- prev_replacment$Time[nrow(prev_replacment)]
      replace_interval <- time_length(as.duration(cur_time - prev_replace_time))
      replacement_list_by_date[[k]]$latency_from_actor_being_replaced[j] <- replace_interval
      
      # was this actor cow being replaced in the past? 
      replaced_time_today <- as.character(sort(unique(prev_replacment$Time)))
      replaced_time_today_str <- paste(unlist(replaced_time_today), collapse="; ")
      replacement_list_by_date[[k]]$was_replaced_earlier[j] <- paste("Yes, ", replaced_time_today_str, sep = "")
      
      # how many times has she been replaced in today before current time?
      replacement_list_by_date[[k]]$frequency_of_being_replaced_earlier_today[j] <- nrow(prev_replacment)
      
      # how many times has she been replaced in 1 hour before current time?
      one_hour_upper_tme <- cur_time - hours(1)
      in_1hour <- prev_replacment[which(prev_replacment$Time >= one_hour_upper_tme), ]
      replacement_list_by_date[[k]]$frequency_of_being_replaced_earlier_1hour[j] <- nrow(in_1hour)
    }
    
    
    # <9> the actor cow visited some other bins, but they were empty before became the actor cow in current replacement event  
    # <10> how many empty bins has this actor cow visited before current replacement event in 1 hour
    prev_empty <- cur_visited_but_no_feed[which((cur_visited_but_no_feed$Cow == cur_actor)& (cur_visited_but_no_feed$Start < cur_time)), ]
    if (nrow(prev_empty) > 0) {
      # did this actor cow visited empty bins in the past? 
      empty_bin_visited_today <- as.character(sort(unique(prev_empty$Start)))
      empty_bin_today_str <- paste(unlist(empty_bin_visited_today), collapse="; ")
      replacement_list_by_date[[k]]$visited_empty_bins_earlier[j] <- paste("Yes, ", empty_bin_today_str, sep = "")
      
      one_hour_upper_tme <- cur_time - hours(1)
      # did this actor cow visited empty bins in the past 1 hour before replacement happened
      empty_in_1hour <- prev_empty[which(prev_empty$Start >= one_hour_upper_tme), ]
      replacement_list_by_date[[k]]$numbre_of_empty_bins_visited_1hour_before[j] <- nrow(empty_in_1hour)
    }
    
    
    # <11> end time of feeding for actor cow in previous feeding event? 
    # <12> how much feed is left in the bin when the actor finish eating from previous event
    cur_master_sheet <- master_feeding3[which((master_feeding3$Cow == cur_actor) & (master_feeding3$End < cur_time)),]
    if (nrow(cur_master_sheet) > 0) {
      cur_master_sheet <- cur_master_sheet[order(cur_master_sheet$End),]
      prev_end_time <- cur_master_sheet$End[nrow(cur_master_sheet)]
      prev_end_weight <- cur_master_sheet$Endweight[nrow(cur_master_sheet)]
      feeding_dur <- time_length(as.duration(cur_time - prev_end_time), unit = "second")
      replacement_list_by_date[[k]]$interval_from_actor_last_feeding_end_time[j] <- feeding_dur
      replacement_list_by_date[[k]]$actor_prev_bin_end_weight[j] <- prev_end_weight 
    }
  }
}


# check aliby for actor cow Step 2
for (k in 1:length(replacement_list_by_date)) {
  # only keep the records for actor cows that passed aliby test. 
  replacement_list_by_date[[k]] <- replacement_list_by_date[[k]][which(replacement_list_by_date[[k]]$actor_at_another_bin == 0), ]
  replacement_list_by_date[[k]]$actor_at_another_bin <- NULL # delete this helper column
}

###################################################################################################
#################################### Drinking Replacement ##########################################
###################################################################################################

# split master sheet based on bin number
master_drinking5 <- master_drinking3[order(master_drinking3$Bin, master_drinking3$Start, master_drinking3$End),]
master_drinking5 <- master_drinking5[, -c(1, 6:9)]
bin_list <- sort(unique(master_drinking5$Bin))
master_drinking5_bin_list <- list()
for (i in 1:length(bin_list)) {
  cur_bin <- bin_list[i]
  master_drinking5_bin_list[[i]] <- master_drinking5[which(master_drinking5$Bin == cur_bin),]
  # you can only find the next drinking event if there is more than 1 rows of data
  if (nrow(master_drinking5_bin_list[[i]]) >= 2) {
    # record the next start time for each row
    next_start_list <- master_drinking5_bin_list[[i]]$Start[2:nrow(master_drinking5_bin_list[[i]])]
    next_cow_list <- master_drinking5_bin_list[[i]]$Cow[2:nrow(master_drinking5_bin_list[[i]])]
    # delete the last row of the current sheet because there will not be possible to have another cow replace the last row
    master_drinking5_bin_list[[i]] <- master_drinking5_bin_list[[i]][-nrow( master_drinking5_bin_list[[i]]), ]
    # register next bout's CowID and start time
    master_drinking5_bin_list[[i]]$next_start <- next_start_list
    master_drinking5_bin_list[[i]]$next_cow <- next_cow_list
    # calculate time difference between the end of current bout and the start of the next
    time_interval <- master_drinking5_bin_list[[i]]$End %--% master_drinking5_bin_list[[i]]$next_start
    master_drinking5_bin_list[[i]]$time_dif <- as.duration(time_interval)
    replace_cutoff <- as.duration("26s") # set the curoff
    # only keep the replacement records
    master_drinking5_bin_list[[i]] <- master_drinking5_bin_list[[i]][
      which((master_drinking5_bin_list[[i]]$time_dif <= replace_cutoff) & 
              (master_drinking5_bin_list[[i]]$Cow != master_drinking5_bin_list[[i]]$next_cow)),]
    master_drinking5_bin_list[[i]] <- master_drinking5_bin_list[[i]][, -c(3,6)]
    
    # handle special cases: there is only 1 row of data for current bin
  } else {
    # first create those new columns to make sure data type is consistant for each column compared to other dates
    master_drinking5_bin_list[[i]]$actor_cow <- 0
    time_interval <- master_drinking5_bin_list[[i]]$End %--% master_drinking5_bin_list[[i]]$Start
    master_drinking5_bin_list[[i]]$time_dif <- as.duration(time_interval)
    master_drinking5_bin_list[[i]]$Start <- NULL
    master_drinking5_bin_list[[i]]$End[1] <- NA
    master_drinking5_bin_list[[i]]$actor_cow[1] <- NA
    master_drinking5_bin_list[[i]]$time_dif[1] <- NA
    # if there is only 1 row of data for current bin, then there could not be any replacement events for this day, delete all rows
    master_drinking5_bin_list[[i]] <- master_drinking5_bin_list[[i]][-c(1:nrow(master_drinking5_bin_list[[i]])),]
  }
  
  colnames(master_drinking5_bin_list[[i]]) <- c("Reactor_cow", "Bin", "Time", "date", "Actor_cow", "Bout_interval")
}



# process the replacement records
replacement_drinking_list <- master_drinking5_bin_list[[1]]
for (i in 2:length(master_drinking5_bin_list)) {
  replacement_drinking_list <- rbind(replacement_drinking_list, master_drinking5_bin_list[[i]])
}
replacement_drinking_list$available_wat_bin_num <- 0
replacement_drinking_list$neccessary_replacement <- "NO"
replacement_drinking_list$distance_to_nearst_unoccupied_bin <- 0 # this locate the nearst unoccupied bin from where the replacment happened. the unit is "bin"
replacement_drinking_list$was_replaced_earlier <- "NO"# this cow has been replaced by other cows before she replaced somebody else in the past 1 hour
replacement_drinking_list$frequency_of_being_replaced_earlier_today <- 0 # how many times was this actor cow replaced by others for today before current replacement event
replacement_drinking_list$frequency_of_being_replaced_earlier_1hour <- 0 # how many times was this actor cow replaced by others in 1 hour before current replacement
replacement_drinking_list$actor_at_another_bin <- 0 # this is to mark down, at the end time of the reactor cow's drinking event, if the actor is eating/drinking from another bin. If that's the case, then the actor has an aliby to prove she is not the one pushing the reactor away. 
replacement_drinking_list_by_date <- list()
date_list <- sort(unique(replacement_drinking_list$date))
for (h in 1:length(date_list)) {
  cur_date <- date_list[h]
  replacement_drinking_list_by_date[[h]] <- replacement_drinking_list[which(replacement_drinking_list$date == cur_date),]
  replacement_drinking_list_by_date[[h]] <- replacement_drinking_list_by_date[[h]][order(replacement_drinking_list_by_date[[h]]$Time),]
  names(replacement_drinking_list_by_date)[h] <- as.character(cur_date)
}



### LONG COMPUTATION TIME WARNING!! The following chunk of for loop takes a long time to run!
# record available bin number, decide if this is neccessary replacement
# iterate through each date
for (k in 1:length(replacement_drinking_list_by_date)) {
  cur_date <- replacement_drinking_list_by_date[[k]]$date[1]
  cur_drinking_synch_master <- drinking_synch_master_cow3[[as.character(cur_date)]]
  cur_drinking_bin_master <- drinking_synch_master_bin3[[as.character(cur_date)]]
  cur_drinking_wat_master <- drinking_synch_master_wat3[[as.character(cur_date)]]
  cur_feeding_drinking_bin_master <- feed_drink_synch_master_bin3[[as.character(cur_date)]]
  
  
  # iterate through the current sheet and record available bin number, total wat in all the other avaialble bin, and neccessary replacement
  for (j in 1:nrow(replacement_drinking_list_by_date[[k]])) {
    #print(paste(k,j))
    cur_time <- ymd_hms(replacement_drinking_list_by_date[[k]]$Time[j], tz="America/Los_Angeles")
    cur_bin_num <- replacement_drinking_list_by_date[[k]]$Bin[j]
    #update water bin number
    if (cur_bin_num == 101) {
      cur_bin_num_renumber <-  207
    } else if (cur_bin_num == 102) {
      cur_bin_num_renumber <- 208
    } else if (cur_bin_num == 103) {
      cur_bin_num_renumber <- 221
    } else if (cur_bin_num == 104) {
      cur_bin_num_renumber <- 222
    } else if (cur_bin_num == 105) {
      cur_bin_num_renumber <- 235
    }
    
    # check aliby for actor cow Step 1
    cur_actor <- replacement_drinking_list_by_date[[k]]$Actor_cow[j]
    actor_cow_occupied_bin <- cur_feeding_drinking_bin_master[which(cur_feeding_drinking_bin_master$Time == cur_time), ][1, as.character(cur_actor)]
    if (actor_cow_occupied_bin > 0) {
      replacement_drinking_list_by_date[[k]]$actor_at_another_bin[j] <- 1
    }
    
    # occupied bin number list
    occupied_bin_list <- unique(unname(unlist(cur_drinking_bin_master[
      which(cur_drinking_bin_master$Time == cur_time), ][1, 2:(ncol(cur_drinking_bin_master) - 1)])))
    occupied_bin_list <- sort(occupied_bin_list[occupied_bin_list>0])
    
    
    # <1> available bin number for current time
    replacement_drinking_list_by_date[[k]]$available_wat_bin_num[j] <- 5 - length(occupied_bin_list)
    
    
    # <2> Necessary replacement: if there is no other bin avaialble, then it's necessary replacement
    if (replacement_drinking_list_by_date[[k]]$available_wat_bin_num[j] == 0) {
      replacement_drinking_list_by_date[[k]]$neccessary_replacement[j] <- "YES"
    }
    
    
    # <3> distance to nearest unoccupied bin
    # get a list of all occupied feed and water bins using 200 level numbering
    occupied_bin_list_renumber <- unique(unname(unlist(cur_feeding_drinking_bin_master[
      which(cur_feeding_drinking_bin_master$Time == cur_time), ][1, 2:(ncol(cur_feeding_drinking_bin_master) - 1)])))
    occupied_bin_list_renumber <- sort(occupied_bin_list_renumber[occupied_bin_list_renumber>0])
    feed_and_occupied_bin_list <- sort(unique(append(occupied_bin_list_renumber, feed_bin))) # a list of all occupied bins + all feed bins
    all_rest_bin <- all_bin[-match(feed_and_occupied_bin_list, all_bin)] # this is a list of bins that are not occupied among the 30 bins
    # if there is unoccupied bins left
    if (length(all_rest_bin) > 0) {
      # get a list of unoccupied bins that has wat in it
      abs_distance <- abs(all_rest_bin - cur_bin_num_renumber) # absolute distance of the available bins from current bin
      nearst <- min(abs_distance)
      replacement_drinking_list_by_date[[k]]$distance_to_nearst_unoccupied_bin[j] <- nearst
      
    } else { # if all bins are occupied, set the value to NA
      replacement_drinking_list_by_date[[k]]$distance_to_nearst_unoccupied_bin[j] <- NA
    }
    
    
    # <4> the actor cow was replaced by somebody else before she became the actor cow in current replacement event
    cur_actor <- replacement_drinking_list_by_date[[k]]$Actor_cow[j]
    prev_replacment <- replacement_drinking_list_by_date[[k]][which((replacement_drinking_list_by_date[[k]]$Reactor_cow == cur_actor) & (replacement_drinking_list_by_date[[k]]$Time < cur_time)),]
    # <5> frequency of this current actor cow being replaced earlier today before current replacement happened
    # <6> frequency of this current actor cow replaced in an hour before current replacement happened
    if (nrow(prev_replacment) > 0) {
      # was this actor cow being replaced in the past? 
      replaced_time_today <- as.character(sort(unique(prev_replacment$Time)))
      replaced_time_today_str <- paste(unlist(replaced_time_today), collapse="; ")
      replacement_drinking_list_by_date[[k]]$was_replaced_earlier[j] <- paste("Yes, ", replaced_time_today_str, sep = "")
      
      # how many times has she been replaced in today before current time?
      replacement_drinking_list_by_date[[k]]$frequency_of_being_replaced_earlier_today[j] <- nrow(prev_replacment)
      
      # how many times has she been replaced in 1 hour before current time?
      one_hour_upper_tme <- cur_time - hours(1)
      in_1hour <- prev_replacment[which(prev_replacment$Time >= one_hour_upper_tme), ]
      replacement_drinking_list_by_date[[k]]$frequency_of_being_replaced_earlier_1hour[j] <- nrow(in_1hour)
    }
    
    
  }
}

# check aliby for actor cow Step 2
for (k in 1:length(replacement_drinking_list_by_date)) {
  # only keep the records for actor cows that passed aliby test. 
  replacement_drinking_list_by_date[[k]] <- replacement_drinking_list_by_date[[k]][which(replacement_drinking_list_by_date[[k]]$actor_at_another_bin == 0), ]
  replacement_drinking_list_by_date[[k]]$actor_at_another_bin <- NULL # delete this helper column
}

###################################################################################################
######################### Fully occupied feed & water bins ########################################
###################################################################################################
date_list2 <- sort(unique(master_feeding3$date))
fully_occupy <- data.frame(date_list2)
colnames(fully_occupy) <- c("date")
fully_occupy$total_seconds_when_all_waterBins_are_occupied <- 0
fully_occupy$total_seconds_when_all_feedBins_are_occupied <- 0

for (i in 1:nrow(fully_occupy)) {
  cur_date <- as.character(fully_occupy$date[i])
  
  # feed bins fully occupied
  feed_interest_sheet <- feeding_synch_master_cow3[[cur_date]][which(feeding_synch_master_cow3[[cur_date]]$total_bin_occupied == 30),]
  fully_occupy$total_seconds_when_all_feedBins_are_occupied[i] <- nrow(feed_interest_sheet)
  
  # water bins fully occupied
  wat_interest_sheet <- drinking_synch_master_cow3[[cur_date]][which(drinking_synch_master_cow3[[cur_date]]$total_bin_occupied == 5), ]
  fully_occupy$total_seconds_when_all_waterBins_are_occupied[i] <- nrow(wat_interest_sheet)
}


###################################################################################################
################### Average number of cows feeding/drinking together ##############################
###################################################################################################

average_num_feed_together_cow_list <- list()
for (i in 1:length(feed_drink_synch_master_cow3)){
  cur_date <- feed_drink_synch_master_cow3[[i]]$date[1]
  cow_list2 <- sort(unique(master_feeding3[which(master_feeding3$date == cur_date), ]$Cow))
  average_cow_num <- data.frame(cow_list2)
  colnames(average_cow_num) <- c("Cow")
  average_cow_num$average_num_other_cows_feeding_together <- 0
  
  for (k in 1:nrow(average_cow_num)){
    cur_cow <- average_cow_num$Cow[k]
    cow_index <- match(cur_cow, cow_list2) + 1
    interest_sheet <- feed_drink_synch_master_cow3[[i]][which(feed_drink_synch_master_cow3[[i]][cow_index] > 0 & feed_drink_synch_master_cow3[[i]]$total_cow_num > 1),]
    interest_sheet$other_cow <- interest_sheet$total_cow_num - 1
    average_other_cow <- mean(interest_sheet$other_cow)
    average_cow_num$average_num_other_cows_feeding_together[k] <- average_other_cow
  }
  
  cur_index <- length(average_num_feed_together_cow_list) + 1
  average_num_feed_together_cow_list[[cur_index]] <- average_cow_num
  names(average_num_feed_together_cow_list)[cur_index] <- as.character(cur_date)
}


###################################################################################################
##################################### Insentec Result Storage #########################################
###################################################################################################
# load existing social_character_project data set if there is one
# Create a list of all files' name from the current folder
list_file2 = list.files(path=output_dir, pattern="*.Rda", full.names=TRUE)
if (length(list_file2) > 0 ) {
  for (n in 1:length(list_file2)) {
    cur_file <- list_file2[n]
    cur_file_str <- substring(cur_file, nchar(cur_file)-36, nchar(cur_file))
    if (cur_file_str == "social_character_project_all_data.Rda") {
      out_file <- paste(output_dir, "/social_character_project_all_data.Rda", sep = "")
      load(out_file)
      
    }
  }
}


# record all the Insentec lying and standing result data sheet after cleaning
avi_index <- 1# record the next available index to write a datasheet into the list
if (length(Social_character_project) <2) { # if this is the first time we record Insentec result
  # sheet 1: Feeding and drinking analysis
  Insentec[[avi_index]] <- Insentec_final_summary
  names(Insentec)[avi_index] <- "Feeding and drinking analysis"
  # sheet 2: 90 minutes and 3hour time interval after feed is added
  Insentec[[avi_index + 1]] <- time_interval_after_feed_added
  names(Insentec)[avi_index + 1] <- "90 minutes and 3hour time interval after feed is added"
  # sheet 3: Insentec warning 
  Insentec[[avi_index + 2]] <- Insentec_warning
  names(Insentec)[avi_index + 2] <- "Insentec warning"
  # sheet 4: Bins with number of visits daily
  Insentec[[avi_index + 3]] <- bins_visit_num
  names(Insentec)[avi_index + 3] <- "Bins with number of visits daily"
  # sheet 5: number of visits for each bin for each cow
  Insentec[[avi_index + 4]] <- visit_per_bin_per_cow
  names(Insentec)[avi_index + 4] <- "number of visits for each bin for each cow"
  # sheet 6:number of bins visited by each cow 
  Insentec[[avi_index + 5]] <- bin_num_visit_per_cow
  names(Insentec)[avi_index + 5] <- "number of bins visited by each cow"
  # sheet 7:long feed duration
  Insentec[[avi_index + 6]] <- long_feed_dur_list
  names(Insentec)[avi_index + 6] <- "long feed duration"
  # sheet 8: long water duration
  Insentec[[avi_index + 7]] <- long_wat_dur_list
  names(Insentec)[avi_index + 7] <- "long water duration"
  # sheet 9: double_detection_1cow_2bins
  Insentec[[avi_index + 8]] <- double_bin_detection_list
  names(Insentec)[avi_index + 8] <- "double_detection_1cow_2bins"
  # sheet 10:double_detection_1bin_2cows
  Insentec[[avi_index + 9]] <- double_cow_detection_list
  names(Insentec)[avi_index + 9] <- "double_detection_1bin_2cows"
  # sheet 11: eating and lying conflict
  Insentec[[avi_index + 10]] <- eating_lying_conflict_list
  names(Insentec)[avi_index + 10] <- "eating and lying conflict"
  # sheet 12: negative duration bin
  Insentec[[avi_index + 11]] <- negative_dur_list
  names(Insentec)[avi_index + 11] <- "negative duration bin"
  # sheet 13: negative intake bin
  Insentec[[avi_index + 12]] <- negative_intake_list
  names(Insentec)[avi_index + 12] <- "negative intake bin"
  # sheet 14: large feed intake in one bout
  Insentec[[avi_index + 13]] <- large_feed_intake_in_one_bout
  names(Insentec)[avi_index + 13] <- "large feed intake in one bout"
  # sheet 15:large water intake in one bout
  Insentec[[avi_index + 14]] <- large_water_intake_in_one_bout
  names(Insentec)[avi_index + 14] <- "large water intake in one bout"
  # sheet 16:large feed intake in short time
  Insentec[[avi_index + 15]] <- large_feed_intake_in_short_time
  names(Insentec)[avi_index + 15] <- "large feed intake in short time"
  # sheet 17: large water intake in short time
  Insentec[[avi_index + 16]] <- large_water_intake_in_short_time
  names(Insentec)[avi_index + 16] <- "large water intake in short time"
  # sheet 18: which cows are present each second for feed
  Insentec[[avi_index + 17]] <- feeding_synch_master_cow3
  names(Insentec)[avi_index + 17] <- "which cows are present each second for feed"
  # sheet 19: which bins are occupied each second for feed
  Insentec[[avi_index + 18]] <- feeding_synch_master_bin3
  names(Insentec)[avi_index + 18] <- "which bins are occupied each second for feed"
  # sheet 20: how much feed left each bin
  Insentec[[avi_index + 19]] <- feeding_synch_master_feed3
  names(Insentec)[avi_index + 19] <- "how much feed left each bin"
  # sheet 21: which cows are present each second for feed water
  Insentec[[avi_index + 20]] <- drinking_synch_master_cow3
  names(Insentec)[avi_index + 20] <- "which cows are present each second for water"
  # sheet 22: which bins are occupied each second for water
  Insentec[[avi_index + 21]] <- drinking_synch_master_bin3
  names(Insentec)[avi_index + 21] <- "which bins are occupied each second for water"
  # sheet 23: how much water left each bin
  Insentec[[avi_index + 22]] <- drinking_synch_master_wat3
  names(Insentec)[avi_index + 22] <- "how much water left each bin"
  # sheet 24:Feeding/drinking at the same time_bout
  Insentec[[avi_index + 23]] <- paired_feeding_drinking_bout
  names(Insentec)[avi_index + 23] <- "Feeding/drinking at the same time bout"
  # sheet 25:Feeding/drinking at the same time_total time
  Insentec[[avi_index + 24]] <- paired_feeding_drinking_total_time
  names(Insentec)[avi_index + 24] <- "Feeding/drinking at the same time total time"
  # sheet 26: Feeding/drinking at the same time_average duration
  Insentec[[avi_index + 25]] <- paired_feeding_drinking_average_dur
  names(Insentec)[avi_index + 25] <- "Feeding/drinking at the same time average duration"
  # sheet 27: Feeding/drinking neighbour_bout
  Insentec[[avi_index + 26]] <- neighbor_feeding_drinking_bout
  names(Insentec)[avi_index + 26] <- "Feeding/drinking neighbour bout"
  # sheet 28: Feeding/drinking neighbour_total time
  Insentec[[avi_index + 27]] <- neighbor_feeding_drinking_total_time
  names(Insentec)[avi_index + 27] <- "Feeding/drinking neighbour total time"
  # sheet 29: Feeding/drinking neighbor_average duration
  Insentec[[avi_index + 28]] <- neighbor_feeding_drinking_average_dur
  names(Insentec)[avi_index + 28] <- "Feeding/drinking neighbor average duration"
  # sheet 30: Replacement behaviour by date
  Insentec[[avi_index + 29]] <- replacement_list_by_date
  names(Insentec)[avi_index + 29] <- "Replacement behaviour by date"
  # sheet 31: all feed water bins occupied
  Insentec[[avi_index + 30]] <- fully_occupy
  names(Insentec)[avi_index + 30] <- "all feed water bins occupied"
  # sheet 32: average number of feeding buddies
  Insentec[[avi_index + 31]] <- average_num_feed_together_cow_list
  names(Insentec)[avi_index + 31] <- "average number of feeding buddies"
  # sheet 33:Cleaned_feeding_original_data
  Insentec[[avi_index + 32]] <- all.fed2
  names(Insentec)[avi_index + 32] <- "Cleaned_feeding_original_data"
  # sheet 34: Cleaned_drinking_original_data
  Insentec[[avi_index + 33]] <- all.wat2
  names(Insentec)[avi_index + 33] <- "Cleaned_drinking_original_data"
  # sheet 35: Cleaned_combined_original_data
  Insentec[[avi_index + 34]] <- all.comb2
  names(Insentec)[avi_index + 34] <- "Cleaned_combined_original_data"
  # sheet 36: non_nutritive_visits
  Insentec[[avi_index + 35]] <- non_nutritive_visits
  names(Insentec)[avi_index + 35] <- "non_nutritive_visits"
  # sheet 37: visited_but_no_feed_record
  Insentec[[avi_index + 36]] <- visited_but_no_feed_record
  names(Insentec)[avi_index + 36] <- "visited_but_no_feed_record"
  # sheet 38: non_nutritive_visits
  Insentec[[avi_index + 37]] <- visited_but_no_feed_freq
  names(Insentec)[avi_index + 37] <- "visited_but_no_feed_freq"
  # sheet 39: culled_cow
  Insentec[[avi_index + 38]] <- culled_cow
  names(Insentec)[avi_index + 38] <- "culled_cow"
  # sheet 40: warning_days
  Insentec[[avi_index + 39]] <- warning_days
  names(Insentec)[avi_index + 39] <- "warning_days"
  # sheet 41:Drinking replacement behaviour by date
  Insentec[[avi_index + 40]] <- replacement_drinking_list_by_date
  names(Insentec)[avi_index + 40] <- "Drinking replacement behaviour by date"
  # sheet 42: empty_bin_time_detail
  Insentec[[avi_index + 41]] <- feeding_synch_master_feed_hunger
  names(Insentec)[avi_index + 41] <- "empty_bin_time_detail"
  # sheet 43:bin_empty_total_time_summary
  Insentec[[avi_index + 42]] <- bin_empty_time_summary
  names(Insentec)[avi_index + 42] <- "bin_empty_total_time_summary"
  
  
  # add the Insentec list to social_character_project
  Social_character_project[[2]] <- Insentec
  names(Social_character_project)[2] <- "Insentec"
  
}else { # if this is not the first time
  # sheet 1: Insentec_final_summary
  Social_character_project[["Insentec"]][[avi_index]] <- rbind(Social_character_project[["Insentec"]][[avi_index]], Insentec_final_summary)
  # sheet 2: time_interval_after_feed_added
  Social_character_project[["Insentec"]][[avi_index + 1]] <- rbind(Social_character_project[["Insentec"]][[avi_index + 1]], time_interval_after_feed_added)
  # sheet 3: Insentec_warning
  Social_character_project[["Insentec"]][[avi_index + 2]] <- rbind(Social_character_project[["Insentec"]][[avi_index + 2]], Insentec_warning)
  # sheet 4: bins_visit_num
  Social_character_project[["Insentec"]][[avi_index + 3]] <- append(Social_character_project[["Insentec"]][[avi_index + 3]], bins_visit_num)
  # sheet 5: visit_per_bin_per_cow
  Social_character_project[["Insentec"]][[avi_index + 4]] <- append(Social_character_project[["Insentec"]][[avi_index + 4]], visit_per_bin_per_cow)
  # sheet 6:bin_num_visit_per_cow
  Social_character_project[["Insentec"]][[avi_index + 5]] <- append(Social_character_project[["Insentec"]][[avi_index + 5]], bin_num_visit_per_cow)
  # sheet 7: long_feed_dur_list
  Social_character_project[["Insentec"]][[avi_index + 6]] <- append(Social_character_project[["Insentec"]][[avi_index + 6]], long_feed_dur_list)
  # sheet 8: long_wat_dur_list
  Social_character_project[["Insentec"]][[avi_index + 7]] <- append(Social_character_project[["Insentec"]][[avi_index + 7]], long_wat_dur_list)
  # sheet 9: double_bin_detection_list
  Social_character_project[["Insentec"]][[avi_index + 8]] <- append(Social_character_project[["Insentec"]][[avi_index + 8]], double_bin_detection_list)
  # sheet 10: double_cow_detection_list
  Social_character_project[["Insentec"]][[avi_index + 9]] <- append(Social_character_project[["Insentec"]][[avi_index + 9]], double_cow_detection_list)
  # sheet 11: eating_lying_conflict_list
  Social_character_project[["Insentec"]][[avi_index + 10]] <- append(Social_character_project[["Insentec"]][[avi_index + 10]], eating_lying_conflict_list)
  # sheet 12: negative_dur_list
  Social_character_project[["Insentec"]][[avi_index + 11]] <- append(Social_character_project[["Insentec"]][[avi_index + 11]], negative_dur_list)
  # sheet 13: negative_intake_list
  Social_character_project[["Insentec"]][[avi_index + 12]] <- append(Social_character_project[["Insentec"]][[avi_index + 12]], negative_intake_list)
  # sheet 14: large_feed_intake_in_one_bout
  Social_character_project[["Insentec"]][[avi_index + 13]] <- append(Social_character_project[["Insentec"]][[avi_index + 13]], large_feed_intake_in_one_bout)
  # sheet 15: large_water_intake_in_one_bout
  Social_character_project[["Insentec"]][[avi_index + 14]] <- append(Social_character_project[["Insentec"]][[avi_index + 14]], large_water_intake_in_one_bout)
  # sheet 16: large_feed_intake_in_short_time
  Social_character_project[["Insentec"]][[avi_index + 15]] <- append(Social_character_project[["Insentec"]][[avi_index + 15]], large_feed_intake_in_short_time)
  # sheet 17: large_water_intake_in_short_time
  Social_character_project[["Insentec"]][[avi_index + 16]] <- append(Social_character_project[["Insentec"]][[avi_index + 16]], large_water_intake_in_short_time)
  # sheet 18: feeding_synch_master_cow3
  Social_character_project[["Insentec"]][[avi_index + 17]] <- append(Social_character_project[["Insentec"]][[avi_index + 17]], feeding_synch_master_cow3)
  # sheet 19: feeding_synch_master_bin3
  Social_character_project[["Insentec"]][[avi_index + 18]] <- append(Social_character_project[["Insentec"]][[avi_index + 18]], feeding_synch_master_bin3)
  # sheet 20: feeding_synch_master_feed3
  Social_character_project[["Insentec"]][[avi_index + 19]] <- append(Social_character_project[["Insentec"]][[avi_index + 19]], feeding_synch_master_feed3)
  # sheet 21: drinking_synch_master_cow3
  Social_character_project[["Insentec"]][[avi_index + 20]] <- append(Social_character_project[["Insentec"]][[avi_index + 20]], drinking_synch_master_cow3)
  # sheet 22: drinking_synch_master_bin3
  Social_character_project[["Insentec"]][[avi_index + 21]] <- append(Social_character_project[["Insentec"]][[avi_index + 21]], drinking_synch_master_bin3)
  # sheet 23: drinking_synch_master_wat3
  Social_character_project[["Insentec"]][[avi_index + 22]] <- append(Social_character_project[["Insentec"]][[avi_index + 22]], drinking_synch_master_wat3)
  # sheet 24: paired_feeding_drinking_bout
  Social_character_project[["Insentec"]][[avi_index + 23]] <- append(Social_character_project[["Insentec"]][[avi_index + 23]], paired_feeding_drinking_bout)
  # sheet 25: paired_feeding_drinking_total_time
  Social_character_project[["Insentec"]][[avi_index + 24]] <- append(Social_character_project[["Insentec"]][[avi_index + 24]], paired_feeding_drinking_total_time)
  # sheet 26: paired_feeding_drinking_average_dur
  Social_character_project[["Insentec"]][[avi_index + 25]] <- append(Social_character_project[["Insentec"]][[avi_index + 25]], paired_feeding_drinking_average_dur)
  # sheet 27: neighbor_feeding_drinking_bout
  Social_character_project[["Insentec"]][[avi_index + 26]] <- append(Social_character_project[["Insentec"]][[avi_index + 26]], neighbor_feeding_drinking_bout)
  # sheet 28: neighbor_feeding_drinking_total_time
  Social_character_project[["Insentec"]][[avi_index + 27]] <- append(Social_character_project[["Insentec"]][[avi_index + 27]], neighbor_feeding_drinking_total_time)
  # sheet 29: neighbor_feeding_drinking_average_dur
  Social_character_project[["Insentec"]][[avi_index + 28]] <- append(Social_character_project[["Insentec"]][[avi_index + 28]], neighbor_feeding_drinking_average_dur)
  # sheet 30: replacement_list_by_date
  Social_character_project[["Insentec"]][[avi_index + 29]] <- append(Social_character_project[["Insentec"]][[avi_index + 29]], replacement_list_by_date)
  # sheet 31: fully_occupy
  Social_character_project[["Insentec"]][[avi_index + 30]] <- rbind(Social_character_project[["Insentec"]][[avi_index + 30]], fully_occupy)
  # sheet 32: average_num_feed_together_cow_list
  Social_character_project[["Insentec"]][[avi_index + 31]] <- append(Social_character_project[["Insentec"]][[avi_index + 31]], average_num_feed_together_cow_list)
  # sheet 33: all.fed2
  Social_character_project[["Insentec"]][[avi_index + 32]] <- append(Social_character_project[["Insentec"]][[avi_index + 32]], all.fed2)
  # sheet 34: all.wat2
  Social_character_project[["Insentec"]][[avi_index + 33]] <- append(Social_character_project[["Insentec"]][[avi_index + 33]], all.wat2)
  # sheet 35: all.comb2
  Social_character_project[["Insentec"]][[avi_index + 34]] <- append(Social_character_project[["Insentec"]][[avi_index + 34]], all.comb2)
  # sheet 36: non_nutritive_visits
  Social_character_project[["Insentec"]][[avi_index + 35]] <- append(Social_character_project[["Insentec"]][[avi_index + 35]], non_nutritive_visits)
  # sheet 37: visited_but_no_feed_record
  Social_character_project[["Insentec"]][[avi_index + 36]] <- append(Social_character_project[["Insentec"]][[avi_index + 36]], visited_but_no_feed_record)
  # sheet 38: visited_but_no_feed_freq
  Social_character_project[["Insentec"]][[avi_index + 37]] <- append(Social_character_project[["Insentec"]][[avi_index + 37]], visited_but_no_feed_freq)
  # sheet 39: culled_cow
  Social_character_project[["Insentec"]][[avi_index + 38]] <- culled_cow
  # sheet 40: warning_days
  Social_character_project[["Insentec"]][[avi_index + 39]] <- warning_days
  # sheet 41: replacement_drinking_list_by_date
  Social_character_project[["Insentec"]][[avi_index + 40]] <- append(Social_character_project[["Insentec"]][[avi_index + 40]], replacement_drinking_list_by_date)  
  # sheet 42: empty_bin_time_detail
  Social_character_project[["Insentec"]][[avi_index + 41]] <- append(Social_character_project[["Insentec"]][[avi_index + 41]], feeding_synch_master_feed_hunger)  
  # sheet: 43: bin_empty_total_time_summary
  Social_character_project[["Insentec"]][[avi_index + 42]] <- rbind(Social_character_project[["Insentec"]][[avi_index + 42]], bin_empty_time_summary)
  
 
  }


# output the social_character_project as a RDA file
out_file <- paste(output_dir, "/social_character_project_all_data.Rda", sep = "")
save(Social_character_project, file = out_file)



###################################################################################################
###                                                                                             ###
### Chapter 4: Milking data                                                                     ###
### Description: The part of the code read in the milking data, creates milking order, and      ###
###              integrate the milking data into the Rda file. It calculates total milking time ###
###              and take it out of the cow's total 24h time budget.                            ###
### Instruction: this part of the code only need to be run once. So this code should be run in  ###
###              in the very end to be added into the giant Rda file generated by super computer###
###                                                                                             ###
###################################################################################################

###################################################################################################
########################################## Milk Order #############################################
###################################################################################################
# clean datasheet, delete NA, change data types
parlor_has_pen <- parlor_final2[which(!is.na(parlor_final2$pen2)), ]
parlor_has_pen$stall_number <- as.numeric(parlor_has_pen$stall_number)
parlor_has_pen$pen <- as.numeric(parlor_has_pen$pen)
parlor_has_pen$pen2 <- as.numeric(parlor_has_pen$pen2)
parlor_has_pen2 <- parlor_has_pen[which(parlor_has_pen$start_time != 0), ]
parlor_has_pen2$dateTime <- ymd_hm(paste(as.character(parlor_has_pen2$date), parlor_has_pen2$start_time), tz="America/Los_Angeles")
parlor_has_pen3 <- parlor_has_pen2[order(parlor_has_pen2$date, parlor_has_pen2$pen, parlor_has_pen2$dateTime, parlor_has_pen2$stall_number),]

# seperate morning milking from afternoon
parlor_has_pen3$hour <- hour(parlor_has_pen3$dateTime)
parlor_has_pen3$milking_order <- 0
parlor_has_pen3$reattached <- "NO"
morning <- parlor_has_pen3[which(parlor_has_pen3$hour <= 11), ]
afternoon <- parlor_has_pen3[which(parlor_has_pen3$hour >= 13), ]
morning2 <- morning[order(morning$date, morning$pen, morning$stall_number, morning$dateTime),]
afternoon2 <- afternoon[order(afternoon$date, afternoon$pen, afternoon$stall_number, afternoon$dateTime),]

### Background information:
# stall number on left side of milking parlor is 1-12, stall number on right side
# of milking parlor is 13-24. every cow got milked 2 times in a day, 1 in the morning 
# and 1 in the afternoon. Cows are grouped and milked by pen. Each pen usually has 
# more than 12 cows. So in each milking for each pen, stall number are registered as
# from 1, 2, 3...12, and then start from 1 again. 

### Algorithm: 
# To determine the milking order, it's not just depends on time. So milking order
# within each pen for each milking, should be: 
# milking order = stall_number + (12 * (n-1))
# n means the number of batch in this milking order.

# handle morning milking first
for (i in 1:nrow(morning2)) {
  # if this is the first row in the entire datasheet, then it starts from 1
  if (i == 1) {
    morning2$milking_order[i] <- 1
    
    # if this is not the first row, then there must always be 1 row above current row
    # if it moved to a new pen, then the first cow in the new pen must be of order 1
  } else if (morning2$pen[i] != morning2$pen[i-1]) {
    morning2$milking_order[i] <- 1
    
    # if it's not the first row, and not the first cow in the new pen, calculate the milking order
  } else {
    # if this is milking in stall 1-12
    if (morning2$stall_number[i] <= 12) {
      # if stall number changed, so moved on to a new stall
      if (morning2$stall_number[i] != morning2$stall_number[i-1]) {
        morning2$milking_order[i] <- morning2$stall_number[i] # milking order equals to stall number
        
        # if stall number did not change from a row above and current row, the same stall
        # then it's most likely a new batch within the same stall
      } else {
        
        # if it's the same stall, the same cow, then it's because the milking attachment droped and was re-attached again.
        if (morning2$cowID[i] == morning2$cowID[i-1]) {
          morning2$reattached[i] <- "YES"
          morning2$milking_order[i] <- morning2$milking_order[i-1]
          
          # if it's the same stall, but different cow, then it's a new batch
        } else {
          morning2$milking_order[i] <- morning2$milking_order[i-1] + 12
        }
        
      }
      
      # if this is milking in stall 13-24
    } else {
      # if stall number changed, so moved on to a new stall
      if (morning2$stall_number[i] != morning2$stall_number[i-1]) {
        morning2$milking_order[i] <- morning2$stall_number[i] - 12 # milking order equals to stall number -12
        
        # if stall number did not change from a row above and current row, the same stall
        # then it's most likely a new batch within the same stall
      } else {
        
        # if it's the same stall, the same cow, then it's because the milking attachment droped and was re-attached again.
        if (morning2$cowID[i] == morning2$cowID[i-1]) {
          morning2$reattached[i] <- "YES" # mark this down as reattachment 
          morning2$milking_order[i] <- morning2$milking_order[i-1] # milking order remain the same
          
          # if it's the same stall, but different cow, then it's a new batch
        } else {
          morning2$milking_order[i] <- morning2$milking_order[i-1] + 12
        }
        
      }
    }
  }
}

morning2 <- morning2[order(morning2$date, morning2$pen, morning2$milking_order),]
morning3 <- morning2[which(morning2$reattached == "NO"),] # exclude the reattachment records
#count duplicated cows in each milking. Sometimes due to data reading errrors, 
# we could get a cow recorded as being milked twiced in the morning/afternoon, 
# which is impossible. We need to exclude data in those days. 
milk_freq_per_cow <- count(morning3, vars = c("cowID", "date"))
# if there are cows milked more than once in a milking event
to_exclude <- milk_freq_per_cow[which(milk_freq_per_cow$freq > 1),]
if (nrow(to_exclude) > 0){
  exclude_date_list <- sort(unique(to_exclude$date))
  for (j in 1:length(exclude_date_list)) {
    morning3 <- morning3[which(morning3$date == exclude_date_list[j]), ]
  }
}


# handle afternoon milking
# handle afternoon milking first
for (i in 1:nrow(afternoon2)) {
  # if this is the first row in the entire datasheet, then it starts from 1
  if (i == 1) {
    afternoon2$milking_order[i] <- 1
    
    # if this is not the first row, then there must always be 1 row above current row
    # if it moved to a new pen, then the first cow in the new pen must be of order 1
  } else if (afternoon2$pen[i] != afternoon2$pen[i-1]) {
    afternoon2$milking_order[i] <- 1
    
    # if it's not the first row, and not the first cow in the new pen, calculate the milking order
  } else {
    # if this is milking in stall 1-12
    if (afternoon2$stall_number[i] <= 12) {
      # if stall number changed, so moved on to a new stall
      if (afternoon2$stall_number[i] != afternoon2$stall_number[i-1]) {
        afternoon2$milking_order[i] <- afternoon2$stall_number[i] # milking order equals to stall number
        
        # if stall number did not change from a row above and current row, the same stall
        # then it's most likely a new batch within the same stall
      } else {
        
        # if it's the same stall, the same cow, then it's because the milking attachment droped and was re-attached again.
        if (afternoon2$cowID[i] == afternoon2$cowID[i-1]) {
          afternoon2$reattached[i] <- "YES"
          afternoon2$milking_order[i] <- afternoon2$milking_order[i-1]
          
          # if it's the same stall, but different cow, then it's a new batch
        } else {
          afternoon2$milking_order[i] <- afternoon2$milking_order[i-1] + 12
        }
        
      }
      
      # if this is milking in stall 13-24
    } else {
      # if stall number changed, so moved on to a new stall
      if (afternoon2$stall_number[i] != afternoon2$stall_number[i-1]) {
        afternoon2$milking_order[i] <- afternoon2$stall_number[i] - 12 # milking order equals to stall number -12
        
        # if stall number did not change from a row above and current row, the same stall
        # then it's most likely a new batch within the same stall
      } else {
        
        # if it's the same stall, the same cow, then it's because the milking attachment droped and was re-attached again.
        if (afternoon2$cowID[i] == afternoon2$cowID[i-1]) {
          afternoon2$reattached[i] <- "YES" # mark this down as reattachment 
          afternoon2$milking_order[i] <- afternoon2$milking_order[i-1] # milking order remain the same
          
          # if it's the same stall, but different cow, then it's a new batch
        } else {
          afternoon2$milking_order[i] <- afternoon2$milking_order[i-1] + 12
        }
        
      }
    }
  }
}

afternoon2 <- afternoon2[order(afternoon2$date, afternoon2$pen, afternoon2$milking_order),]
afternoon3 <- afternoon2[which(afternoon2$reattached == "NO"),] # exclude the reattachment records
#count duplicated cows in each milking. Sometimes due to data reading errrors, 
# we could get a cow recorded as being milked twiced in the afternoon/afternoon, 
# which is impossible. We need to exclude data in those days. 
milk_freq_per_cow <- count(afternoon3, vars = c("cowID", "date"))
# if there are cows milked more than once in a milking event
to_exclude <- milk_freq_per_cow[which(milk_freq_per_cow$freq > 1),]
if (nrow(to_exclude) > 0){
  exclude_date_list <- sort(unique(to_exclude$date))
  for (j in 1:length(exclude_date_list)) {
    afternoon3 <- afternoon3[-which(afternoon3$date == exclude_date_list[j]), ]
  }
}

# merge morning and afternoon data together
parlor_has_pen4 <- rbind(morning3, afternoon3)

# calculate milking order in percentage based on the number of cows in current group
parlor_has_pen7 <- parlor_has_pen4
parlor_has_pen7$time_interval<- NULL
parlor_has_pen7$stall_difference <- NULL
parlor_has_pen7$bout <- NULL
parlor_has_pen7$bout <- 0
parlor_has_pen7$dur <- 0
parlor_has_pen7$dur[nrow(parlor_has_pen7)] <- parlor_has_pen7$milking_order[nrow(parlor_has_pen7)]

for (i in 1:nrow(parlor_has_pen7)) {
  if (i == 1) {
    parlor_has_pen7$bout[i] <- 1
  } else if (parlor_has_pen7$milking_order[i] == 1) {
    parlor_has_pen7$bout[i] <- parlor_has_pen7$bout[i-1] + 1
    parlor_has_pen7$dur[i-1] <- parlor_has_pen7$milking_order[i-1]
  } else {
    parlor_has_pen7$bout[i] <- parlor_has_pen7$bout[i-1]
  }
}

dur_record <- parlor_has_pen7[which(parlor_has_pen7$dur != 0),]
dur_record2 <- dur_record[, c("bout", "dur")]
parlor_has_pen8 <- parlor_has_pen7
parlor_has_pen8$dur <- NULL
parlor_has_pen9 <- merge(parlor_has_pen8, dur_record2, all = TRUE)
parlor_has_pen9$milking_order_percentage <- ((parlor_has_pen9$milking_order)/(parlor_has_pen9$dur))

parlor_has_pen10 <- parlor_has_pen9
parlor_has_pen10$bout <- NULL
parlor_has_pen10$dur <- NULL
save(parlor_has_pen10, file = "milking_order_version2.Rda")

# store it into the giant Rda file
milking_data <- list()
milking_data[[1]] <- parlor_has_pen10
names(milking_data)[1] <- "parlor_clean_data_with_milking_order"
Social_character_project[[3]] <- milking_data
names(Social_character_project)[3] <- "Milking Machine"


###################################################################################################
################################## Individualize milking time #####################################
###################################################################################################
milk_date_list <- names(Social_character_project[["Insentec"]][["Cleaned_combined_original_data"]])
cleaned_hobo <- Social_character_project[["HOBO"]][["duration_for_each_bout"]] 
cleaned_hobo$date <- date(cleaned_hobo$End)

# iterate every day one by one to mark individualized milk time
for (i in 1:length(milk_date_list)) {
  cur_date <- ymd(milk_date_list[i], tz="America/Los_Angeles") 
  cur_insentec <- Social_character_project[["Insentec"]][["Cleaned_combined_original_data"]][[i]] # date for this day
  cur_hobo <- cleaned_hobo[which(cleaned_hobo$date == cur_date),] # date for this day
  cur_milk_cow_list <- unique(cur_insentec$Cow)
  
  # iterate every cow in today's pen 26 to get their morning& afternoon milk start & end time
  for (k in 1:length(cur_milk_cow_list)) {
    
    # create variables and assign them to be NA
    cur_morning_start <- ymd_hms(NA, tz="America/Los_Angeles")
    cur_morning_end <- ymd_hms(NA, tz="America/Los_Angeles")
    cur_afternoon_start <- ymd_hms(NA, tz="America/Los_Angeles")
    cur_afternoon_end <- ymd_hms(NA, tz="America/Los_Angeles")
    last_insentec_end_before_milk_morning <- ymd_hms(NA, tz="America/Los_Angeles")
    last_lying_end_before_milk_morning <- ymd_hms(NA, tz="America/Los_Angeles")
    milk_start_time_hobo_insentec_morning <- ymd_hms(NA, tz="America/Los_Angeles")
    first_insentec_start_after_milking_morning <- ymd_hms(NA, tz="America/Los_Angeles")
    first_lying_start_after_milking_morning <- ymd_hms(NA, tz="America/Los_Angeles")
    milk_end_time_hobo_insentec_morning <- ymd_hms(NA, tz="America/Los_Angeles")
    last_insentec_end_before_milk_afternoon <- ymd_hms(NA, tz="America/Los_Angeles")
    last_lying_end_before_milk_afternoon <- ymd_hms(NA, tz="America/Los_Angeles")
    milk_start_time_hobo_insentec_afternoon <- ymd_hms(NA, tz="America/Los_Angeles")
    first_insentec_start_after_milking_afternoon <-ymd_hms(NA, tz="America/Los_Angeles")
    first_lying_start_after_milking_afternoon <- ymd_hms(NA, tz="America/Los_Angeles")
    milk_end_time_hobo_insentec_afternoon <- ymd_hms(NA, tz="America/Los_Angeles")
    tech_source_before_milking_morning <- NA # record if the time is recorded from HOBO or Insentec
    tech_source_after_milking_morning <- NA
    tech_source_before_milking_afternoon <- NA
    tech_source_after_milking_afternoon <- NA
    
    # get a list of cows for today
    cur_cow <- cur_milk_cow_list[k]
    cur_insentec2 <- cur_insentec[which(cur_insentec$Cow == cur_cow),] # date for this day, this cow
    cur_hobo2 <- cur_hobo[which(cur_hobo$Cow == cur_cow),] # date for this day, this cow
    
    # morning milking
    cur_morning_milk <- morning3[which((morning3$cowID == cur_cow) & (morning3$date == cur_date)),]
    # there are times when milking machine is malfunctioning and lost data for a milking
    # check if we have milking data for current day and current cow, if so, proceed
    if (nrow(cur_morning_milk) > 0) {
      # mark start and end of morning milking
      cur_morning_start <- cur_morning_milk$dateTime[1]
      cur_morning_end <- cur_morning_start + seconds((cur_morning_milk$duration_in_minutes[1]) * 60)
      # get insentec and hobo records before milking start and after milking ends
      ### Before milkng starts
      # last insentec visit before milking
      cur_insentec3 <- cur_insentec2[which(cur_insentec2$End <= cur_morning_start), ]
      cur_insentec3 <- cur_insentec3[order(cur_insentec3$End), ]
      
      if (nrow(cur_insentec3) > 0) {
        last_insentec_end_before_milk_morning <- cur_insentec3$End[nrow(cur_insentec3)]
      }
      # last hobo stand up before milking, last time a cow switched from lying to standing before milking
      cur_hobo3 <- cur_hobo2[which(cur_hobo2$End <= cur_morning_start),]
      cur_hobo3 <- cur_hobo3[order(cur_hobo3$End), ]
      if (nrow(cur_hobo3) > 0) {
        last_lying_end_before_milk_morning <-cur_hobo3$End[nrow(cur_hobo3)]
      }
      # compare which variable is later
      # if there is no insentec visit or lying end events before milking
      if (is.na(last_insentec_end_before_milk_morning) & is.na(last_lying_end_before_milk_morning)) {
        milk_start_time_hobo_insentec_morning <- ymd_hms(NA, tz="America/Los_Angeles")
        tech_source_before_milking_morning <- NA
        
        # if there is no insentec visit before milking, but there is lying end time
      } else if (is.na(last_insentec_end_before_milk_morning)) {
        milk_start_time_hobo_insentec_morning <- last_lying_end_before_milk_morning
        tech_source_before_milking_morning <- "HOBO"
        
        # if there is no lying end time before milking, but there is insentec visit before milking
      } else if (is.na(last_lying_end_before_milk_morning)) {
        milk_start_time_hobo_insentec_morning <- last_insentec_end_before_milk_morning
        tech_source_before_milking_morning <- "Insentec"
        
        # if last insentec end is earlier than last time a cow switched from lying to standing
      } else if (last_insentec_end_before_milk_morning < last_lying_end_before_milk_morning) {
        milk_start_time_hobo_insentec_morning <- last_lying_end_before_milk_morning
        tech_source_before_milking_morning <- "HOBO"
        
        # if last time a cow switched from lying to standing is earlier than last insentec end
      } else {
        milk_start_time_hobo_insentec_morning <- last_insentec_end_before_milk_morning
        tech_source_before_milking_morning <- "Insentec"
        
      }
      
      ### After milking ends
      # first insentec visit after milking
      cur_insentec5 <- cur_insentec2[which(cur_insentec2$Start >= cur_morning_end), ]
      cur_insentec5 <- cur_insentec5[order(cur_insentec5$Start), ]
      if (nrow(cur_insentec5) > 0) {
        # there is a chance they don't eat or drink at all after milking
        first_insentec_start_after_milking_morning <- cur_insentec5$Start[1]
      }
      # first hobo lie down after milking, first time a cow switched from standing to lying after milking
      cur_hobo5 <- cur_hobo2[which(cur_hobo2$Start >= cur_morning_end),]
      cur_hobo5 <- cur_hobo5[order(cur_hobo5$Start), ]
      if (nrow(cur_hobo5) > 0) {
        first_lying_start_after_milking_morning <- cur_hobo5$Start[1]
      }
      # compare which variable is first
      # if first insentec is earlier than first hobo
      if (is.na(first_insentec_start_after_milking_morning) & is.na(first_lying_start_after_milking_morning)) {
        milk_end_time_hobo_insentec_morning <- ymd_hms(NA, tz="America/Los_Angeles")
        tech_source_after_milking_morning <- NA
        
        # if there is no insentec visit before milking, but there is lying end time
      } else if (is.na(first_insentec_start_after_milking_morning)) {
        milk_end_time_hobo_insentec_morning <- first_lying_start_after_milking_morning
        tech_source_after_milking_morning <- "HOBO"
        
        # if there is no lying end time before milking, but there is insentec visit before milking
      } else if (is.na(first_lying_start_after_milking_morning)) {
        milk_end_time_hobo_insentec_morning <- first_insentec_start_after_milking_morning
        tech_source_after_milking_morning <- "Insentec"
        
        # if first insentec start is earlier than first time a cow switched from standing to lying
      } else if (first_insentec_start_after_milking_morning < first_lying_start_after_milking_morning) {
        milk_end_time_hobo_insentec_morning <- first_insentec_start_after_milking_morning
        tech_source_after_milking_morning <- "Insentec"
        
      } else {
        milk_end_time_hobo_insentec_morning <- first_lying_start_after_milking_morning
        tech_source_after_milking_morning <- "HOBO"
      }
    }
    
    
    # afternoon milking
    cur_afternoon_milk <- afternoon3[which((afternoon3$cowID == cur_cow) & (afternoon3$date == cur_date)),]
    # there are times when milking machine is malfunctioning and lost data for a milking
    # check if we have milking data for current day and current cow, if so, proceed
    if (nrow(cur_afternoon_milk) > 0) {
      # mark start and end of afternoon milking
      cur_afternoon_start <- cur_afternoon_milk$dateTime[1]
      cur_afternoon_end <- cur_afternoon_start + seconds((cur_afternoon_milk$duration_in_minutes[1]) * 60)
      # get insentec and hobo records before milking start and after milking ends
      ### Before milkng starts
      # last insentec visit before milking
      cur_insentec3 <- cur_insentec2[which(cur_insentec2$End <= cur_afternoon_start), ]
      cur_insentec3 <- cur_insentec3[order(cur_insentec3$End), ]
      
      if (nrow(cur_insentec3) > 0) {
        last_insentec_end_before_milk_afternoon <- cur_insentec3$End[nrow(cur_insentec3)]
      }
      # last hobo stand up before milking, last time a cow switched from lying to standing before milking
      cur_hobo3 <- cur_hobo2[which(cur_hobo2$End <= cur_afternoon_start),]
      cur_hobo3 <- cur_hobo3[order(cur_hobo3$End), ]
      if (nrow(cur_hobo3) > 0) {
        last_lying_end_before_milk_afternoon <-cur_hobo3$End[nrow(cur_hobo3)]
      }
      # compare which variable is later
      # if there is no insentec visit or lying end events before milking
      if (is.na(last_insentec_end_before_milk_afternoon) & is.na(last_lying_end_before_milk_afternoon)) {
        milk_start_time_hobo_insentec_afternoon <- ymd_hms(NA, tz="America/Los_Angeles")
        tech_source_before_milking_afternoon <- NA
        
        # if there is no insentec visit before milking, but there is lying end time
      } else if (is.na(last_insentec_end_before_milk_afternoon)) {
        milk_start_time_hobo_insentec_afternoon <- last_lying_end_before_milk_afternoon
        tech_source_before_milking_afternoon <- "HOBO"
        
        # if there is no lying end time before milking, but there is insentec visit before milking
      } else if (is.na(last_lying_end_before_milk_afternoon)) {
        milk_start_time_hobo_insentec_afternoon <- last_insentec_end_before_milk_afternoon
        tech_source_before_milking_afternoon <- "Insentec"
        
        # if last insentec end is earlier than last time a cow switched from lying to standing
      } else if (last_insentec_end_before_milk_afternoon < last_lying_end_before_milk_afternoon) {
        milk_start_time_hobo_insentec_afternoon <- last_lying_end_before_milk_afternoon
        tech_source_before_milking_afternoon <- "HOBO"
        
        # if last time a cow switched from lying to standing is earlier than last insentec end
      } else {
        milk_start_time_hobo_insentec_afternoon <- last_insentec_end_before_milk_afternoon
        tech_source_before_milking_afternoon <- "Insentec"
        
      }
      
      ### After milking ends
      # first insentec visit after milking
      cur_insentec5 <- cur_insentec2[which(cur_insentec2$Start >= cur_afternoon_end), ]
      cur_insentec5 <- cur_insentec5[order(cur_insentec5$Start), ]
      if (nrow(cur_insentec5) > 0) {
        # there is a chance they don't eat or drink at all after milking
        first_insentec_start_after_milking_afternoon <- cur_insentec5$Start[1]
      }
      # first hobo lie down after milking, first time a cow switched from standing to lying after milking
      cur_hobo5 <- cur_hobo2[which(cur_hobo2$Start >= cur_afternoon_end),]
      cur_hobo5 <- cur_hobo5[order(cur_hobo5$Start), ]
      if (nrow(cur_hobo5) > 0) {
        first_lying_start_after_milking_afternoon <- cur_hobo5$Start[1]
      }
      # compare which variable is first
      # if first insentec is earlier than first hobo
      if (is.na(first_insentec_start_after_milking_afternoon) & is.na(first_lying_start_after_milking_afternoon)) {
        milk_end_time_hobo_insentec_afternoon <- ymd_hms(NA, tz="America/Los_Angeles")
        tech_source_after_milking_afternoon <- NA
        
        # if there is no insentec visit before milking, but there is lying end time
      } else if (is.na(first_insentec_start_after_milking_afternoon)) {
        milk_end_time_hobo_insentec_afternoon <- first_lying_start_after_milking_afternoon
        tech_source_after_milking_afternoon <- "HOBO"
        
        # if there is no lying end time before milking, but there is insentec visit before milking
      } else if (is.na(first_lying_start_after_milking_afternoon)) {
        milk_end_time_hobo_insentec_afternoon <- first_insentec_start_after_milking_afternoon
        tech_source_after_milking_afternoon <- "Insentec"
        
        # if first insentec start is earlier than first time a cow switched from standing to lying
      } else if (first_insentec_start_after_milking_afternoon < first_lying_start_after_milking_afternoon) {
        milk_end_time_hobo_insentec_afternoon <- first_insentec_start_after_milking_afternoon
        tech_source_after_milking_afternoon <- "Insentec"
        
      } else {
        milk_end_time_hobo_insentec_afternoon <- first_lying_start_after_milking_afternoon
        tech_source_after_milking_afternoon <- "HOBO"
      }
    }
    
  
    
    temp <- data.frame(cur_cow, cur_date, milk_start_time_hobo_insentec_morning, tech_source_before_milking_morning, cur_morning_start, cur_morning_end, milk_end_time_hobo_insentec_morning, tech_source_after_milking_morning, milk_start_time_hobo_insentec_afternoon, tech_source_before_milking_afternoon, cur_afternoon_start, cur_afternoon_end, milk_end_time_hobo_insentec_afternoon, tech_source_after_milking_afternoon)
    colnames(temp) <- c("Cow", "date", "anticipated_milk_start_morning", "tech_source_before_milking_morning", "parlor_milk_start_morning", "parlor_milk_end_morning", "anticipated_milk_end_morning", "tech_source_after_milking_morning", "anticipated_milk_start_afternoon", "tech_source_before_milking_afternoon", "parlor_milk_start_afternoon", "parlor_milk_end_afternoon", "anticipated_milk_end_afternoon", "tech_source_after_milking_afternoon")
    if ((i == 1) & (k == 1)) {
      individual_milk_time <- temp
    } else {
      individual_milk_time <- rbind(individual_milk_time, temp)
    }
  }
}



# get the time interval between last cow stand up and first milking start
# morning
individual_milk_time$morning_anticipated_to_milking_start <- individual_milk_time$anticipated_milk_start_morning %--% individual_milk_time$parlor_milk_start_morning
individual_milk_time$morning_anticipated_to_milking_start <- seconds(as.duration(individual_milk_time$morning_anticipated_to_milking_start))
individual_milk_time$morning_milking_end_to_anticipated <- individual_milk_time$parlor_milk_end_morning %--% individual_milk_time$anticipated_milk_end_morning
individual_milk_time$morning_milking_end_to_anticipated <- seconds(as.duration(individual_milk_time$morning_milking_end_to_anticipated))
# afternoon
individual_milk_time$afternoon_anticipated_to_milking_start <- individual_milk_time$anticipated_milk_start_afternoon %--% individual_milk_time$parlor_milk_start_afternoon
individual_milk_time$afternoon_anticipated_to_milking_start <- seconds(as.duration(individual_milk_time$afternoon_anticipated_to_milking_start))
individual_milk_time$afternoon_milking_end_to_anticipated <- individual_milk_time$parlor_milk_end_afternoon %--% individual_milk_time$anticipated_milk_end_afternoon
individual_milk_time$afternoon_milking_end_to_anticipated <- seconds(as.duration(individual_milk_time$afternoon_milking_end_to_anticipated))

# store results
milking_data[[2]] <- individual_milk_time
names(milking_data)[2] <- "individualized_milking_time"
Social_character_project[[3]] <- milking_data
names(Social_character_project)[3] <- "Milking Machine"




###################################################################################################
######################## Deduct milking time away from 24h time budget ############################
###################################################################################################
# get a datasheet only including the milk time of the cow
parlor_data <- parlor_has_pen10
parlor_data$duration_in_sec <- as.integer(60 * parlor_data$duration_in_minutes)
parlor_data$end_time <- parlor_data$dateTime + seconds(parlor_data$duration_in_sec)
milking_time <- parlor_data[, c("cowID", "pen", "date", "dateTime", "duration_in_sec", "end_time")]
names(milking_time)[names(milking_time) == "dateTime"] <- "start_time"
names(milking_time)[names(milking_time) == "cowID"] <- "Cow"
milking_time$Cow <- as.integer(milking_time$Cow)
milking_time$date <- ymd(milking_time$date, tz="America/Los_Angeles")
milking_time <- milking_time[which(milking_time$pen == 26), ] # only keep wali's cows in pen 26
# summarize morning and afternoon milking to be 1 milking time
total_milking_time <- aggregate(milking_time[, "duration_in_sec"], list(milking_time$Cow, milking_time$date), sum)
colnames(total_milking_time) <- c("Cow", "date", "total_milking_dur(s)")

# get a datasheet only including the feeding and drinking time of the cow
Insentec_final_summary <- Social_character_project[[2]][[1]]
feed_drink_time <- Insentec_final_summary[, c("date", "Cow", "Feeding_Duration(s)", "Drinking_Duration(s)")]
feed_drink_time$date <- ymd(feed_drink_time$date, tz="America/Los_Angeles")

# get standing and lying time
HOBO_summary <- Social_character_project[["HOBO"]][["lying_standing_summary_by_date"]][, c("Cow", "date", "standing_time(seconds)", "lying_time(seconds)", "total(seconds)")]
HOBO_summary$date <- ymd(HOBO_summary$date, tz="America/Los_Angeles")

# merge feeding, drinking, and milking time together
feed_drink_milk_time <- merge(total_milking_time, feed_drink_time)
feed_drink_milk_time$sum_feed_drink_milk_time <- (feed_drink_milk_time$`total_milking_dur(s)` + feed_drink_milk_time$`Feeding_Duration(s)` + feed_drink_milk_time$`Drinking_Duration(s)`)

# merge feeding, drinking, milking, and standing together
stand_lie_feed_drink_milk_time <- merge(feed_drink_milk_time, HOBO_summary)

# get the 24h - feeding time - drinking time - milking time - 10 minutes before milking - 10 minutes after milking
stand_lie_feed_drink_milk_time$total_minus_feed_drink_milk <- stand_lie_feed_drink_milk_time$`total(seconds)` - stand_lie_feed_drink_milk_time$sum_feed_drink_milk_time - (20 * 60)
stand_lie_feed_drink_milk_time$lying_percentage <- (stand_lie_feed_drink_milk_time$`lying_time(seconds)`/stand_lie_feed_drink_milk_time$total_minus_feed_drink_milk)

# the amount of time when the cow is just standing, not drinking, feeding or milking. this could be negative because some days we get incomplete HOBO files that is less than 24 h 
stand_lie_feed_drink_milk_time$standing_for_nothing <- stand_lie_feed_drink_milk_time$`standing_time(seconds)` - stand_lie_feed_drink_milk_time$sum_feed_drink_milk_time

# save the datasheet to be in the Rda file under HOBO
Social_character_project[["HOBO"]][[(length(Social_character_project[["HOBO"]])+1)]] <- stand_lie_feed_drink_milk_time
names(Social_character_project[["HOBO"]])[(length(Social_character_project[["HOBO"]])+1)] <- "standing_time_with_milking_excluded"


###################################################################################################
################ Frequency of being in the first / last batch of milking process ##################
###################################################################################################
# only include data in the experiment pen (pen 26)
first_last_batch <- parlor_has_pen9[which(parlor_has_pen9$pen == 26), ]
# only include dates during the experiment period
first_last_batch <- first_last_batch[which((first_last_batch$date >= "2020-07-13") & (first_last_batch$date <= "2021-05-30")), ]
colnames(first_last_batch)[which(names(first_last_batch) == "cowID")] <- "Cow"

# because the pen size of pen 26 changed over 1 year, sometimes there are 49 cows, sometimes there are 53, or more,
# we create a variable to mark the percentage cutoff of first 12 cows and last 12 cows based on group size
first_last_batch$first_batch_target <- 12/first_last_batch$dur
first_last_batch$last_batch_target <- ((first_last_batch$dur-11)/first_last_batch$dur)

# mark cows as "first" if they were milked as the first batch (first 12) in the milking process
# mark cows as "last" if they were milked as the last batch (last 12) in the milking process
first_last_batch$first_batch <- ""
first_last_batch$last_batch <- ""
first_last_batch[which((first_last_batch$milking_order_percentage <= first_last_batch$first_batch_target)), c("first_batch")] <- "first"
first_last_batch[which((first_last_batch$milking_order_percentage >= first_last_batch$last_batch_target)), c("last_batch")] <- "last"

# calculate the number of times they were in the first batch or the last batch in the milking process
first_cow <- first_last_batch[which(first_last_batch$first_batch == "first"),]
last_cow <- first_last_batch[which(first_last_batch$last_batch == "last"),]
first_cow_freq <- count(first_cow, vars=c("Cow", "first_batch"))
colnames(first_cow_freq) <- c("Cow", "first_batch", "first_batch_of_pen_freq")
first_cow_freq$first_batch <- NULL
last_cow_freq <- count(last_cow, vars=c("Cow", "last_batch"))
colnames(last_cow_freq) <- c("Cow", "last_batch", "last_batch_of_pen_freq")
last_cow_freq$last_batch <- NULL
first_and_last_cow_freq <- merge(first_cow_freq, last_cow_freq, all = TRUE)
first_and_last_cow_freq[is.na(first_and_last_cow_freq)] <- 0

# total number of milkings this cow went through in pen 26. Or the period when each cow 
# is included in the experiment
total_num_of_milking <- count(first_last_batch, vars=c("Cow"))
colnames(total_num_of_milking) <- c("Cow", "total_milking_num")

# combine the frequency of being in the first or last batch of the milking process, 
# with the total number of milkings a cow went through
first_and_last_cow_freq2 <- merge(first_and_last_cow_freq, total_num_of_milking)
first_and_last_cow_freq2$first_batch_of_pen_freq_percentage <- (first_and_last_cow_freq2$first_batch_of_pen_freq/first_and_last_cow_freq2$total_milking_num)
first_and_last_cow_freq2$lastt_batch_of_pen_freq_percentage <- (first_and_last_cow_freq2$last_batch_of_pen_freq/first_and_last_cow_freq2$total_milking_num)
write.csv(first_and_last_cow_freq2, "frequency_of_being_in_first_and_last_batch.csv")

plot(first_and_last_cow_freq2$first_batch_of_pen_freq_percentage, first_and_last_cow_freq2$lastt_batch_of_pen_freq_percentage)

###################################################################################################
###                                                                                             ###
### Chapter 5: THI data                                                                         ###
### Description: The part of the code read in the raw Temperature, Humidity HOBO files and      ###
###              creates THI index.                                                             ###
### Instruction: this part of the code only need to be run once. So this code should be run in  ###
###              in the very end to be added into the giant Rda file generated by super computer###
###                                                                                             ###
###################################################################################################
############################################ Data Cleaning ########################################
# set work directory (folder) to the location of the file you wish to import
data_origin <- "C:/Users/skysheng/OneDrive - The University Of British Columbia/University of British Columbia/Research/Master's Project/Lameness one year trial/THI Data/Wali's trial/Raw data all csv"
output_folder <- "C:/Users/skysheng/OneDrive - The University Of British Columbia/University of British Columbia/Research/Master's Project/Lameness one year trial/THI Data/Wali's trial/clean data"
setwd(data_origin)
# list all the csv files in the folder in the path
list_file = list.files(path=data_origin, pattern="*.csv", full.names=TRUE) 


# read in all csv files 
for (i in 1:length(list_file)) {
  temp <- read.csv(list_file[i], header = TRUE, skip = 1)
  temp <- na.omit(temp) # delete all the NA in your data sheet
  temp2 <- temp[, c(2, 3, 4)] # include all the rows, but only include the 2, 3, 4 columns.
  colnames(temp2) <- c("dateTime", "temperature(F)", "relative_humidity(%)") # change the 3 column names
  temp2$dateTime <-mdy_hms(temp2$dateTime, tz = "America/Los_Angeles")
  temp3 <- temp2
  name_split <- strsplit(list_file[i], "_")  # split the filename (list_file[i]) string by "_"
  cur_spot <- name_split[[1]][2] # get the spot number
  cur_spot <- as.integer(substr(cur_spot, nchar(cur_spot), nchar(cur_spot)))
  temp3$spot <- cur_spot
  #delete the first 30 minutes and the last 30 minutes of the file because that's 
  #the human handling time putting on and taking off the HOBOs 
  # HOBO records Temperature and Humidity data every 5 minutes
  temp5 <- temp3[-c(1:6, (nrow(temp3)-5):nrow(temp3)),] 
  
  
  ##################################### Daylight saving change ################################
  cur_year <- year(temp5$dateTime[1])
  cur_year2 <- as.integer(cur_year)
  cur_year_line <- daylight_saving_table[which(daylight_saving_table$Year == cur_year2),]
  cur_month <- as.integer(month(temp5$dateTime[1]))
  # determine if current period is in the spring or fall
  if (cur_month >= 8) {  # this is fall
    daylight_change_date <- cur_year_line$Fall[1]
    daylight_change_next_date <- cur_year_line$Fall_nextDay[1]
    daylight_change_time <- ymd_hms(paste(as.character(daylight_change_date), "02:00:00", sep = " "), tz = "America/Los_Angeles")
    daylight_change_time2 <- ymd_hms(paste(as.character(daylight_change_date), "03:00:00", sep = " "), tz = "America/Los_Angeles")
    
    # determine if current data sheet contains daylight saving dates
    contain_daylight_change <- temp5[which(temp5$dateTime == daylight_change_time), ]
    if (nrow(contain_daylight_change) >  0) {
      before_change <- temp5[which(temp5$dateTime <= daylight_change_time), ]
      after_change <- temp5[which(temp5$dateTime > daylight_change_time2), ]
      after_change$dateTime <- after_change$dateTime - hours(1)
      temp5 <- rbind(before_change, after_change)
    }
    
    
  } else { # this is spring
    daylight_change_date <- cur_year_line$Spring[1]
    daylight_change_next_date <- cur_year_line$Spring_nextDay[1]
    daylight_change_time <- ymd_hms(paste(as.character(daylight_change_date), "01:55:00", sep = " "), tz = "America/Los_Angeles")
    daylight_change_time2 <- ymd_hms(paste(as.character(daylight_change_date), "03:00:00", sep = " "), tz = "America/Los_Angeles")
    
    # determine if current data sheet contains daylight saving dates
    contain_daylight_change <- temp5[which(temp5$dateTime == daylight_change_time), ]
    if (nrow(contain_daylight_change) >  0) {
      before_change <- temp5[which(temp5$dateTime <= daylight_change_time), ]
      after_change <- temp5[which(temp5$dateTime >= daylight_change_time2), ]
      
      middle_change <- temp5[is.na(temp5$dateTime), ]
      middle_change$dateTime[1] <- daylight_change_time2
      for (a in 2:nrow(middle_change)) {
        middle_change$dateTime[a] <- middle_change$dateTime[a-1] + minutes(5) # HOBO reads every 5 minutes
      }
      after_change$dateTime <- after_change$dateTime + hours(1)
      temp5 <- rbind(before_change, middle_change)
      temp5 <- rbind(temp5, after_change)
    }
  }
  
  
  # if this is the first file
  if (i == 1) {
    master <- temp5
  
    # if this is not the first file
  } else {
    master <- rbind(master, temp5)
  }
}

master <- master[order(master$dateTime),] # sort the datasheet based on dateTime
master$date <- date(master$dateTime)
# convert temperature unit from F to C
master$`temperature(C)` <- ((master$`temperature(F)` - 32)/1.8)
master$THI <- (0.8*(master$`temperature(C)`)) + ((master$`relative_humidity(%)`)*0.01*((master$`temperature(C)`)-14.4)) + 46.4
  
############################################## Temperature ########################################
# get the average temperature for each day
average_temp <- aggregate(master[, "temperature(C)"], list(master$date), mean)
colnames(average_temp) <- c("date", "temperature(C)_mean")
# get the SD temperature for each day
sd_temp <- aggregate(master[, "temperature(C)"], list(master$date), sd)
colnames(sd_temp) <- c("date", "temperature(C)_standard_deviation")
# get the min temperature for each day
min_temp <- aggregate(master[, "temperature(C)"], list(master$date), min)
colnames(min_temp) <- c("date", "temperature(C)_min")
# get the max temperature for each day
max_temp <- aggregate(master[, "temperature(C)"], list(master$date), max)
colnames(max_temp) <- c("date", "temperature(C)_max")

############################################## Humidity ###########################################
# get the average humidity for each day
average_humidity <- aggregate(master[, "relative_humidity(%)"], list(master$date), mean)
colnames(average_humidity) <- c("date", "relative_humidity(%)_mean")
# get the SD humidity for each day
sd_humidity <- aggregate(master[, "relative_humidity(%)"], list(master$date), sd)
colnames(sd_humidity) <- c("date", "relative_humidity(%)_standard_deviation")
# get the min humidity for each day
min_humidity <- aggregate(master[, "relative_humidity(%)"], list(master$date), min)
colnames(min_humidity) <- c("date", "relative_humidity(%)_min")
# get the max humidity for each day
max_humidity <- aggregate(master[, "relative_humidity(%)"], list(master$date), max)
colnames(max_humidity) <- c("date", "relative_humidity(%)_max")

############################################## THI ################################################
# get the average THI for each day
average_THI <- aggregate(master[, "THI"], list(master$date), mean)
colnames(average_THI) <- c("date", "THI_mean")
# get the SD THI for each day
sd_THI <- aggregate(master[, "THI"], list(master$date), sd)
colnames(sd_THI) <- c("date", "THI_standard_deviation")
# get the min THI for each day
min_THI <- aggregate(master[, "THI"], list(master$date), min)
colnames(min_THI) <- c("date", "THI_min")
# get the max THI for each day
max_THI <- aggregate(master[, "THI"], list(master$date), max)
colnames(max_THI) <- c("date", "THI_max")

######################################### Summary #################################################
# merge all the aggregated information from above to a summary datasheet
aggregated_sheets <- list(average_temp, sd_temp, min_temp, max_temp, average_humidity, sd_humidity, min_humidity, max_humidity, average_THI, sd_THI, min_THI, max_THI)
master_summary <- Reduce(function(x, y) merge(x, y, all=TRUE), aggregated_sheets)

################################### Export results ################################################
# direct to the output directory
setwd(output_folder)
write.csv(master_summary, "Wali_trial_summarized_THI.csv")
save(master_summary, file = "Wali_trial_summarized_THI.Rda")

# save in the giant Rda
Social_character_project[[4]] <- master_summary
names(Social_character_project)[4] <- "THI"


###################################################################################################
###                                                                                             ###
### Chapter 6: Feed data                                                                        ###
### Description: The part of the code read in the raw feed data PDF files and transform it to   ###
###              a datasheet format. It calculates meal pattern as well.                        ###
### Instruction: this part of the code only need to be run once. So this code should be run in  ###
###              in the very end to be added into the giant Rda file generated by super computer###
###                                                                                             ###
###################################################################################################

###################################################################################################
##################################### Dry Matter Intake (DMI) #####################################
###################################################################################################
# convert pdf file into a list
dmi_pdf <- pdf_text("C:/Users/skysheng/OneDrive - The University Of British Columbia/University of British Columbia/Research/Master's Project/Lameness one year trial/Feed composition & nutrition/DMIntakeUBC.pdf") %>% 
  str_split("\n")

# clean the list to remove header and footer on each page
for (i in 1:length(dmi_pdf)) {
  # if this is the first page
  if (i == 1) {
    # delete header text, and text at bottom of each page
    dmi_pdf[[i]] <- dmi_pdf[[i]][-c(1:15, (length(dmi_pdf[[i]])-1):length(dmi_pdf[[i]]))]  
    dmi_pdf_master <- dmi_pdf[[i]]
  } else if (i == length(dmi_pdf)) {
    # delete header text, and text at bottom of each page
    dmi_pdf[[i]] <- dmi_pdf[[i]][-c(1:7, (length(dmi_pdf[[i]])-5):length(dmi_pdf[[i]]))]  
    dmi_pdf_master <- append(dmi_pdf_master, dmi_pdf[[i]])
  }else {
    # delete header text, and text at bottom of each page
    dmi_pdf[[i]] <- dmi_pdf[[i]][-c(1:7, (length(dmi_pdf[[i]])-1):length(dmi_pdf[[i]]))]  
    dmi_pdf_master <- append(dmi_pdf_master, dmi_pdf[[i]])
  }
  dmi_pdf[[i]] <- str_squish(dmi_pdf[[i]])
}

# delete extra white space before, after and between items 
dmi_pdf_master <- str_squish(dmi_pdf_master)

# create a 2 dimensional list to store data for each column to prepare for table convertion
dmi_table_list <- list()
for (j in 1:length(dmi_pdf_master)) {
  dmi_table_list[[j]] <- strsplit(dmi_pdf_master[j], " ")[[1]] # seperate items in each row by white space
}

# now the 2 dimensional list's rows is of different length, fix this 
# by adding NAs to places where there is missing values, so that each row has 
# the same length
dmi_table_list2 <- list()
for (i in 1:length(dmi_table_list)) {
  # if this is the first row, the first element in the list is pen number
  if (i == 1) {
    element1 <- paste(dmi_table_list[[i]][1], dmi_table_list[[i]][2], sep = " ") # merge "pen" and "26" together to be 1 element
    link1 <- append(c(element1), dmi_table_list[[i]][3:6])
    link2 <- append(link1, c(NA, NA)) # add 2 NAs to save space for 2 empty columns
    link3 <- append(link2, dmi_table_list[[i]][7:length(dmi_table_list[[i]])])
    dmi_table_list2[[i]] <- link3
    
    # if there are 6 elements in a row, add a NA in the begining, and 2 NA in the middle
  } else if (length(dmi_table_list[[i]]) == 6) {
    link1 <- append(c(NA), dmi_table_list[[i]][1:4])
    link2 <- append(link1, c(NA, NA))
    link3 <- append(link2, dmi_table_list[[i]][5:length(dmi_table_list[[i]])])
    dmi_table_list2[[i]] <- link3
    
    # if there are 8 elements in a row, add a NA in the begining
  } else if (length(dmi_table_list[[i]]) == 8) {
    dmi_table_list2[[i]] <- append(c(NA), dmi_table_list[[i]])
    
    # if this is the third row to the last
  } else if (i == (length(dmi_table_list) - 2)) {
    # merge words together as 1 element
    element1 <- paste(dmi_table_list[[i]][1], dmi_table_list[[i]][2], dmi_table_list[[i]][3], sep = " ") 
    link1 <- c(element1, NA)
    link2 <- append(link1, dmi_table_list[[i]][4:length(dmi_table_list[[i]])])
    dmi_table_list2[[i]] <- link2
    
    # if this is the second row to the last
  } else if (i == (length(dmi_table_list) - 1)) {
    # do nothing, delete this row
    
    # if this is the last row
  } else if (i == (length(dmi_table_list))) {
    # merge words together as 1 element
    element1 <- paste(dmi_table_list[[i]][1], dmi_table_list[[i]][2],  sep = " ") 
    link1 <- c(element1, NA)
    link2 <- append(link1, dmi_table_list[[i]][3:length(dmi_table_list[[i]])])
    dmi_table_list2[[(i-1)]] <- link2 # since the previous row was deleted, index - 1
  }
}

# convert 2 dimensional list into a dataframe
dmi_table <- data.frame(t(data.frame(dmi_table_list2)))
colnames(dmi_table) <- c("Pen_Name", "Report_Date", "Avg_Pen_Count", "Tot_Dropped", "Avg_Dropped", "Tot_Weighbacks", "Avg_Weighbacks", "Total DM", "Avg_DM/Animal")
rownames(dmi_table) <- seq(1, nrow(dmi_table), by = 1)

# save as a csv file and Rda file
# direct to the output directory
write.csv(dmi_table, "C:/Users/skysheng/OneDrive - The University Of British Columbia/University of British Columbia/Research/Master's Project/Lameness one year trial/Feed composition & nutrition/DMI.csv")
save(dmi_table, file = "C:/Users/skysheng/OneDrive - The University Of British Columbia/University of British Columbia/Research/Master's Project/Lameness one year trial/Feed composition & nutrition/DMI.Rda")


###################################################################################################
########################################## As Fed (AF) ############################################
###################################################################################################
# convert pdf file into a list
af_pdf <- pdf_text("C:/Users/skysheng/OneDrive - The University Of British Columbia/University of British Columbia/Research/Master's Project/Lameness one year trial/Feed composition & nutrition/AFIntakeUBC.pdf") %>% 
  str_split("\n")

# clean the list to remove header and footer on each page
for (i in 1:length(af_pdf)) {
  # if this is the first page
  if (i == 1) {
    # delete header text, and text at bottom of each page
    af_pdf[[i]] <- af_pdf[[i]][-c(1:13, (length(af_pdf[[i]])-2):length(af_pdf[[i]]))]  
    af_pdf_master <- af_pdf[[i]]
  } else if (i == length(af_pdf)) {
    # delete header text, and text at bottom of each page
    af_pdf[[i]] <- af_pdf[[i]][-c(1:7, (length(af_pdf[[i]])-5):length(af_pdf[[i]]))]  
    af_pdf_master <- append(af_pdf_master, af_pdf[[i]])
  }else {
    # delete header text, and text at bottom of each page
    af_pdf[[i]] <- af_pdf[[i]][-c(1:7, (length(af_pdf[[i]])-1):length(af_pdf[[i]]))]  
    af_pdf_master <- append(af_pdf_master, af_pdf[[i]])
  }
  af_pdf[[i]] <- str_squish(af_pdf[[i]])
}

# delete extra white space before, after and between items 
af_pdf_master <- str_squish(af_pdf_master)

# create a 2 dimensional list to store data for each column to prepare for table convertion
af_table_list <- list()
for (j in 1:length(af_pdf_master)) {
  af_table_list[[j]] <- strsplit(af_pdf_master[j], " ")[[1]] # seperate items in each row by white space
}

# now the 2 dimensional list's rows is of different length, fix this 
# by adding NAs to places where there is missing values, so that each row has 
# the same length
af_table_list2 <- list()
for (i in 1:length(af_table_list)) {
  # if this is the first row, the first element in the list is pen number
  if (i == 1) {
    element1 <- paste(af_table_list[[i]][1], af_table_list[[i]][2], sep = " ") # merge "pen" and "26" together to be 1 element
    af_table_list2[[i]] <- append(c(element1), af_table_list[[i]][3:length(af_table_list[[i]])])
    
    # if there are 6 elements in a row, add a NA in the begining, and 2 NA in the middle
  } else if (length(af_table_list[[i]]) == 6) {
    link1 <- append(c(NA), af_table_list[[i]][1:4])
    link2 <- append(link1, c(NA, NA))
    link3 <- append(link2, af_table_list[[i]][5:length(af_table_list[[i]])])
    af_table_list2[[i]] <- link3
    
    # if there are 8 elements in a row, add a NA in the begining
  } else if (length(af_table_list[[i]]) == 8) {
    af_table_list2[[i]] <- append(c(NA), af_table_list[[i]])
    
    # if this is the third row to the last
  } else if (i == (length(af_table_list) - 2)) {
    # merge words together as 1 element
    element1 <- paste(af_table_list[[i]][1], af_table_list[[i]][2], af_table_list[[i]][3], sep = " ") 
    link1 <- c(element1, NA)
    link2 <- append(link1, af_table_list[[i]][4:length(af_table_list[[i]])])
    af_table_list2[[i]] <- link2
    
    # if this is the second row to the last
  } else if (i == (length(af_table_list) - 1)) {
    # do nothing, delete this row
    
    # if this is the last row
  } else if (i == (length(af_table_list))) {
    # merge words together as 1 element
    element1 <- paste(af_table_list[[i]][1], af_table_list[[i]][2],  sep = " ") 
    link1 <- c(element1, NA)
    link2 <- append(link1, af_table_list[[i]][3:length(af_table_list[[i]])])
    af_table_list2[[(i-1)]] <- link2 # since the previous row was deleted, index - 1
  }
}

# convert 2 dimensional list into a dataframe
af_table <- data.frame(t(data.frame(af_table_list2)))
colnames(af_table) <- c("Pen_Name", "Report_Date", "Avg_Pen_Count", "Tot_Dropped", "Avg_Dropped", "Tot_Weighbacks", "Avg_Weighbacks", "Total AF", "Avg_AF/Animal")
rownames(af_table) <- seq(1, nrow(af_table), by = 1)

# save as a csv file and Rda file
# direct to the output directory
write.csv(af_table, "C:/Users/skysheng/OneDrive - The University Of British Columbia/University of British Columbia/Research/Master's Project/Lameness one year trial/Feed composition & nutrition/af.csv")
save(af_table, file = "C:/Users/skysheng/OneDrive - The University Of British Columbia/University of British Columbia/Research/Master's Project/Lameness one year trial/Feed composition & nutrition/af.Rda")




################################################ END ##############################################

