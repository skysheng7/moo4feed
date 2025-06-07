library(lubridate)


location <- "vancouver"
if (location == "vancouver") {
  setwd("C:/Users/skysheng/OneDrive - The University Of British Columbia/University of British Columbia/Research/PhD Project/Lameness one year trial/Super Computer Computation/hobo_insentec_round6/result/HOBO_insentec_milking/Insentec") 
  output_dir <- "C:/Users/skysheng/OneDrive - The University Of British Columbia/University of British Columbia/Research/PhD Project/Lameness one year trial/Data cleaning/step3_outlier_removal/feeding/results"
  } else if (location == "vet_room") {
  setwd("C:/Users/skysheng/OneDrive - UBC/University of British Columbia/Research/Master's Project/Lameness one year trial/Super Computer Computation/hobo_insentec_round6/result/HOBO_insentec_milking/Insentec")
  
}



######################## merge a list of dataframes together ###################
load("Cleaned_feeding_original_data.rda")
load("Cleaned_drinking_original_data.rda")


# create a function to merge a list of datasheets into 1 datasheet
combine_datasheet_list <- function(datasheet_list) {
  for (m in 1:length(datasheet_list)) {
    if (m == 1) {
      master <- datasheet_list[[m]]
    } else {
      master <- rbind(master, datasheet_list[[m]])
    }
  }
  
  return(master)
}

feed_master <- combine_datasheet_list(Cleaned_feeding_original_data)
save(feed_master, file = "Cleaned_feeding_original_data_combined.rda")

water_master <- combine_datasheet_list(Cleaned_drinking_original_data)
save(water_master, file = "Cleaned_drinking_original_data_combined.rda")

###################################################################################################
################################### Days to Be Discarded ##########################################
###################################################################################################
date <- c("2020-07-03","2020-07-04","2020-07-05","2020-07-06","2020-07-07","2020-07-08","2020-07-09","2020-07-10","2020-07-11","2020-07-12","2020-07-13","2020-07-14",
          "2020-07-27","2020-07-28","2020-07-29","2020-07-30","2020-07-31","2020-09-26","2020-09-27",
          "2021-02-03", "2021-02-04", "2021-02-05", "2021-02-06", "2021-02-07", "2021-02-08", "2021-02-09", "2021-02-10", "2021-03-06", "2021-03-11", "2021-03-13", "2021-03-17", "2021-03-22",  "2021-04-27", "2021-04-28", "2021-05-02", "2021-05-03")
Red_warning <- c(rep("Trial not started",12),rep("Insentec break down",7), "Human present disturbance", "Human present disturbance", "Insentec break down", "Insentec break down", "Insentec break down", "bin 5 & 6 down", "bin 5 & 6 down", "bin 3, 4, 5 & 6 down", "Insentec compressor was not working, and Cow 6062 lost transponder before 8AM", "Insentec disturbed in the morning", "Insentec aren't opening for cows, manually turned to all open in the morning", "Water bin broken down", "Insentec compressor down", "Feed composition change, no feed access during night", "Feed composition change, no feed access during night", "Missing data for half a day", "Missing data for half a day")
days_to_be_discarded <- data.frame(date, Red_warning)
days_to_be_discarded$date <- ymd(days_to_be_discarded$date, tz="America/Los_Angeles")

# days to be deleted
delete_days <- function(master_df, days_list) {
  to_delete <- master_df[which(master_df$date %in% days_list),]
  if (nrow(to_delete) != 0) {
    master_df <- master_df[-which(master_df$date %in% days_list),]
  }
  
  return(master_df)
}

### Some of the orange warning days may need to be excluded when considering water bin data separately, see warning_days data frame!!
orange_date <- c("2021-02-11", "2021-02-12", "2021-02-16", "2021-02-17", "2021-02-18", "2021-03-16", "2021-03-23", "2021-04-08", "2021-04-09", "2021-04-10", "2021-04-11", "2021-04-15", "2021-05-06", "2021-05-07", "2021-05-08", "2021-05-09", "2021-05-10", "2021-05-11", "2021-05-12", "2021-05-13", "2021-05-17")
orange_warning <- c("Power Outage from 17:30 - 18:20; extreme cold weather", "extreme cold weather, bins not closing properly", "Cow 5120 lost both tages, registered as 1111", "Cow 5120 lost both tages, registered as 1111", "Morning only:Cow 5120 lost both tages, registered as 1111", "No access to feed for several hours in the afternoon due to hoof trimming", "Compressor down again", "cow 7064 was switched to 0", "cow 7064 was switched to 0", "cow 5096 was removed for half a day due to difficulty turning around in the parlor", "cow 5096 was removed for half a day due to difficulty turning around in the parlor", "Brush crew people were in the pen for significnat amount of time", "water bin 104 was temperorily closed", "water bin 104 was temperorily closed", "water bin 104 was temperorily closed", "water bin 104 was temperorily closed", "water bin 104 was temperorily closed", "water bin 104 was temperorily closed", "water bin 104 was temperorily closed", "water bin 104 was temperorily closed", "Cows escaped to the pasture from around 9pm to 10:30 pm; bin 9 left closed for some part of the day")
orange_date_sheet <- data.frame(orange_date, orange_warning)
colnames(orange_date_sheet) <- c("date", "orange_warning")
orange_date_sheet$date <- ymd(orange_date_sheet$date, tz="America/Los_Angeles")


# cows to be deleted
cow_delete_list <- c(1556, 5015, 1111, 1112, 1113, 1114)
delete_cow <- function(master_df, delete_cow_list) {
  to_delete <- master_df[which(master_df$Cow %in% cow_delete_list),]
  if (nrow(to_delete) != 0) {
    master_df <- master_df[-which(master_df$Cow %in% cow_delete_list),]
  }
  return(master_df)
}


# summarize all warning days
warning_days <- merge(days_to_be_discarded, orange_date_sheet, all = TRUE)
warning_days[is.na(warning_days)] <- ""


############################## calculate outliers ##############################
# detect outliers
load("Cleaned_feeding_original_data_combined.rda")
feed_master$date <- date(feed_master$Start)
feed_master$date <- ymd(feed_master$date, tz="America/Los_Angeles")
feed_master$start_hour <- hour(feed_master$Start)

# only use data happened after November 24 2020 to May 19, 2021, which is a stable period
feed_master <- feed_master[which((feed_master$date >= "2020-11-24") & (feed_master$date <= "2021-05-19")),]

feed_master2 <- delete_cow(feed_master, cow_delete_list)
feed_master2 <- delete_days(feed_master2, days_to_be_discarded$date)
feed_master2$Cow <- as.character(feed_master2$Cow)
feed_master2$start_hour <- as.character(feed_master2$start_hour)
feed_master2$rate <- feed_master2$Intake/feed_master2$Duration


###### find outlier using mahalanobis distance
### Step 1 : using original untransformed intake & duration, fit the model on herd level, outlier on herd level
# Finding the center point 
feed.center  = colMeans(feed_master2[, c("Intake", "Duration")])
# Finding the covariance matrix
feed.cov     = cov(feed_master2[, c("Intake", "Duration")])
distances <- mahalanobis(x = feed_master2[, c("Intake", "Duration")] , center = feed.center , cov = feed.cov)
# Cutoff value for ditances from Chi-Sqaure Dist. 
# with p = 0.95 df = 2 which in ncol(air)
cutoff <- qchisq(p = 0.9999999999999999 , df = ncol(feed_master2[, c("Intake", "Duration")]))
## Display observation whose distance greater than cutoff value
feed_master2$outlier_raw <- ""
feed_master2$outlier_raw[distances > cutoff] <- "outlier"
feed_master2$outlier_raw[distances <= cutoff] <- "normal"


### Step 2: using original untransformed intake & duration, but control for hour
#fit the Mahalanobis distance approach to each hour
hour_list <- unique(feed_master2$start_hour)
for (i in 1:length(hour_list)) {
  cur_hour_df <- feed_master2[which(feed_master2$start_hour == hour_list[i]),]
  # Finding the center point
  cur.hour.center  = colMeans(cur_hour_df[, c("Intake", "Duration")])
  # Finding the covariance matrix
  cur.hour.cov     = cov(cur_hour_df[, c("Intake", "Duration")])
  distances <- mahalanobis(x = cur_hour_df[, c("Intake", "Duration")] , center = cur.hour.center , cov = cur.hour.cov)
  # Cutoff value for ditances from Chi-Sqaure Dist. 
  # with p = 0.95 df = 2 which in ncol(air)
  cutoff <- qchisq(p = 0.9999999999999999 , df = ncol(cur_hour_df[, c("Intake", "Duration")]))
  ## Display observation whose distance greater than cutoff value
  cur_hour_df$outlier_raw_hour_ctl <- ""
  cur_hour_df$outlier_raw_hour_ctl[distances > cutoff] <- "outlier"
  cur_hour_df$outlier_raw_hour_ctl[distances <= cutoff] <- "normal"
  
  if (i == 1) {
    feed_master3 <- cur_hour_df
  } else {
    feed_master3 <- rbind(feed_master3, cur_hour_df)
  }
}

### Step 3: fit the Mahalanobis distance approach to each cow
cow_list <- unique(feed_master3$Cow)
for (i in 1:length(cow_list)) {
  cur_cow_df <- feed_master3[which(feed_master3$Cow == cow_list[i]),]
  # Finding the center point
  cur.cow.center  = colMeans(cur_cow_df[, c("Intake", "Duration")])
  # Finding the covariance matrix
  cur.cow.cov     = cov(cur_cow_df[, c("Intake", "Duration")])
  distances <- mahalanobis(x = cur_cow_df[, c("Intake", "Duration")] , 
                           center = cur.cow.center , 
                           cov = cur.cow.cov)
  # Cutoff value for ditances from Chi-Sqaure Dist. 
  # with p = 0.95 df = 2 which in ncol(air)
  cutoff <- qchisq(p = 0.9999999999999999 , df = ncol(cur_cow_df[, c("Intake", "Duration")]))
  ## Display observation whose distance greater than cutoff value
  cur_cow_df$outlier_raw_cow_ctl <- ""
  cur_cow_df$outlier_raw_cow_ctl[distances > cutoff] <- "outlier"
  cur_cow_df$outlier_raw_cow_ctl[distances <= cutoff] <- "normal"
  
  if (i == 1) {
    feed_master4 <- cur_cow_df
  } else {
    feed_master4 <- rbind(feed_master4, cur_cow_df)
  }
}

### Step 4: combine everything above, and fit the Mahalanobis distance approach to both individual cow and hour
feed_master5 <- feed_master4
feed_master5$outlier_raw_cow_hour_ctl <- "normal"
for (i in 1:nrow(feed_master5)) {
  
  # if on herd level, individual cow level, and on hour level, one of them were identified as normal or outlier, identify as normal or outlier
  if ((feed_master5$outlier_raw_hour_ctl[i] == "outlier") | (feed_master5$outlier_raw[i] == "outlier") | (feed_master5$outlier_raw_cow_ctl[i] == "outlier")) {
    feed_master5$outlier_raw_cow_hour_ctl[i] = "outlier"
  } 
}



outlier_num <- nrow(feed_master5[which(feed_master5$outlier_raw_cow_hour_ctl == "outlier"),])
outlier_pct <- outlier_num/nrow(feed_master5)
print(paste("The total percentage of outlier is:", outlier_pct))


###################################################################################################
###################################### 0 intake visits ############################################
###################################################################################################
feed_master5$zero_intake = ""
feed_master5$empty_bin_visit = ""
feed_master5$non_nutritive_visit = ""
for (i in 1:nrow(feed_master5)) {
  if (feed_master5$Intake[i] == 0) {
    feed_master5$zero_intake[i] = "zero intake"
  }
  if ((feed_master5$Intake[i] == 0) && (feed_master5$Startweight[i] < 0.5)) {
    feed_master5$empty_bin_visit[i] = "empty bin visit"
  }
  if ((feed_master5$Intake[i] == 0) && (feed_master5$Startweight[i] >= 0.5)) {
    feed_master5$non_nutritive_visit[i] = "non-nutritive visit"
  }
}


total_bin_visit <- nrow(feed_master5)
visit_type_sum <- data.frame(total_bin_visit)
colnames(visit_type_sum) <- c("frequency")
visit_type_sum$visit_type <- "total bin visit"

zero_intake_visit <- nrow(feed_master5[which(feed_master5$zero_intake == "zero intake"),])
temp <- data.frame(zero_intake_visit)
colnames(temp) <- c("frequency")
temp$visit_type <- "zero intake"
visit_type_sum <- rbind(visit_type_sum, temp)

feeding_visit <- total_bin_visit - zero_intake_visit
temp <- data.frame(feeding_visit)
colnames(temp) <- c("frequency")
temp$visit_type <- "total feeding visits"
visit_type_sum <- rbind(visit_type_sum, temp)

empty_bin_visit <- nrow(feed_master5[which(feed_master5$empty_bin_visit == "empty bin visit"),])
temp <- data.frame(empty_bin_visit)
colnames(temp) <- c("frequency")
temp$visit_type <- "empty bin visit"
visit_type_sum <- rbind(visit_type_sum, temp)

non_nutritive_visit <-nrow(feed_master5[which(feed_master5$non_nutritive_visit == "non-nutritive visit"),])
temp <- data.frame(non_nutritive_visit)
colnames(temp) <- c("frequency")
temp$visit_type <- "non-nutritive visit"
visit_type_sum <- rbind(visit_type_sum, temp)

feed_master5$visit_type = "feeding visits"
feed_master5$visit_type[which(feed_master5$empty_bin_visit != "")] <- "empty bin visit"
feed_master5$visit_type[which(feed_master5$non_nutritive_visit != "")] <- "non-nutritive visit"
feed_master5$n <- 1

setwd(output_dir)
save(feed_master5, file ="feed_outlier_201124_210519.rdata")


###################################################################################################
################## remove outlier & recalculate feeding duration on daily level####################
###################################################################################################
feed_master5_outlier_free <- feed_master5[which(feed_master5$outlier_raw_cow_hour_ctl == "normal"),] # exclude outliers on 
# aggregate intake
feed_intake_sum <- aggregate(feed_master5_outlier_free$Intake, by = list(feed_master5_outlier_free$date, feed_master5_outlier_free$Cow), FUN =sum )
colnames(feed_intake_sum) <- c("date", "Cow", "Feeding_Intake(kg)")
Feeding_analysis_no_outlier <- feed_intake_sum

# aggregate non-nutritive visit = visits where intake == 0 but feed bin weight >= 0.5 kg
non_nutri <- feed_master5_outlier_free[which(feed_master5_outlier_free$non_nutritive_visit != ""),] # get all non-nutritive vistis
non_nutri_visit_num <- aggregate(non_nutri$n, by = list(non_nutri$date, non_nutri$Cow), FUN =sum ) # calculate the total number of non-nutritive visit per day per cow
colnames(non_nutri_visit_num) <- c("date", "Cow", "non_nutritive_Visits")
Feeding_analysis_no_outlier2 <- merge(Feeding_analysis_no_outlier, non_nutri_visit_num, all = TRUE)
Feeding_analysis_no_outlier2[is.na(Feeding_analysis_no_outlier2)] <- 0
non_nutri_visit_dur <- aggregate(non_nutri$Duration, by = list(non_nutri$date, non_nutri$Cow), FUN =sum ) # calculate the total time of non-nutritive visit per day per cow
colnames(non_nutri_visit_dur) <- c("date", "Cow", "non_nutritive_visit_duration")
Feeding_analysis_no_outlier3 <- merge(Feeding_analysis_no_outlier2, non_nutri_visit_dur, all = TRUE)
Feeding_analysis_no_outlier3[is.na(Feeding_analysis_no_outlier3)] <- 0

# aggregate empty_bin visit = visits where intake == 0 but feed bin weight <= 0.5 kg
empty_bin <- feed_master5_outlier_free[which(feed_master5_outlier_free$empty_bin_visit != ""),] # get all empty bin vistis
empty_bin_visit_num <- aggregate(empty_bin$n, by = list(empty_bin$date, empty_bin$Cow), FUN =sum ) # calculate the total number of empty bin vistis per day per cow
colnames(empty_bin_visit_num) <- c("date", "Cow", "empty_bin_Visits")
Feeding_analysis_no_outlier4 <- merge(Feeding_analysis_no_outlier3, empty_bin_visit_num, all = TRUE)
Feeding_analysis_no_outlier4[is.na(Feeding_analysis_no_outlier4)] <- 0
empty_bin_visit_dur <- aggregate(empty_bin$Duration, by = list(empty_bin$date, empty_bin$Cow), FUN =sum ) # calculate the total time of empty bin vistis per day per cow
colnames(empty_bin_visit_dur) <- c("date", "Cow", "empty_bin_visit_duration")
Feeding_analysis_no_outlier5 <- merge(Feeding_analysis_no_outlier4, empty_bin_visit_dur, all = TRUE)
Feeding_analysis_no_outlier5[is.na(Feeding_analysis_no_outlier5)] <- 0

# zero intake visit = non-nutritive visit + empty_bin visits; visits where intake = 0
Feeding_analysis_no_outlier5$zero_intake_visits <- Feeding_analysis_no_outlier5$non_nutritive_Visits + Feeding_analysis_no_outlier5$empty_bin_Visits
Feeding_analysis_no_outlier5$zero_intake_visit_duration <- Feeding_analysis_no_outlier5$non_nutritive_visit_duration + Feeding_analysis_no_outlier5$empty_bin_visit_duration

# feeding visits = visits where intake > 0
feeding <- feed_master5_outlier_free[which(feed_master5_outlier_free$visit_type == "feeding visits"),] # get all empty bin vistis
feeding_visit_num <- aggregate(feeding$n, by = list(feeding$date, feeding$Cow), FUN =sum ) # calculate the total number of empty bin vistis per day per cow
colnames(feeding_visit_num) <- c("date", "Cow", "Feeding_Visits")
Feeding_analysis_no_outlier5 <- merge(Feeding_analysis_no_outlier5, feeding_visit_num, all = TRUE)
Feeding_analysis_no_outlier5[is.na(Feeding_analysis_no_outlier5)] <- 0
feeding_visit_dur <- aggregate(feeding$Duration, by = list(feeding$date, feeding$Cow), FUN =sum ) # calculate the total time of empty bin vistis per day per cow
colnames(feeding_visit_dur) <- c("date", "Cow", "Feeding_Duration(s)")
Feeding_analysis_no_outlier5 <- merge(Feeding_analysis_no_outlier5, feeding_visit_dur, all = TRUE)
Feeding_analysis_no_outlier5[is.na(Feeding_analysis_no_outlier5)] <- 0

# feeding rate 
feed_rate_ave <- aggregate(feed_master5_outlier_free$rate, by = list(feed_master5_outlier_free$date, feed_master5_outlier_free$Cow), FUN =mean )
colnames(feed_rate_ave) <- c("date", "Cow", "Feeding_rate_mean")
Feeding_analysis_no_outlier5<- merge(Feeding_analysis_no_outlier5, feed_rate_ave)

# total bin visit = all visits = zero intake visit + feeding visits
Feeding_analysis_no_outlier5$total_bin_visits <- Feeding_analysis_no_outlier5$Feeding_Visits + Feeding_analysis_no_outlier5$zero_intake_visits
Feeding_analysis_no_outlier5$total_bin_visit_duration <- Feeding_analysis_no_outlier5$`Feeding_Duration(s)` + Feeding_analysis_no_outlier5$zero_intake_visit_duration


setwd(output_dir)
save(Feeding_analysis_no_outlier5, file ="feeding_summary_201124_210519_no_outlier.rda")
save(Feeding_analysis_no_outlier5, file ="feeding_summary_201124_210519_no_outlier.rdata")



