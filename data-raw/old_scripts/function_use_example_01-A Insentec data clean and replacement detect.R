################################################################################
###################### data loading & processing ###############################
################################################################################
###### Load feed and water file names
# set work directory and load data
input_dir <- here("data")
output_dir <- here("result")
# load in feed and water data
fileNames.f <- list.files(path = here("data/feed"),full.names = TRUE,
                          recursive = TRUE,pattern =".DAT")
fileNames.w <- list.files(path = here("data/water"),full.names = TRUE,
                          recursive = TRUE,pattern =".DAT")
fileNames.f <- sort(fileNames.f)
fileNames.w <- sort(fileNames.w)

################################################################################
############# customized processing for this study (hard-coded) ################
################################################################################
# this part involves processing the data that is customized for this study only
# please change this based on your study
# process daylight saving dates dataframe and warning date data frame
daylight_saving_table <- daylight_saving_process(daylight_saving_csv)
warning_days$date <- ymd(warning_days$date, tz=time_zone)

################################################################################
##### Feeding & Drinking loading and initial processing from Insentec Data #####
################################################################################
# check if you have both water and feed data for each day
# we only retain the dates when both drinking and feeding data are available at
# the same time
date_compare <- compare_files(fileNames.f, fileNames.w)
fileNames.f <- date_compare$Feed_dir
fileNames.w <- date_compare$Drink_dir

# get date range for this dataset
date_result <- get_date_range(date_compare)
start_date <- date_result$start_date
end_date <- date_result$end_date
date_range <- date_result$date_range

# read in feed and water data into a list of dataframes
all.fed <- process_all_feed(fileNames.f, coln, cow_delete_list, feed_transponder_delete_list, min_feed_bin, max_feed_bin, feed_coln_to_keep)
all.wat <- process_all_water(fileNames.w, coln.wat, cow_delete_list, wat_transponder_delete_list, min_wat_bin, max_wat_bin, wat_coln_to_keep, bin_id_add)


################################################################################
############################## Quality Check ###################################
################################################################################
# initial process and quaity check for Insentec data, and generate warning dataframe
results <- generate_warning_df(data_source = "feed and water", all_feed = all.fed, 
                                all_water = all.wat, high_feed_dur_threshold, 
                                high_water_dur_threshold, min_feed_bin, max_feed_bin, 
                                min_wat_bin, max_wat_bin, bin_id_add, 
                                total_cow_expt, low_visit_threshold, time_zone) 
Insentec_warning <- results$Insentec_warning
all.fed2 <- results$feed
all.wat2 <- results$water
all.comb2 <- results$comb

# combine data frame different dates into 1 master dataframe
# Calling the function for each data list:
master_feeding <- merge_data_add_date(all.fed2)
master_drinking <- merge_data_add_date(all.wat2)
master_comb <- merge_data_add_date(all.comb2)
save(master_feeding, file = (here::here(paste0("data/results/", "Cleaned_feeding_original_data_combined.rdata"))))
save(master_drinking, file = (here::here(paste0("data/results/", "Cleaned_drinking_original_data_combined.rdata"))))
save(master_comb, file = (here::here(paste0("data/results/", "Cleaned_feeding_drinking_original_data_combined.rdata"))))

# get daily feed and water intake, duration, total visit summary
result <- merge_feed_water_summary(master_feeding, master_drinking, Insentec_warning, 
                                   feed_intake_low_bar, feed_intake_high_bar,
                                   water_intake_low_bar, water_intake_high_bar)
Insentec_final_summary <- result$Insentec_final_summary
Insentec_warning <- result$Insentec_warning

# store all intermediate files
cache("all.fed2")
cache("all.wat2")
cache("all.comb2")
cache("master_feeding")
cache("master_drinking")
cache("master_comb")
cache("Insentec_final_summary")
cache("Insentec_warning")


################################################################################
#################### Feeding Synchrony Matrix Preparation ######################
################################################################################
results <- matrix_process(all.fed2, min_feed_bin, max_feed_bin)
feeding_synch_master_cow <- results$synch_master_cow
feeding_synch_master_bin <- results$synch_master_bin
feeding_synch_master_feed <- results$synch_master_feed

save(feeding_synch_master_cow, file = (here::here(paste0("data/results/", "which cows are present each second for feed.rdata"))))
save(feeding_synch_master_bin, file = (here::here(paste0("data/results/", "which bins are occupied each second for feed.rdata"))))
save(feeding_synch_master_feed, file = (here::here(paste0("data/results/", "how much feed left each bin.rdata"))))

################################################################################
###################### record feeding replacements #############################
################################################################################
replacement_list_by_date <- record_replacement_allDay(all.fed2, replacement_threshold)
# filter replacement based on actor cow's alibi (the actor is feeding/drinking at another place when replacement happened)
replacement_list_by_date <- check_alibi_all(replacement_list_by_date, all.comb2)
master_feed_replacement_all <- merge_data(replacement_list_by_date)

cache("master_feed_replacement_all")
save(replacement_list_by_date, file = (here::here(paste0("data/results/", "Replacement_behaviour_by_date.rdata"))))
save(master_feed_replacement_all, file = (here::here(paste0("data/results/", "master_feed_replacement_all.rdata"))))


################################################################################
#################### Non-nutritive Visits ######################################
################################################################################

results <- nutrition_data_process(all.fed2)
non_nutritive_visits <- results[[1]]
visited_but_no_feed_freq <- results[[2]]

# save the non_nutritive_sheet and visited_but_no_feed_freq
save(non_nutritive_visits, file = (here::here(paste0("data/results/", "non_nutritive-visits.rdata"))))
save(visited_but_no_feed_freq, file = (here::here(paste0("data/results/", "visited_but_no_feed_freq.rdata"))))


################################################################################
#################### Feeding & Drinking Combined Synchrony Matrix Preparation ##
################################################################################

results <- feed_drink_matrix_process(all.comb2,total_fed_wat_bin = total_fed_wat_bin)
feed_drink_synch_master_cow <- results[[1]]
feed_drink_synch_master_bin <- results[[2]]

#saving the results
save(feed_drink_synch_master_cow, file = (here::here(paste0("data/results/", "feed_drink_synch_master_cow.rdata"))))
save(feed_drink_synch_master_bin, file = (here::here(paste0("data/results/", "feed_drink_synch_master_bin.rdata"))))

################################################################################
#################### Feeding & Drinking Synchrony analysis ####################
################################################################################

results <- feeding_drinking_analysis(all.comb2)
paired_feeding_drinking_bout <- results[[1]]
paired_feeding_drinking_total_time <- results[[2]]
paired_feeding_drinking_average_dur <- results[[3]]
neighbor_feeding_drinking_bout <- results[[4]]
neighbor_feeding_drinking_total_time <- results[[5]]
neighbor_feeding_drinking_average_dur <- results[[6]]

#saving the results
save(paired_feeding_drinking_bout, file = (here::here(paste0("data/results/", "paired_feeding_drinking_bout.rdata"))))
save(paired_feeding_drinking_total_time, file = (here::here(paste0("data/results/", "paired_feeding_drinking_total_time.rdata"))))
save(paired_feeding_drinking_average_dur, file = (here::here(paste0("data/results/", "paired_feeding_drinking_average_dur.rdata"))))
save(neighbor_feeding_drinking_bout, file = (here::here(paste0("data/results/", "neighbor_feeding_drinking_bout.rdata"))))
save(neighbor_feeding_drinking_total_time, file = (here::here(paste0("data/results/", "neighbor_feeding_drinking_total_time.rdata"))))
save(neighbor_feeding_drinking_average_dur, file = (here::here(paste0("data/results/", "neighbor_feeding_drinking_average_dur.rdata"))))