###### Specify customized parameters related to data cleaning for this study ######

# List of cow IDs to be deleted because they are not actual cows
# Instead, they are test ear tags used during calibration
cow_delete_list <- c(0, 1556, 5015, 1111, 1112, 1113, 1114)

# Column names in the feed bin log files (change based on your own file format)
coln <- c("Transponder", "Cow", "Bin", "Start", "End", "Duration", "Startweight", "Endweight", "Comment", "Intake", "Intake2", "X1", "X2", "X3", "X4")

# Column names in the water bin log files
coln.wat <- c("Transponder", "Cow", "Bin", "Start", "End", "Duration", "Startweight", "Endweight", "Intake")

# List of feed transponder IDs to be deleted
feed_transponder_delete_list <- c(0)

# Min and Max values for feed bin
min_feed_bin <- 1
max_feed_bin <- 30

# Columns to retain in the processed feed data
feed_coln_to_keep <- c("Transponder", "Cow", "Bin", "Start", "End", "Duration", "Startweight", "Endweight", "Intake")

# List of water transponder IDs to be deleted
wat_transponder_delete_list <- c(0)

# Min and Max values for water bin
min_wat_bin <- 1
max_wat_bin <- 5

# how many cows do we expect to live in the pen
total_cow_expt <- 48

# Columns to retain in the processed water data
wat_coln_to_keep <- c("Transponder", "Cow", "Bin", "Start", "End", "Duration", "Startweight", "Endweight", "Intake")

# Additional value for bin ID, possibly used for specific calculations or mapping
bin_id_add <- 100

# Time zone to be used in date-time operations
time_zone <- "America/Los_Angeles"

# Insentec data source, is it just feed data, or just water data, or both.
data_source <- "feed and water"  # can be "feed", or "water", or "feed and water" means has both water and feed data

# duration threshold for what is considered a long feeding visit  
high_feed_dur_threshold = 2000

# duration threshold for what is considered a long drinking visit  
high_water_dur_threshold = 1800

#intake threshold for what is considered a large intake in 1 bout for feed
large_feed_intake_bout_threshold = 8

#intake threshold for what is considered a large intake in 1 bout for water
large_water_intake_bout_threshold = 30

#intake threshold for what is considered a large intake in short time for feed
large_feed_intake_short_time_threshold = 5

#rate threshold for what is considered a large intake in short time for feed
large_feed_rate_short_time_threshold = 0.008

#intake threshold for what is considered a large intake in short time for water
large_water_intake_short_time_threshold = 10

#rate threshold for what is considered a large intake in short time for feed
large_water_rate_short_time_threshold = 0.35

# for bins with low total number of visits, what is the threshold for low total visits
low_visit_threshold = 10

# what is considered low daily feed intake for a cow
feed_intake_low_bar = 35

# what is considered high daily feed intake for a cow
feed_intake_high_bar = 75

# what is considered low daily water intake for a cow
water_intake_low_bar = 60

# what is considered high daily water intake for a cow
water_intake_high_bar = 180

# threshold for replacement behaviours in seconds. the interval between the first cow leaving and the next cow entering
replacement_threshold = 26

# calibration error is adjustment for the errors in the readings of the feeder bins
calibration_error = 0.5
  
# which method you wish to use to calculate feeder occupancy. there are 4 different methods
################################################################################
# Feeder Occupancy calculation method 1:             
#                    Occupied bin num                  
#   -------------------------------------------------- 
#                    total bin num (30)   
################################################################################
# Feeder Occupancy calculation method 2:             
#                    Occupied bin num                  
#   -------------------------------------------------- 
#                 total bin with feed num   
################################################################################
# Feeder Occupancy calculation method 3:
#                    Occupied bin num
#   --------------------------------------------------
#      total bin num - unoccupied bins that are empty 
################################################################################
# Feeder Occupancy calculation method 4:             
#                 Occupied bin with feed num                  
#   -------------------------------------------------- 
#                 total bin with feed num   
################################################################################
method_type = 3

#what is the total number of bins
total_fed_wat_bin = 35

#what is the total number of feed bins
total_feed_bin = 30

#what is the total number of water bins
total_wat_bin = 5