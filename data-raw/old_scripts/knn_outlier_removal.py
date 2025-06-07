# -*- coding: utf-8 -*-
"""
Created on Thu Oct 6 09:57:04 2022

@author: skysheng
"""
###############################################################################
######################## remove feeding outlier ###############################
###############################################################################

# import packages
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.neighbors import NearestNeighbors
import os
# import data
feed = pd.read_csv("/Users/skysheng/Library/CloudStorage/OneDrive-UBC/University of British Columbia/Research/PhD Project/Lameness one year trial/Super Computer Computation/hobo_insentec_round6/result/HOBO_insentec_milking/Insentec/Cleaned_feeding_original_data_combined.csv")
feed['rate'] = feed['Intake']/feed['Duration']


####### option 1: remove outlier based on each cow
feed['rate'] = feed['rate'] * 10000
unique_cow = pd.unique(feed['Cow'])

for cur_cow in unique_cow:
    cur_df = feed.loc[feed['Cow'] == cur_cow]
    feed_input = cur_df[["Duration", "Intake", "rate"]]

    #plot the 2 variables
    plt.scatter(feed_input["Duration"], feed_input["Intake"])
    # create arrays
    X = feed_input.values

    # instantiate model
    nbrs = NearestNeighbors(n_neighbors = 5)
    # fit model
    nbrs.fit(X)

    # distances and indexes of k-neaighbors from model outputs
    distances, indexes = nbrs.kneighbors(X)
    # calculate the average KNN distance for each point
    average_dist = distances.mean(axis =1)
    # plot mean of k-distances of each observation
    plt.plot(average_dist)

    plt.hist(average_dist, bins = 200)

    Q3 = np.percentile(average_dist , 99.9)
    
    # visually determine cutoff values 
    outlier_index = np.where(average_dist > Q3)
    outlier_index

    # filter outlier values
    outlier_values = feed_input.iloc[outlier_index]
    outlier_values

    # plot data
    plt.scatter(feed_input["Duration"], feed_input["Intake"], color = "b", s = 65)
    # plot outlier values
    plt.scatter(outlier_values["Duration"], outlier_values["Intake"], color = "r")



####### option 2: remove outlier on herd level
feed['rate'] = feed['rate'] * 10000
feed_input = feed[["Duration", "Intake", "rate"]]

#plot the 2 variables
plt.scatter(feed_input["Duration"], feed_input["Intake"])
# create arrays
X = feed_input.values

# instantiate model
nbrs = NearestNeighbors(n_neighbors = 5)
# fit model
nbrs.fit(X)

# distances and indexes of k-neaighbors from model outputs
distances, indexes = nbrs.kneighbors(X)
# calculate the average KNN distance for each point
average_dist = distances.mean(axis =1)
# plot mean of k-distances of each observation
plt.plot(average_dist)

plt.hist(average_dist, bins = 200)

Q3 = np.percentile(average_dist , 99.987)
    
# visually determine cutoff values 
outlier_index = np.where(average_dist > Q3)
outlier_index

# filter outlier values
outlier_values = feed_input.iloc[outlier_index]
outlier_values

# plot data Duration X Intake
plt.scatter(feed_input["Duration"], feed_input["Intake"], color = "b", s = 65)
# plot outlier values
plt.scatter(outlier_values["Duration"], outlier_values["Intake"], color = "r")  


# mark down outliers
feed['outlier'] = "N"
feed.iloc[outlier_index[0], 11] = "Y"
feed['rate'] = feed['rate'] /10000
outlier_values['rate'] = outlier_values['rate'] /10000

os.chdir("/Users/skysheng/Library/CloudStorage/OneDrive-UBC/University of British Columbia/Research/PhD Project/Lameness one year trial/Analysis/wali lameness prediction/results")
feed.to_csv('Cleaned_feeding_original_data_combined_no_outlier.csv') 



####### option 3: remove outlier on herd level, feed rate re-scaling
# rescaling of the absolute value to make sure that they are weighted as similar in KNN
# more punishment for high rate and high duration
feed['rate'] = feed['rate'] * 100
feed['Duration'] = feed['Duration'] * 0.1
feed_input = feed[["Duration", "Intake", "rate"]]

#plot the 2 variables
plt.scatter(feed_input["Duration"], feed_input["Intake"])
# create arrays
X = feed_input.values

# instantiate model
nbrs = NearestNeighbors(n_neighbors = 5)
# fit model
nbrs.fit(X)

# distances and indexes of k-neaighbors from model outputs
distances, indexes = nbrs.kneighbors(X)
# calculate the average KNN distance for each point
average_dist = distances.mean(axis =1)
# plot mean of k-distances of each observation
plt.plot(average_dist)

plt.hist(average_dist, bins = 200)

Q3 = np.percentile(average_dist , 99.986)
    
# visually determine cutoff values 
outlier_index = np.where(average_dist > Q3)
outlier_index

# filter outlier values
outlier_values = feed_input.iloc[outlier_index]
outlier_values

# plot data Duration X Intake
plt.scatter(feed_input["Duration"], feed_input["Intake"], color = "b", s = 65)
# plot outlier values
plt.scatter(outlier_values["Duration"], outlier_values["Intake"], color = "r")  


# mark down outliers
feed['outlier'] = "N"
feed.iloc[outlier_index[0], 11] = "Y"
# convert back to original value
feed['rate'] = feed['rate'] * 0.01
feed['Duration'] = feed['Duration'] * 10
outlier_values['rate'] = outlier_values['rate'] *0.01
outlier_values['Duration'] = outlier_values['Duration'] * 10


os.chdir("/Users/skysheng/Library/CloudStorage/OneDrive-UBC/University of British Columbia/Research/PhD Project/Lameness one year trial/Analysis/wali lameness prediction/results")
feed.to_csv('Cleaned_feeding_original_data_combined_no_outlier.csv', index=False) 




####### The picked one: option 4: remove outlier on herd level, feed rate re-scaling, 
# rescaling of the absolute value to make sure that they are weighted as similar in KNN
# more punishment for high rate and high duration, increase punishment for intake
# import data
feed = pd.read_csv("/Users/skysheng/Library/CloudStorage/OneDrive-UBC/University of British Columbia/Research/PhD Project/Lameness one year trial/Super Computer Computation/hobo_insentec_round6/result/HOBO_insentec_milking/Insentec/Cleaned_feeding_original_data_combined.csv")
feed['rate'] = feed['Intake']/feed['Duration']
feed['rate'] = feed['rate'] * 10000
feed['Intake'] = feed['Intake'] * 7
feed['Duration'] = feed['Duration'] * 0.03
feed_input = feed[["Duration", "Intake", "rate"]]

#plot the 2 variables
plt.scatter(feed_input["Duration"], feed_input["Intake"])
# create arrays
X = feed_input.values

# instantiate model
# increase k, which also increase reliability, and increase punishment for doimnant weight (i.e., rate)
nbrs = NearestNeighbors(n_neighbors = 50)
# fit model
nbrs.fit(X)

# distances and indexes of k-neaighbors from model outputs
distances, indexes = nbrs.kneighbors(X)
# calculate the average KNN distance for each point
average_dist = distances.mean(axis =1)
# plot mean of k-distances of each observation
plt.plot(average_dist)
plt.hist(average_dist, bins = 200)

thereshold = np.percentile(average_dist , 99.936)
    
# visually determine cutoff values 
outlier_index = np.where(average_dist > thereshold)
outlier_index

# filter outlier values
outlier_values = feed_input.iloc[outlier_index]
outlier_values

# plot data Duration X Intake
plt.scatter(feed_input["Duration"], feed_input["Intake"], color = "b", s = 65)
# plot outlier values
plt.scatter(outlier_values["Duration"], outlier_values["Intake"], color = "r")  


# mark down outliers
feed['outlier'] = "N"
feed.iloc[outlier_index[0], 11] = "Y"
# convert back to original value
feed['rate'] = feed['rate'] * 0.0001
feed['Intake'] = feed['Intake'] /7
feed['Duration'] = feed['Duration'] /0.03


os.chdir("/Users/skysheng/Library/CloudStorage/OneDrive-UBC/University of British Columbia/Research/PhD Project/Lameness one year trial/Analysis/wali lameness prediction/results")
feed.to_csv('Cleaned_feeding_original_data_combined_no_outlier.csv', index=False) 



###############################################################################
######################## remove drinking outlier ##############################
###############################################################################

# import data
wat = pd.read_csv("/Users/skysheng/Library/CloudStorage/OneDrive-UBC/University of British Columbia/Research/PhD Project/Lameness one year trial/Super Computer Computation/hobo_insentec_round6/result/HOBO_insentec_milking/Insentec/Cleaned_drinking_original_data_combined.csv")
wat['rate'] = wat['Intake']/wat['Duration']

# punish for high rate and high duration
wat['rate'] = wat['rate'] * 20
wat['Intake'] = wat['Intake'] 
wat['Duration'] = wat['Duration'] * 0.01
wat_input = wat[["Duration", "Intake", "rate"]]

#plot the 2 variables
plt.scatter(wat_input["Duration"], wat_input["Intake"])
# create arrays
X = wat_input.values

# instantiate model
# increase k, which also increase reliability, and increase punishment for doimnant weight (i.e., rate)
nbrs = NearestNeighbors(n_neighbors = 50)
# fit model
nbrs.fit(X)

# distances and indexes of k-neaighbors from model outputs
distances, indexes = nbrs.kneighbors(X)
# calculate the average KNN distance for each point
average_dist = distances.mean(axis =1)
# plot mean of k-distances of each observation
plt.plot(average_dist)
plt.hist(average_dist, bins = 200)

thereshold = np.percentile(average_dist , 99.9)
    
# visually determine cutoff values 
outlier_index = np.where(average_dist > thereshold)
outlier_index

# filter outlier values
outlier_values = wat_input.iloc[outlier_index]
outlier_values

# plot data Duration X Intake
plt.scatter(wat_input["Duration"], wat_input["Intake"], color = "b", s = 65)
# plot outlier values
plt.scatter(outlier_values["Duration"], outlier_values["Intake"], color = "r")  


# mark down outliers
wat['outlier'] = "N"
wat.iloc[outlier_index[0], 11] = "Y"
# convert back to original value
wat['rate'] = wat['rate'] /20
wat['Intake'] = wat['Intake']
wat['Duration'] = wat['Duration'] * 100


os.chdir("/Users/skysheng/Library/CloudStorage/OneDrive-UBC/University of British Columbia/Research/PhD Project/Lameness one year trial/Analysis/wali lameness prediction/results")
wat.to_csv('Cleaned_drinking_original_data_combined_no_outlier.csv', index=False) 







