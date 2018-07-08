#import packages
library(ggplot2)
library(stringr)
library(dplyr)

#import full dataset
full_script_data <- read.csv("~/R Projects/trekvis/full_script_data.csv")

#TODO - all the vis!
unique(full_script_data$char)

#Add length to full_script_data
#TODO - Count how many words in each line!  What chars have the longest lines?  Shortest?  Most versatile?
# full_script_data[10,"line"]
# Steady as we go.
# words(full_script_data[10,"line"]) #Not correct .. when back online find this function.

#TODO try to integrate with Google NL API.  Pull out sentence structure -- see what else is available.

