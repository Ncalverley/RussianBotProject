####################################################################################
##                          RUSSIAN BOT PROJECT
##
## This program will use Natural Language Processing to analyze a data set of tweets
## published by confirmed Russian-linked accounts that were used to influence public
## option prior to the 2016 presidential election. The data set contains tweets from
## 454 accounts that have been confirmed by Twitter as Russian troll accounts.
####################################################################################

## Define the run mode. This can take on one of two values, "dev" or "prod". When
## running in "dev" mode, data will be sampled to keep runtimes to a minimum. When
## running in "prod" mode, the full data will be used.
runMode <- "prod"

## Define the number of most commonly-occuring user hashtags you want to analyze.
nHashtags <- 5

## Load all libraries
source(file = "R/loadPackages.R")

## Clean the data sets
source(file = "R/cleanLegitTweetData.R")
source(file = "R/cleanRussianTweetData.R")

## Construct the final analysis data
source(file = "R/constructAnalysisData.R")











