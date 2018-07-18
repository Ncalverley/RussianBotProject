####################################################################################
##                        CONSTRUCT FULL TWEET DATA
##
## This file will bring in the cleaned twitter data from both the Russian trolls and
## the legitimate tweets, and combine them into a single evaluable data set.
####################################################################################

####################################################################################
## Load the data sets
####################################################################################

load(file = "data/cleanedLegitTweetData.RData"); legitData <- tweetData; rm(tweetData)
load(file = "data/cleanedRussianTweetData.RData"); russianData <- tweetData; rm(tweetData)

####################################################################################
## Load the Russian users file to bring in user information for the Russian trolls
####################################################################################
russianUsers <- read_csv("data/russianUsers.csv")
## Remove missing IDs
russianUsers <- subset(russianUsers, !is.na(id))

####################################################################################
## Prepare the Russian tweet data for merging with their user profiles
####################################################################################
names(russianUsers)[names(russianUsers) == "created_at"] <- "user_created_at"
names(russianUsers)[names(russianUsers) == "id"] <- "user_id"

####################################################################################
## Merge the Russian user account data onto their tweet data
####################################################################################
russianData <- merge(russianData, russianUsers, by = "user_id")

####################################################################################
## Rename a bunch of fields in the legit data to match the Russian troll data
####################################################################################

names(legitData)[names(legitData) == "user_id_str"] <- "user_id"
names(legitData)[names(legitData) == "full_name"] <- "name"
names(legitData)[names(legitData) == "expanded_url"] <- "expanded_urls"
names(legitData)[names(legitData) == "id_str"] <- "tweet_id"
names(legitData)[names(legitData) == "user_lang"] <- "lang"
names(legitData)[names(legitData) == "created_at"] <- "created_str"

####################################################################################
## Trim down the data sets by intersecting the fields
####################################################################################
legitData <- legitData[,match(intersect(names(legitData), names(russianData)), names(legitData))]
russianData <- russianData[,match(intersect(names(legitData), names(russianData)), names(russianData))]

####################################################################################
## Put in a troll status field
####################################################################################
russianData$troll <- 1
legitData$troll <- 0

####################################################################################
## Bind the data together
####################################################################################
tweetData <- rbind(russianData, legitData)

####################################################################################
## Save and clean up
####################################################################################
save(tweetData, file = "data/cleanedTweetData.RData")
rm(tweetData, russianData, legitData, russianUsers)

