####################################################################################
##                            CLEAN RUSSIAN TROLL DATA
##
## This file will import the twitter data containing the full tweets of all the 
## Russian-linked troll accounts, and construct the corpora for each account.
####################################################################################


####################################################################################
## Load the data
####################################################################################

tweetData <- import_loadTweets(dataSet = "legit")

## If running in dev mode, take a small sample for development
if(runMode == "dev") {
  idx <- sample(row.names(tweetData), size = sampleSize, replace = FALSE)
  tweetData <- tweetData[idx, ]; rm(idx)
}

####################################################################################
## Clean out tweets from bots - the data currently have a ton of tweets that are
## bring sent out from twitter bots. These are currently outside the scope of this
## project, so I'm going to remove their tweets for now.
####################################################################################

## Only run the following if running in "prod" mode.
if(runMode == "prod") {
  
  ## Identify the total number of tweets each user made
  temp <- data.table(tweetData)
  temp <- temp[, .(n_records = .N),
               by = .(user_id_str)]
  
  ## The total collection time was about 6 hours, and I'm going to assume that no
  ## normal human being would send out more than 10 tweets per hour, so I'm going to
  ## limit the sample to users who tweeted less than 60 total times.
  temp <- subset(temp, n_records < 60)
  
  ## Isolate the tweet data of the remaining users
  tweetData <- tweetData[tweetData$user_id_str %in% temp$user_id_str, ]
  
  ## There's still way too many records, so I'm going to trim it down further by
  ## removing any user with fewer than 10 tweets. This is so that we get a sufficient
  ## corpus of tweets to work with.
  temp <- subset(temp, n_records >= 10)
  
  ## Isolate the tweet data of the remaining users
  tweetData <- tweetData[tweetData$user_id_str %in% temp$user_id_str, ]
  
  ## Not bad, but there's still a large number of users. I'm going to trim it down
  ## one last time by taking a random sample of 2,000 users.
  keys <- sample(temp$user_id_str, size = 2000, replace = F)
  temp <- temp[temp$user_id_str %in% keys, ]; rm(keys)
  
  ## Isolate the tweet data of the remaining users
  tweetData <- tweetData[tweetData$user_id_str %in% temp$user_id_str, ]; rm(temp)
  
}

####################################################################################
## Identify and store the hashtags used in each tweet
####################################################################################
tweetData$hashtags <- apply(tweetData[match("text", names(tweetData))], 1, scrub_storeHashtags)

####################################################################################
## Replace hashtags and user handles with actual persons' names
####################################################################################

## Load the replacement words
replacementWords <- import_fetchReplacementWords()

## Update hashtags
tweetData$text <- apply(tweetData[match("text", names(tweetData))], 1, scrub_replaceString, replacementWords = replacementWords)

####################################################################################
## Clean the tweets by removing contaminants
####################################################################################

## Remove hashtags
tweetData$text <- apply(tweetData[match("text", names(tweetData))], 1, scrub_removeString, strType = "hashtag")

## Remove URLs
tweetData$text <- apply(tweetData[match("text", names(tweetData))], 1, scrub_removeString, strType = "url")

## Remove User IDs
tweetData$text <- apply(tweetData[match("text", names(tweetData))], 1, scrub_removeString, strType = "user")

## Remove Retweets
tweetData$text <- apply(tweetData[match("text", names(tweetData))], 1, scrub_removeString, strType = "retweet")

## Remove garbage characters
tweetData$text <- apply(tweetData[match("text", names(tweetData))], 1, scrub_removeString, strType = "garbage")

## Re-run the functions to remove hashtags, URLs, etc. The removal of the garbage
## characters will have revealed some additional contaminants.

## Remove hashtags
tweetData$text <- apply(tweetData[match("text", names(tweetData))], 1, scrub_removeString, strType = "hashtag")

## Remove URLs
tweetData$text <- apply(tweetData[match("text", names(tweetData))], 1, scrub_removeString, strType = "url")

## Remove User IDs
tweetData$text <- apply(tweetData[match("text", names(tweetData))], 1, scrub_removeString, strType = "user")

## Remove Retweets
tweetData$text <- apply(tweetData[match("text", names(tweetData))], 1, scrub_removeString, strType = "retweet")

####################################################################################
## Save the cleaned data
####################################################################################

save(tweetData, file = "data/cleanedLegitTweetData.RData")
rm(tweetData)


