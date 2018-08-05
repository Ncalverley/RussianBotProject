####################################################################################
##                            CLEAN RUSSIAN TROLL DATA
##
## This file will import the twitter data containing the full tweets of all the 
## Russian-linked troll accounts, and construct the corpora for each account.
####################################################################################


####################################################################################
## Load the data
####################################################################################

## Load data
tweetData <- import_loadTweets(dataSet = "russian")

## Keep only English tweets. The rest are basically gibberish and not useful for 
## this project.
tweetData <- subset(tweetData, language == "English")

## If running in dev mode, take a small sample for development
if(runMode == "dev") {
  idx <- sample(row.names(tweetData), size = sampleSize, replace = FALSE)
  tweetData <- tweetData[idx, ]; rm(idx)
}

####################################################################################
## Update the names of the data
####################################################################################
names(tweetData)[names(tweetData) == "content"] <- "text"
names(tweetData)[names(tweetData) == "external_author_id"] <- "user_id"
names(tweetData)[names(tweetData) == "author"] <- "screen_name"

####################################################################################
## Extract the hashtags into a separate field
####################################################################################
tweetData$hashtags <- apply(tweetData[match("text", names(tweetData))], 1, scrub_storeHashtags)

####################################################################################
## Replace hashtags and user handles with actual persons' names
####################################################################################

## Load the replacement words
replacementWords <- import_fetchReplacementWords()

## Update replacement terms
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

save(tweetData, file = "data/cleanedRussianTweetData.RData")
rm(tweetData)


