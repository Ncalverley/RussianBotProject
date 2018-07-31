####################################################################################
##                            CLEAN RUSSIAN TROLL DATA
##
## This file will import the twitter data containing the full tweets of all the 
## Russian-linked troll accounts, and construct the corpora for each account.
####################################################################################


####################################################################################
## Load the data
####################################################################################

tweetData <- import_loadTweets()

## If running in dev mode, take a small sample for development
if(runMode == "dev") {
  tweetData <- tweetData[1:10000, ]
  tweetData$original_text <- tweetData$text
}

####################################################################################
## Convert the hashtags and expanded URLs from JSON format to strings
####################################################################################
tweetData$hashtags <- apply(tweetData[match("hashtags", names(tweetData))], 1, scrub_removeJSON)
tweetData$expanded_urls <- apply(tweetData[match("expanded_urls", names(tweetData))], 1, scrub_removeJSON)

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

save(tweetData, file = "data/cleanedRussianTweetData.RData")
rm(tweetData)


