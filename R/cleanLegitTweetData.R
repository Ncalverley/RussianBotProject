####################################################################################
##                            CLEAN RUSSIAN TROLL DATA
##
## This file will import the twitter data containing the full tweets of all the 
## Russian-linked troll accounts, and construct the corpora for each account.
####################################################################################


####################################################################################
## Load the data
####################################################################################

load(file = "data/legitTwitterData.RData")

## Take a small sample for development
tweetData <- tweetData[1:1000, ]
tweetData$original_text <- tweetData$text

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


