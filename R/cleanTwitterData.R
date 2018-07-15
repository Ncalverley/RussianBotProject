####################################################################################
##                            CONSTRUCT CORPORA
##
## This file will import the twitter data containing the full tweets of all the 
## Russian-linked troll accounts, and construct the corpora for each account.
####################################################################################


####################################################################################
## Load the data
####################################################################################

tweetData <- import_loadTweets()

## Take a small sample for development
tweetData <- tweetData[1:100, ]

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

# ####################################################################################
# ## Run a spellchecker on the tweets - we want to identify any mis-spelled words and
# ## replace them with their correct versions.
# ####################################################################################
# 
# ## Load the list of protected words
# protectedWords <- import_fetchProtectedWords()
# 
# ## Run the spellcheck
# tweetData$text <- apply(tweetData[match("text", names(tweetData))], 1, 
#                         utils_spellCheckTweet, 
#                         protectedWords = protectedWords)


####################################################################################
## Save the cleaned data
####################################################################################

save(tweetData, file = "data/cleanedTweetData.RData")
rm(tweetData)


