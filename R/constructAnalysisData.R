####################################################################################
##                          CONSTRUCT USER PROFILE
##
## This file will analyze each user's language use based on all of their collective
## tweets. It will develop an overall language use profile for each user, as well as
## attempting to derive how each user speaks about both of the candidates by looking
## at the words immediately surrounding each candidate's name.
####################################################################################


####################################################################################
## Load the cleaned data sets
####################################################################################
load(file = "data/cleanedLegitTweetData.RData"); legitData <- tweetData; rm(tweetData)
load(file = "data/cleanedRussianTweetData.RData"); russianData <- tweetData; rm(tweetData)

####################################################################################
## Rename a bunch of fields in the legit data to match the Russian troll data
####################################################################################
names(legitData)[names(legitData) == "user_id_str"] <- "user_id"
names(legitData)[names(legitData) == "full_name"] <- "name"
names(legitData)[names(legitData) == "expanded_url"] <- "expanded_urls"
names(legitData)[names(legitData) == "id_str"] <- "tweet_id"
names(legitData)[names(legitData) == "user_lang"] <- "lang"
names(legitData)[names(legitData) == "created_at"] <- "created_str"
names(legitData)[names(legitData) == "followers_count"] <- "followers"
names(legitData)[names(legitData) == "lang"] <- "language"

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
tweetData <- rbind(russianData, legitData); rm(russianData, legitData)

####################################################################################
## Vectorize each group's most commonly-used hashtags
####################################################################################

## Determine each user's most commonly-used hashtags
userHashtags <- data.table(tweetData)
userHashtags <- userHashtags[, .(hashtag = assemble_identifyCommonHashtags(hashtags, n = nHashtags),
                                 troll = max(troll, na.rm=TRUE)),
                             by = .(user_id)]

## Determine the most commonly-used hashtags among trolls and non-trolls
groupHashtags <- data.table(userHashtags)
groupHashtags <- groupHashtags[, .(count = .N),
                               by = .(troll, hashtag)]
groupHashtags$total_count <- NA
groupHashtags$total_count[groupHashtags$troll == 1] <- length(unique(userHashtags$user_id[userHashtags$troll == 1]))
groupHashtags$total_count[groupHashtags$troll == 0] <- length(unique(userHashtags$user_id[userHashtags$troll == 0]))

## Get the hashtag percentages
groupHashtags$pct <- groupHashtags$count / groupHashtags$total_count

## Get the top 5 hashtags by group
groupHashtags_troll <- subset(groupHashtags, troll == 1)
groupHashtags_legit <- subset(groupHashtags, troll == 0)
groupHashtags_troll <- groupHashtags_troll[order(-groupHashtags_troll$pct), ]
groupHashtags_legit <- groupHashtags_legit[order(-groupHashtags_legit$pct), ]


## Convert the results back to a data frame
userHashtags <- setDF(userHashtags)

## Standardize the hashtags by cleaning them and transforming them all to lowercase
userHashtags$hashtag <- apply(userHashtags[match("hashtag", names(userHashtags))], 1, scrub_cleanHashtags)

## Vectorize the hashtags
tokenizer <- text_tokenizer(num_words = length(unique(userHashtags$hashtag)))
tokenizer %>% fit_text_tokenizer(unique(userHashtags$hashtag))
temp <- texts_to_sequences(tokenizer, userHashtags$hashtag)
for(i in 1:length(temp)) {
  if(is.null(unlist(temp[i]))) {
    userHashtags$hashtag[i] <- 99
  } else {
    userHashtags$hashtag[i] <- unlist(temp[i])
  }
}; rm(temp, i)

## Put in a key value for reshaping the data from long to wide
userHashtags$key <- rep(c(1:nHashtags), times = nrow(userHashtags) / nHashtags)

## Reshape the data to wide format
userHashtags <- reshape(userHashtags, idvar = "user_id", timevar = "key", direction = "wide")

## Remove the hashtag lists from the data - we don't need it anymore, and it messes with
## some functions that come into play further down.
tweetData$hashtags <- NULL

####################################################################################
## Vectorize each user's most commonly-linked websites
##
## This section takes FOREVER to run because of the function that transforms tiny
## URLs into expanded URLs, and the addition of commonly-linked websites to the
## model didn't significantly improve accuracy, so I'm commenting this out for now.
####################################################################################

# ## Determine each user's most commonly-used hashtags
# userWebsites <- data.table(tweetData)
# userWebsites <- userWebsites[, .(website = assemble_identifyCommonWebsites(expanded_urls)),
#                              by = .(user_id)]
# 
# ## Convert the results back to a data frame
# userWebsites <- setDF(userWebsites)
# 
# ## Standardize the hashtags by cleaning them and transforming them all to lowercase
# userWebsites$website <- apply(userWebsites[match("website", names(userWebsites))], 1, scrub_cleanWebsites)
# 
# ## Vectorize the hashtags
# tokenizer <- text_tokenizer(num_words = length(unique(userWebsites$website)))
# tokenizer %>% fit_text_tokenizer(unique(userWebsites$website))
# temp <- texts_to_sequences(tokenizer, userWebsites$website)
# for(i in 1:length(temp)) {
#   if(is.null(unlist(temp[i]))) {
#     userWebsites$website[i] <- 99
#   } else {
#     userWebsites$website[i] <- unlist(temp[i])
#   }
# }
# 
# ## Put in a key value for reshaping the data from long to wide
# userWebsites$key <- rep(c(1:3), times = nrow(userWebsites) / 3)
# 
# ## Reshape the data to wide format
# userWebsites <- reshape(userWebsites, idvar = "user_id", timevar = "key", direction = "wide")

## Remove the website lists from the data - we don't need it anymore, and it messes with
## some functions that come into play further down.
# tweetData$expanded_urls <- NULL

####################################################################################
## Extract sentiment data from tidytext and put into a data set
####################################################################################

afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")
loughran <- get_sentiments("loughran")

## Format the bing data set. I want the sentiment value to be a numeric score so
## that I can create a numeric representation of sentiment.
bing <- within(bing, {
  score <- NA
  score[sentiment == "negative"] <- -1
  score[sentiment == "positive"] <- 1
  sentiment <- NULL
})

## Format the nrc data set. I need the column containing the sentiment value to be
## called "score" so that I can use it in the assemble_constructLexicalScore function.
names(nrc)[names(nrc) == "sentiment"] <- "score"

## Format the loughran data set. I need the column containing the sentiment value to be
## called "score" so that I can use it in the assemble_constructLexicalScore function.
names(loughran)[names(loughran) == "sentiment"] <- "score"

####################################################################################
## Identify the subject or subjects of each tweet
####################################################################################
tweetData$subject <- apply(tweetData[match("text", names(tweetData))], 1, assemble_identifySubject,
                           termCategories = termCategories)

####################################################################################
## Calculate the sentiment and emotions of each tweet towards each major group.
## This function WILL result in additional records being created.
####################################################################################
tweetData <- assemble_constructAllSentiment(data = tweetData,
                                            termCategories = termCategories)

####################################################################################
## Calculate each user's overall sentiment towards each of the target entities. This
## is done by simply taking the average (for numeric sentiments) or the mode (for
## categorical sentiments) across all of the user's tweets. We're going to calculate
## overall sentiment this way because if we instead merge all of a user's tweets
## into one large corpus and analyze them that way, we run the risk of getting biased
## sentiment results if we calculate sentiment values across multiple tweets.
####################################################################################
userSentimentProfileData <- assemble_constructUserProfiles(data = tweetData)

####################################################################################
## Isolate each user's basic profile data and calculate some basic information about
## the account.
####################################################################################

## Isolate unique user IDs, and the needed fields
userBasicProfileData <- subset(tweetData, !duplicated(user_id),
  select = c("user_id",
             "screen_name",
             "language",
             "followers",
             "troll"))

####################################################################################
## Construct the final analysis data set
####################################################################################

## Merge the profile and sentiment data
analysisData <- merge(userBasicProfileData, userSentimentProfileData, by = "user_id", all = TRUE)

## Merge the profile and vectorized hashtag data
analysisData <- merge(analysisData, userHashtags, by = "user_id", all = TRUE)

## Merge the profile and vectorized website data
# analysisData <- merge(analysisData, userWebsites, by = "user_id", all = TRUE)

####################################################################################
## Save and clean up
####################################################################################
save(analysisData, file = "data/analysisData.RData")
rm(analysisData, userBasicProfileData, userSentimentProfileData, afinn, bing,
   loughran, nrc, tweetData, userHashtags)
