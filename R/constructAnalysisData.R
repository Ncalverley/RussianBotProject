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
tweetData <- rbind(russianData, legitData); rm(russianData, legitData)

####################################################################################
## Vectorize each user's most commonly-used hashtags
####################################################################################

## Determine each user's most commonly-used hashtags
userHashtags <- data.table(tweetData)
userHashtags <- userHashtags[, .(hashtag = assemble_identifyCommonHashtags(hashtags)),
                             by = .(user_id)]

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
}

## Put in a key value for reshaping the data from long to wide
userHashtags$key <- rep(c(1:3), times = nrow(userHashtags) / 3)

## Reshape the data to wide format
userHashtags <- reshape(userHashtags, idvar = "user_id", timevar = "key", direction = "wide")

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
             "name",
             "description",
             "verified",
             "user_created_at",
             "statuses_count",
             "followers_count",
             "favourites_count",
             "friends_count",
             "time_zone",
             "troll"))

## Format the account creation dates
userBasicProfileData$user_created_at <- apply(userBasicProfileData[match("user_created_at", names(userBasicProfileData))], 
                                              1, scrub_formatDate)

## Update the creation dates to a usable format
userBasicProfileData$user_created_at <- strptime(userBasicProfileData$user_created_at, 
                                                 "%a, %d %b %Y %H:%M:%S %z", tz = "GMT")
userBasicProfileData$user_created_at <- as.POSIXct(userBasicProfileData$user_created_at, tz = "GMT")
userBasicProfileData$user_created_at <- as.Date(userBasicProfileData$user_created_at, format="%Y/%m/%d")

## Put in the current date
userBasicProfileData$current_date <- as.Date(Sys.Date(), format="%Y/%m/%d")

## Calculate the age of the account
userBasicProfileData$account_age <- as.numeric(userBasicProfileData$current_date - userBasicProfileData$user_created_at)

####################################################################################
## Construct the final analysis data set
####################################################################################

## Merge the profile and sentiment data
analysisData <- merge(userBasicProfileData, userSentimentProfileData, by = "user_id", all = TRUE)

## Merge the profile and vectorized hashtag data
analysisData <- merge(analysisData, userHashtags, by = "user_id", all = TRUE)

## Merge the profile and vectorized website data
analysisData <- merge(analysisData, userWebsites, by = "user_id", all = TRUE)

####################################################################################
## Save and clean up
####################################################################################
save(analysisData, file = "data/analysisData.RData")
rm(analysisData, userBasicProfileData, userSentimentProfileData, afinn, bing,
   loughran, nrc, tweetData)
