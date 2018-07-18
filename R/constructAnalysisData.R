####################################################################################
##                          CONSTRUCT USER PROFILE
##
## This file will analyze each user's language use based on all of their collective
## tweets. It will develop an overall language use profile for each user, as well as
## attempting to derive how each user speaks about both of the candidates by looking
## at the words immediately surrounding each candidate's name.
####################################################################################


####################################################################################
## Load the data sets
####################################################################################
load(file = "data/cleanedTweetData.RData")
positive_words <- read.csv("data/positive_words.csv", stringsAsFactors = FALSE)
negative_words <- read.csv("data/negative_words.csv", stringsAsFactors = FALSE)

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
## Calculate each user's overall sentiment
####################################################################################
tweetData <- assemble_constructAllSentiment(data = tweetData, 
                                           target = NA, 
                                           varName = "overall")

####################################################################################
## Calculate each user's overall sentiment towards Democrats
####################################################################################
tweetData <- assemble_constructAllSentiment(data = tweetData,
                                            target = list("hillary", 
                                                          "clinton",
                                                          "obama",
                                                          "democrats",
                                                          "progressives"), 
                                            varName = "democrats")

####################################################################################
## Calculate each user's overall sentiment towards Republicans
####################################################################################
tweetData <- assemble_constructAllSentiment(data = tweetData,
                                            target = list("donald", 
                                                          "trump",
                                                          "pence",
                                                          "republicans",
                                                          "conservatives"), 
                                            varName = "democrats")

####################################################################################
## Calculate each user's overall sentiment towards Liberal Media
####################################################################################
tweetData <- assemble_constructAllSentiment(data = tweetData, 
                                            target = list("abc", 
                                                          "cnn",
                                                          "msnbc",
                                                          "cbsnews",
                                                          "nytimes",
                                                          "washpost"), 
                                            varName = "liberal_media")

####################################################################################
## Calculate each user's overall sentiment towards Conservative Media
####################################################################################
tweetData <- assemble_constructAllSentiment(data = tweetData,
                                            target = list("fox", 
                                                          "foxnews",
                                                          "breitbart",
                                                          "drudge"), 
                                            varName = "conservative_media")

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
analysisData <- merge(userBasicProfileData, userSentimentProfileData, by = "user_id")

####################################################################################
## Save and clean up
####################################################################################
save(analysisData, file = "data/analysisData.RData")
rm(analysisData, userBasicProfileData, userSentimentProfileData, afinn, bing,
   loughran, nrc, negative_words, positive_words, tweetData)
