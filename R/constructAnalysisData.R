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















