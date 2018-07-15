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
users <- read.csv("data/users.csv", stringsAsFactors = FALSE)
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
users$overall_sentiment <- apply(users[match("id", names(users))], 1, 
                                 assemble_constructLexicalScore, 
                                 data = tweetData, 
                                 sentData = bing)

####################################################################################
## Calculate each user's overall affinity
####################################################################################
users$overall_affinity <- apply(users[match("id", names(users))], 1, 
                                assemble_constructLexicalScore, 
                                 data = tweetData, 
                                 sentData = afinn)

####################################################################################
## Calculate each user's overall emotional sentiment
####################################################################################
users$overall_emotion <- apply(users[match("id", names(users))], 1, 
                               assemble_constructLexicalScore, 
                               data = tweetData, 
                               sentData = nrc)

####################################################################################
## Calculate each user's overall loughran emotional sentiment
####################################################################################
users$overall_loughran <- apply(users[match("id", names(users))], 1, 
                                assemble_constructLexicalScore, 
                                data = tweetData, 
                                sentData = loughran)

####################################################################################
## Calculate each user's overall sentiment towards Hillary Clinton
####################################################################################
users$clinton_sentiment <- apply(users[match("id", names(users))], 1, 
                                 assemble_constructLexicalScore, 
                                 data = tweetData, 
                                 sentData = bing,
                                 target = list("hillary", "clinton"))

####################################################################################
## Calculate each user's overall affinity towards Hillary Clinton
####################################################################################
users$clinton_affinity <- apply(users[match("id", names(users))], 1, 
                                assemble_constructLexicalScore, 
                                data = tweetData, 
                                sentData = afinn,
                                target = list("hillary", "clinton"))

####################################################################################
## Calculate each user's overall emotional sentiment towards Hillary Clinton
####################################################################################
users$clinton_emotion <- apply(users[match("id", names(users))], 1, 
                               assemble_constructLexicalScore, 
                               data = tweetData, 
                               sentData = nrc,
                               target = list("hillary", "clinton"))

####################################################################################
## Calculate each user's overall loughran emotional sentiment towards Hillary Clinton
####################################################################################
users$clinton_loughran <- apply(users[match("id", names(users))], 1, 
                                assemble_constructLexicalScore, 
                                data = tweetData, 
                                sentData = loughran,
                                target = list("hillary", "clinton"))

####################################################################################
## Calculate each user's overall sentiment towards Hillary Clinton
####################################################################################
users$trump_sentiment <- apply(users[match("id", names(users))], 1, 
                               assemble_constructLexicalScore, 
                               data = tweetData, 
                               sentData = bing,
                               target = list("donald", "trump"))

####################################################################################
## Calculate each user's overall affinity towards Hillary Clinton
####################################################################################
users$trump_affinity <- apply(users[match("id", names(users))], 1, 
                              assemble_constructLexicalScore, 
                              data = tweetData, 
                              sentData = afinn,
                              target = list("donald", "trump"))

####################################################################################
## Calculate each user's overall emotional sentiment towards Hillary Clinton
####################################################################################
users$trump_emotion <- apply(users[match("id", names(users))], 1, 
                             assemble_constructLexicalScore, 
                             data = tweetData, 
                             sentData = nrc,
                             target = list("donald", "trump"))

####################################################################################
## Calculate each user's overall loughran emotional sentiment towards Hillary Clinton
####################################################################################
users$trump_loughran <- apply(users[match("id", names(users))], 1, 
                              assemble_constructLexicalScore, 
                              data = tweetData, 
                              sentData = loughran,
                              target = list("donald", "trump"))






