####################################################################################
##                            ANALYZE NGRAMS
##
## This file will analyze the use of Ngrams within the Russian troll tweets. The 
## goal of this file is to identify Ngrams that are very common to use by Russian
## troll accounts. Then, when it comes time to build a model, we can identify whether
## or not a tweet contains one of these ngrams as a predictor.
####################################################################################

####################################################################################
## LOAD DATA
####################################################################################

## Load the saved cleaned Russian troll twitter data
load(file = "data/cleanedTweetData.RData")


####################################################################################
##                            TWO ELEMENT NGRAMS
####################################################################################

ngrams <- assemble_constructNgrams(data = tweetData, nGrams = 2, threshold = 0.001)


####################################################################################
##                            THREE ELEMENT NGRAMS
####################################################################################

ngrams <- assemble_constructNgrams(data = tweetData, nGrams = 3, threshold = 0.001)








