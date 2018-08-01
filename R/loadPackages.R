####################################################################################
##                            LOAD PACKAGES
##
## This file will initialize the program to run by loading all required packages and
## initializing any required custom functions and global parameters.
####################################################################################

####################################################################################
## Load required packages
####################################################################################

library(data.table)
library(ggplot2)
library(hunspell)
library(tm)
library(tidytext)
library(stringr)
library(keras)
library(caret)
library(readr)
library(dplyr)
library(longurl)
library(jsonlite)

####################################################################################
## Initialize data import functions
####################################################################################

#' Import Tweet Data
#' 
#' Import the CSV containing the tweets of the Russian troll accounts.
#'
#' @return A data frame containing the raw tweet data.
import_loadTweets <- function() {
  data <- read.csv(file = "data/russianTweets.csv", stringsAsFactors = FALSE)
  return(data)
}


####################################################################################
## Initialize utility functions
####################################################################################

#' Remove Garbage
#' 
#' This is a utility function that will remove garbage characters from a word. 
#' Garbage is defined as non-alpha-numeric characters, or technical computer
#' language like the linebreak command. This function will remove all garbage
#' from a word, and return the cleaned word.
#' 
#' @param x A string.
#'
#' @return A logical indicating whether or not the string is garbage.
utils_removeGarbage <- function(x = NA) {
  ## Define the list of characters that are considered garbage.
  garbage <- c("\n", "&amp", "https", "http")
  ## Remove all garbage characters from the word by replacing them with spaces
  for(i in garbage) x <- gsub(i, " ", x)
  ## Remove all punctuation except # and @. We need to preserve
  ## these symbols so that other functions can detect and remove
  ## entire hashtags and user IDs from tweets.
  x <- gsub("(?!#)(?!@)[[:punct:]]", " ", x, perl=TRUE)
  ## Remove all non-ASCII characters
  x <- iconv(x, "latin1", "ASCII", sub="")
  ## Return the result
  return(x)
}

#' Identify Hashtag
#' 
#' This is a utility function that will identify whether or not a word is
#' a hashtag. Any word that begins with the pound sign will result in this
#' function returning a value of TRUE. All other words will return FALSE.
#' 
#' @param x A string.
#'
#' @return A logical indicating whether or not the string is a hashtag.
utils_identHashtag <- function(x = NA) {
  ## Break the word into its constituent letters
  x <- unlist(strsplit(x, split = ""))
  ## If the first letter is a hashtag, return true. Otherwise, return false.
  if(length(x) > 0) {
    if(x[1] == "#") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

#' Identify URL
#' 
#' This is a utility function that will identify whether or not a word is
#' a URL. Any word that contains two forward slashes will result in this
#' function returning a value of TRUE. All other words will return FALSE.
#' 
#' @param x A string.
#'
#' @return A logical indicating whether or not the string is a URL.
utils_identURL <- function(x = NA) {
  ## If the string contains a web address, return true. Otherwise, return false.
  if(length(grep("://", x)) > 0){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Identify User
#' 
#' This is a utility function that will identify whether or not a word is
#' a user ID. Any word that begins with the @ sign will result in this
#' function returning a value of TRUE. All other words will return FALSE.
#' 
#' @param x A string.
#'
#' @return A logical indicating whether or not the string is a user ID.
utils_identUser <- function(x = NA) {
  ## Break the word into its constituent letters
  x <- unlist(strsplit(x, split = ""))
  ## If the first letter is a hashtag, return true. Otherwise, return false.
  if(length(x) > 0) {
    if(x[1] == "@") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

#' Identify Retweet
#' 
#' This is a utility function that will identify whether or not a word is
#' a retweet. Any word whose entire value is "RT" will result in this
#' function returning a value of TRUE. All other words will return FALSE.
#' 
#' @param x A string.
#'
#' @return A logical indicating whether or not the string is a retweet.
utils_identRetweet <- function(x = NA) {
  ## If the string contains a web address, return true. Otherwise, return false.
  if(x == "RT"){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Spellcheck Tweet
#' 
#' This is a utility function that will run a spellchecker on each word in a
#' tweet. If a word is not found to be a real word, the function will replace
#' that word with the first suggested replacement value returned by the spell-
#' checker function.
#' 
#' @param x A string.
#'
#' @return A logical indicating whether or not the string is a retweet.
utils_spellCheckTweet <- function(txt = NA, protectedWords = NA) {
  ## Break out the data into individual words
  txt <- unlist(strsplit(txt, split = " "))
  ## Run the spellchecker on each word
  txt <- unlist(lapply(txt, utils_spellCheckWord, protectedWords = protectedWords))
  ## Return the result
  return(paste(txt, sep = " ", collapse = " "))
}

#' Spellcheck Word
#' 
#' This is a utility function that checks if a word is a valid word. If
#' it isn't, then the function will replace it with the first suggested 
#' replacement value returned by the spell-checker function.
#' 
#' @param x A string.
#'
#' @return The updated string.
utils_spellCheckWord <- function(x = NA, protectedWords = NA) {
  ## Identify if the word is valid
  if(!hunspell_check(x)) {
    ## Make sure the word doesn't fall into the list of protected words
    if(!x %in% protectedWords) {
      ## Run the spellchecker on the word
      x <- unlist(hunspell_suggest(x))[1]
    }
  }
  return(x)
}

#' Count Stopwords
#' 
#' This is a utility function that will count the number of stopwords 
#' present in a string.
#' 
#' @param x A string.
#'
#' @return An integer value indicating how many elements of x are an English stopword.
utils_countStopwords <- function(x = NA) {
  ## Break the string into individual words
  x <- unlist(strsplit(x, split = " "))
  ## Return the number of stopwords
  return(length(intersect(x, stopwords("en"))))
}

#' Clean Text
#' 
#' This is a utility function that will clean a set of text by removing all punctuation
#' and transforming everything to lowercase.
#' 
#' @param x A string.
#'
#' @return The cleaned string.
utils_cleanText <- function(x = NA) {
  ## Remove all punctuation
  x <- gsub("[[:punct:]]", "", x, perl=TRUE)
  ## Format the text by transforming to lowercase and removing excess whitespace
  x <- tolower(x)
  x <- str_replace(gsub("\\s+", " ", str_trim(x)), "B", "b")
  ## Return
  return(x)
}

#' Calculate Modal Value
#' 
#' This is a utility function that provides a basic Mode function.
#' 
#' @param x A string.
#'
#' @return The cleaned string.
utils_calcMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Get Categorical Terms
#' 
#' This is a utility function that imports a list of strings, and returns the 
#' categorical terms associated with each string.
#' 
#' @param x A string.
#' @param termCategories
#'
#' @return The cleaned string.
utils_getCatTerms <- function(x = NA, termCategories = NA) {
  termCategories <- termCategories[match(intersect(x, termCategories$term), termCategories$term), ]
  return(termCategories$category)
}

#' Check for JSON Formatted String
#' 
#' This is a utility function that checks if a string is in JSON format.
#' 
#' @param x A string.
#'
#' @return Either FALSE, if the string is NOT a JSON, or the JSON-converted result if it is.
utils_checkJSON <- function(x) {
  out <- tryCatch({x = fromJSON(txt=x)},
                  error=function(cond) {
                    return(FALSE)
                  },
                  finally=function(cond) {
                    return(TRUE)
                  }
  )
  return(out)
}

####################################################################################
## Initialize data scrubbing functions
####################################################################################

#' Remove String
#' 
#' Tweets often contain strings that are undesirable when constructing a corpus -
#' things like URLs and hashtags. This is a wrapper function that will remove any
#' such undesirable string from a tweet.
#' 
#' @param txt A vector containing a list of words.
#' @param strType The type of string to be removed. Options include 'hashtag', 'url',
#' 'user', and 'retweet'.
#'
#' @return The same vector of words but with any and all hashtags removed.
scrub_removeString <- function(txt = NA, strType = NA) {
  ## Break out the data into individual words
  txt <- unlist(strsplit(txt, split = " "))
  ## Identify any 
  switch(strType, 
         "garbage" = {
           txt <- sapply(txt, utils_removeGarbage)
           return(paste(txt, sep = " ", collapse = " "))
         },
         "hashtag" = {
           target <- sapply(txt, utils_identHashtag)
           return(paste(txt[!target], sep = " ", collapse = " "))
         },
         "url" = {
           target <- sapply(txt, utils_identURL)
           return(paste(txt[!target], sep = " ", collapse = " "))
         },
         "user" = {
           target <- sapply(txt, utils_identUser)
           return(paste(txt[!target], sep = " ", collapse = " "))
         },
         "retweet" = {
           target <- sapply(txt, utils_identRetweet)
           return(paste(txt[!target], sep = " ", collapse = " "))
         }
  )
}

#' Replace String
#' 
#' Tweets often contain hashtags or user IDs that identify the subject of the tweet, rather
#' than the person's actual name. In this case, we want to replace the hashtags or user ID
#' handles with the person's actual name. This will allow the sentiment analysis algorithms
#' to provide more information about how a twitter user feels about each person.
#' 
#' @param x A string.
#' @param replacementWords A data frame containing a list of words and phrases to be replaced, 
#' along with their replacement values.
#'
#' @return The updated string.
scrub_replaceString <- function(txt = NA, replacementWords = NA) {
  ## Remove punctuation except '@' and '#' symbols
  txt <- gsub("(?!#)(?!@)[[:punct:]]", " ", txt, perl=TRUE)
  ## Define the list of characters that are considered garbage.
  garbage <- c("\n", "&amp", "https", "http")
  ## Remove all garbage characters from the word by replacing them with spaces
  for(i in garbage) {txt <- gsub(i, " ", txt); rm(i, garbage)}
  ## Remove all non-ASCII characters
  txt <- iconv(txt, "latin1", "ASCII", sub="")
  ## Change all text to lowercase
  txt <- tolower(txt)
  ## Check for any multi-word keywords, and replace them manually
  if(length(grep("new york times", txt)) > 0)
    txt <- gsub("new york times", "nytimes", txt)
  ## Break out the data into individual words
  txt <- unlist(strsplit(txt, split = " "))
  ## Identify any strings that have been specified for replacement
  txt <- unlist(lapply(txt, scrub_updateWord, replacementWords = replacementWords))
  ## Return the updated string
  return(paste(txt, sep = " ", collapse = " "))
}

#' Update Word
#' 
#' This is a utility function to check if a string has been assigned a replacement value. 
#' If it has, the function will return the replacement value for the string. Otherwise, it
#' will return the string itself without any modification.
#' 
#' @param x A string.
#' @param replacementWords A data frame containing a list of words and phrases to be replaced, 
#' along with their replacement values.
#'
#' @return The updated string.
scrub_updateWord <- function(x = NA, replacementWords = NA) {
  if(x %in% replacementWords$word) {
    return(replacementWords$replacement[replacementWords$word == x])
  } else {
    return(x)
  }
}

#' Format Twitter Date
#' 
#' This is a utility function to format the date field as provided by Twitter
#' 
#' @param x A date.
#'
#' @return The updated date.
scrub_formatDate <- function(x = NA) {
  ## Break the string by whitespace
  temp <- unlist(strsplit(x, split = " "))
  ## Reassemble in the correct format
  temp <- paste(paste0(temp[1], ","), temp[3], temp[2], temp[6], temp[4], temp[5])
  ## Return
  return(temp)
}

#' Store Hashtags
#' 
#' This function will examine each tweet, and store the hashtags contained within the tweet.
#' The "hashtags" field provided by twitter does not accurately reflect the hashtags contained
#' in each tweet, so we need to do this on our own.
#' 
#' @param txt A string.
#'
#' @return A list of hashtags found in the tweet.
scrub_storeHashtags <- function(txt = NA) {
  ## Break the tweet into its constituent words
  txt <- unlist(strsplit(txt, split = " "))
  ## Run each of the words through the hashtag identifier function
  x <- unlist(lapply(txt, utils_identHashtag))
  ## Keep the words that were identified as hashtags
  txt <- txt[x]
  ## Clean the results
  if(length(txt) > 0) txt <- sapply(txt, utils_removeGarbage)
  ## Return the hashtags as a list
  return(as.vector(txt))
}

####################################################################################
## Initialize data import functions
####################################################################################

#' Fetch Protected Words
#' 
#' This is a utility function that checks if a word is a valid word. If
#' it isn't, then the function will replace it with the first suggested 
#' replacement value returned by the spell-checker function.
#'
#' @return A vector of protected words.
import_fetchProtectedWords <- function() {
  protectedWords <- read.csv(file = "data/protectedWords.csv", stringsAsFactors = FALSE)
  return(protectedWords$word)
}

#' Fetch Replacement Words
#' 
#' This is a utility function that imports a list of words that need to
#' be replaced, as well as their replacement values.
#'
#' @return A vector of protected words.
import_fetchReplacementWords <- function() {
  replacementWords <- read.csv(file = "data/replacementWords.csv", stringsAsFactors = FALSE)
  return(replacementWords)
}


####################################################################################
## Initialize data assembly functions
####################################################################################

#' Construct Ngrams
#' 
#' This function constructs the most commonly used Ngrams from a corpus of tweets.
#' The corpus of tweets is derived from a data set that has been provided to the
#' function.
#' 
#' @param data A data frame containing cleaned twitter data.
#' @param nGrams The number of elements contained in each ngram.
#' @param threshold The minimum frequency an ngram must be used to be returned.
#'
#' @return A data frame containing the most commonly occuring ngrams.
assemble_constructNgrams <- function(data = NA, nGrams = 2, threshold = 0.001) {
  
  ## Put all of the tweets into one large corpus
  myCorpus <- paste(data$text, collapse = ". ")
  
  ## Now remove all punctuation
  myCorpus <- gsub("[[:punct:]]", "", myCorpus, perl=TRUE)
  
  ## Format the data by transforming to lowercase and removing excess whitespace
  myCorpus <- tolower(myCorpus)
  myCorpus <- str_replace(gsub("\\s+", " ", str_trim(myCorpus)), "B", "b")
  
  ## Create the Ngrams
  ng <- ngram(myCorpus, n = nGrams)
  
  ## Put the results into a data frame
  ng <- as.data.frame(get.phrasetable(ng))
  
  ## Now I want to identify the number of stopwords contained
  ## in each ngram. A lot of these ngrams are combinations of
  ## two stopwords. These are not useful to me.
  ng$stopwords <- apply(ng[match("ngrams", names(ng))], 1, utils_countStopwords)
  
  ## Remove any ngram where all words are stopwords
  ng <- subset(ng, stopwords < nGrams)
  
  ## Isolate only the ngrams whose frequencies exceed the specified threshold
  ng <- subset(ng, prop > threshold)
  
  ## Return
  return(ng)
  
}

#' Construct Lexical Scores
#' 
#' This function will analyze all of the tweets of a user, and construct the overall
#' lexical score from all of their tweets. The specific score that is calculated depends
#' on the data set that is provided to the function. For example, feeding the "afinn" 
#' data set into the function will result in the average affinity score being calculated.
#' 
#' @param txt A tweet text to be analyzed.
#' @param sentData A data frame containing the lexical data and scores to be calculated.
#' @param target A list containing a vector of strings. If this parameter is not specified,
#' then all words in the user's tweets will be analyzed. Otherwise, the two words on either
#' side of each target word will be analyzed for sentiment.
#'
#' @return A data frame containing the most commonly occuring ngrams.
assemble_constructLexicalScore <- function(txt, sentData = NA, target = NA) {
  ## Put all of the user's tweets into one corpus
  userCorpus <- paste(txt, collapse = ". ")
  ## Clean the corpus
  userCorpus <- utils_cleanText(userCorpus)
  ## Split into individual words
  userCorpus <- unlist(strsplit(userCorpus, split = " "))
  ## Check if a list of target words has been provided. If it has, 
  ## trim the userCorpus to only the target words and the two words
  ## on either side of the target words.
  if(class(target) == "list") {
    ## Loop through each word in the target list, and identify
    ## all places in the corpus where the word occurs.
    for(idx in unlist(target)) {
      ## Make sure the target appears in the corpus
      if(length(intersect(idx, userCorpus)) > 0) {
        ## Identify the index of the target word
        locs <- match(intersect(idx, userCorpus), userCorpus)
        ## Get the two words on either side of the target word
        locs <- seq(from = locs-2, to = locs+2)
        ## Bind the locations of the words to a master object. We need
        ## this in case multiple target words have been specified.
        if(!exists("allLocs")) {
          allLocs <- locs; rm(locs)
        } else {
          allLocs <- unique(c(allLocs, locs)); rm(locs)
        }
        ## Remove any location indices that are out of bounds
        allLocs <- allLocs[allLocs > 0 & allLocs <= length(userCorpus)]
      }
    }
    ## Trim the user corpus to only the target words
    if(exists("allLocs")) {
      userCorpus <- userCorpus[allLocs]
    } else {
      userCorpus <- character(0)
    }
  }
  ## Check to make sure data are remaining
  if(length(userCorpus) > 0) {
    ## Calculate sentiment for all words
    if(is.numeric(sentData$score)) {
      sentiment <- mean(sentData$score[match(intersect(sentData$word, userCorpus), sentData$word)])
    } else {
      sentiment <- utils_calcMode(sentData$score[match(intersect(sentData$word, userCorpus), sentData$word)])
    }
  } else {
    sentiment <- NA
  }
  ## Return the sentiment
  return(sentiment)
}

expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))

assemble_expandSubjects <- function(funcData = NA, termCategories = NA) {
  ## Identify the overall categorical terms each tweet contains
  catTerms <- unique(utils_getCatTerms(x = unlist(funcData$subject), termCategories = termCategories))
  ## Check to make sure there were terms found
  if(length(catTerms) > 0) {
    funcData <- expand.grid.df(funcData, data.frame("category" = catTerms))
  } else {
    funcData$category <- NA
  }
  ## Return the data
  return(funcData)
}

#' Construct All Sentiment Scores
#' 
#' This function is a wrapper function that will create all of the different sentiment scores
#' for a specified set of target words.
#' 
#' @param data A data frame containing all tweets by each user.
#' @param termCategories A data frame containing the categorical terms global object.
#'
#' @return A data frame containing the most commonly occuring ngrams.
assemble_constructAllSentiment <- function(data = NA, termCategories = NA) {
  
  ####################################################################################
  ## Break out each tweet by the overall categorical terms it contains. For example,
  ## if the main subjects of a tweet were "hillary clinton" and "donald trump", this
  ## will result in the tweet being duplicated. One of the records will have a new
  ## subject of "democrats", and the additional record will have a new subject of
  ## "republicans".
  ####################################################################################
  
  print("Expanding the data...")
  
  ## Faster solution, but not currently working
  newData <- suppressWarnings(apply(data, 1, FUN = assemble_expandSubjects, termCategories = termCategories) %>% bind_rows())
  data <- newData; rm(newData)

  ####################################################################################
  ## Calculate each user's overall sentiment
  ####################################################################################
  
  print("Constructing overall sentiment scores...")
  
  data$sentiment <- apply(data[match("text", names(data))], 1, 
                          assemble_constructLexicalScore,  
                          sentData = bing,
                          target = NA)
  
  ####################################################################################
  ## Calculate each user's overall affinity
  ####################################################################################
  
  print("Constructing affinity scores...")
  
  data$affinity <- apply(data[match("text", names(data))], 1, 
                         assemble_constructLexicalScore, 
                         sentData = afinn,
                         target = NA)
  
  ####################################################################################
  ## Calculate each user's overall emotional sentiment
  ####################################################################################
  
  print("Constructing emotion categories...")
  
  data$emotion <- apply(data[match("text", names(data))], 1, 
                        assemble_constructLexicalScore, 
                        sentData = nrc,
                        target = NA)
  
  ####################################################################################
  ## Calculate each user's overall loughran emotional sentiment
  ####################################################################################
  
  print("Constructing loughran categories...")
  
  data$loughran <- apply(data[match("text", names(data))], 1, 
                         assemble_constructLexicalScore, 
                         sentData = loughran,
                         target = NA)
  
  ## Return the data
  return(data)
  
}

#' Construct User Profiles
#' 
#' This function will construct the overall sentiment profile for each
#' user. This is done by simply taking the average (for numeric sentiments) 
#' or the mode (for categorical sentiments) across all of the user's tweets. 
#' We're going to calculate overall sentiment this way because if we instead 
#' merge all of a user's tweets into one large corpus and analyze them that 
#' way, we run the risk of getting biased sentiment results if we calculate 
#' sentiment values across multiple tweets.
#' 
#' @param data A data frame containing all tweets by each user.
#'
#' @return A data frame containing the each user's sentiment profile.
assemble_constructUserProfiles <- function(data) {
  ## Remove data records with missing categories
  data <- subset(data, !is.na(category))
  ## Change the data to a data table
  data <- data.table(data)
  ## Loop through each of the sentiment fields and calculate the mean or the mode
  for(i in c("sentiment", "affinity", "emotion", "loughran")) {
    eval(parse(text = paste0("data$idx <- data$", i)))
    if(is.numeric(data$idx)) {
      temp <- data[, .(idx = mean(idx, na.rm=TRUE)),
                   by = .(user_id, category)]
    } else {
      temp <- data[, .(idx = utils_calcMode(idx)),
                   by = .(user_id, category)]
    }
    ## Change the name of the target field
    names(temp)[names(temp) == "idx"] <- i
    ## Merge onto one main data frame to be returned
    if(!exists("userProfiles")) {
      userProfiles <- temp; rm(temp)
    } else {
      userProfiles <- merge(userProfiles, temp, by = c("user_id", "category")); rm(temp)
    }
  }
  ## Convert user profiles to wide format
  for(i in unique(userProfiles$category)) {
    tempdata <- subset(userProfiles, category == i)
    tempdata$category <- NULL
    for(j in 2:ncol(tempdata)) {
      names(tempdata)[j] <- paste(names(tempdata)[j], i, sep = "_")
    }; rm(j)
    if(!exists("finalData")) {
      finalData <- tempdata; rm(tempdata)
    } else {
      finalData <- merge(finalData, tempdata, by = "user_id", all = TRUE)
    }
  }
  ## Calculate the total number of tweets made per user, and the number of 
  ## tweets per category
  temp_all <- data[, .(total_tweets = .N),
               by = .(user_id)]
  temp_cat <- data[, .(total_tweets_by_cat = .N),
               by = .(user_id, category)]
  temp <- merge(temp_cat, temp_all, by = "user_id"); rm(temp_cat)
  temp$pct <- temp$total_tweets_by_cat / temp$total_tweets
  ## Convert categorical tweet percentages to wide format
  for(i in unique(temp$category)) {
    tempdata <- subset(temp, category == i)
    tempdata$category <- NULL
    for(j in 2:ncol(tempdata)) {
      names(tempdata)[j] <- paste(names(tempdata)[j], i, sep = "_")
    }; rm(j)
    finalData <- merge(finalData, tempdata, by = "user_id", all = TRUE); rm(tempdata)
  }
  ## Merge the total tweet counts onto the final data
  finalData <- merge(finalData, temp_all, by = "user_id", all = TRUE)
  ## Return the results
  return(finalData)
}

#' Identify Subject
#' 
#' This function will identify the subject of a tweet by looking for the most commonly-
#' occuring target word or words.
#' 
#' @param txt A data frame containing all tweets by each user.
#' @param termCategories A data frame containing the list of key terms and their 
#' associated category. See the description of the termCategories object for more info.
#'
#' @return A list of the most frequently occuring target words.
assemble_identifySubject <- function(txt, termCategories = NA) {
  ## Clean the tweet
  txt <- utils_cleanText(txt)
  ## Split into individual words
  txt <- unlist(strsplit(txt, split = " "))
  ## Isolate the target words
  txt <- txt[match(intersect(txt, termCategories$term), txt)]
  ## Check if there were no target words present
  if(length(txt) == 0) {
    return(NA)
  } else {
    ## Calculate the number of occurances of each word
    txt <- table(unlist(txt))
    ## Preserve only the most occuring words
    txt <- txt[txt == max(txt)]
    ## Return
    return(names(txt))
  }
}

#' Identify Common Hashtags
#' 
#' This function will identify the 3 most commonly-used hashtags tweeted by each
#' user, and return them as a list.
#' 
#' @param x All lists of hashtags contained across all of a user's tweets.
#'
#' @return A list of the 5 most frequently-used hashtags.
assemble_identifyCommonHashtags <- function(x, n = 5) {
  ## Check if there are at least 3 resu## Compile all of the user's hashtags into a single object
  vec <- unlist(x)
  ## Count the number of occurances of each hashtag
  vec <- as.data.frame(table(vec))
  if(nrow(vec) > 0) {
    ## Order by frequency, highest to lowest
    vec <- vec[order(-vec$Freq), ]
    ## Keep the top <n>
    vec <- vec[1:n, ]
    ## Change the hashtags to character format
    vec$vec <- as.character(vec$vec)
    ## If there were less than 3 hashtags but more than 0 hashtags,
    ## there will be some NA values in the resulting data frame. We
    ## need to replace these with a random hashtag that indicates a
    ## blank value.
    vec$Freq[is.na(vec$vec)] <- 1
    vec$vec[is.na(vec$vec)] <- "99"
    ## Return the hashtags
    return(vec$vec)
  } else {
   return(rep("99", times = n))
  }
}

#' Remove JSON
#' 
#' This function converts a list of hashtags that are stored in JSON format
#' to a list of strings.
#' 
#' @param txt A vector containing a list of hashtags.
#'
#' @return The same vector of words but with any and all hashtags removed.
scrub_removeJSON <- function(txt = NA) {
  ## Convert from JSON if necessary
  test <- utils_checkJSON(x=txt)
  if(class(test) != "logical") {
    return(test)
  } else {
    return(txt)
  }
}

#' Clean Hashtags
#' 
#' This function cleans the hashtags before they are to be vectorized.
#' 
#' @param x A hashtag.
#'
#' @return The cleaned hashtag.
scrub_cleanHashtags <- function(x) {
  ## Remove all punctuation
  x <- gsub("[[:punct:]]", " ", x, perl=TRUE)
  ## Remove all non-ASCII characters
  x <- iconv(x, "latin1", "ASCII", sub="")
  ## Remove all leading and trailing whitespace
  x <- sub("^\\s+", "", x)
  x <- sub("\\s+$", "", x)
  ## Convert to lowercase
  x <- tolower(x)
  ## Check for multiple items. If multiple items found,
  ## keep only the first one.
  x <- unlist(strsplit(x, split = " ")); x <- x[1]
  ## Return
  return(x)
}

#' Identify Common Websites
#' 
#' This function will identify the 3 most commonly-linked websites tweeted by each
#' user, and return them as a list.
#' 
#' @param x All lists of websites contained across all of a user's tweets.
#'
#' @return A list of the 3 most frequently-linked websites.
assemble_identifyCommonWebsites <- function(x, count = 3) {
  print(paste0("x = ", x))
  ## Check if there are at least 3 resu## Compile all of the user's hashtags into a single object
  vec <- unlist(x)
  ## Expand the URL(s)
  vec <- suppressWarnings(unlist(lapply(vec, scrub_expandURL)))
  ## Modify the website by keeping only the parent website (eg. if the link shared is
  ## "http://imdb.com/title/12345", the parent website is imdb.)
  vec <- unlist(lapply(vec, scrub_cleanURL))
  ## Count the number of occurances of each URL
  vec <- as.data.frame(table(vec))
  if(nrow(vec) > 0) {
    ## Order by frequency, highest to lowest
    vec <- vec[order(-vec$Freq), ]
    ## Keep the top 3
    vec <- vec[1:3, ]
    ## Change the hashtags to character format
    vec$vec <- as.character(vec$vec)
    ## If there were less than 3 hashtags but more than 0 hashtags,
    ## there will be some NA values in the resulting data frame. We
    ## need to replace these with a random hashtag that indicates a
    ## blank value.
    vec$Freq[is.na(vec$vec)] <- 1
    vec$vec[is.na(vec$vec)] <- "99"
    ## Return the hashtags
    return(vec$vec)
  } else {
    return(c("99", "99", "99")) 
  }
}

#' Clean URLs
#' 
#' This function cleans a URL by identifying the parent website.
#' 
#' @param x A URL.
#'
#' @return The cleaned URL.
scrub_cleanURL <- function(x) {
  testGlobal <<- x
  ## Identify the location of the 2nd and 3rd backslash
  temp <- unlist(strsplit(x, split = ""))
  ## Make sure the URL is valid
  if(length(temp) > 1) {
    idx1 <- which(temp == "/")[2] + 1
    idx2 <- which(temp == "/")[3] - 1
    ## Check if the second index is NA. If it is, it means that there
    ## was no second backslash, and we can just take the URL to the end.
    if(is.na(idx2)) idx2 <- length(temp)
    ## Identify the parent website
    x <- paste(temp[idx1:idx2], sep = "", collapse = "")
  }
  ## Return
  return(x)
}

#' Expand URLs
#' 
#' This function expands a URL to its full form.
#' 
#' @param x A URL.
#'
#' @return The expanded URL.
scrub_expandURL <- function(x) {
  ## Expand the URL
  big_url <- expand_urls(x)
  ## Check the status code
  if(is.na(big_url$status_code)) {
    ## Return the original URL
    return(big_url$orig_url)
  } else {
    if(big_url$status_code != 404) {
      ## Return the expanded URL
      return(big_url$expanded_url)
    } else {
      ## Return the original URL
      return(big_url$orig_url)
    }
  }
}

#' Clean Websites
#' 
#' This function cleans the website URLs before they are to be vectorized.
#' 
#' @param x A URL.
#'
#' @return The cleaned URL.
scrub_cleanWebsites <- function(x) {
  ## Remove all non-ASCII characters
  x <- iconv(x, "latin1", "ASCII", sub="")
  ## Remove all leading and trailing whitespace
  x <- sub("^\\s+", "", x)
  x <- sub("\\s+$", "", x)
  ## Convert to lowercase
  x <- tolower(x)
  ## Check for multiple items. If multiple items found,
  ## keep only the first one.
  x <- unlist(strsplit(x, split = " ")); x <- x[1]
  ## Return
  return(x)
}

####################################################################################
## Set global variables
####################################################################################

## Create an object that holds the categories of entities that we want to analyze the 
## sentiment for. For example, terms like "hillary", "clinton", and "obama" will all 
## go into the category of "democrats".
termCategories <- data.frame("term" = c("hillary",
                                        "clinton",
                                        "barack",
                                        "obama",
                                        "democrats",
                                        "progressives",
                                        "snowflake",
                                        "snowflakes",
                                        "liberal",
                                        "liberals",
                                        "donald", 
                                        "trump",
                                        "pence",
                                        "republicans",
                                        "conservatives",
                                        "abc", 
                                        "cnn",
                                        "msnbc",
                                        "cbsnews",
                                        "nytimes",
                                        "washpost",
                                        "fox", 
                                        "foxnews",
                                        "breitbart",
                                        "drudge",
                                        "drudgereport",
                                        "washtimes",
                                        "lorettalynch",
                                        "gop",
                                        "bannon",
                                        "kaine",
                                        "kaepernick",
                                        "takeaknee",
                                        "cruz",
                                        "maga",
                                        "rubio",
                                        "sanders",
                                        "feelthebern",
                                        "trumparmy",
                                        "blm",
                                        "carson",
                                        "dems",
                                        "demdebate"),
                             "category" = NA)

termCategories$category[termCategories$term == "hillary"] <- "democrats"
termCategories$category[termCategories$term == "clinton"] <- "democrats"
termCategories$category[termCategories$term == "barack"] <- "democrats"
termCategories$category[termCategories$term == "obama"] <- "democrats"
termCategories$category[termCategories$term == "democrat"] <- "democrats"
termCategories$category[termCategories$term == "democrats"] <- "democrats"
termCategories$category[termCategories$term == "progressives"] <- "democrats"
termCategories$category[termCategories$term == "snowflake"] <- "democrats"
termCategories$category[termCategories$term == "snowflakes"] <- "democrats"
termCategories$category[termCategories$term == "liberal"] <- "democrats"
termCategories$category[termCategories$term == "liberals"] <- "democrats"
termCategories$category[termCategories$term == "lorettalynch"] <- "democrats"
termCategories$category[termCategories$term == "kaine"] <- "democrats"
termCategories$category[termCategories$term == "kaepernick"] <- "democrats"
termCategories$category[termCategories$term == "takeaknee"] <- "democrats"
termCategories$category[termCategories$term == "sanders"] <- "democrats"
termCategories$category[termCategories$term == "feelthebern"] <- "democrats"
termCategories$category[termCategories$term == "blm"] <- "democrats"
termCategories$category[termCategories$term == "dems"] <- "democrats"
termCategories$category[termCategories$term == "demdebate"] <- "democrats"

termCategories$category[termCategories$term == "donald"] <- "republicans"
termCategories$category[termCategories$term == "trump"] <- "republicans"
termCategories$category[termCategories$term == "pence"] <- "republicans"
termCategories$category[termCategories$term == "republicans"] <- "republicans"
termCategories$category[termCategories$term == "conservatives"] <- "republicans"
termCategories$category[termCategories$term == "gop"] <- "republicans"
termCategories$category[termCategories$term == "bannon"] <- "republicans"
termCategories$category[termCategories$term == "maga"] <- "republicans"
termCategories$category[termCategories$term == "cruz"] <- "republicans"
termCategories$category[termCategories$term == "rubio"] <- "republicans"
termCategories$category[termCategories$term == "trumparmy"] <- "republicans"
termCategories$category[termCategories$term == "carson"] <- "republicans"

termCategories$category[termCategories$term == "abc"] <- "liberal_media"
termCategories$category[termCategories$term == "cnn"] <- "liberal_media"
termCategories$category[termCategories$term == "msnbc"] <- "liberal_media"
termCategories$category[termCategories$term == "cbsnews"] <- "liberal_media"
termCategories$category[termCategories$term == "nytimes"] <- "liberal_media"
termCategories$category[termCategories$term == "washpost"] <- "liberal_media"

termCategories$category[termCategories$term == "fox"] <- "conservative_media"
termCategories$category[termCategories$term == "foxnews"] <- "conservative_media"
termCategories$category[termCategories$term == "breitbart"] <- "conservative_media"
termCategories$category[termCategories$term == "drudge"] <- "conservative_media"
termCategories$category[termCategories$term == "drudgereport"] <- "conservative_media"
termCategories$category[termCategories$term == "washtimes"] <- "conservative_media"


