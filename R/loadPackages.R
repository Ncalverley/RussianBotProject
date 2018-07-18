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


#' Construct All Sentiment Scores
#' 
#' This function is a wrapper function that will create all of the different sentiment scores
#' for a specified set of target words.
#' 
#' @param userID A numeric indicating the ID of the user.
#' @param data A data frame containing all tweets by each user.
#' @param sentData A data frame containing the lexical data and scores to be calculated.
#' @param target A list containing a vector of strings. If this parameter is not specified,
#' then all words in the user's tweets will be analyzed. Otherwise, the two words on either
#' side of each target word will be analyzed for sentiment.
#'
#' @return A data frame containing the most commonly occuring ngrams.
assemble_constructAllSentiment <- function(data = NA, target = NA, varName = NA, sentData = NA) {
  
  ####################################################################################
  ## Calculate each user's overall sentiment
  ####################################################################################
  print("Running overall sentiment")
  data$sentiment <- apply(data[match("text", names(data))], 1, 
                          assemble_constructLexicalScore,  
                          sentData = bing,
                          target = target)
  
  ####################################################################################
  ## Calculate each user's overall affinity
  ####################################################################################
  print("Running overall affinity")
  data$affinity <- apply(data[match("text", names(data))], 1, 
                         assemble_constructLexicalScore, 
                         sentData = afinn,
                         target = target)
  
  ####################################################################################
  ## Calculate each user's overall emotional sentiment
  ####################################################################################
  print("Running overall emotion")
  data$emotion <- apply(data[match("text", names(data))], 1, 
                        assemble_constructLexicalScore, 
                        sentData = nrc,
                        target = target)
  
  ####################################################################################
  ## Calculate each user's overall loughran emotional sentiment
  ####################################################################################
  print("Running overall loughran")
  data$loughran <- apply(data[match("text", names(data))], 1, 
                         assemble_constructLexicalScore, 
                         sentData = loughran,
                         target = target)
  
  ####################################################################################
  ## Append the specified variable name onto the sentiment fields
  ####################################################################################
  names(data)[names(data) == "sentiment"] <- paste(names(data)[names(data) == "sentiment"], varName, sep = "_")
  names(data)[names(data) == "affinity"] <- paste(names(data)[names(data) == "affinity"], varName, sep = "_")
  names(data)[names(data) == "emotion"] <- paste(names(data)[names(data) == "emotion"], varName, sep = "_")
  names(data)[names(data) == "loughran"] <- paste(names(data)[names(data) == "loughran"], varName, sep = "_")
  
  
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
  
  ## Identify the fields that need to be calculated
  calcFields <- names(data)[grep("sentiment", names(data))]
  ## Remove the sentiment_ prefix
  calcFields <- gsub("sentiment", "", calcFields)
  ## Now identify all fields containing the values contained in the calcFields
  ## object. This will get us all of our target fields to be aggregated.
  allFields <- character(0)
  for(i in calcFields) {
    allFields <- c(allFields, names(data)[grep(i, names(data))])
  }; rm(i, calcFields)
  ## Remove duplicates
  allFields <- unique(allFields)
  ## Change the data to a data table
  data <- data.table(data)
  ## Loop through each field and calculate the mean or the mode
  for(i in allFields) {
    eval(parse(text = paste0("data$idx <- data$", i)))
    if(is.numeric(data$idx)) {
      temp <- data[, .(idx = mean(idx, na.rm=TRUE)),
                   by = .(user_id)]
    } else {
      temp <- data[, .(idx = utils_calcMode(idx)),
                   by = .(user_id)]
    }
    ## Change the name of the target field
    names(temp)[names(temp) == "idx"] <- i
    ## Merge onto one main data frame to be returned
    if(!exists("userProfiles")) {
      userProfiles <- temp; rm(temp)
    } else {
      userProfiles <- merge(userProfiles, temp, by = "user_id"); rm(temp)
    }
  }
  ## Return the results
  return(userProfiles)
}

####################################################################################
## Set global variables
####################################################################################






