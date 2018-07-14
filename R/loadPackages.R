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

####################################################################################
## Initialize data import functions
####################################################################################

#' Import Tweet Data
#' 
#' Import the CSV containing the tweets of the Russian troll accounts.
#'
#' @return A data frame containing the raw tweet data.
import_loadTweets <- function() {
  data <- read.csv(file = "data/tweets.csv", stringsAsFactors = FALSE)
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
  x <- gsub("(?!#)(?!@)[[:punct:]]", "", x, perl=TRUE)
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



####################################################################################
## Set global variables
####################################################################################






