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
## Initialize lexical manipulation functions
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
  garbage <- c("\n", "&amp")
  ## Remove all garbage characters from the word
  for(i in garbage) x <- gsub(i, "", x)
  ## Remove all punctuation except # and @. We need to preserve
  ## these symbols so that other functions can detect and remove
  ## entire hashtags and user IDs from tweets.
  x <- gsub("(?!#)(?!@)[[:punct:]]", "", x, perl=TRUE)
  ## Remove all non-ASCII characters
  x <- iconv(x, "latin1", "ASCII", sub="")
  ## Return the result
  return(x)
}

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




####################################################################################
## Set global variables
####################################################################################






