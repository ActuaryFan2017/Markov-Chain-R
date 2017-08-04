
#-----------------------------------#
#   -Markov Chain Text generator-   #
#    created by: Ethan Edens        #  
#    Date: 8/4/2017                 #
#-----------------------------------#

# removes all global variables
rm(list = ls())

# Library
library(readtext)
require(stringi)
require(random)

# The next block of code formats the text so that there is no punctuation or double spaces as well as replacing all the "\n" with spaces
TEXT <- readtext("C:\\Users\\Ethan Edens\\Documents\\bookofmormon.txt")
TEXT <- stri_replace_all(TEXT, " ", fixed = "\n")
TEXT <- gsub('[[:punct:]]','',TEXT)
TEXT <- stri_replace_all(TEXT, " ", fixed = "  ")

# This creats a vector with all the words in order that they appear
WORDS <- unlist(strsplit(TEXT, split = " "))

# this removes all Numeric values
x <- (1:10000000)
IDX <- which(WORDS %in% x)
WORDS = WORDS[-IDX]

# This creates a dummy list of all unique words.  It is used later on in the MARKOV function also in creating a vector 
# containing all words and the words that follow them.
WORDS_VECTORS_DUMMY_VARIABLE <- WORDS[match(WORDS,WORDS) == seq_along(WORDS)]

# List of all unique words in the text
i <- 1
WORDS_VECTORS <- list()
while (i <= length(WORDS_VECTORS_DUMMY_VARIABLE)){
  
  WORDS_VECTORS[[i]] = WORDS_VECTORS_DUMMY_VARIABLE[i]
  i = i + 1
  
}

# Creates list that corespond with each unique word that shows all the words that come after it.
j <- 1
while (j <= length(WORDS_VECTORS)){
  
  x = WORDS[c(which(WORDS == WORDS_VECTORS[j]) + 1)]
  WORDS_VECTORS[[j]] = append(WORDS_VECTORS[[j]],x)
  j = j + 1
  
}

#---------------------------#
# The Markov Chain Function #
#---------------------------#
MARKOV <- function(){
  
  # Creates a random number that will dictate the length of the sentence.
  LENGTH_OF_SENTENCE = sample(15:20, 1,replace = TRUE)
  
  # Creates a starting number.
  seed = sample(1:length(WORDS_VECTORS), 1,replace = TRUE)
  
  # Creates the sentence vector as well as the first word.
  SENTENCE_DATA = WORDS_VECTORS[seed]
  SENTENCE_DATA = SENTENCE_DATA[[1]][1]
  CURRENT_WORD = WORDS_VECTORS[seed]
  CURRENT_WORD = CURRENT_WORD[[1]][1]
  
  # Builds the sentence by taking the words that would most likely follow the current word then makes that word the current word and runs again.
  # It runs till the lenght of the sentnece vector is equal to the length of the random sentnece length number.
  while (length(SENTENCE_DATA) < LENGTH_OF_SENTENCE){
      
    CURRENT_INDEX = match(CURRENT_WORD,WORDS_VECTORS_DUMMY_VARIABLE)
    NEW_WORD = WORDS_VECTORS[CURRENT_INDEX]
    NEW_WORD = NEW_WORD[[1]][sample(2:length(NEW_WORD[[1]]), 1,replace = TRUE)]
    SENTENCE_DATA = append(SENTENCE_DATA, NEW_WORD)
    CURRENT_WORD = NEW_WORD
    
  }
  SENTENCE_DATA = paste(unlist(SENTENCE_DATA), collapse = ' ')
  return(SENTENCE_DATA)
}

  
MARKOV()  
  

