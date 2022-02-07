library(tidyverse)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Count how many words in a vector of words match a given pattern
#' 
#' @param vectorOfWords A vector of words to examine for a pattern
#' @param aPattern A pattern to look for
#'
#' @returns the number of words which match the pattern
number_of_matches <- function(vectorOfWords, aPattern) {
  length(vectorOfWords[str_detect(vectorOfWords, aPattern)])
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Count how many words in input tibble contain each letter A-Z, case
#' insensitive
#' 
#' @param candidateTibble A vector of words, count letter presence in it
#' 
#' @return A a 2-column tibble, Letter = c("a":"z"), Words = number of words
#' in a vector of words input to the function which contain the Letter
countWordsByLetter <- function(candidateTibble) {
  theAlphabet <-  "abcdefghijklmnopqrstuvwxyz"
  letterVector <- unlist(strsplit(theAlphabet, ""))
  countVector <- rep(0, times = length(letterVector))
  vectorOfWords <- candidateTibble$NoMatch
  for (i in 1:length(letterVector)) {
    # Make pattern '[aA]' from input 'a' or 'A'
    aPattern <- paste0("[",
                       str_to_upper(letterVector[i]),
                       str_to_lower(letterVector[i]),
                       "]")
    countVector[i] <- number_of_matches(vectorOfWords, aPattern)
  }
  letter_counts <- tibble(Letter = letterVector, Words = countVector)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Return a vector of letterCountTotal for each word in a vector of words 
#' 
#' @param vectorOfWords A vector of words
#' @param letterCounts A tibble with Letter = c('a':'z') as it were,
#' Words = an integer
#' 
#' @return a vector of the sum of letterCounts for all letters of each word
letterCountTotalVector <- function(vectorOfWords, letterCounts) {
  # FUN <- function(aWord) {
  #   theFilter <- letterCounts %>% 
  #     filter(Letter %in% unlist(str_split(aWord, "")))
  #   as.integer(sum(theFilter$Words))
  # }
  # 
  # theCountVector0 <- unlist(lapply(vectorOfWords, FUN))
  # 
  theCountVector <- unlist(lapply(vectorOfWords,
                                  function(aWord) {
                                    theFilter <- letterCounts %>% 
                                      filter(Letter %in% unlist(str_split(aWord, "")))
                                    as.integer(sum(theFilter$Words))
                                  }))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Sort the list of candidate words (suggested next guess) by probability of
#' hitting at least one UNMATCHED letter in a random word chosen from that
#' list, that is, a letter in a position where no guess has returned green.
#' Every candidate has all the matched letters in common with every other one,
#' so we split all the words, count how many distinct letters there are in
#' each position, throw away those positions, count how many times each letter
#' occurs in what's left; use those counts to compute probability of a word
#' not hitting anything useful.
#' 
#' @param candidateList A vector of words which are possible solutions given what we
#' know so far
#' @return That list sorted by the word which hits the most others in unmatched places
#' (and therefore eliminates the most if it returns empty)
sortCandidatesByUnmatchedLettersHit <- function(candidateList) {
  message("sortCandidatesByUnmatchedLettersHit")

  # Count letter occurrence among words in target list
  candidateTibble <- tibble(Candidates = candidateList) %>%
    mutate(L1 = substr(Candidates, 1,1),
           L2 = substr(Candidates, 2,2),
           L3 = substr(Candidates, 3,3),
           L4 = substr(Candidates, 4,4),
           L5 = substr(Candidates, 5,5))
  
  message("Split them into lists of letters!")
  
  summary <- candidateTibble %>%
    summarize(L1 = n_distinct(L1),
              L2 = n_distinct(L2),
              L3 = n_distinct(L3),
              L4 = n_distinct(L4),
              L5 = n_distinct(L5))
  
  # unmatchedLetters <- candidateTibble %>%
  #   select("Candidates", all_of([[summary]]))
  
  # Replace the letters all candidates have in common with blank
  for (i in 2:6) {
    if (summary[i - 1] == 1)
      candidateTibble[,i] <- ""
  }

  message("Zapped common letters!")
  
  # Paste back all the letters that are not in common. These are the ones
  # that we need more information on.
  reassemble <- candidateTibble %>%
    mutate(NoMatch = paste0(L1, L2, L3, L4, L5), .keep = "unused")

  message("Reassembled them!")
  
  # I now have a tibble with columns "Candidates" and "NoMatch". Count the number
  # of words each letter occurs in.
  letterCounts <- countWordsByLetter(reassemble)

  message("Counted their letters!")
  
  # I want a vector of sum(letter_counts[foreach(letter in the word)]
  # corresponding to a vector of words
  theLetterCountVector <- letterCountTotalVector(unlist(reassemble$NoMatch), letterCounts)
  
  wordsAndCounts <- tibble(Words = candidateList, TotalCounts = unlist(theLetterCountVector))
  
  topCountsFirst <- arrange(wordsAndCounts, desc(TotalCounts))
  
  return(unlist(topCountsFirst$Words))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Sort a list of candidate words to have the most informative ones highest
#' @param remainingWords A vector of candidate words
#' @param nToKeep The number to pass along
#'
#' @return The nToKeep most informative candidates
topNRemainingWords <- function(remainingWords, nToKeep) {
  if (is.null(remainingWords)) {
    paste(tags$h4("Start typing!", class="suggestions"))
  } else {
    message("topNRemaining - number of remaining words: ", length(remainingWords))

    # Sort the input list first
    sortedRemainingWords <- sortCandidatesByUnmatchedLettersHit(remainingWords)

    message("Finished sorting them!")

    # Clamp list to desired length
    if (length(sortedRemainingWords) > nToKeep) {
      sortedRemainingWords <- str_to_upper(head(sortedRemainingWords, nToKeep))
    } else {
      sortedRemainingWords <- str_to_upper(sortedRemainingWords)
    }
    
    message("Uppercased them!")
    
    listOfPTags <- lapply(sortedRemainingWords,
                          function(aWord) as.character(tags$p(aWord,
                                                              class = "suggestions")))
  
    message("Created HTML list!")

    paste(tags$h4("Suggestions:", class="suggestions"),
          paste(unlist(listOfPTags), collapse = ""))
  }
}
