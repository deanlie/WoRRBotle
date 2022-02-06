library(tidyverse)

# Notes:
#  of 10230 words in /usr/share/dict/words,
#  "stare" will give at least 1 "somewhere" response for all but 920
#  "adieu", for all but 605
#  "until", misses 2050
#  "adieu" followed by "story" is guaranteed to have at least one hit


get_words_of_given_length <- function(wordLength = 5) {
  # Make letter_counts table like letter_counts.csv from /usr/share/dict/words
  allWords <- read_lines("/usr/share/dict/words")
  wordsWithGivenLength <- allWords[wordLength == str_length(allWords)]
}

# Moved routine filter_words_by_pattern to TopNRemainingWords.R

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

make_p_miss <- function(letter_counts) {
  p_miss <- letter_counts %>%
    mutate(One = 1.0, .after = Words) %>%
    mutate(Total = 10230.0, .after = One) %>%
    mutate(p_miss = One - Words/Total, .after = Words) %>%
    select(-One, -Total)
}

p_word_no_hits <- function(aWord, miss_probs, known_letters = c()) {
  # Estimate the probability that aWord has no letters in common with a random possible
  # word
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Return a the total of letter_counts[<all letters in a word>]
#' 
#' @param aWord A word
#' @param theCounts A tibble with Letter = c('a':'z') as it were,
#' Words = an integer
#' 
#' @return an integer
LCTotal <- function(aWord, countTable = countTable) {
  # sum(filter(letterCounts, Letter %in% unlist(str_split({aWord}, "")))$Words)
  message("letterCountTotal ", aWord)
  message("countTable", countTable[1,])
  sum(filter(countTable, Letter %in% unlist(str_split({aWord}, "")))$Words)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Return a vector of letterCountTotal for each word in a vector of words 
#' 
#' @param vectorOfWords A vector of words
#' @param letterCounts A tibble with Letter = c('a':'z') as it were,
#' Words = an integer
#' 
#' @return a tibble with vectorOfWords as first column, letterCountTotal of
#' each word as second column
letterCountTotalVector <- function(vectorOfWords, letterCounts) {
  FUN <- function(aWord) {
    theFilter <- letterCounts %>% 
      filter(Letter %in% unlist(str_split(aWord, "")))
    as.integer(sum(theFilter$Words))
  }
  
  # theCountVector <- unlist(lapply(vectorOfWords, FUN))
  # hgn <- tibble(Words = vectorOfWords, TotalCounts = theCountVector)

  # theCountVector <- unlist(lapply(vectorOfWords, FUN))
  hgn <- tibble(Words = vectorOfWords, TotalCounts = unlist(lapply(vectorOfWords,
                                                                   FUN)))
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
sortCandidatesByChanceOfHittingLetters <- function(candidateList) {
  # Count letter occurrence among words in target list
  candidateTibble <- tibble(Candidates = candidateList) %>%
    mutate(L1 = substr(Candidates, 1,1),
           L2 = substr(Candidates, 2,2),
           L3 = substr(Candidates, 3,3),
           L4 = substr(Candidates, 4,4),
           L5 = substr(Candidates, 5,5))
  
  summary <- candidateTibble %>%
    summarize(L1 = n_distinct(L1),
              L2 = n_distinct(L2),
              L3 = n_distinct(L3),
              L4 = n_distinct(L4),
              L5 = n_distinct(L5))
  
  # unmatchedLetters <- candidateTibble %>%
  #   select("Candidates", all_of([[summary]]))

  for (i in 2:6) {
    if (summary[i - 1] == 1)
      candidateTibble[,i] <- ""
  }
  
  reassemble <- candidateTibble %>%
    mutate(NoMatch = paste0(L1, L2, L3, L4, L5), .keep = "unused")
  
  # I now have a tibble with columns "Candidates" and "NoMatch". Count the number
  # of words each letter occurs in.
  letterCounts <<- countWordsByLetter(reassemble)

  # I want a vector of count of letter_counts[A] in a vector of words
  HGN <- reassemble %>%
    mutate(TotalCounts = letterCountTotal(NoMatch, {letterCounts}))
  return(HGN)
}


testSort <- function(aVectorOfWords = c("stamp", "trAmp", "cramp", "clAmp",
                                        "clump", "crimp")) {
  result <- sortCandidatesByChanceOfHittingLetters(aVectorOfWords)
}
