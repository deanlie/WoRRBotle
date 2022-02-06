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

make_letter_counts <- function(vectorOfWords) {
  # Make table like letter_counts.csv from list of words of given length
  theAlphabet <-  "abcdefghijklmnopqrstuvwxyz"
  letterVector <- unlist(strsplit(theAlphabet, ""))
  countVector <- rep(0, times = length(letterVector))
  for (i in 1:length(letterVector)) {
    countVector[i] <- number_of_matches(vectorOfWords, letterVector[i])
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
  # of words each letter occurs in (see or modify make_letter_counts). It's going
  # to become clearer in the morning.
  
  return(reassemble)
}

# Moved function pattern_for_match to TopNRemainingWords.R
# Moved function pattern_for_not_here to TopNRemainingWords.R
# Moved function negative_pattern_for_not_here to TopNRemainingWords.R
# Moved function negative_pattern_for_nowhere to TopNRemainingWords.R
# Moved function possibilities_from_one_probe_letter to TopNRemainingWords.R
# Moved function possibilities_from_one_probe to TopNRemainingWords.R
# Moved function possibilities_from_history to TopNRemainingWords.R
# Moved function evaluate_a_guess to its own module EvaluateAGuess.R
# Moved function probe_from_guess to TopNRemainingWords.R

possibilities_from_guesses <- function(theTarget, theGuesses, vectorOfWords = NULL,
                                       traceThisRoutine = FALSE, prepend = "") {
  theProbes <- vector()
  for (i in length(theGuesses)) {
    theProbes[i] <- probe_from_guess(theTarget, theGuesses[i])
  }
  possibilities_from_history(theProbes, vectorOfWords = vectorOfWords,
                             debug = FALSE, prepend = myPrepend)
}

testFilter <- function(switchArg) {
  cat(file = stderr(), "\n\nTesting filter in mode", switchArg, "\n")
  theProbes <- switch(switchArg,
                      one = c("starexxxYx", "untilxxxYx", "comfyGxYxx", "crimpGGGGG"),
                      two = c("starexxxYx", "untilxxxYx", "comfyGxYxx", "crimpGGGGG"),
                      three = c("starexxxYx", "untilxxxYx", "comfyGxYxx", "crimpGGGGG"),
                      four = c("starexxxYx", "untilxxxYx", "comfyGxYxx", "crimpGGGGG"))
  theWordVector <- switch(switchArg,
                          one = NULL,
                          two = NULL,
                          three = c("stamp", "tramp", "cramp", "clamp", "axiom",
                                    "clump", "chirp", "brick", "corgi", "crick",
                                    "crier", "crimp"),
                          four = c("stamp", "tramp", "cramp",
                                    "clump", "chirp", "brick", "corgi", "crick",
                                    "crier", "crimp"))
  possibilities_from_history(theProbes, theWordVector, traceThisRoutine = TRUE)
}

testSort <- function(aVectorOfWords = c("stamp", "tramp", "cramp", "clamp",
                                        "clump", "crimp")) {
  result <- sortCandidatesByChanceOfHittingLetters(aVectorOfWords)
}
