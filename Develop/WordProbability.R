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
                             debug = traceThisRoutine, prepend = myPrepend)
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
