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

filter_words_by_pattern <- function(aPattern, aVectorOfWords, negate = FALSE) {
  wordsWithPattern <- aVectorOfWords[str_detect(aVectorOfWords, aPattern, negate)]
}

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

pattern_for_match <- function(matchLetter, position, wordLength = 5) {
  # Pattern will be "^.a...$"
  dots <- rep(".", times = wordLength)
  dots[position] <- matchLetter
  thePattern <- paste(c("^", dots, "$"), sep = "", collapse = "")
}

pattern_for_not_here <- function(matchLetter, position, wordLength = 5) {
  thePattern <- matchLetter
}

negative_pattern_for_not_here <- function(matchLetter, position, wordLength = 5) {
  return(pattern_for_match(matchLetter, position, wordLength = 5))
}

negative_pattern_for_nowhere <- function(matchLetter, position, wordLength = 5) {
  thePattern <- matchLetter
}

possibilities_from_one_probe_letter <- function(vectorOfWords, aProbe, i,
                                                traceThisRoutine = FALSE, prepend = "") {
  # OUCH change tracing based on did anything get eliminated!
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  # if (traceFlagOnEntry) {
  #   cat(file = stderr(), prepend, "Entered possibilities_from_one_probe_letter\n")
  # }
  
  wordLength <- as.integer(str_length(aProbe) / 2.0)
  if (wordLength * 2 != str_length(aProbe)) {
    stop("probe ", aProbe, " length is not even!")
  }

  # if (traceThisRoutine) {
  #   cat(file = stderr(), myPrepend, "Filtering by letter", i, "of probe", aProbe, "\n")    
  # }

  probeLetter <- substr(aProbe, i, i)
  probeDatum <- substr(aProbe, i + wordLength, i + wordLength)
  remainingWords <- vectorOfWords
  if (length(remainingWords) > 1) {
    if (probeDatum == "G" || probeDatum == "g") {
      n_before <- length(remainingWords)
      remainingWords <- filter_words_by_pattern(pattern_for_match(probeLetter,
                                                                  i,
                                                                  wordLength),
                                                remainingWords, negate = FALSE)
    
      if (traceThisRoutine) {
        if (n_before > length(remainingWords)) {
          cat(file = stderr(), myPrepend, " Must have '", probeLetter,
              "' in position ", i, ": ",
              length(remainingWords), sep = "")
          if (length(remainingWords) == 1) {
            cat(file = stderr(), " word is left.\n", sep = "") 
          } else {
            cat(file = stderr(), " words are left.\n", sep ="") 
          }
          if (length(remainingWords) < 21) {
            for (j in 1:length(remainingWords)) {
              cat(file = stderr(), myPrepend, remainingWords[j], "\n")
            }
          }
        } else {
          cat(file = stderr(), myPrepend, " Nothing removed by correct '", probeLetter,
              "' in position ", i, "\n", sep = "")    
        }
      }
      if (length(remainingWords) == 0) {
        stop("1. Nothing left after requiring correct letter")
      }
    } else {
      if (probeDatum == "Y" || probeDatum == "y") {
        n_before <- length(remainingWords)
        remainingWords <- filter_words_by_pattern(pattern_for_not_here(probeLetter,
                                                                       i,
                                                                       wordLength),
                                                  remainingWords, negate = FALSE)
        if (traceThisRoutine) {
          if (n_before > length(remainingWords)) {
            cat(file = stderr(), myPrepend,
                " Must have '", probeLetter,
                "' somewhere: ",
                length(remainingWords), sep = "")
            if (length(remainingWords) == 1) {
              cat(file = stderr(), " word is left.\n", sep = "") 
            } else {
              cat(file = stderr(), " words are left.\n", sep = "") 
            }
            if (length(remainingWords) < 21) {
              for (j in 1:length(remainingWords)) {
                cat(file = stderr(), myPrepend, remainingWords[j], "\n")
              }
            }
          } else {
            cat(file = stderr(), myPrepend, " Nothing removed by needing '", probeLetter,
                "' (in probe position ", i, ") somewhere\n", sep = "")    
          }
        }
        if (length(remainingWords) == 0) {
          stop("2. Nothing left after requiring somewhere letter")
        }
        n_before <- length(remainingWords)
        remainingWords <- filter_words_by_pattern(negative_pattern_for_not_here(probeLetter,
                                                                                i,
                                                                                wordLength),
                                                  remainingWords, negate = TRUE)
      
        if (traceThisRoutine) {
          if (n_before > length(remainingWords)) {
            cat(file = stderr(), myPrepend,
                " Can't have '", probeLetter,
                "' at position ", i, ": ",
                length(remainingWords), sep = "")
            if (length(remainingWords) == 1) {
              cat(file = stderr(), " word is left.\n", sep = "") 
            } else {
              cat(file = stderr(), " words are left.\n", sep = "") 
            }
            if (length(remainingWords) < 21) {
              for (j in 1:length(remainingWords)) {
                cat(file = stderr(), myPrepend, remainingWords[j], "\n")
              }
            }
          } else {
            cat(file = stderr(), myPrepend, " Nothing removed by not allowing '", probeLetter,
                "' at probe position ", i, "\n", sep = "")    
          }
        }
        if (length(remainingWords) == 0) {
          stop("3. Nothing left after removing letter in incorrect place")
        }
      } else {
        n_before <- length(remainingWords)
        remainingWords <- filter_words_by_pattern(negative_pattern_for_nowhere(probeLetter,
                                                                               i,
                                                                               wordLength),
                                                  remainingWords, negate = TRUE)
      
        if (traceThisRoutine) {
          if (n_before > length(remainingWords)) {
            cat(file = stderr(), myPrepend, " Can't have '", probeLetter,
                "' anywhere: ",
                length(remainingWords), sep ="")
            if (length(remainingWords) == 1) {
              cat(file = stderr(), " word is left.\n", sep ="") 
            } else {
              cat(file = stderr(), " words are left.\n", sep ="") 
            }
            if (length(remainingWords) < 40) { # OUCH should be 21
              for (j in 1:length(remainingWords)) {
                cat(file = stderr(), myPrepend, remainingWords[j], "\n")
              }
            }
          } else {
            cat(file = stderr(), myPrepend, " Nothing removed by not allowing '", probeLetter,
                "' (in probe position ", i, ") anywhere\n", sep = "")    
          }
        }
      }
      if (length(remainingWords) == 0) {
        stop("4. Nothing left after removing incorrect letter")
      }
    }
  }  
  # if (traceFlagOnEntry) {
  #   cat(file = stderr(), prepend, "Leaving possibilities_from_one_probe_letter\n")
  # }
  return(remainingWords)
}

possibilities_from_one_probe <- function(vectorOfWords, aProbe,
                                         traceThisRoutine = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered possibilities_from_one_probe\n")
  }
  
  remainingWords <- vectorOfWords
  
  wordLength <- as.integer(str_length(aProbe) / 2.0)
  if (wordLength * 2 != str_length(aProbe)) {
    stop("probe ", aProbe, " length is not even!")
  }
  
  for (i in 1:wordLength) {
    remainingWords <- possibilities_from_one_probe_letter(remainingWords, aProbe, i,
                                                          traceThisRoutine = traceThisRoutine,
                                                          prepend = myPrepend)
    # if (traceThisRoutine) {
    #   cat(file = stderr(), myPrepend, "After", i, "letter(s),",
    #       length(remainingWords), "words are left.\n")
    #   if (length(remainingWords) < 21) {
    #     for (j in 1:length(remainingWords)) {
    #       cat(file = stderr(), myPrepend, remainingWords[j], "\n")
    #     }
    #   }
    # }
  }

  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving possibilities_from_one_probe\n")
  }

  return(remainingWords)
}

possibilities_from_history <- function(theProbes, vectorOfWords = NULL,
                                      traceThisRoutine = FALSE, prepend = "") {
  # theProbes is a vector of <2 * wordLength>-character feedback words which are
  # <wordLength> characters of guess, <wordLength> characters of response
  # given as GYx, green, yellow, miss
  myPrepend = paste("  ", prepend, sep = "")
  traceFlagOnEntry <- traceThisRoutine
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Entered possibilities_from_history\n")
  }
  
  if (traceThisRoutine) {
    # cat(file = stderr(), myPrepend, "\n")    
  }
  wordLength <- as.integer(str_length(theProbes[1]) / 2.0)
  if (wordLength * 2 != str_length(theProbes[1])) {
    stop("probe ", theProbes[1], " length is not even!")
  }
  
  if (is.null(vectorOfWords)) {
    vectorOfWords <- get_words_of_given_length(wordLength)
  }
  for (i in 1:length(theProbes)) {
    if (length(vectorOfWords) > 1) {
      vectorOfWords <- possibilities_from_one_probe(vectorOfWords, theProbes[i],
                                                    traceThisRoutine = traceThisRoutine,
                                                    prepend = myPrepend)
    }
  }
  # theProbes is a vector of <2 * wordLength>-character feedback words which are
  # <wordLength> characters of guess, <wordLength> characters of response
  # given as GYx, green, yellow, miss
  
  if (traceFlagOnEntry) {
    cat(file = stderr(), prepend, "Leaving possibilities_from_history\n")
  }

  return(vectorOfWords)
}

evaluate_a_guess <- function(correct, guessed) {
  if (str_length(correct) != str_length(guessed)) {
    return("LENGTH MISMATCH")
  }
  dots <- rep("x", times = str_length(correct))
  correctLetters <- str_split(correct, "")
  guessLetters <- str_split(guessed, "")
  for (i in 1:str_length(correct)) {
    if (str_sub(guessed, i, i) == str_sub(correct, i, i)) {
      dots[i] <- "G"
    } else {
      if (str_detect(correct, str_sub(guessed, i, i))) {
        dots[i] <- "Y"
      }
    }
  }
  return(paste(dots, sep = "", collapse = ""))
}

probe_from_guess <- function(correct, guessed) {
  return(paste(guessed, evaluate_a_guess(correct, guessed), sep = ""))
}

possibilities_from_guesses <- function(theTarget, theGuesses, vectorOfWords = NULL,
                                       traceThisRoutine = FALSE, prepend = "") {
  theProbes <- vector()
  for (i in length(theGuesses)) {
    theProbes[i] <- probe_from_guess(theTarget, theGuesses[i])
  }
  possibilities_from_history(theProbes, vectorOfWords = vectorOfWords,
                             traceThisRoutine = traceThisRoutine, prepend = myPrepend)
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

testFilter("four")
