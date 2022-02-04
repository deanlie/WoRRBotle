source("R/words.R")
source("R/EvaluateAGuess.R")

probe_from_guess <- function(correct, guessed) {
  return(paste(guessed, evaluate_a_guess(correct, guessed), sep = ""))
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

negative_pattern_for_nowhere <- function(aProbe, position, wordLength = 5) {
  matchLetter <- substr(aProbe, position, position)
  mungedProbe <- aProbe
  substr(mungedProbe, position, position) <- ""
  
  thePattern <- matchLetter
  if (str_detect(mungedProbe, thePattern)) {
    # We can't just reject anything with this letter! The "not here" is only
    #  telling us that there aren't two of them. And of course, it's really worse...
  }
}

filter_words_by_pattern <- function(aPattern, aVectorOfWords, negate = FALSE) {
  message("filter_words_by_pattern, aPattern = ", aPattern,
          ", length(aVectorOfWords = ", length(aVectorOfWords))
  if ((!is.null(aPattern)) && length(aPattern) > 0) {
    message("in str_detect branch")
    wordsWithPattern <- aVectorOfWords[str_detect(aVectorOfWords, aPattern, negate)]
  } else {
    wordsWithPattern <- aVectorOfWords
  }
  return(wordsWithPattern)
}

possibilities_from_one_probe_letter <- function(vectorOfWords, aProbe, i,
                                                debug = FALSE, prepend = "") {
  # OUCH change tracing based on did anything get eliminated!
  myPrepend = paste("  ", prepend, sep = "")
  if (debug) {
    message(prepend, "Entered possibilities_from_one_probe_letter")
    message(prepend, "aProbe = ", aProbe, "  i = ", i)
  }
  
  wordLength <- as.integer(str_length(aProbe) / 2.0)
  if (wordLength * 2 != str_length(aProbe)) {
    stop("probe ", aProbe, " length is not even!")
  }
  
  if (debug) {
    message(myPrepend, "Filtering by letter", i, "of probe", aProbe)
  }
  
  probeLetter <- substr(aProbe, i, i)
  probeDatum <- substr(aProbe, i + wordLength, i + wordLength)
  theGuess <- substr(aProbe, 1, wordLength)
  remainingWords <- vectorOfWords
  if (length(remainingWords) > 1) {
    if (probeDatum == "G" || probeDatum == "g") {
      n_before <- length(remainingWords)
      remainingWords <- filter_words_by_pattern(pattern_for_match(probeLetter,
                                                                  i,
                                                                  wordLength),
                                                remainingWords, negate = FALSE)
      
      if (debug) {
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
          message(myPrepend, "Nothing removed by correct '", probeLetter,
              "' in position ", i)    
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
        if (debug) {
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
            if (debug) {
              message(myPrepend, "Nothing removed by needing '", probeLetter,
                  "' (in probe position ", i, ") somewhere")
            }
          }
        }
        if (length(remainingWords) == 0) {
          stop("2. Nothing left after requiring somewhere letter")
        }
        n_before <- length(remainingWords)
        remainingWords <- filter_words_by_pattern(negative_pattern_for_not_here(aProbe,
                                                                                i,
                                                                                wordLength),
                                                  remainingWords, negate = TRUE)
        
        if (debug) {
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
                message(myPrepend, remainingWords[j])
              }
            }
          } else {
            message(myPrepend, "Nothing removed by not allowing '", probeLetter,
                    "' at probe position ", i)
          }
        }
        if (length(remainingWords) == 0) {
          stop("3. Nothing left after removing letter in incorrect place")
        }
      } else {
        n_before <- length(remainingWords)
        remainingWords <- filter_words_by_pattern(negative_pattern_for_nowhere(theGuess,
                                                                               i,
                                                                               wordLength),
                                                  remainingWords, negate = TRUE)
        
        if (debug) {
          if (n_before > length(remainingWords)) {
            message(myPrepend, "Can't have '", probeLetter,
                "' anywhere.")
            if (length(remainingWords) == 1) {
              message(myPrepend, "1 word is left") 
            } else {
              message(myPrepend, length(remainingWords), " words are left") 
            }
            if (length(remainingWords) < 21) {
              for (j in 1:length(remainingWords)) {
                message(myPrepend, remainingWords[j])
              }
            }
          } else {
            if (debug) {
              message(myPrepend, "Nothing removed by not allowing '", probeLetter,
                  "' (in probe position ", i, ") anywhere")
            }
          }
        }
      }
      if (length(remainingWords) == 0) {
        stop("4. Nothing left after removing incorrect letter")
      }
    }
  }  
  if (debug) {
    message(prepend, "Leaving possibilities_from_one_probe_letter")
  }
  return(remainingWords)
}

possibilities_from_one_probe <- function(vectorOfWords, aProbe,
                                         debug = FALSE, prepend = "") {
  myPrepend = paste("  ", prepend, sep = "")
  if (debug) {
    cat(file = stderr(), prepend, "Entered possibilities_from_one_probe\n")
  }
  
  remainingWords <- vectorOfWords
  
  wordLength <- as.integer(str_length(aProbe) / 2.0)
  if (wordLength * 2 != str_length(aProbe)) {
    stop("probe ", aProbe, " length is not even!")
  }
  
  for (i in 1:wordLength) {
    remainingWords <- possibilities_from_one_probe_letter(remainingWords, aProbe, i,
                                                          debug = debug,
                                                          prepend = myPrepend)
    if (debug) {
      cat(file = stderr(), myPrepend, "After", i, "letter(s),",
          length(remainingWords), "words are left.\n")
      if (length(remainingWords) < 21) {
        for (j in 1:length(remainingWords)) {
          cat(file = stderr(), myPrepend, remainingWords[j], "\n")
        }
      }
    }
  }
  
  if (debug) {
    cat(file = stderr(), prepend, "Leaving possibilities_from_one_probe\n")
  }
  
  return(remainingWords)
}

possibilities_from_history <- function(theProbes, vectorOfWords = NULL,
                                       debug = FALSE, prepend = "") {
  # theProbes is a vector of <2 * wordLength>-character feedback words which are
  # <wordLength> characters of guess, <wordLength> characters of response
  # given as GYx, green, yellow, miss
  myPrepend = paste("  ", prepend, sep = "")
  if (debug) {
    message(prepend, "Entered possibilities_from_history")
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
                                                    debug = debug,
                                                    prepend = myPrepend)
    }
  }
  # theProbes is a vector of <2 * wordLength>-character feedback words which are
  # <wordLength> characters of guess, <wordLength> characters of response
  # given as GYx, green, yellow, miss
  
  if (debug) {
    message(prepend, "Leaving possibilities_from_history")
  }
  
  return(vectorOfWords)
}

topNRemainingWords <- function(sought, guessVector, nGuesses, nToKeep,
                               remainingWords,
                               debug = FALSE) {
  # remainingWords <- str_to_upper(wordle_dict)
  # 
  # if (debug) {
  #   message("topNRemainingWords, nGuesses = ", nGuesses)
  # }
  # 
  # theProbes <- vector()
  # if (nGuesses > 1) {
  #   for (i in 1:(nGuesses - 1)) {
  #     message("guessVector[", i, "] = ", guessVector[i])
  #     theProbes[i] <- probe_from_guess(sought, guessVector[i])
  #   }
  # 
  #   remainingWords <- possibilities_from_history(theProbes,
  #                                                vectorOfWords = remainingWords,
  #                                                debug = debug,
  #                                                prepend = "")
  # }
    
  # OUCH sort remaining words by probability of something or other
  if (length(remainingWords) > nToKeep) {
    remainingWords <- head(remainingWords, nToKeep)
  }
  
  listOfPTags <- lapply(remainingWords,
                        function(aWord) as.character(tags$p(aWord,
                                                            class = "suggestions")))
  
  paste(tags$h4("Suggestions:", class="suggestions"),
        paste(unlist(listOfPTags), collapse = ""))
}

test <- function() {
  topNRemainingWords("PANIC", c("PASTA", "PAINS"), 2, 25)
}
