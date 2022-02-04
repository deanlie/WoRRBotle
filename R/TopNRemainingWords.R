source("R/words.R")
source("R/EvaluateAGuess.R")

topNRemainingWords <- function(remainingWords, nToKeep) {
  # OUCH sort remaining words by which remove the most candidates
  if (length(remainingWords) > nToKeep) {
    remainingWords <- str_to_upper(head(remainingWords, nToKeep))
  } else {
    remainingWords <- str_to_upper(remainingWords)
  }
  
  listOfPTags <- lapply(remainingWords,
                        function(aWord) as.character(tags$p(aWord,
                                                            class = "suggestions")))
  
  paste(tags$h4("Suggestions:", class="suggestions"),
        paste(unlist(listOfPTags), collapse = ""))
}
