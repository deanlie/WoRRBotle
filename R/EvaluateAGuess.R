
evaluate_a_guess <- function(correct, guessed) {
  # Sanity check
  if (str_length(correct) != str_length(guessed)) {
    return("LENGTH MISMATCH")
  }
  
  # Set up record keeping. Assume misses everywhere.
  dots <- rep("x", times = str_length(correct))
  
  # Look for letters in the correct place
  for (i in 1:str_length(correct)) {
    if (substr(str_to_upper(guessed), i, i) ==
        substr(str_to_upper(correct), i, i)) {
      dots[i] <- "G"
      # Letter in position i is accounted for. Don't count
      #  a second guess of that letter as "wrong place."
      substr(guessed, i, i) <- " "
      substr(correct, i, i) <- " "
    }
  }
  
  # Look for letters in the wrong place
  for (i in 1:str_length(correct)) {
    # Skip letters which are correct in this guess per pass 1
    theLetter <- str_to_upper(substr(correct, i, i))
    if (theLetter != " ") {
      
      if (str_detect(str_to_upper(guessed), theLetter)) {
        # There is at least one matching letter in the guess.
        # Find the first one and mark it "wrong place".
        # Break, because there could be a subsequent occurrence of
        #  this letter, and we don't want to mark it "wrong place"
        #  unless it matches a subsequent occurrence of the letter
        #  in the correct word.
        for (j in 1:str_length(guessed)) {
          if (theLetter == str_to_upper(substr(guessed, j, j))) {
            substr(guessed, j, j) <- " "
            dots[j] <- "Y"
            break
          }
        }
      }
    }
  }
  return(paste(dots, sep = "", collapse = ""))
}
