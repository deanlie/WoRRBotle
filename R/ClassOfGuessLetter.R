library(stringr)

class_of_a_guess_letter <- function(correct, guessed, position) {
  if (str_to_upper(substr(guessed, position, position)) ==
      str_to_upper(substr(correct, position, position))) {
    return("correct")
  } else {
    if (str_detect(str_to_upper(correct),
                   str_to_upper(substr(guessed, position, position)))) {
      return("wrong_place")
    } else {
      return("absent")
    }
  }
}
