
class_of_a_guess_letter <- function(correct, guessed, position) {
  if (substr(guessed, position, position) == substr(correct, position, position)) {
    return("correct")
  } else {
    if (str_detect(correct, substr(guessed, position, position))) {
      return("wrong_place")
    } else {
      return("absent")
    }
  }
}
