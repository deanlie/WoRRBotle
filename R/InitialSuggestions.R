
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Vector to use for initial suggestions (initialize sorted suggestion list)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
initial_suggestions <- c(
 "arose", "arise", "raise", "aloes", "arles", "earls", "laser",
 "reals", "stoae", "aster", "rates", "stare", "tares", "taser",
 "tears", "aisle", "aeons", "earns", "nares", "nears", "saner",
 "snare", "anise", "ureas", "dares", "dears", "rased", "reads",
 "least", "slate", "stale", "steal", "stela", "taels", "tales",
 "teals", "tesla", "osier", "resay", "sayer", "years", "lanes",
 "leans", "aides", "aside", "ideas", "acres", "cares", "races",
 "scare", "serac", "antes", "etnas", "neats", "paseo", "apers",
 "apres", "pares", "parse", "pears", "rapes", "reaps", "spare",
 "spear", "mares", "maser", "reams", "smear", "hares", "hears",
 "rheas", "share", "shear", "lores", "loser", "roles", "rotes",
 "store", "sepia", "saute", "agers", "gears", "rages", "sager",
 "sarge", "bares", "baser", "bears", "braes", "saber", "sabre",
 "dales", "deals", "lades", "lased", "leads", "rakes", "dates",
 "sated", "stead", "tased", "liers", "riles", "noser", "oners",
 "renos", "senor", "snore", "orals", "solar", "usnea", "resit",
 "rites", "tiers", "tires", "tries", "roast", "sorta", "taros",
 "yales", "aegis", "deans", "sedan", "yeast", "alecs", "laces",
 "scale", "lapse", "leaps", "pales", "peals", "pleas", "sepal",
 "lames", "males", "meals", "eosin", "noise", "reins", "resin")

filterInitialSuggestionsBySolutions <- function() {
  return(intersect(initial_suggestions, wordle_solns))
}

 
