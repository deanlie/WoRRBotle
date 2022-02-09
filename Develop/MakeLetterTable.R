makeLetterTibbleA <- function(aWordVector) {
  candidateTibble <- tibble(Candidates = aWordVector) %>%
    mutate(L1 = substr(Candidates, 1,1),
           L2 = substr(Candidates, 2,2),
           L3 = substr(Candidates, 3,3),
           L4 = substr(Candidates, 4,4),
           L5 = substr(Candidates, 5,5))
}

makeLetterTibbleB <- function(aWordVector) {
  r1 <- strsplit(aWordVector, '')
  names(r1) <- aWordVector
  r2 <- as_tibble(r1) %>%
    rowid_to_column(var = "Position")
  return(r2)
}

abProfileWordList <- function(profileThis_1, profileThis_2,
                              aWordVector, nReps) {
  Rprof(NULL)

  Rprof(tmp <- tempfile())
  for (i in 1:nReps) {
    profileThis_1(aWordVector)
    profileThis_2(aWordVector)
  }
  Rprof()
  mySum <- summaryRprof(tmp)
  unlink(tmp)
  mySumIndexVector <- str_detect(rownames(mySum$by.total),
                                 "profileThis")
  
  profileRes <- mySum$by.total[mySumIndexVector,]
  theNames <- rownames(profileRes)
  # newNames <- str_replace(theNames, '"profileThis_1"',
  #                         (profileThis_1))
  # rownames(profileRes) <- newNames
  # return(profileRes)
  return(mySum$by.total[mySumIndexVector,])
}

testABProfile <- function(nReps) {
  abProfileWordList(makeLetterTibbleA, makeLetterTibbleB,
                    wordle_solns, nReps)
}

testQuote <- function(anArgument) {
  exec(anArgument, )
}
