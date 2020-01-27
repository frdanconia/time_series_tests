library(testthat)


list = c("n", "e", "w", "n", "e", "w", "n", "e", "w", "s")
pattern = c("e","w")

direction_test <- function(list,pattern){
tf = c()
if(typeof(list) == "character" & length(list) > 1){
for(i in 1:(length(list)-1)){
  tf[length(tf)+1] <- unique(c(list[i],list[i + 1]) == pattern)
}
if(length(unique(tf)) != 1){return(FALSE)}
  else{return(TRUE)}
}
return(FALSE)
}

direction_test(list,c("e","w"))
direction_test(list,c("w","e"))
direction_test(list,c("n","s"))
direction_test(list,c("s","n"))




isValidWalk <- function(walk){
  if(length(walk) == 10 & length(unique(walk)) != 1){return(TRUE)}
  else{return(FALSE)}}

test_that("Sample Tests", {
  # A valid walk passes
  expect_equal(isValidWalk(c("n", "e", "w", "s", "n", "e", "w", "s", "n", "s")), TRUE)
  # Only north and south passes
  expect_equal(isValidWalk(c("n", "n", "n", "s", "s", "n", "s", "n", "s", "s")), TRUE)
  # Only east and west passes
  expect_equal(isValidWalk(c("w", "w", "e", "w", "w", "e", "e", "e", "e", "w")), TRUE)
  # End at the right time, but you're in the north pole
  expect_equal(isValidWalk(rep("n", 10)), FALSE)
  # An otherwise valid walk that returns on time, but then keeps going
  expect_equal(isValidWalk(c("n", "e", "w", "s", "n", "e", "w", "s", "n", "s", "e", "w")), FALSE)
  # Get back early
  expect_equal(isValidWalk(c("n", "e", "w", "s", "n", "e", "w", "s")), FALSE)
  # ns matches, but not ew
  expect_equal(isValidWalk(c("n", "s", "e", "n", "s", "e", "n", "s", "e", "w")), FALSE)
  # ew matches, but not ns
  expect_equal(isValidWalk(c("n", "e", "w", "n", "e", "w", "n", "e", "w", "s")), FALSE)
})
