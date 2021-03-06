# Midterm 3

# Write a function called numBracElements. Your function should take the following
# arguments
#   <chvec>: A character vector containing strings of letters, possibly 
#     with the "[" symbol
#
# and return the following
#   <num.brac>: an integer indicating how many elements of <chvec> contain the "["
#     symbol. For example: numBracElements(c('digit', '[:digit:]', '[]')) should return 2
numBracElements <- function(chvec){
  num.brac <- 0
  ind.brac <- gregexpr("[", chvec)[[1]]
  if(ind.brac[1] != -1) num.brac <- length(ind.brac)
  return(num.brac)
}



# Write a function called maxDigits that return the maximum of all (single) digits in
# a string.  The function should return 0 if there is no digit in the
# string. Your function should take the following arguments:
#   <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and return the following
#   <total>: A single number (the maximum of all digits in chvec)
maxDigit <- function(chvec){
  chvec.digit <-gsub("[^0-9]","",chvec)
  chvec.digit.num<- as.vector(chvec.digit)
  total <- max(chvec.digit.num)
  return(total)
}


# Some test cases:
all.equal(maxDigits("1z3p ! 28"), 8)
all.equal(maxDigits("abcdefg"), 0)



# Write a function called hisToHer that converts every instance of 
# him in a string to her; every instance of he to she and every instance 
# of his to hers. You can assume everything is lower case. Be careful not 
# to replace words that contain him/he/his (eg you don't want to
# replace the with ther). Your function should take the argument
#   <chvec>: A character vector
#
# and return
#   <herchvec>: The same character vector with the required substitutions.
hisToHer <- function(chvec){
  herchvec3 <- gsub("\\<him\\>","her",chvec)
  herchvec2 <- gsub("\\<his\\>","her",chvec3)
  herchvec  <- gsub("\\<he\\>","she",chvec2)
  return(herchvec)
}


# A test case
all.equal(
  hisToHer("he went to the store his mother gave him"), 
  "she went to the store her mother gave her"
)



# Write a function called mostCommonLetter that finds the most common 
# letter in a string. If there is a tie for most common letter return 
# all of the letters that were most common. Your function should 
# convert all letters in the string to *lower case* and you should 
# remove  everything other than letters. 
# Your function has the argument
#  <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and it should return
#  <letter> The most common letter or letters in the string.
# For example mostCommonLetter("aabbccccdddd") should return 
# [1] "c" "d"

mostCommonLetter <- function(chvec){
  tolower(chvec)
  letter.only <- gsub("[^a-z]","",chvec)
  common.letter<- max(gregexpr("[a-z]",letter.only)[[1]])
  letter<- letter.only[common.letter]
  return(letter)
}

