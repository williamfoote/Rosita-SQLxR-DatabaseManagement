input_number <- function(prompt) {
  # This is a function that prompts a user to input a number
  # A helper function to create a menu for the Rosita program.
  
  # How to use: num = input_number(prompt) displays a prompt and asks user to
  # Input a number. Repeats until user inputs a valid number.
  
  # Source: Mikkel N. Schmidt, mnsc@dtu.dk, 2015 (found on YouTube video)
  
  while (TRUE) { # creates infinite loop
    num <- suppressWarnings(as.numeric(readline(prompt)))
    if (!is.na(num)) {
      break
    }
  }
  num
}