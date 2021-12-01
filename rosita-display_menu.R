display_menu <- function(options) {
  # display_menu Displays a menu of options, and then asks the user to choose
  # an item and returns the number of the menu chosen.
  
  # Usage: choice <- diplay_menu(options)
  # Input: options, which is a string of "options" that user wants to display
  # Output: the chosen option (integer)
  # Helper function for Rosita program
  
  # Source: Mikkel N. Schmidt, mnsc@dtu.dk, 2015 (found on YouTube)
  
  # Display menu options using for loop
  for (i in seq_len(length(options))) {
    cat(sprintf("%d. %s\n", i, options[i]))
  }
  
  # Get a valid menu choice
  choice <- 0 # initialize choice as 0
  while(!any(choice == seq_len(length(options)))) {
    choice <- input_number("Please choose a menu option: ")
  }
  choice
}