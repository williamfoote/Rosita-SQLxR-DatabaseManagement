rosita <- function(start = TRUE) {
  # Main Rosita Program
  
  # Loading right packages and sourcing files for functions
  
  library(RMySQL)
  library(chron)
  library(getPass)

  source('~/Desktop/work stuff/fall_20_work/rosita-sales.R')
  source('~/Desktop/work stuff/fall_20_work/rosita-inventory.R')
  source('~/Desktop/work stuff/fall_20_work/rosita-employee.R')
  source('~/Desktop/work stuff/fall_20_work/rosita-customer.R')
  
  source('~/Desktop/work stuff/fall_20_work/rosita-display_menu.R')
  source('~/Desktop/work stuff/fall_20_work/rosita-input_number.R')
  
  # Need to change this above line to the right directory ^
  
  cat("=======================\n")
  cat("Welcome to Rosita!\n")
  cat("=======================\n")
  
  # Shows the options to the user of Rosita.
  
  options <- c("Enter MySQL Password", "Sales", "ItemInfo Management", "Employee Management",
               "Customer Management", "Exit")
  choice <- display_menu(options)
  
  while (TRUE) {
    if(choice == 1) {
      stored_password <- getPass("Enter your MySQL password: ")
      assign("stored_password", value = stored_password, pos = 1)
      cat("=======================\n")
      cat("Your password has been logged. Returning you to the main menu.\n")
      cat("=======================\n")
      rosita()
      break
    } else if (choice == 2) {
      rosita.sales()
      break
    } else if (choice == 3) {
      rosita.inventory()
      break
    } else if (choice == 4) {
      rosita.employee()
      break
    } else if (choice == 5) {
      rosita.customer()
      break
    }
    else if (choice == 6) {
      exit_options <- c("Yes, I would like to end my session AND exit.", "No, I do not want to end my session, but still EXIT the app.")
      exit_choice <- display_menu(exit_options)
      if (exit_choice == 1) {
        suppressWarnings(rm(stored_password, envir = .GlobalEnv))
        cat("Goodbye, thanks for using Rosita.")
        break
      } else if (exit_choice == 2) {
        cat("Goodbye, thanks for using Rosita.")
        break
      }
    }
  }
}
