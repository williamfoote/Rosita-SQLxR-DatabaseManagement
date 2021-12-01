rosita.customer <- function(start = TRUE) {
  mydb <- dbConnect(MySQL(), user = "root", password = stored_password, dbname = "SUPERMARKET", host = "localhost")
  options <- c("Insert Customer Record", "Update Customer Record",
               "Delete Customer Record", "Search Customer Record",
               "Show All Customer Records", "Return to Main Menu")
  choice <- display_menu(options)
  
  # Choice 1. Insert a new customer into the database
  if (choice == 1) {
    last_name <- readline("Insert Customer's last name: ")
    first_name <- readline("Insert Customer's first name: ")
    while (TRUE) { # creates infinite loop, broken once phone number is a numeric
      phone_number <- suppressWarnings(as.numeric(readline("Insert Customer's phone number (XXXXXXXXXX): ")))
      if (!is.na(phone_number)) {
        break
      }
    }
    # Find new id for the new Customer.
    id <- max(dbReadTable(mydb, "Customer")$id) + 1
    
    # Make it all into a data.frame
    input <- data.frame("id" = id,
                        "last" = last_name,
                        "first" = first_name,
                        "phoneNumber" = phone_number)
    
    # Add data frame to the database in MySQL
    dbWriteTable(mydb, "Customer", input, row.names = NA, append = TRUE)
    
    cat("The Customer's record has been added to the database.")
    cat("=======================\n")
    after_options <- c("Return to Customer menu", "Return to Main menu")
    after_choice <- display_menu(after_options)
    if (after_choice == 1) {
      rosita.customer()
    } else if (after_choice == 2) {
      rosita()
    }
    # Choice 2. Update a Customer's record.
  } else if (choice == 2) {
    update_options_a <- c("Update Customer record by id", "Return to Customer Menu", "Return to Main Menu")
    update_choice_a <- display_menu(update_options_a)
    
    # Choice 2.1. Update Customer record by id
    if (update_choice_a == 1) {
      temp <- dbReadTable(mydb, "Customer")
      list_of_ids <- temp$id
      while (TRUE) {
        id_to_search <- suppressWarnings(as.numeric(readline("Insert the id of the Customer to update: ")))
        if (id_to_search %in% list_of_ids) {
          break
        }
      }
      name <- paste(temp[id_to_search, 3], " ", temp[id_to_search, 2], "'s", collapse = "", sep = "")
      update_options_b <- c(paste("Update", name, names(temp)[-1]), "Return to Customer menu")
      update_choice_b <- display_menu(update_options_b)
      
      # Choice 2.1.1. Update Customer's Last Name
      if (update_choice_b == 1) {
        updated_last <- read
        line("Insert udpated Customer last name: ")
        statement <- paste("UPDATE Customer SET last = '", updated_last, "' WHERE id = ", id_to_search, sep = "")
        rs <- dbSendStatement(mydb, statement)
        dbClearResult(rs)
        
        cat("The Customer's last name has been updated in the database.")
        cat("=======================\n")
        after_options <- c("Return to Customer menu", "Return to Main menu")
        after_choice <- display_menu(after_options)
        if (after_choice == 1) {
          rosita.customer()
        } else if (after_choice == 2) {
          rosita()
        }
        
      # Choice 2.1.2. Update Customer's First
      } else if (update_choice_b == 2) {
        updated_first <- readline("Insert udpated Customer first name: ")
        statement <- paste("UPDATE Customer SET first = '", updated_first, "' WHERE id = ", id_to_search, sep = "")
        rs <- dbSendStatement(mydb, statement)
        dbClearResult(rs)
        
        cat("The Customer's first name has been updated in the database.")
        cat("=======================\n")
        after_options <- c("Return to Customer menu", "Return to Main menu")
        after_choice <- display_menu(after_options)
        if (after_choice == 1) {
          rosita.customer()
        } else if (after_choice == 2) {
          rosita()
        }
        
      # Choice 2.1.3. Update Customer's phone number
      } else if (update_choice_b == 3) {
        while (TRUE) { # creates infinite loop, broken once phone number is an integer
          updated_pn <- suppressWarnings(as.integer(readline("Insert Customer's new phone number: ")))
          if (!is.na(updated_pn)) {
            break
          }
        }
        statement <- paste("UPDATE Customer SET phoneNumber = ", updated_pn, " WHERE id = ", id_to_search, sep = "")
        rs <- dbSendStatement(mydb, statement = statement)
        dbClearResult(rs)
        
        cat("The Customer's phone number has been updated in the database.")
        cat("=======================\n")
        after_options <- c("Return to Customer menu", "Return to Main menu")
        after_choice <- display_menu(after_options)
        if (after_choice == 1) {
          rosita.customer()
        } else if (after_choice == 2) {
          rosita()
        }
        
      # Choice 2.1.4. Go back to Customer menu
      } else if (update_choice_b == 4) {
        rosita.customer()
      }
      
      # Choice 2.2. Go back to Customer menu
    } else if (update_choice_a == 2) {
      rosita.customer()
      
      # Choice 2.3. Go back to Main menu
    } else if (update_choice_a == 3) {
      rosita()
    }
    
    # Choice 3. Delete Customer Record
  } else if (choice == 3) {
    delete_options_a <- c("Delete Customer by id", "Return to Customer Menu", "Return to Main Menu")
    delete_choice_a <- display_menu(delete_options_a)
    
    # Choice 3.1. Delete Customer by id
    if (delete_choice_a == 1) {
      temp <- dbReadTable(mydb, "Customer")
      list_of_ids <- temp$id
      while (TRUE) {
        id_to_search <- suppressWarnings(as.numeric(readline("Insert the id of the Customer to delete: ")))
        if (id_to_search %in% list_of_ids) {
          break
        }
      }
      name <- paste(temp[id_to_search, 3], " ", temp[id_to_search, 2], "'s", collapse = "", sep = "")
      delete_options_b <- paste(c("YES, I would like to", "NO, I do not want to"),
                                paste("delete", name, "records from the database."))
      delete_choice_b <- display_menu(delete_options_b)
      
      # Choice 3.1.1. Confirm Deletion
      if (delete_choice_b == 1) {
        statement <- paste("DELETE FROM Customer WHERE id =", id_to_search)
        rs <- dbSendStatement(mydb, statement = statement)
        dbClearResult(rs)
        
        cat("The Customer's record has been deleted from the database.")
        cat("=======================\n")
        after_options <- c("Return to Customer menu", "Return to Main menu")
        after_choice <- display_menu(after_options)
        if (after_choice == 1) {
          rosita.customer()
        } else if (after_choice == 2) {
          rosita()
        }
        
      # Choice 3.1.2. Cancel Deletion
      } else if (delete_choice_b == 2) {
        cat("Delete process halted.")
        cat("=======================\n")
        after_options <- c("Return to Customer menu", "Return to Main menu")
        after_choice <- display_menu(after_options)
        if (after_choice == 1) {
          rosita.customer()
        } else if (after_choice == 2) {
          rosita()
        }
      }
      
      # Choice 3.2. Go back to Customer menu
    } else if (delete_choice_a == 2) {
      rosita.customer()
      
      # Choice 3.3. Go back to main menu
    } else if (delete_choice_a == 3) {
      rosita()
    }
    
  # Choice 4. Search Customer
  } else if (choice == 4) {
    search_options_a <- c("Search Customer by id", "Return to Customer Menu", "Return to Main Menu")
    search_choice_a <- display_menu(search_options_a)
    
    # Choice 4.1. Search Customer by id
    if (search_choice_a == 1) {
      temp <- dbReadTable(mydb, "Customer")
      list_of_ids <- temp$id
      while (TRUE) {
        id_to_search <- suppressWarnings(as.numeric(readline("Insert the id of the Customer to search: ")))
        if (id_to_search %in% list_of_ids) {
          break
        }
      }
      print(temp[temp$id == id_to_search, ])
      cat("=======================\n")
      after_options <- c("Return to Customer menu", "Return to Main menu")
      after_choice <- display_menu(after_options)
      if (after_choice == 1) {
        rosita.customer()
      } else if (after_choice == 2) {
        rosita()
      }
    
    # Choice 4.2. Go back to Customer menu
    } else if (search_choice_a == 2) {
      rosita.customer()
      
    # Choice 4.3. Go back to Main menu
    } else if (search_choice_a == 3) {
      rosita()
    }
    
    # Choice 5. Show all Customer records
  } else if (choice == 5) {
    output <- dbReadTable(mydb, "Customer")
    print(output)
    cat("=======================\n")
    after_options <- c("Return to Customer menu", "Return to Main menu")
    after_choice <- display_menu(after_options)
    if (after_choice == 1) {
      rosita.customer()
    } else if (after_choice == 2) {
      rosita()
    }
  } else if (choice == 6 ) {
    rosita()
  }
  dbDisconnect(mydb)
}