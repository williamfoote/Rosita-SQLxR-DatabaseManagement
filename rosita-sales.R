rosita.sales <- function(start = TRUE) {
  mydb <- dbConnect(MySQL(), user = "root", password = stored_password, dbname = "SUPERMARKET", host = "localhost")
  options <- c("Complete a Sale", "Search Sales by Date",
               "Search Sales by Employee ID", "Print all Sales Records",
               "Return to Main Menu")
  choice <- display_menu(options)
  
  # Choice 1. Completing a new Sale
  if (choice == 1) {
    
    # Get item id (iid)
    temp <- dbReadTable(mydb, "ItemInfo")
    list_of_ids <- temp$id
    while (TRUE) {
      id_to_search <- suppressWarnings(as.numeric(readline("Insert the id of the ItemInfo to sell: ")))
      if (id_to_search %in% list_of_ids) {
        break
      }
    }
    name <- paste(temp[id_to_search, 2])
    iid <- id_to_search
    unit_price <- temp[temp$id == id_to_search, 3] # Get the price from the ItemInfo table
    available <- temp[temp$id == id_to_search, 4] # Get the stock of the item
    while (TRUE) {
      quant_to_sell <- suppressWarnings(as.integer(readline(
        paste("Insert the amount of", name, "to sell: \n
              (Tip: Check amount available in ItemInfo if your number to sell is not being accepted.)"))))
      if (quant_to_sell <= available) { # Make sure that employee isn't selling more than is available
        break
      }
    }
    new_available <- available - quant_to_sell
    sale_price <- unit_price * quant_to_sell
    
    # Find id of Employee completing the sale.
    temp_2 <- dbReadTable(mydb, "Employee")
    list_of_ids_2 <- temp_2$id
    while (TRUE) {
      emp_id_to_search <- suppressWarnings(as.numeric(readline("Insert the id of the Employee completing the sale: ")))
      if (emp_id_to_search %in% list_of_ids_2) {
        break
      }
    }
    
    while (TRUE) { 
      payment_method <- suppressWarnings(toupper(as.character(readline("Insert payment method of Sale (Credit, Debit, Cash): "))))
      if (payment_method %in% c("CREDIT", "DEBIT", "CASH")) {
        break
      }
    }
    
    # Log the current time of the sale.
    timeSold <- Sys.time()
    
    # Make everything into a data frame.
    
    input <- data.frame("timeSold" = timeSold,
                        "iid" = iid,
                        "price" = sale_price,
                        "paymentMethod" = payment_method,
                        "employeeID" = emp_id_to_search,
                        "available" = new_available)
    # Print the data.frame so that the 
    print(input)
    confirm_options <- c("The above Sale is Correct", "The above Sale is Incorrect")
    confirm_choice <- display_menu(confirm_options)
    if (confirm_choice == 1) {
      
      # Update the ItemInfo (i.e. reducing the number available to be the previous stock - amount sold)
      # We don't wanto to actually update the stock until the employee confirms it.
      statement <- paste("UPDATE ItemInfo SET available = '", new_available, "' WHERE id = ", id_to_search, sep = "")
      rs <- dbSendStatement(mydb, statement)
      dbClearResult(rs)
      
      # Write the table.
      dbWriteTable(mydb, "Sales", input, row.names = NA, append = TRUE)
      cat("The Sale has been completed.\n")
      cat("=======================\n")
      after_options <- c("Return to Sales menu", "Return to main menu", "Exit")
      after_choice <- display_menu(after_options)
      
      # After Choice 1. Go back to Sales menu
      if (after_choice == 1) {
        rosita.sales()
      
      # After Choice 2. Go back to Main menu
      } else if (after_choice == 2) {
        rosita()
        
      # After Choice 3. Exit the program
      } else if (after_choice == 3) {
        cat("Goodbye, thanks for using Rosita!")
        break
      }
      
     
    # Confirm Choice 2. Returns to Sale menu, cancels the sale
    } else if (confirm_choice == 2) {
      cat("Sale Cancelled. Returning to Sale menu...\n")
      rosita.sales()
    }
    
  # Choice 2. Search Sales by Date
  } else if (choice == 2) {
    search_options_a <- c(paste("Search Sales", c("BEFORE", "BETWEEEN TWO", "AFTER"),
                                c("a date", "dates")),
                          "Return to Sales menu", "Return to main menu")
    search_choice_a <- display_menu(search_options_a)
    temp <- dbReadTable(mydb, "Sales")
    as_dates <- as.POSIXct(temp$timeSold)
    # earliest_date <- sort(as_dates)[1]
    # latest_date <- sort(as_dates)[length(as_dates)]
    
    # Choice 2.1 Search Sales by Before a date
    if (search_choice_a == 1) {
      
      # Get the date that one wants to search before
      while (TRUE) {
        search_month <- suppressWarnings(as.integer(readline("Insert MONTH of date to search before (MM): ")))
        if (search_month %in% 1:12) {
          break
        }
      }
      while (TRUE) { 
        search_day <- suppressWarnings(as.integer(readline("Insert DAY of date to search before (DD): ")))
        if (search_day %in% 1:31) {
          break
        }
      }
      while (TRUE) {
        search_year <- suppressWarnings(as.integer(readline("Insert YEAR of date to search before (YYYY): ")))
        if (search_year %in% 1900:2020) {
          break
        }
      }
      seam_char <- as.character(search_month)
      sead_char <- as.character(search_day)
      seay_char <- as.character(search_year)
      search_date <- as.POSIXct(paste(c(seay_char, seam_char, sead_char), collapse = "-"))
      
      output <- temp[as_dates < search_date, ]
      if (nrow(output) == 0) {
        cat("No search results found before given date.\n")
      } else {
        print(output)
      }
      cat("=======================\n")
      after_options <- c("Return to Sales menu", "Return to Main menu")
      after_choice <- display_menu(after_options)
      if (after_choice == 1) {
        rosita.sales()
      } else if (after_choice == 2) {
        rosita()
      }
      
    # Choice 2.2. Search Sales by Between dates
    } else if (search_choice_a == 2) {
      
      # Get DATE 1
      while (TRUE) {
        search_month_1 <- suppressWarnings(as.integer(readline("Insert MONTH of DATE 1 to search between (MM): ")))
        if (search_month_1 %in% 1:12) {
          break
        }
      }
      while (TRUE) { 
        search_day_1 <- suppressWarnings(as.integer(readline("Insert DAY of DATE 1  to search between (DD): ")))
        if (search_day_1 %in% 1:31) {
          break
        }
      }
      while (TRUE) {
        search_year_1 <- suppressWarnings(as.integer(readline("Insert YEAR of DATE 1 to search between (YYYY): ")))
        if (search_year_1 %in% 1900:2020) {
          break
        }
      }
      
      # Get DATE 2
      while (TRUE) {
        search_month_2 <- suppressWarnings(as.integer(readline("Insert MONTH of DATE 2 to search between (MM): ")))
        if (search_month_2 %in% 1:12) {
          break
        }
      }
      while (TRUE) { 
        search_day_2 <- suppressWarnings(as.integer(readline("Insert DAY of DATE 2 to search between (DD): ")))
        if (search_day_2 %in% 1:31) {
          break
        }
      }
      while (TRUE) {
        search_year_2 <- suppressWarnings(as.integer(readline("Insert YEAR of DATE 2 to search between (YYYY): ")))
        if (search_year_2 %in% 1900:2020) {
          break
        }
      }
      
    # Compile and sort the dates
      seam1_char <- as.character(search_month_1)
      sead1_char <- as.character(search_day_1)
      seay1_char <- as.character(search_year_1)
      search_date_1 <- as.POSIXct(paste(c(seay1_char, seam1_char, sead1_char), collapse = "-"))
      
      seam2_char <- as.character(search_month_2)
      sead2_char <- as.character(search_day_2)
      seay2_char <- as.character(search_year_2)
      search_date_2 <- as.POSIXct(paste(c(seay2_char, seam2_char, sead2_char), collapse = "-"))
      
      if (search_date_1 == search_date_2) {
        cat("Can't search between two dates if the dates are the same. Please try again.\n")
        cat("=======================\n")
        after_options <- c("Return to Sales menu", "Return to Main menu")
        after_choice <- display_menu(after_options)
        if (after_choice == 1) {
          rosita.sales()
        } else if (after_choice == 2) {
          rosita()
        }
      } else if (search_date_1 < search_date_2) { # If date 1 is earlier
        output <- temp[as_dates > search_date_1 & as_dates < search_date_2, ]
        if (nrow(output) == 0) {
          cat("No search results found between given dates.\n")
        } else {
          print(output)
        }
      } else if (search_date_1 > search_date_2) { # If date 2 is earlier
        output <- temp[as_dates < search_date_1 & as_dates > search_date_2, ]
        if (nrow(output) == 0) {
          cat("No search results found between given dates.\n")
        } else {
          print(output)
        }
      }
      cat("=======================\n")
      after_options <- c("Return to Sales menu", "Return to Main menu")
      after_choice <- display_menu(after_options)
      if (after_choice == 1) {
        rosita.sales()
      } else if (after_choice == 2) {
        rosita()
      }
    # Choice 2.3. Search Sales by After dates
    } else if (search_choice_a == 3) {
      # Get the date that one wants to search after
      while (TRUE) {
        search_month <- suppressWarnings(as.integer(readline("Insert MONTH of date to search after (MM): ")))
        if (search_month %in% 1:12) {
          break
        }
      }
      while (TRUE) { 
        search_day <- suppressWarnings(as.integer(readline("Insert DAY of date to search after (DD): ")))
        if (search_day %in% 1:31) {
          break
        }
      }
      while (TRUE) {
        search_year <- suppressWarnings(as.integer(readline("Insert YEAR of date to search after (YYYY): ")))
        if (search_year %in% 1900:2020) {
          break
        }
      }
      seam_char <- as.character(search_month)
      sead_char <- as.character(search_day)
      seay_char <- as.character(search_year)
      search_date <- as.POSIXct(paste(c(seay_char, seam_char, sead_char), collapse = "-"))
      
      output <- temp[as_dates > search_date, ]
      if (nrow(output) == 0) {
        cat("No search results found after given date.\n")
      } else {
        print(output)
      }
      cat("=======================\n")
      after_options <- c("Return to Sales menu", "Return to Main menu")
      after_choice <- display_menu(after_options)
      if (after_choice == 1) {
        rosita.sales()
      } else if (after_choice == 2) {
        rosita()
      }
    } else if (search_choice_a == 4) {
      rosita.sales()
    } else if (search_choice_a == 5) {
      rosita()
    }
  # Choice 3. Search Sales by Employee id
  } else if (choice == 3) {
    search_options_a <- c("Search Sales by Employee id", "Return to Sales Menu", "Return to Main Menu")
    search_choice_a <- display_menu(search_options_a)
    if (search_choice_a == 1) {
      temp <- dbReadTable(mydb, "Sales")
      list_of_ids <- temp$employeeID
      while (TRUE) {
        id_to_search <- suppressWarnings(as.numeric(readline("Insert the id of the Employee to search Sales by: ")))
        if (id_to_search %in% list_of_ids) {
          break
        }
      }
      print(temp[temp$employeeID == id_to_search, ])
      cat("=======================\n")
      after_options <- c("Return to Sales menu", "Return to Main menu")
      after_choice <- display_menu(after_options)
      if (after_choice == 1) {
        rosita.sales()
      } else if (after_choice == 2) {
        rosita()
      }
    } else if (search_choice_a == 2) { # To return to Sales menu
      rosita.Sales()
    } else if (search_choice_a == 3) { # To return to Main menu
      rosita()
    }
    
  # Print all sales records
  } else if (choice == 4) {
    output <- dbReadTable(mydb, "Sales")
    print(output)
    cat("=======================\n")
    after_options <- c("Return to Sales menu", "Return to Main menu")
    after_choice <- display_menu(after_options)
    if (after_choice == 1) {
      rosita.sales()
    } else if (after_choice == 2) {
      rosita()
    }
  } else if (choice == 5) {
    rosita()
  }
  dbDisconnect(mydb)
}