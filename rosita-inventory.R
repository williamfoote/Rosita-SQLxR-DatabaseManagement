rosita.inventory <- function(start = TRUE) {
  mydb <- dbConnect(MySQL(), user = "root", password = stored_password, dbname = "SUPERMARKET", host = "localhost")
  options <- c("Insert ItemInfo Record", "Update ItemInfo Record",
               "Delete ItemInfo Record", "Search ItemInfo Record",
               "Show All ItemInfo Records", "Return to Main Menu")
  choice <- display_menu(options)
    
  # Choice 1. Insert a new ItemInfo record into the database
  if (choice == 1) {
    item_name <- readline("Insert ItemInfo's name: ")
    while (TRUE) { # creates infinite loop, broken once item_price is a numeric
      item_price <- suppressWarnings(as.numeric(readline("Insert Item's price: ")))
      if (!is.na(item_price)) {
        break
      }
    }
    while (TRUE) { # creates infinite loop, broken once item_price is an integer
      item_stock <- suppressWarnings(as.integer(readline("Insert Item's stock: ")))
      if (!is.na(item_stock)) {
        break
      }
    }
    # Find new id for the new ItemInfo.
    id <- max(dbReadTable(mydb, "ItemInfo")$id) + 1
    
    # Make it all into a data.frame
    input <- data.frame("id" = id,
                        "itemName" = item_name,
                        "price" = item_price,
                        "available" = item_stock)
    
    # Add data frame to the database in MySQL
    dbWriteTable(mydb, "ItemInfo", input, row.names = NA, append = TRUE)
    
    cat("ItemInfo has been added to the database.\n")
    cat("=======================\n")
    after_options <- c("Return to ItemInfo menu", "Return to Main menu")
    after_choice <- display_menu(after_options)
    if (after_choice == 1) {
      rosita.inventory()
    } else if (after_choice == 2) {
      rosita()
    }
    
  # Choice 2. Update an ItemInfo's record.
  } else if (choice == 2) {
    update_options_a <- c("Update ItemInfo by id", "Return to ItemInfo Menu", "Return to Main Menu")
    update_choice_a <- display_menu(update_options_a)
    
    # Choice 2.1. Update ItemInfo by id.
    if (update_choice_a == 1) {
      temp <- dbReadTable(mydb, "ItemInfo")
      list_of_ids <- temp$id
      while (TRUE) {
        id_to_search <- suppressWarnings(as.numeric(readline("Insert the id of the ItemInfo to update: ")))
        if (id_to_search %in% list_of_ids) {
          break
        }
      }
      name <- paste(temp[id_to_search, 2])
      update_options_b <- c(paste("Update", name, names(temp)[-1]), "Return to ItemInfo menu")
      update_choice_b <- display_menu(update_options_b)
      
      # Choice 2.1.1. Update ItemInfo's itemName
      if (update_choice_b == 1) {
        updated_name <- readline("Insert udpated ItemInfo new name: ")
        statement <- paste("UPDATE ItemInfo SET itemName = '", updated_name, "' WHERE id = ", id_to_search, sep = "")
        rs <- dbSendStatement(mydb, statement)
        dbClearResult(rs)
        
        cat("ItemInfo's item name has been updated in the database.\n")
        cat("=======================\n")
        after_options <- c("Return to ItemInfo menu", "Return to Main menu")
        after_choice <- display_menu(after_options)
        if (after_choice == 1) {
          rosita.inventory()
        } else if (after_choice == 2) {
          rosita()
        }
        
      # Choice 2.1.2. Update ItemInfo's price
      } else if (update_choice_b == 2) {
        while (TRUE) { # creates infinite loop, broken once item_price is a numeric
          updated_price <- suppressWarnings(as.numeric(readline("Insert Item's new price: ")))
          if (!is.na(update_item_price)) {
            break
          }
        }
        statement <- paste("UPDATE ItemInfo SET price = ", updated_price, " WHERE id = ", id_to_search, sep = "")
        rs <- dbSendStatement(mydb, statement = statement)
        dbClearResult(rs)
        
        cat("ItemInfo's price has been updated in the database.\n")
        cat("=======================\n")
        after_options <- c("Return to ItemInfo menu", "Return to Main menu")
        after_choice <- display_menu(after_options)
        if (after_choice == 1) {
          rosita.inventory()
        } else if (after_choice == 2) {
          rosita()
        }
        
      # Choice 2.1.3. Update ItemInfo's stock
      } else if (update_choice_b == 3) {
        while (TRUE) { # creates infinite loop, broken once item_price is an integer
          updated_stock <- suppressWarnings(as.integer(readline("Insert Item's new stock: ")))
          if (!is.na(updated_stock)) {
            break
          }
        }
        statement <- paste("UPDATE ItemInfo SET available = ", updated_stock, " WHERE id = ", id_to_search, sep = "")
        rs <- dbSendStatement(mydb, statement = statement)
        dbClearResult(rs)
        
        cat("ItemInfo's stock has been updated in the database.\n")
        cat("=======================\n")
        after_options <- c("Return to ItemInfo menu", "Return to Main menu")
        after_choice <- display_menu(after_options)
        if (after_choice == 1) {
          rosita.inventory()
        } else if (after_choice == 2) {
          rosita()
        }
        
      # Choice 2.1.4. Go back to ItemInfo menu
      } else if (update_choice_b == 4) {
        rosita.inventory()
      }
    
    # Choice 2.2. Go back to ItemInfo menu
    } else if (update_choice_a == 2) {
      rosita.inventory()
    
    # Choice 2.3. Go back to Main menu
    } else if (update_choice_a == 3) {
      rosita()
    }
  
  # Choice 3. Delete ItemInfo Record
  } else if (choice == 3) {
    delete_options_a <- c("Delete ItemInfo by id", "Return to ItemInfo Menu", "Return to Main Menu")
    delete_choice_a <- display_menu(delete_options_a)
    
    # Choice 3.1. Delete ItemInfo by id
    if (delete_choice_a == 1) {
      temp <- dbReadTable(mydb, "ItemInfo")
      list_of_ids <- temp$id
      while (TRUE) {
        id_to_search <- suppressWarnings(as.numeric(readline("Insert the id of the ItemInfo record to delete: ")))
        if (id_to_search %in% list_of_ids) {
          break
        }
      }
      name <- paste(temp[id_to_search, 2])
      delete_options_b <- paste(c("YES, I would like to", "NO, I do not want to"),
                                paste("delete", name, "records from the database."))
      delete_choice_b <- display_menu(delete_options_b)
      
      # Choice 3.1.1. Confirm Deletion
      if (delete_choice_b == 1) {
        statement <- paste("DELETE FROM ItemInfo WHERE id =", id_to_search)
        rs <- dbSendStatement(mydb, statement = statement)
        dbClearResult(rs)
        
        cat("ItemInfo's record has been deleted from the database.\n")
        cat("=======================\n")
        after_options <- c("Return to ItemInfo menu", "Return to Main menu")
        after_choice <- display_menu(after_options)
        if (after_choice == 1) {
          rosita.inventory()
        } else if (after_choice == 2) {
          rosita()
        }
      
      # Choice 3.1.2. Cancel Deletion
      } else if (delete_choice_b == 2) {
        cat("Delete process halted.\n")
        cat("=======================\n")
        after_options <- c("Return to ItemInfo menu", "Return to Main menu")
        after_choice <- display_menu(after_options)
        if (after_choice == 1) {
          rosita.inventory()
        } else if (after_choice == 2) {
          rosita()
        }
      }
      
    # Choice 3.2. Go back to ItemInfo menu
    } else if (delete_choice_a == 2) {
      rosita.inventory()
      
    # Choice 3.3. Go back to main menu
    } else if (delete_choice_a == 3) {
      rosita()
    }
  
  # Choice 4. Search ItemInfo
  } else if (choice == 4) {
    search_options_a <- c("Search ItemInfo by id", "Return to ItemInfo Menu", "Return to Main Menu")
    search_choice_a <- display_menu(search_options_a)
    
    # Choice 4.1. Search ItemInfo by id
    if (search_choice_a == 1) {
      temp <- dbReadTable(mydb, "ItemInfo")
      list_of_ids <- temp$id
      while (TRUE) {
        id_to_search <- suppressWarnings(as.numeric(readline("Insert the id of the ItemInfo to search: ")))
        if (id_to_search %in% list_of_ids) {
          break
        }
      }
      print(temp[temp$id == id_to_search, ])
      
      cat("=======================\n")
      after_options <- c("Return to ItemInfo menu", "Return to Main menu")
      after_choice <- display_menu(after_options)
      if (after_choice == 1) {
        rosita.inventory()
      } else if (after_choice == 2) {
        rosita()
      }
    # Choice 4.2. Go back to ItemInfo menu
    } else if (search_choice_a == 2) {
      rosita.inventory()
      
    # Choice 4.3. Go back to Main menu
    } else if (search_choice_a == 3) {
      rosita()
    }
    
  # Choice 5. Show all ItemInfo records
  } else if (choice == 5) {
    output <- dbReadTable(mydb, "ItemInfo")
    print(output)
    cat("=======================\n")
    after_options <- c("Return to ItemInfo menu", "Return to Main menu")
    after_choice <- display_menu(after_options)
    if (after_choice == 1) {
      rosita.inventory()
    } else if (after_choice == 2) {
      rosita()
    }
  } else if (choice == 6 ) {
    rosita()
  }
  dbDisconnect(mydb)
}