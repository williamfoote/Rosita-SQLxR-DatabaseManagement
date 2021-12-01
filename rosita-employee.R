rosita.employee <- function(start = TRUE) {
  mydb <- dbConnect(MySQL(), user = "root", password = stored_password, dbname = "SUPERMARKET", host = "localhost")
  options <- c("Insert New Employee Record", "Update Employee Record",
               "Delete Employee Record", "Search Employee Record",
               "Show All Employee Records", "Return to Main Menu")
  choice <- display_menu(options)
  # Choice 1. Insert a new employee's information into the database
  if (choice == 1) {
    last_name <- readline("Insert Employee's last name: ")
    first_name <- readline("Insert Employee's first name: ")
    while (TRUE) { # creates infinite loop
      salary <- suppressWarnings(as.numeric(readline("Insert Employee's salary: ")))
      if (!is.na(salary)) {
        break
      }
    }
    while (TRUE) { 
      sex <- suppressWarnings(toupper(as.character(readline("Insert Employee's sex (M, F, or X): "))))
      if (sex %in% c("M", "F", "X")) {
        break
      }
    }
    
    # Inserting Birth Month
    
    while (TRUE) {
      birth_month <- suppressWarnings(as.numeric(readline("Insert Employee's birth month (MM): ")))
      if (birth_month %in% 1:12) {
        break
      }
    }
    while (TRUE) { 
      birth_day <- suppressWarnings(as.numeric(readline("Insert Employee's birth day (DD): ")))
      if (birth_day %in% 1:31) {
        break
      }
    }
    while (TRUE) {
      birth_year <- suppressWarnings(as.numeric(readline("Insert Employee's year (YYYY): ")))
      if (birth_year %in% 1900:2020) {
         break
      }
    }
    bm_char <- as.character(birth_month)
    bd_char <- as.character(birth_day)
    by_char <- as.character(birth_year)
    dob <- as.Date(paste(c(by_char, bm_char, bd_char), collapse = "-"))
    
    # Inserting Date Started Working
    
    while (TRUE) {
      start_month <- suppressWarnings(as.numeric(readline("Insert Employee's start month (MM): ")))
      if (start_month %in% 1:12) {
        break
      }
    }
    while (TRUE) { 
      start_day <- suppressWarnings(as.numeric(readline("Insert Employee's start day (DD): ")))
      if (start_day %in% 1:31) {
        break
      }
    }
    while (TRUE) {
      start_year <- suppressWarnings(as.numeric(readline("Insert Employee's start year (YYYY): ")))
      if (start_year %in% 1900:2020) {
        break
      }
    }
    sm_char <- as.character(start_month)
    sd_char <- as.character(start_day)
    sy_char <- as.character(start_year)
    start_date <- as.Date(paste(c(sy_char, sm_char, sd_char), collapse = "-"))
    
    # Inserting TimeWorks variable
    
    while (TRUE) {
      shift_hour <- suppressWarnings(as.integer(readline("Insert Employee's shift start hour (0-24): ")))
      if (shift_hour %in% 0:24) {
        break
      }
    }
    while (TRUE) { 
      shift_minute <- suppressWarnings(as.integer(readline("Insert Employee's shift start minute (0-60): ")))
      if (shift_minute %in% 0:60) {
        break
      }
    }
    while (TRUE) {
      shift_second <- suppressWarnings(as.integer(readline("Insert Employee's shift start second (0-60): ")))
      if (shift_second %in% 0:60) {
        break
      }
    }
    sh_char <- as.character(shift_hour)
    smin_char <- as.character(shift_minute)
    ss_char <- as.character(shift_second)
    timeWorks <- chron(times = paste(c(sh_char, smin_char, ss_char), collapse = ":"))
    
    # Now make data frame of everything
    id <- max(dbReadTable(mydb, "Employee")$id) + 1
    
    input <- data.frame("id" = id,
                        "last" = last_name,
                        "first" = first_name,
                        "salary" = salary,
                        "sex" = sex,
                        "dob" = dob,
                        "dateStarted" = start_date,
                        "timeWorks" = timeWorks)
    
    dbWriteTable(mydb, "Employee", input, row.names = NA, append = TRUE) # Adds this new employee to the database.
    
    cat("Employee has been added to the database.\n")
    cat("=======================\n")
    after_options <- c("Return to Employee menu", "Return to Main menu")
    after_choice <- display_menu(after_options)
    if (after_choice == 1) {
      rosita.employee()
    } else if (after_choice == 2) {
      rosita()
    }
  
  # Choice 2. Updating Employee info
  } else if (choice == 2) { # If one wants to update an employee's information
    update_options_a <- c("Update Employee by id", "Return to Employee Menu", "Return to Main Menu")
    update_choice_a <- display_menu(update_options_a)
      if (update_choice_a == 1) {
        temp <- dbReadTable(mydb, "Employee")
        list_of_ids <- temp$id
        while (TRUE) {
          id_to_search <- suppressWarnings(as.numeric(readline("Insert the id of the Employee to update: ")))
          if (id_to_search %in% list_of_ids) {
            break
          }
        }
        name <- paste(temp[id_to_search, 3], " ", temp[id_to_search, 2], "'s", collapse = "", sep = "")
        update_options <- c(paste("Update", name, names(temp)[-1]), "Return to Employee menu")
        update_choice <- display_menu(update_options)
        if (update_choice == 1) { # To update last name
          updated_last <- readline("Insert udpated last name: ")
          statement <- paste("UPDATE Employee SET last = '", updated_last, "' WHERE id = ", id_to_search, sep = "")
          rs <- dbSendStatement(mydb, statement)
          dbClearResult(rs)
          
          cat("Employee's last name has been updated in the database.\n")
          cat("=======================\n")
          after_options <- c("Return to Employee menu", "Return to Main menu")
          after_choice <- display_menu(after_options)
          if (after_choice == 1) {
            rosita.employee()
          } else if (after_choice == 2) {
            rosita()
          }
        } else if (update_choice == 2) { # To update first name
          updated_first <- readline("Insert udpated first name: ")
          statement <- paste("UPDATE Employee SET first = '", updated_first, "' WHERE id = ", id_to_search, sep = "")
          rs <- dbSendStatement(mydb, statement = statement)
          dbClearResult(rs)
          
          cat("Employee's first name has been updated in the database.\n")
          cat("=======================\n")
          after_options <- c("Return to Employee menu", "Return to Main menu")
          after_choice <- display_menu(after_options)
          if (after_choice == 1) {
            rosita.employee()
          } else if (after_choice == 2) {
            rosita()
          }
        } else if (update_choice == 3) { # To update salary
          while (TRUE) { 
            updated_salary <- suppressWarnings(as.numeric(readline("Insert Employee's updated salary: ")))
            if (!is.na(updated_salary)) {
              break
            }
            statement <- paste("UPDATE Employee SET salary = ", updated_salary, " WHERE id = ", id_to_search, sep = "")
            rs <- dbSendStatement(mydb, statement = statement)
            dbClearResult(rs)
            
            cat("Employee's salary has been updated in the database.\n")
            cat("=======================\n")
            after_options <- c("Return to Employee menu", "Return to Main menu")
            after_choice <- display_menu(after_options)
            if (after_choice == 1) {
              rosita.employee()
            } else if (after_choice == 2) {
              rosita()
            }
          }
        } else if (update_choice == 4) { # To update sex
          while (TRUE) { 
            updated_sex <- suppressWarnings(toupper(as.character(readline("Insert Employee's sex (M, F, or X): "))))
            if (updated_sex %in% c("M", "F", "X")) {
              break
            }
          }
          statement <- paste("UPDATE Employee SET sex = '", updated_sex, "' WHERE id = ", id_to_search, sep = "")
          rs <- dbSendStatement(mydb, statement = statement)
          dbClearResult(rs)
          
          cat("Employee's sex has been updated in the database.\n")
          cat("=======================\n")
          after_options <- c("Return to Employee menu", "Return to Main menu")
          after_choice <- display_menu(after_options)
          if (after_choice == 1) {
            rosita.employee()
          } else if (after_choice == 2) {
            rosita()
          }
        } else if (update_choice == 5) { # To update dob
          while (TRUE) {
            birth_month <- suppressWarnings(as.numeric(readline("Insert Employee's birth month (MM): ")))
            if (birth_month %in% 1:12) {
              break
            }
          }
          while (TRUE) { 
            birth_day <- suppressWarnings(as.numeric(readline("Insert Employee's birth day (DD): ")))
            if (birth_day %in% 1:31) {
              break
            }
          }
          while (TRUE) {
            birth_year <- suppressWarnings(as.numeric(readline("Insert Employee's year (YYYY): ")))
            if (birth_year %in% 1900:2020) {
              break
            }
          }
          bm_char <- as.character(birth_month)
          bd_char <- as.character(birth_day)
          by_char <- as.character(birth_year)
          updated_dob <- as.Date(paste(c(by_char, bm_char, bd_char), collapse = "-"))
          statement <- paste("UPDATE Employee SET dob = '", updated_dob, "' WHERE id = ", id_to_search, sep = "")
          rs <- dbSendStatement(mydb, statement = statement)
          dbClearResult(rs)
          
          cat("Employee's dob has been updated in the database.\n")
          cat("=======================\n")
          after_options <- c("Return to Employee menu", "Return to Main menu")
          after_choice <- display_menu(after_options)
          if (after_choice == 1) {
            rosita.employee()
          } else if (after_choice == 2) {
            rosita()
          }
        } else if (update_choice == 6) { # To update dateStarted
          while (TRUE) {
            start_month <- suppressWarnings(as.numeric(readline("Insert Employee's new start month (MM): ")))
            if (start_month %in% 1:12) {
              break
            }
          }
          while (TRUE) { 
            start_day <- suppressWarnings(as.numeric(readline("Insert Employee's new start day (DD): ")))
            if (start_day %in% 1:31) {
              break
            }
          }
          while (TRUE) {
            start_year <- suppressWarnings(as.numeric(readline("Insert Employee's new start year (YYYY): ")))
            if (start_year %in% 1900:2020) {
              break
            }
          }
          sm_char <- as.character(start_month)
          sd_char <- as.character(start_day)
          sy_char <- as.character(start_year)
          update_start_date <- as.Date(paste(c(sy_char, sm_char, sd_char), collapse = "-"))
          statement <- paste("UPDATE Employee SET dateStarted = '", update_start_date,
                             "' WHERE id = ", id_to_search, sep = "")
          rs <- dbSendStatement(mydb, statement = statement)
          dbClearResult(rs)
          
          cat("Employee's dateStarted has been updated in the database.\n")
          cat("=======================\n")
          after_options <- c("Return to Employee menu", "Return to Main menu")
          after_choice <- display_menu(after_options)
          if (after_choice == 1) {
            rosita.employee()
          } else if (after_choice == 2) {
            rosita()
          }
        } else if (update_choice == 7) { # To update timeWorks
          while (TRUE) {
            shift_hour <- suppressWarnings(as.integer(readline("Insert Employee's new shift start hour (0-24): ")))
            if (shift_hour %in% 0:24) {
              break
            }
          }
          while (TRUE) { 
            shift_minute <- suppressWarnings(as.integer(readline("Insert Employee's new shift start minute (0-60): ")))
            if (shift_minute %in% 0:60) {
              break
            }
          }
          while (TRUE) {
            shift_second <- suppressWarnings(as.integer(readline("Insert Employee's new shift start second (0-60): ")))
            if (shift_second %in% 0:60) {
              break
            }
          }
          sh_char <- as.character(shift_hour)
          smin_char <- as.character(shift_minute)
          ss_char <- as.character(shift_second)
          updated_timeWorks <- chron(times = paste(c(sh_char, smin_char, ss_char), collapse = ":"))
          statement <- paste("UPDATE Employee SET timeWorks = '", updated_timeWorks,
                             "' WHERE id = ", id_to_search, sep = "")
          rs <- dbSendStatement(mydb, statement = statement)
          dbClearResult(rs)
          
          cat("Employee's timeWorks has been updated in the database.\n")
          cat("=======================\n")
          after_options <- c("Return to Employee menu", "Return to Main menu")
          after_choice <- display_menu(after_options)
          if (after_choice == 1) {
            rosita.employee()
          } else if (after_choice == 2) {
            rosita()
          }
        } else if (update_choice == 8) { # To go back to Employee menu
          rosita.employee()
        }
      } else if (update_choice_a == 2) { # To go back to Employee menu
        rosita.employee()
      } else if (update_choice_a == 3) { # To go back to Main menu
        rosita()
      }
    } else if (choice == 3) { # To Delete records
      delete_options <- c("Delete Employee by id", "Return to Employee Menu", "Return to Main Menu")
      delete_choice <- display_menu(delete_options)
      if (delete_choice == 1) { # To Delete records
        temp <- dbReadTable(mydb, "Employee")
        list_of_ids <- temp$id
        while (TRUE) {
          id_to_search <- suppressWarnings(as.numeric(readline("Insert the id of the Employee to delete: ")))
          if (id_to_search %in% list_of_ids) {
            break
          }
        }
        name <- paste(temp[id_to_search, 3], " ", temp[id_to_search, 2], "'s", collapse = "", sep = "")
        delete_options_b <- paste(c("YES, I would like to", "NO, I do not want to"),
                                paste("delete", name, "record from the database."))
        delete_choice_b <- display_menu(delete_options_b)
        if (delete_choice_b == 1) { # To Confirm delete records
          statement <- paste("DELETE FROM Employee WHERE id =", id_to_search)
          rs <- dbSendStatement(mydb, statement = statement)
          dbClearResult(rs)
          
          cat("The employee's record has been deleted from the database.\n")
          cat("=======================\n")
          after_options <- c("Return to Employee menu", "Return to Main menu")
          after_choice <- display_menu(after_options)
          if (after_choice == 1) {
            rosita.employee()
          } else if (after_choice == 2) {
            rosita()
          }
        } else if (delete_choice_b == 2) { # To halt delete records
          cat("Delete process halted. Begin the program again to complete another task.")
          cat("=======================\n")
          after_options <- c("Return to Employee menu", "Return to Main menu")
          after_choice <- display_menu(after_options)
          if (after_choice == 1) {
            rosita.employee()
          } else if (after_choice == 2) {
            rosita()
          }
        }
      } else if (delete_choice == 2) { # To return to Employee menu
        rosita.employee()
      } else if (delete_choice == 3) { # To return to main menu
        rosita()
      }
    } else if (choice == 4) { # To search Employee records
      search_options_a <- c("Search by id", "Return to Employee Menu", "Return to Main Menu")
      search_choice_a <- display_menu(search_options_a)
      if (search_choice_a == 1) {
        temp <- dbReadTable(mydb, "Employee")
        list_of_ids <- temp$id
        while (TRUE) {
          id_to_search <- suppressWarnings(as.numeric(readline("Insert the id of the Employee to search: ")))
          if (id_to_search %in% list_of_ids) {
            break
          }
        }
        print(temp[temp$id == id_to_search, ])
        cat("=======================\n")
        after_options <- c("Return to Employee menu", "Return to Main menu")
        after_choice <- display_menu(after_options)
        if (after_choice == 1) {
          rosita.employee()
        } else if (after_choice == 2) {
          rosita()
        }
      } else if (search_choice_a == 2) { # To return to Employee menu
        rosita.employee()
      } else if (search_choice_a == 3) { # To return to Main menu
        rosita()
      }
    }
  else if (choice == 5) { # To show all Employee records
    output <- dbReadTable(mydb, "Employee")
    print(output)
    cat("=======================\n")
    after_options <- c("Return to Employee menu", "Return to Main menu")
    after_choice <- display_menu(after_options)
    if (after_choice == 1) {
      rosita.employee()
    } else if (after_choice == 2) {
      rosita()
    }
  } else if (choice == 6) { # To return to Main menu
    rosita()
  }
  dbDisconnect(mydb) # Need to close the connection that was opened in line 2
}