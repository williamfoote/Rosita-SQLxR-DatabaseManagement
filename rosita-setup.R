### 1. Installing and Setting Up the RMySQL package to be used with MySQL ###

# install.packages("RMySQL")
# install.packages("chron")
# install.packages("getPass")
# the "chron" package will be needed to use the Rosita program, so I've included the
# installation process here to get that task out of the way.

# getPass prompts the user to put in their password in the rosita setup process, and is only required once.

library(RMySQL)
library(chron)
library(getPass)

### 2. Connecting the Database ###

stored_password <- getPass("Please enter your MySQL password")

mydb <- dbConnect(MySQL(), user = "root", password = stored_password, dbname = "SUPERMARKET", host = "localhost")

# Things to change above ^. 

## user might need to be changed to your MySQL username
## password might need to be changed to your MySQL password
## dbname is what the database would be named in MySQL in the create.sql commands.

dbSendQuery(mydb, "SET GLOBAL local_infile = true;") # Leave as-is

### 3. Exploring the database ###

dbListTables(mydb) # Lists tables in the connected database
# Make sure the right tables exist in the database

### 4. Uploading information to the database ###

### 4.1 Read in data to R ###

ItemInfo <- read.csv("~/Desktop/work stuff/fall_20_work/item.del")
Customer <- read.csv("~/Desktop/work stuff/fall_20_work/customer.del")
Employee <- read.csv("~/Desktop/work stuff/fall_20_work/employee.del")
Sales <- read.csv("~/Desktop/work stuff/fall_20_work/inventory.del")

### 4.2 Upload data frames from R to the database in MySQL ###

dbWriteTable(conn = mydb, name = "ItemInfo", value = ItemInfo,
             append = TRUE, row.names = NA)
dbWriteTable(conn = mydb, name = "Customer", value = Customer,
             append = TRUE, row.names = NA)
dbWriteTable(conn = mydb, name = "Employee", value = Employee,
             append = TRUE, row.names = NA)
dbWriteTable(conn = mydb, name = "Sales", value = Sales,
             append = TRUE, row.names = NA)

### 4.3 Make sure tables have been read correctly ###

dbReadTable(mydb,"ItemInfo")
dbReadTable(mydb,"Customer")
dbReadTable(mydb,"Employee")
dbReadTable(mydb,"Sales")

### 5 Disconnect from the database ###

dbDisconnect(mydb) # Always need to close the connection with dbDisconnect(mydb)
# to prevent too many connections from being opened. 

suppressWarnings(rm(stored_password, envir = .GlobalEnv)) # To remove the password from the global environment.
