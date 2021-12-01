
-- This statement will DROP a database if it exists
-- DROP before CREATE is common in SQL script.
-- It is for making sure to delete the old database before making a new one
-- Because if database exists already you will get an error
DROP DATABASE IF EXISTS SUPERMARKET;

-- This statement will CREATE a database called 'SUPERMARKET'
-- An error occurs if the database exists and you did not specify IF NOT EXISTS.
CREATE DATABASE IF NOT EXISTS SUPERMARKET;

-- This statement will make 'SUPERMARKET' the current database
USE SUPERMARKET;

-- This statement will DROP tables
-- It is for making sure to delete the old table before making a new one
-- Because if a table exists already you will get an error
DROP TABLE IF EXISTS Sales;
DROP TABLE IF EXISTS Employee;
DROP TABLE IF EXISTS ItemInfo;
DROP TABLE IF EXISTS Customer;

/*
This table describes information regarding Employee information.
It specifies an identification number unique to each employee,
the last name, first name of the employee,
the sex of the person, the dateofbirth of the person, date started, 
and time of day it works if applicable.
The schema of the Employee table is: 
Employee(id, last, first, salary, dob, dateStarted, timeWork)
*/
CREATE TABLE Employee
(
     id INT NOT NULL AUTO_INCREMENT, -- INT a medium integer and the NOT NULL constraint enforces a column to NOT accept NULL values.
     last VARCHAR(20) NOT NULL, -- VARCHAR(size): A VARIABLE length string (can contain letters, numbers, and special characters). 
     first VARCHAR(20) NOT NULL,
     salary INT NOT NULL,
     sex ENUM ('M','F') NOT NULL, -- ENUM: A string object that can have only one value, chosen from a list of possible values. 
     dob DATE NOT NULL, -- DATE: A date. Format: YYYY-MM-DD.
     dateStarted DATE NOT NULL,
     timeWorks TIME, -- A time. Format: hh:mm:ss
     /*The PRIMARY KEY constraint uniquely identifies each record in a table. 
     Primary keys must contain UNIQUE values, and cannot contain NULL values.
     A table can have only ONE primary key; and in the table, this primary key can consist of single or multiple columns *
     */
     PRIMARY KEY(id)
)ENGINE = INNODB;
/*
INNODB is the default storage engine for MySQL 5.5 and higher.
It provides transaction-safe (ACID compliant) tables, supports FOREIGN KEY referential-integrity constraints. 
*/



/*
This table describes information regarding ItemInfo information.
It specifies an identification number unique to each item in the supermarket,
the name of the item,
and the price of the item.
The schema of the ItemInfo table is: 
ItemInfo(id, itemName, price)
*/
CREATE TABLE ItemInfo
(
     id INT NOT NULL AUTO_INCREMENT,
     itemName VARCHAR(20) NOT NULL,
     price DOUBLE NOT NULL,
     available INT NOT NULL,
     PRIMARY KEY(id)
)ENGINE = INNODB;


/*
This table describes information regarding Customer information.
It specifies an identification number unique to each customer,
the first name, and the last name of the customer,
and the phone number of the customer
The schema of the Customer table is: 
Customer(id, last, first, phoneNumber)
*/
CREATE TABLE Customer
(
     id INT NOT NULL AUTO_INCREMENT,
     last VARCHAR(20) NOT NULL,
     first VARCHAR(20) NOT NULL,
     phoneNumber BIGINT NOT NULL,
     PRIMARY KEY(id)
)ENGINE = INNODB;


/*
This table describes information regarding inventory in a day in the database.
It specifies an identification number unique to each item sold which,
the price of the item, payment customer used, employee who sold the item,
the inventory available for the item,
The schema of the Inventory table is: 
Inventory(item, price, paymentMethod, employee, available, timeSold)
*/
CREATE TABLE Sales(
     -- A date and time combination. Format: YYYY-MM-DD hh:mm:ss.
     -- Adding DEFAULT and ON UPDATE in the column definition to get automatic initialization and updating to the current date and time
     -- it will help us to store the historical record of employee sales, inventory... over time 
     timeSold DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
     iid INT,
     price DOUBLE NOT NULL,
     paymentMethod VARCHAR(20) NOT NULL,
     employeeID INT,
     available INT NOT NULL, 
     /* A FOREIGN KEY is a field (or collection of fields) in one table that refers to the PRIMARY KEY in another table.
     The table containing the foreign key is called the child table, and the table containing the candidate key is called the referenced or parent table.
     */
     FOREIGN KEY (iid) REFERENCES ItemInfo(id) ON UPDATE SET NULL ON DELETE SET NULL, -- iid must reference ItemInfo id
     FOREIGN KEY (employeeID) REFERENCES Employee(id) ON UPDATE SET NULL ON DELETE SET NULL -- employeeID must reference Employee id
)ENGINE = INNODB;

