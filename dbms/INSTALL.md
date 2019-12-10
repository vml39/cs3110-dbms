rlwrap must be installed for 'make query' to work, as it's part of the command
history implementation.
  - for example to install on macOS using homebrew type <brew install rlwrap>
  - otherwise use whatever package management software/method you prefer
If you're unable to install rlwrap you can run queries using 'make sad-query'
instead which runs the same engine but without command history, which is sad.

Type `make query` (or 'make sad-query') in the command prompt **in the modules folder** to start the
database.  When promted type the database you want to use, <testdb> or <bigdb> 
Current functionality only allows the user to SELECT, INSERT, CREATE TABLE, DELETE, 
DROP TABLE, TRUNCATE TABLE, CHANGE DATABASE, READ FROM, HELP, and QUIT. 
Testdb tables are buildings, customers, dorms, orders, and students.
Bigdb tables are customers, icecream, and info

Queries whose results don't fit in your terminal window are written to the 
file displayed after the query is processed.  Tables that contain special 
characters, such as letters with accents, will not be displayed correctly because
characters with accents count for length 2 instead of 1 which messes with 
computation of row lengths.  Displaying tables in terminal is meant for only
letters in the English language a-zA-Z.

Examples:
  **In testdb**
- SELECT * FROM students
- SELECT name class netid FROM students ORDER BY name
- SELECT name class netid FROM students WHERE name LIKE %i% ORDER BY name
- INSERT INTO students VALUES (Joe, jfs9, 1969, ECE, Collegetown)
- INSERT INTO students (name, netid, major) VALUES (Lucy, jfs9, ECE) (* class and home will be empty*)
- SELECT * FROM students (* See what you inserted *)
- DELETE FROM students WHERE name = Joe
- DELETE FROM students WHERE name = Lucy
- SELECT * FROM students (* See what you deleted *)
- CREATE TABLE test (field1, field2, field3) (* Creates an empty table called test.txt and appends new schema to schema.txt in current database *)
- QUIT (* quits dbms *)

**In bigdb**
- SELECT * FROM icecream WHERE calories LIKE %2% (* Selects all rows where calorie field includes a 2*)
- SELECT * FROM icecream (* Due to large table size, the table is written to file in output folder, not to terminal*)

- more query examples can be found in readable files, found in /input/commands