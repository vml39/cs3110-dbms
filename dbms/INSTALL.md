No installation needed.

Type `make query` in the command prompt **in the modules folder** to start the
database.  When promted type the database you want to use, <testdb> or <bigdb> 
Current functionality only allows the user to SELECT, INSERT, CREATE TABLE, DELETE, and QUIT. 
Test tables are students and buildings.
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