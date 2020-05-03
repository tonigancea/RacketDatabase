# RacketDatabase

## About Racket
Racket is a general-purpose, multi-paradigm programming language based on the Scheme dialect of Lisp. It is designed to be a platform for programming language design and implementation.[9] In addition to the core Racket language, Racket is also used to refer to the family of programming languages[10] and set of tools supporting development on and with Racket.[11] Racket is also used for scripting, computer science education, and research.

## This project
Inside this project you can find some `Racket` functions for managing a database.  

**Implemented functions:**  
 * init-database()
 * create-table(_name_, _columns_)
 * get-name(_table_)
 * get-columns(_table_)
 * get-tables(_database_)
 * get-table(_database_, _table-name_)
 * add-table(_database_, _table-name_)
 * remove-table(_database_, _table-name_)
<br>  

 * insert(_database_, _table-name_, _new-record_)
 * simple-select(_database_, _table-name_, _columns_)
 * select(_database_, _table-name_, _columns_, _conditions_)
 * update(_database_, _table-name_, _values_, _conditions_)
 * delete(_database_, _table-name_, _conditions_)


For other examples check the `tester.rkt` file.

## Keep on coding! :)
