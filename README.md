# recon
A workaround that provides qualified expressions (aka record constructors) in Oracle versions &lt;18c.

This is an early working prototype. One should expect breaking changes to be introduced. I don't recommend using it in production environments.

## Caveats

* `ALL_TAB_COLS.DATA_DEFAULT` over 32k causes error. (ALL_TAB_COLS.DATA_DEFAULT is a LONG column; this could be fixed using dbms_sql, see http://www.oracle-developer.net/display.php?id=430)


## TODO:

* extract code that builds functions into separate, deterministic functions;
* move types to separate package;
* add table comments;
* create tutorial;
* create user guide;
* associative arrays;
