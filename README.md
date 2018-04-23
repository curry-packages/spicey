The Spicey Web Application Framework
====================================

To generate an application, follow the steps below.

1. Create a Curry program containing a constant of type `Database.ERD.ERD`
   (the module `Database.ERD` is part of the package `cdbi`)
   which describes your entity-relationship model
   (see the file "examples/BlogERD.curry" as an example).

2. Execute `spiceup` and supply the name of the Curry ERD program, e.g.,

       spiceup .../BlogERD.curry

   This generates the complete source code of the initial application
   as a Curry package
   (see the generated file README.txt for some explanations).

   You can also provide a file name for the SQLite3 database in which
   all data is stored, e.g.,

       spiceup --db BlogData.db .../BlogERD.curry

   If the parameter "--db ..." is not provided, then the name of database
   file is "<ERD>.db" where <ERD> is the name of the specified ER model.
   Since this file name will be used in the _generated_ cgi programs,
   a relative file name will be relative to the place where
   the cgi programs are stored. In order to avoid confusion due to
   relative file names, it might be better to specify
   an absolute path name for the database file.
   This path could also be set in the definition of the constant
   `sqliteDBFile` in the generated Curry program `Model/<ERD>.curry`.

3. Change into the generated directory containing all sources as a
   Curry package, e.g., by `cd Blog`.

4. Install all required packages by `make install`.

5. Compile the generated programs by `make compile`.

6. Configure the Makefile (variable WEBSERVERDIR) and execute
   `make deploy` to deploy the web application.

7. After the successful compilation, the application is executable
   in a web browser by loading `<URL of web dir>/spicey.cgi`.

Note that the database is generated with the `cdbi` package.
Hence, one can also use embedded SQL statements when further developing
the Curry code. The syntax and use of such embedded SQL statements
is described in the Curry preprocessor.
