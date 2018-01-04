Welcome to the Spicey web application framework!

To generate an application, follow the steps below.

1. Create a Curry program containing a constant of type `Database.ERD.ERD`
   which describes your entity-relationship model
   (see the file "examples/BlogERD.curry" as an example).

2. Since the imported module `Database.ERD` is part of the package `cdbi`,
   this package must be installed, e.g., by the command

       cypm add --dependency cdbi
       
3. Execute `spiceup` and supply the name of the Curry ERD program, e.g.,

       cypm exec spiceup .../BlogERD.curry

   This generates the complete source code of the initial application
   (see the generated file README.txt for some explanations).

   You can also provide a file name for the SQLite3 database in which
   all data is stored, e.g.,

       cypm exec spiceup --db BlogData.db .../Blog.erdterm

   If the parameter "--db ..." is not provided, then the name of database
   file is "<ERD>.db" where <ERD> is the name of the specified ER model.
   Since this file name will be used in the _generated_ cgi programs,
   a relative file name will be relative to the place where
   the cgi programs are stored.

4. Compile the generated programs by `make compile`.

5. Configure the Makefile (variable WEBSERVERDIR) and execute
   `make deploy` to deploy the web application.

6. After the successful compilation, the application is executable
   in a web browser by loading `<URL of web dir>/spicey.cgi`.
