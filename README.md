Spicey: An ER-based Web Framework for Curry
===========================================

Description
-----------

Spicey is a framework to support the implementation of web-based systems in 
the multi-paradigm declarative language
[Curry](http://www.curry-lang.org).
Spicey generates an initial implementation from an
entity-relationship (ER) description of the underlying data.
The generated implementation contains operations to create and
manipulate entities of the data model, supports authentication,
authorization, session handling, and the composition of
individual operations to user processes.
Furthermore, the implementation ensures
the consistency of the database w.r.t. the data dependencies
specified in the ER model, i.e., updates initiated by the user
cannot lead to an inconsistent state of the database.


Example
-------

As an example, we consider the implementation of a web log system with Spicey.
A "blog" consists of "Entry" articles
having title, text, author, and date as attributes, and
"Comments" to each entry. Furthermore, there are a number
of "Tags" to classify Entry articles.
The complete entity-relationship model of this blog structure can be
specified in Curry by a Curry data term defines as a top-level entity in a
[Curry module](https://cpm.curry-lang.org/PACKAGES/spicey-4.2.0/examples/BlogERD.curry).

From this ER specification, Spicey generates the source code
of a web-based system that provides access to the data via
standard web browsers.
For instance, here is a snapshot of the web interface
generated by Spicey for the blog description:

![Main view of Spicey](https://cpm.curry-lang.org/PACKAGES/spicey-4.2.0/images/spicey_main.jpg)

Spicey generates also forms to insert new entities or edit
the attributes of existing ones.
If an entity is related to other entities,
the form allows the manipulation of these relations.
For instance, each Entry of the blog can be related to multiple
tags. Therefore, a form to edit an Entry also allows to select
related tags, as shown below:

![Edit form of Spicey](https://cpm.curry-lang.org/PACKAGES/spicey-4.2.0/images/spicey_edit.jpg)

Beyond the basic CRUD (create/read/update/delete) functionality,
a Spicey applications has also infrastructures for many features
of web-based systems, like session management (storing of session data),
authentication, authorization, or complex user processes.
These aspects are described in the documents shown below.


Documentation
-------------

Apart from a
[short user manual](https://cpm.curry-lang.org/DOC/spicey-4.2.0/main.pdf),
there are documents where one can
find more details about Spicey and its implementation.

* [An ER-based Framework for Declarative Web Programming](http://dx.doi.org/10.1007/978-3-642-11503-5_18) (PADL 2010):
  This conference paper introduces the basic ideas of Spicey and
  its implementation.
* [An ER-based Framework for Declarative Web Programming](http://dx.doi.org/10.1017/S1471068412000385) (TPLP 2012):
  This is an expanded version of the previous paper. It provides some
  more details about Spicey and its implementation.
* [Master Thesis](https://www.michaelhanus.de/lehre/abschlussarbeiten/msc/koschnicke.pdf) (in German):
  This is a master thesis on Spicey, written by Sven Koschnicke, the initial
  developer of Spicey. The thesis contains details about the design and
  implementation of Spicey.


Installation
------------

The implementation of Spicey is a package managed by the Curry
Package Manager CPM. Thus, to install the newest version of Spicey,
use the following commands:

    > cypm update
    > cypm install spicey

This downloads the newest package, compiles it, and places the
executable `spiceup` into the directory `$HOME/.cpm/bin`. Hence it is
recommended to add this directory to your path in order to execute
Spicey as described below.

The default database system used by Spicey is
[SQLite3](http://www.sqlite.org/) so that it should be also installed.


How to generate a web application
---------------------------------

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

4. Define in the Makefile the variable WEBSERVERDIR (and possibly
   other variables, like SYSTEM or CURRYOPTIONS).

5. Install all required packages by `make install`.

6. Compile the generated programs by `make compile`.

7. Execute `make deploy` to deploy the web application.

8. After the successful compilation, the application is executable
   in a web browser by loading `<URL of web dir>/spicey.cgi`.

Note that the database is generated with the `cdbi` package.
Hence, one can also use embedded SQL statements when further developing
the Curry code. The syntax and use of such embedded SQL statements
is described in the Curry preprocessor.
