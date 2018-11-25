# Generic Makefile for Spicey applications

CURRYOPTIONS=:set -time

# Target directory where the compiled cgi programs, style sheets, etc
# should be stored, e.g.: $(HOME)/public_html
WEBSERVERDIR = $(error "Define variable WEBSERVERDIR!")

# Definition of the Curry installation bin directory to be used:
export CURRYBIN=XXXCURRYBINXXX

# Executable of the Curry Package Manager CPM:
CPM := $(CURRYBIN)/cypm

# The root directory of the sources of the Spicey application:
SRCDIR := $(CURDIR)/src

# The load path for the Spicey application:
export CURRYPATH := $(SRCDIR):$(SRCDIR)/Model

# Executable of CPNSD:
CPNSD := $(shell which curry-cpnsd)
# Executable of the CGI registry and submission form:
CURRYCGI := $(shell which curry-cgi)
# Executable of the makecgi:
MAKECGI := $(shell which curry-makecgi)

############################################################################

.PHONY: all
all:
	@echo "make: deploy install compile load run clean?"

# Install the packages required by the generated Spicey application:
.PHONY: install
install:
	$(CPM) install

# check presence of tools required for deployment and install them:
.PHONY: checkdeploy
checkdeploy:
	@if [ ! -x "$(CPNSD)" ] ; then \
	   echo "Installing required executable 'curry-cpnsd'..." ; \
           $(CPM) install cpns ; fi
	@if [ ! -x "$(CURRYCGI)" ] ; then \
	   echo "Installing required executable 'curry-cgi'..." ; \
           $(CPM) install html-cgi ; fi
	@if [ ! -x "$(MAKECGI)" ] ; then \
	   echo "Installing required executable 'curry-makecgi'..." ; \
           $(CPM) install html ; fi

# Compile the generated Spicey application:
.PHONY: compile
compile:
	$(CURRYBIN)/curry $(CURRYOPTIONS) :load Main :quit

# Load the generated Spicey application into the Curry system so that
# one can evaluate some expressions:
.PHONY: load
load:
	$(CURRYBIN)/curry $(CURRYOPTIONS) :load Main

# Runs the generated Spicey application by evaluating the main expression.
# This might be useful to test only the initial web page without a web server
.PHONY: run
run:
	$(CURRYBIN)/curry $(CURRYOPTIONS) :load Main :eval main :q

# Deploy the generated Spicey application, i.e., install it in the
# web directory WEBSERVERDIR:
.PHONY: deploy
deploy: checkdeploy
	mkdir -p $(WEBSERVERDIR)
	$(CPM) exec $(MAKECGI) -standalone -m main -o $(WEBSERVERDIR)/spicey.cgi Main.curry
	# copy other files (style sheets, images,...)
	cp -r public/* $(WEBSERVERDIR)
	mkdir -p $(WEBSERVERDIR)/data # create private data dir
	cp -p data/htaccess $(WEBSERVERDIR)/data/.htaccess # and make it private
	chmod -R go+rX $(WEBSERVERDIR)

# clean up generated the package directory
.PHONY: clean
clean: 
	$(CPM) clean

# clean everything, including the deployed files (be sure to save the
# database files first!)
.PHONY: cleanall
cleanall: clean
	/bin/rm -rf $(WEBSERVERDIR)
