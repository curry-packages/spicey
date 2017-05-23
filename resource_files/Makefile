# Generic Makefile for Spicey applications

CURRYOPTIONS=:set -time

# Target directory where the compiled cgi programs, style sheets, etc
# should be stored, e.g.: $(HOME)/public_html
WEBSERVERDIR = $(error "Define variable WEBSERVERDIR!")

# Definition of the Curry installation bin directory to be used:
export CURRYBIN=XXXCURRYBINXXX

# Executable of the Curry Package Manager CPM:
CPM := $(CURRYBIN)/cpm

# The root directory of the sources of the Spicey application:
SRCDIR := $(CURDIR)/src

# The load path for the Spicey application:
export CURRYPATH := $(SRCDIR):$(SRCDIR)/Model

.PHONY: all
all:
	@echo "make: deploy install compile load run clean?"

# Install the packages required by the generated Spicey application:
.PHONY: install
install:
	$(CPM) install

# Compile the generated Spicey application:
.PHONY: compile
compile:
	$(CPM) exec $(CURRYBIN)/curry $(CURRYOPTIONS) :load Main :quit

# Load the generated Spicey application into the Curry system so that
# one can evaluate some expressions:
.PHONY: load
load:
	$(CPM) exec $(CURRYBIN)/curry $(CURRYOPTIONS) :load Main

# Runs the generated Spicey application by evaluating the main expression.
# This might be useful to test only the initial web page without a web server
.PHONY: run
run:
	$(CPM) exec $(CURRYBIN)/curry $(CURRYOPTIONS) :load Main :eval main :q

# Deploy the generated Spicey application, i.e., install it in the
# web directory WEBSERVERDIR:
.PHONY: deploy
deploy:
	mkdir -p $(WEBSERVERDIR)
	$(CPM) exec $(CURRYBIN)/curry makecgi -standalone -m main -o $(WEBSERVERDIR)/spicey.cgi Main.curry
	# copy other files (style sheets, images,...)
	cp -r public/* $(WEBSERVERDIR)
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
