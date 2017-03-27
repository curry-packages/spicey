# Generic Makefile for Spicey applications

CURRYOPTIONS=:set -time

# Target directory where the compiled cgi programs, style sheets, etc
# should be stored, e.g.: $(HOME)/public_html
WEBSERVERDIR = $(error "Define variable WEBSERVERDIR!")

# Definition of the Curry installation bin directory to be used:
export CURRYBIN=XXXCURRYBINXXX

# The root directory of the sources of the Spicey application:
SRCDIR := $(CURDIR)
# The load path for the Spicey application:
export CURRYPATH := $(SRCDIR)/views:$(SRCDIR)/controllers:$(SRCDIR)/models:$(SRCDIR)/system:$(SRCDIR)/config

.PHONY: all
all:
	@echo "make: deploy compile load run clean?"

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
	$(CURRYBIN)/curry $(CURRYOPTIONS) :load Main :eval main :quit

# Deploy the generated Spicey application, i.e., install it in the
# web directory WEBSERVERDIR:
.PHONY: deploy
deploy:
	mkdir -p $(WEBSERVERDIR)
	$(CURRYBIN)/curry makecgi -standalone -m main -o $(WEBSERVERDIR)/spicey.cgi Main.curry
	# copy other files (style sheets, images,...)
	cp -r $(SRCDIR)/public/* $(WEBSERVERDIR)
	chmod -R go+rX $(WEBSERVERDIR)

# clean up generated programs
.PHONY: clean
clean: 
	$(CURRYBIN)/cleancurry -r
