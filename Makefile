# vi: fdm=marker

# Global variables {{{1
################################################################

# Mute R 3.6 "Registered S3 method overwritten" warning messages.
# Messages that were output:
#     Registered S3 method overwritten by 'R.oo':
#       method        from
#       throw.default R.methodsS3
#     Registered S3 method overwritten by 'openssl':
#       method      from
#       print.bytes Rcpp
export _R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_=no

# Set cache folder
ifndef BIODB_CACHE_DIRECTORY
export BIODB_CACHE_DIRECTORY=$(PWD)/cache
endif

# Set testthat reporter
ifndef TESTTHAT_REPORTER
ifdef VIM
TESTTHAT_REPORTER=summary
else
TESTTHAT_REPORTER=progress
endif
endif

PKG_VERSION=$(shell grep '^Version:' DESCRIPTION | sed 's/^Version: //')
GIT_VERSION=$(shell git describe --tags | sed 's/^v\([0-9.]*\)[a-z]*.*$$/\1/')
ZIPPED_PKG=biodbHmdb_$(PKG_VERSION).tar.gz
REF_BIB:=$(wildcard ../public-notes/references.bib)

# Display values of main variables
$(info "BIODB_CACHE_DIRECTORY=$(BIODB_CACHE_DIRECTORY)")
$(info "BIODB_CACHE_READ_ONLY=$(BIODB_CACHE_READ_ONLY)")
$(info "PKG_VERSION=$(PKG_VERSION)")

RFLAGS=--slave --no-restore

# For R CMD SHLIB
export PKG_CXXFLAGS=$(shell R --slave -e "Rcpp:::CxxFlags()")

# Set test file filter
ifndef TEST_FILE
TEST_FILE=NULL
else
TEST_FILE:='$(TEST_FILE)'
endif

# Default target {{{1
################################################################

all: compile
	R $(RFLAGS) CMD SHLIB src/*.cpp

compile: R/RcppExports.R
	R $(RFLAGS) CMD SHLIB -o src/biodbHmdb.so src/*.cpp

R/RcppExports.R: src/*.cpp
	R $(RFLAGS) -e "Rcpp::compileAttributes('$(CURDIR)')"

coverage:
	R $(RFLAGS) -e "covr::codecov()"

# Check and test {{{1
################################################################

check: clean.vignettes $(ZIPPED_PKG)
	R $(RFLAGS) -e 'BiocCheck::BiocCheck("$(ZIPPED_PKG)", `new-package`=TRUE, `quit-with-status`=TRUE, `no-check-formatting`=TRUE)'

test: compile
ifdef VIM
	R $(RFLAGS) -e "devtools::test('$(CURDIR)', filter=$(TEST_FILE), reporter=c('$(TESTTHAT_REPORTER)', 'fail'))" | sed 's!\([^/A-Za-z_-]\)\(test[^/A-Za-z][^/]\+\.R\)!\1tests/testthat/\2!'
else
	R $(RFLAGS) -e "devtools::test('$(CURDIR)', filter=$(TEST_FILE), reporter=c('$(TESTTHAT_REPORTER)', 'fail'))"
endif

win:
	R $(RFLAGS) -e "devtools::check_win_devel('$(CURDIR)')"

# Build {{{1
################################################################

build: $(ZIPPED_PKG)

$(ZIPPED_PKG): doc
	R CMD build .

# Documentation {{{1
################################################################

doc: R/RcppExports.R
	R $(RFLAGS) -e "devtools::document('$(CURDIR)')"

vignettes: clean.vignettes
	@echo Build vignettes for already installed package, not from local soures.
	R $(RFLAGS) -e "devtools::build_vignettes('$(CURDIR)')"

ifneq ($(REF_BIB),)
vignettes: vignettes/references.bib

vignettes/references.bib: $(REF_BIB)
	cp $< $@
endif

# Install {{{1
################################################################

install.deps:
	R $(RFLAGS) -e "devtools::install_dev_deps('$(CURDIR)')"

install: uninstall install.local list.classes

install.local:
	R $(RFLAGS) -e "devtools::install_local('$(CURDIR)', dependencies = TRUE)"

list.classes:
	R $(RFLAGS) -e 'library(biodbHmdb) ; cat("Exported methods and classes:", paste(" ", ls("package:biodbHmdb"), collapse = "\n", sep = ""), sep = "\n")'

uninstall:
	R $(RFLAGS) -e "try(devtools::uninstall('$(CURDIR)'), silent = TRUE)"

# Clean {{{1
################################################################

clean: clean.build clean.vignettes
	$(RM) src/*.o src/*.so src/*.dll
	$(RM) -r tests/test.log tests/output tests/test\ *.log
	$(RM) -r biodbHmdb.Rcheck
	$(RM) -r Meta

clean.vignettes:
	$(RM) vignettes/*.R vignettes/*.html
	$(RM) -r doc

clean.build:
	$(RM) biodbHmdb_*.tar.gz

clean.cache:
	$(RM) -r $(BIODB_CACHE_DIRECTORY)

# Phony targets {{{1
################################################################

.PHONY: all clean win test build check vignettes install uninstall devtools.check devtools.build clean.build clean.cache doc check.version coverage
