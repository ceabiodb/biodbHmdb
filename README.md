# biodbHmdb

[![Build Status](https://travis-ci.org/pkrog/biodbHmdb.svg?branch=master)](https://travis-ci.org/pkrog/biodbHmdb)

An R package for accessing [HMDB](http://www.hmdb.ca) online database, based on R package/framework [biodb](https://github.com/pkrog/biodb/).

## Introduction

This extension package to [biodb](https://github.com/pkrog/biodb/) implements a connector to the [HMDB](http://www.hmdb.ca) database.

## Installation

Install the latest version of this package by running the following commands:
```r
devtools::install_github('pkrog/biodb', dependencies=TRUE)
devtools::install_github('pkrog/biodbHmdb', dependencies=TRUE)
```

## Examples

Searching for entries with multiple words inside the field "description":
```r
mybiodb <- biodb::Biodb()
conn <- mybiodb$getFactory()$createConn('hmdb.metabolites')
ids <- conn$searchForEntries(fields=list(description=c('milk', 'Oligosaccharide')))
```

## Documentation

To get documentation on the implemented connector once inside R, run:
```r
?biodbHmdb::HmdbMetabolitesConn
```

## Citations

HMDB website: <http://www.hmdb.ca>.

 * Wishart DS, Tzur D, Knox C, et al., HMDB: the Human Metabolome Database. Nucleic Acids Res. 2007 Jan;35(Database issue):D521-6, <https://doi.org/10.1093/nar/gkl923>.
 * Wishart DS, Knox C, Guo AC, et al., HMDB: a knowledgebase for the human metabolome. Nucleic Acids Res. 2009 37(Database issue):D603-610, <https://doi.org/10.1093/nar/gkn810>.
 * Wishart DS, Jewison T, Guo AC, Wilson M, Knox C, et al., HMDB 3.0 — The Human Metabolome Database in 2013. Nucleic Acids Res. 2013. Jan 1;41(D1):D801-7, <https://doi.org/10.1093/nar/gks1065>.

