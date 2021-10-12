# biodbHmdb

[![Codecov test coverage](https://codecov.io/gh/pkrog/biodbHmdb/branch/master/graph/badge.svg)](https://codecov.io/gh/pkrog/biodbHmdb?branch=master)

An R package for accessing [HMDB](http://www.hmdb.ca) online database, based on
R package/framework [biodb](https://github.com/pkrog/biodb/).

## Introduction

This extension package to [biodb](https://github.com/pkrog/biodb/) implements a
connector to the [HMDB](http://www.hmdb.ca) database.

## Installation

Install the latest stable version using Bioconductor:
```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install('biodbHmdb')
```

## Examples

Searching for entries with multiple words inside the field "description":
```r
mybiodb <- biodb::newInst()
conn <- mybiodb$getFactory()$createConn('hmdb.metabolites')
ids <- conn$searchForEntries(fields=list(description=c('milk', 'Oligosaccharide')))
```

## Documentation

See the introduction vignette:
```r
vignette('biodbHmdb', package='biodbHmdb')
```

To get documentation on the implemented connector once inside R, run:
```r
?biodbHmdb::HmdbMetabolitesConn
```

## Citations

HMDB website: <http://www.hmdb.ca>.

 * Wishart DS, Tzur D, Knox C, et al., HMDB: the Human Metabolome Database. Nucleic Acids Res. 2007 Jan;35(Database issue):D521-6, <https://doi.org/10.1093/nar/gkl923>.
 * Wishart DS, Knox C, Guo AC, et al., HMDB: a knowledgebase for the human metabolome. Nucleic Acids Res. 2009 37(Database issue):D603-610, <https://doi.org/10.1093/nar/gkn810>.
 * Wishart DS, Jewison T, Guo AC, Wilson M, Knox C, et al., HMDB 3.0 — The Human Metabolome Database in 2013. Nucleic Acids Res. 2013. Jan 1;41(D1):D801-7, <https://doi.org/10.1093/nar/gks1065>.

