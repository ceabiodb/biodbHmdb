test.searchMultipleWordsInDescription <- function(conn) {

    words <- c('and')
    ids <- conn$searchForEntries(fields=list(description=words),
                                 max.results=3)
    testthat::expect_is(ids, 'character')
    testthat::expect_length(ids, 2)
    testthat::expect_equal(ids, c('HMDB0000001', 'HMDB0000002'))

    words <- c('histidine', 'cerebrovascular')
    ids <- conn$searchForEntries(fields=list(description=words),
                                 max.results=3)
    testthat::expect_is(ids, 'character')
    testthat::expect_length(ids, 1)
    testthat::expect_equal(ids, 'HMDB0000001')

    words <- c('monoalkylamines', 'shiitakes')
    ids <- conn$searchForEntries(fields=list(description=words),
                                 max.results=3)
    testthat::expect_is(ids, 'character')
    testthat::expect_length(ids, 1)
    testthat::expect_equal(ids, 'HMDB0000002')
}

# Set test context
biodb::testContext("Test search of entries")

source('zip_builder.R')

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
defFile <- system.file("definitions.yml", package='biodbHmdb')
biodb$loadDefinitions(defFile)

# Create connector
conn <- biodb$getFactory()$createConn('hmdb.metabolites')
conn$setPropValSlot('urls', 'db.zip.url', two_entries_zip_file)

# Run tests
biodb::testThat("We can search for multiple words inside description field.",
                test.searchMultipleWordsInDescription, conn=conn)

# Terminate Biodb
biodb$terminate()

