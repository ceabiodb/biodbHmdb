test.searchMultipleWordsInDescription <- function(conn) {
    words <- c('biomarker', 'muscle')
    ids <- conn$searchForEntries(fields=list(description=words),
                                 max.results=3)
    testthat::expect_is(ids, 'character')
    testthat::expect_length(ids, 3)
}

# Set test context
biodb::testContext("Test search of entries")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
defFile <- system.file("definitions.yml", package='biodbHmdb')
biodb$loadDefinitions(defFile)

# Create connector
conn <- biodb$getFactory()$createConn('hmdb.metabolites')

# Run tests
biodb::testThat("We can search for multiple words inside description field.",
                test.searchMultipleWordsInDescription, conn=conn)

# Terminate Biodb
biodb$terminate()

