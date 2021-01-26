test.hmdbmetabolite.nbentries <- function(conn) {

	# Check number of entries
	n <- conn$getNbEntries(count = TRUE)
	expect_is(n, 'integer')
	if (conn$isDownloaded())
		expect_gt(n, 0)
	else
		expect_true(is.na(n))
}

test.old.accession <- function(conn) {

    entry <- conn$getEntry('HMDB06006')
    testthat::expect_is(entry, 'HmdbMetabolitesEntry')
    testthat::expect_equal(entry$getFieldValue('accession'), 'HMDB0006006')
}

test.searchMultipleWordsInDescription <- function(conn) {
    words <- c('biomarker', 'muscle')
    ids <- conn$searchForEntries(fields=list(description=words),
                                 max.results=3)
    testthat::expect_is(ids, 'character')
    testthat::expect_length(ids, 3)
}

# MAIN
########

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='hmdb_test.log', ack=TRUE)

# Load package definitions
file <- system.file("definitions.yml", package='biodbHmdb')
biodb$loadDefinitions(file)

# Set test context
biodb::setTestContext(biodb, "Test HMDB Metabolites connector.")

# Create connector
conn <- biodb$getFactory()$createConn('hmdb.metabolites')

# Run tests
biodb::runGenericTests(conn, list(max.results=1))
biodb::testThat("HMDB metabolite returns enough entries.", test.hmdbmetabolite.nbentries, conn=conn)
biodb::testThat("We can retrieve entries using old accession numbers.", test.old.accession, conn=conn)
biodb::testThat("We can find entries by searching multiple words inside description field.", test.searchMultipleWordsInDescription, conn=conn)

# Terminate Biodb
biodb$terminate()
