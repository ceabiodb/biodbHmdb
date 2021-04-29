# Set test context
biodb::testContext("Generic tests")

source('zip_builder.R')

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
defFile <- system.file("definitions.yml", package='biodbHmdb')
biodb$loadDefinitions(defFile)

# Create connector
conn <- biodb$getFactory()$createConn('hmdb.metabolites')
conn$setPropValSlot('urls', 'db.zip.url', two_entries_zip_file)

# Run generic tests
biodb::runGenericTests(conn, list(max.results=1))

# Terminate Biodb
biodb$terminate()
