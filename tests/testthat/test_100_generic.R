# Set test context
biodb::testContext("Generic tests")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='test_100_generic.log', ack=TRUE)

# Load package definitions
defFile <- system.file("definitions.yml", package='biodbHmdb')
biodb$loadDefinitions(defFile)

# Create connector
conn <- biodb$getFactory()$createConn('hmdb.metabolites')

# Run generic tests
biodb::runGenericTests(conn, list(max.results=1))

# Terminate Biodb
biodb$terminate()