TEST_DIR <- getwd()
RES_DIR <- file.path(TEST_DIR, 'res')
XML_EMPTY <- file.path(RES_DIR, 'hmdb_empty.xml')
XML_SINGLE_ENTRY <- file.path(RES_DIR, 'hmdb_one_entry.xml')
XML_WITH_HEADER_TAGS <- file.path(RES_DIR, 'hmdb_one_entry_with_header_tags.xml')
XML_TWO_ENTRIES <- file.path(RES_DIR, 'hmdb_two_entries.xml')
XML_NO_ID <- file.path(RES_DIR, 'hmdb_single_entry_no_id.xml')
XML_WRONG_ID_TAG <- file.path(RES_DIR, 'hmdb_single_entry_wrong_id_tag.xml')
XML_WRONG_ENTRY_TAG <- file.path(RES_DIR, 'hmdb_single_entry_wrong_entry_tag.xml')
DST_DIR <- file.path(TEST_DIR, 'output', 'extract_dir')

test_extractXmlEntries <- function() {

    # Test bad XML file
    testthat::expect_error(extractXmlEntries("", ""))
    testthat::expect_error(extractXmlEntries("some_non_existing_file", ""))

    # Test bad output dir
    testthat::expect_error(extractXmlEntries(XML_SINGLE_ENTRY, "some_non_existing_dir"))

    # Test empty XML file
    unlink(DST_DIR, recursive=TRUE)
    dir.create(DST_DIR, recursive=TRUE)
    extractXmlEntries(XML_EMPTY, DST_DIR)
    testthat::expect_length(Sys.glob(file.path(DST_DIR, '*')), 0)

    # Test XML file with wrong entry tags inside
    unlink(DST_DIR, recursive=TRUE)
    dir.create(DST_DIR, recursive=TRUE)
    extractXmlEntries(XML_WRONG_ENTRY_TAG, DST_DIR)
    files <- Sys.glob(file.path(DST_DIR, '*'))
    testthat::expect_length(files, 0)

    # Test XML file with single entry, no ID
    testthat::expect_error(extractXmlEntries(XML_NO_ID, DST_DIR))

    # Test XML file with single entry, wrong ID tag
    testthat::expect_error(extractXmlEntries(XML_WRONG_ID_TAG, DST_DIR))

    # Test XML file with single entry
    unlink(DST_DIR, recursive = TRUE)
    dir.create(DST_DIR, recursive=TRUE)
    files <- extractXmlEntries(XML_SINGLE_ENTRY, DST_DIR)
    testthat::expect_length(files, 1)
    testthat::expect_equal(names(files), 'HMDB00001')
    found_files <- Sys.glob(file.path(DST_DIR, '*'))
    testthat::expect_identical(unname(files), found_files)
    testthat::expect_true(all(vapply(found_files, function(f) file.info(f)$size, FUN.VALUE=1) > 0))

    # Test XML file with single entry and header tags
    unlink(DST_DIR, recursive = TRUE)
    dir.create(DST_DIR, recursive=TRUE)
    files <- extractXmlEntries(XML_WITH_HEADER_TAGS, DST_DIR)
    testthat::expect_length(files, 1)
    testthat::expect_equal(names(files), 'HMDB00001')
    found_files <- Sys.glob(file.path(DST_DIR, '*'))
    testthat::expect_identical(unname(files), found_files)
    testthat::expect_true(all(vapply(found_files, function(f) file.info(f)$size, FUN.VALUE=1) > 0))

    # Test XML file with two entries
    unlink(DST_DIR, recursive = TRUE)
    dir.create(DST_DIR, recursive=TRUE)
    files <- extractXmlEntries(XML_TWO_ENTRIES, DST_DIR)
    testthat::expect_length(files, 2)
    testthat::expect_equal(names(files), c('HMDB00001', 'HMDB00143'))
    found_files <- Sys.glob(file.path(DST_DIR, '*'))
    testthat::expect_identical(unname(files), found_files)
    testthat::expect_true(all(vapply(found_files, function(f) file.info(f)$size, FUN.VALUE=1) > 0))
}

# Set test context
biodb::testContext("Tests of independent functions")

# Run tests
biodb::testThat("extractXmlEntries() works fine.", test_extractXmlEntries)
