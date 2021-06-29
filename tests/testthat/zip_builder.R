# Create output directory
outdir <- file.path(getwd(), 'output')
if ( ! dir.exists(outdir))
    dir.create(outdir)

# Build HMDB zip
ext_data <- system.file("extdata", package="biodbHmdb")
test_ref <- system.file("testref", package="biodbHmdb")
two_entries_zip_file <- file.path(ext_data,'generated', "hmdb_extract.zip")
if ( ! file.exists(two_entries_zip_file)) {
    folder <- dirname(two_entries_zip_file)
    if ( ! dir.exists(folder))
        dir.create(folder, recursive=TRUE)
    xmlfile <- file.path(test_ref, 'hmdb_two_full_entries.xml')
    testthat::expect_true(file.exists(xmlfile))
    utils::zip(two_entries_zip_file, xmlfile, flags='-jq')
}
