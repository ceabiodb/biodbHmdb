#' The connector class for the HMDB Metabolites database.
#'
#' This is a concrete connector class. It must never be instantiated directly,
#' but instead be instantiated through the factory \code{\link{BiodbFactory}}.
#' Only specific methods are described here. See super classes for the
#' description of inherited methods.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('hmdb.metabolites')
#'
#' # Get an entry
#' \donttest{ # Getting one entry requires the download of the whole database.
#' e <- conn$getEntry('HMDB0000001')
#' }
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import R6
#' @export
HmdbMetabolitesConn <- R6::R6Class("HmdbMetabolitesConn",
inherit=biodb::BiodbConn,

public=list(
),

private=list(
    ns=NULL

,doCorrectIds=function(ids) {

    # Select IDs to correct
    idsToCorrect <- grep('^[Hh][Mm][Dd][Bb][0-9]+$', perl=TRUE, ids)

    # Extract ID numbers
    idsNb <- as.integer(sub('^[A-Za-z]+([0-9]+)$', '\\1', ids[idsToCorrect]))

    # Rewrite IDs
    ids[idsToCorrect] <- sprintf('HMDB%07d', idsNb)

    return(ids)
}

,doGetNbEntries=function(count=FALSE) {

    n <- NA_integer_

    ids <- self$getEntryIds()
    if ( ! is.null(ids))
        n <- length(ids)

    return(n)
}

,doGetEntryPageUrl=function(id) {

    fct <- function(x) {
        u <- c(self$getPropValSlot('urls', 'base.url'), 'metabolites', x)
        BiodbUrl$new(url=u)$toString()
    }

    return(vapply(id, fct, FUN.VALUE=''))
}

,doGetEntryImageUrl=function(id) {

    fct <- function(x) {
        u <- c(self$getPropValSlot('urls', 'base.url'), 'structures', x,
            'image.png')
        BiodbUrl$new(url=u)$toString()
    }

    return(vapply(id, fct, FUN.VALUE=''))
}

,doSearchForEntries=function(fields=NULL, max.results=0) {
    # Overrides super class' method.

    ids <- character()
    biodb::logDebug0("fields NAMES = ", paste(names(fields), collapse=", "))
    biodb::logDebug0("fields VALUES = ", paste(unname(fields), collapse=", "))
    biodb::logDebug0("fields VALUES = ", paste(unname(fields), collapse=", "))

    # Loop on all entries
    allIds <- self$getEntryIds()
    biodb::logDebug("All IDs = %s", biodb::lst2str(allIds))
    biodb::logDebug("Total number of IDs = %d", length(allIds))
    prg <- biodb::Progress$new(biodb=self$getBiodb(),
        msg='Searching for entries.', total=length(allIds))
    for (id in allIds) {

        # Get entry
        entry <- self$getEntry(id)

        # Try to match entry
        tryMatch <- function(f) {
            m <- entry$hasField(f)
            if (m) {
                fct <- function(x) {
                    n <- grep(tolower(x), tolower(entry$getFieldValue(f)),
                        fixed=TRUE)
                    return(length(n) > 0)
                }
                m <- all(vapply(fields[[f]], fct, FUN.VALUE=TRUE))
            }
            return(m)
        }
        matched <- all(vapply(names(fields), tryMatch, FUN.VALUE=TRUE))
        biodb::logDebug0("MATCHED = ", matched)
        if (matched)
            ids <- c(ids, id)
        biodb::logDebug0("COUNT IDS = ", length(ids))

        # Enough results?
        if (max.results > 0 && length(ids) >= max.results)
            break

        # Send progress message
        prg$increment()
    }

    return(ids)
},

doGetEntryContentRequest=function(id, concatenate=TRUE) {

    u <- c(self$getPropValSlot('urls', 'base.url'), 'metabolites',
        paste(id, 'xml', sep='.'))
    url <- BiodbUrl$new(url=u)$toString()

    return(url)
},

doDownload=function() {

    u <- self$getPropValSlot('urls', 'db.zip.url')
    biodb::logInfo('Downloading HMDB metabolite database at "%s" ...', u)
    cch <- self$getBiodb()$getPersistentCache()
    
    # Real URL
    if (grepl('^([a-zA-Z]+://)', u)) {
        ext <- self$getPropertyValue('dwnld.ext')
        tmpFile <- tempfile("hmdb.metabolites", tmpdir=cch$getTmpFolderPath(),
            fileext=ext)
        zip.url <- BiodbUrl$new(url=u)
        sched <- self$getBiodb()$getRequestScheduler()
        sched$downloadFile(url=zip.url, dest.file=tmpFile)
        self$setDownloadedFile(tmpFile, action='move')
        
    # Path to local file
    } else {
        if ( ! file.exists(u))
            biodb::error("Source file %s does not exist.", u)
        self$setDownloadedFile(u, action='copy')
    }
}

,findXmlDatabaseFile=function(extract.dir, zip.path) {

    xml.file <- NULL

    files <- list.files(path=extract.dir)
    biodb::logDebug("Found files %s into %s.", lst2str(files), zip.path)
    if (length(files) == 0)
        biodb::error0("No XML file found in zip file \"",
            self$getDownloadPath(), "\".")
    else if (length(files) == 1)
        xml.file <- file.path(extract.dir, files)
    else {
        for (f in c('hmdb_metabolites.xml', 'hmdb_metabolites_tmp.xml'))
            if (f %in% files)
                xml.file <- file.path(extract.dir, f)
        if (is.null(xml.file))
            biodb::error0("More than one file found in zip file \"",
                        self$getDownloadPath(), "\":",
                        paste(files, collapse=", "), ".")
    }
    if (is.null(xml.file))
        biodb::error("No XML file found in ZIP file.")
    biodb::logDebug0("Found XML file ", xml.file, " in ZIP file.")

    return(xml.file)
}

,doExtractDownload=function() {

    biodb::logInfo0("Extracting content of downloaded',
                    ' HMDB metabolite database...")
    cch <- self$getBiodb()$getPersistentCache()

    # Expand zip
    extract.dir <- cch$getTmpFolderPath()
    zip.path <- self$getDownloadPath()
    biodb::logDebug("Unzipping %s into %s...", zip.path, extract.dir)
    utils::unzip(zip.path, exdir=extract.dir)

    # Search for extracted XML file
    xml.file <- private$findXmlDatabaseFile(extract.dir=extract.dir,
        zip.path=zip.path)

    # Delete existing cache files
    biodb::logDebug('Delete existing entry files in cache system.')
    cch$deleteFiles(self$getCacheId(),
                    ext=self$getPropertyValue('entry.content.type'))

    # Extract entries
#    entryFiles <- private$extractEntriesFromXmlFile(xml.file, extract.dir)
    biodb::logDebug0('Extract entries from XML file "', xml.file,
                '", into "', extract.dir, '".')
    entryFiles <- extractXmlEntries(normalizePath(xml.file),
                                    normalizePath(extract.dir))

    # Move extracted files into cache
    ctype <- self$getPropertyValue('entry.content.type')
    cch$moveFilesIntoCache(unname(entryFiles), cache.id=self$getCacheId(),
        name=names(entryFiles), ext=ctype)

    # Remove extracted XML database file
    biodb::logDebug('Delete extracted database.')
    unlink(xml.file)
},

doGetEntryIds=function(max.results=NA_integer_) {

    ids <- NULL
    cch <- self$getBiodb()$getPersistentCache()

    # Download
    self$download()

    biodb::logDebug(".doGetEntryIds 10")
    if (self$isDownloaded()) {

        biodb::logDebug(".doGetEntryIds 11")
        # Get IDs from cache
        ctype <- self$getPropertyValue('entry.content.type')
        ids <- cch$listFiles(self$getCacheId(), ext=ctype, extract.name=TRUE)
        biodb::logDebug0("COUNT IDS = ", length(ids))

        # Filter out wrong IDs
        ids <- ids[grepl("^HMDB[0-9]+$", ids, perl=TRUE)]
    }

    return(ids)
}
))
