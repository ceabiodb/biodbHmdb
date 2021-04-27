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
#' \dontrun{
#' e <- conn$getEntry('HMDB0000001')
#' }
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @export HmdbMetabolitesConn
#' @exportClass HmdbMetabolitesConn
HmdbMetabolitesConn <- methods::setRefClass("HmdbMetabolitesConn",
    contains=c("BiodbRemotedbConn", "BiodbCompounddbConn", 'BiodbDownloadable'),
    fields=list(
        .ns="character"
        ),

methods=list(

getNbEntries=function(count=FALSE) {
    # Overrides super class' method.

    n <- NA_integer_

    ids <- .self$getEntryIds()
    if ( ! is.null(ids))
        n <- length(ids)

    return(n)
},

correctIds=function(ids) {
    # Overrides super class' method.

    # Select IDs to correct
    idsToCorrect <- grep('^[Hh][Mm][Dd][Bb][0-9]+$', perl=TRUE, ids)

    # Extract ID numbers
    idsNb <- as.integer(sub('^[A-Za-z]+([0-9]+)$', '\\1', ids[idsToCorrect]))

    # Rewrite IDs
    ids[idsToCorrect] <- sprintf('HMDB%07d', idsNb)

    return(ids)
},

.doSearchForEntries=function(fields=NULL, max.results=0) {
    # Overrides super class' method.

    ids <- character()
    biodb::logDebug0("fields NAMES = ", paste(names(fields), collapse=", "))
    biodb::logDebug0("fields VALUES = ", paste(unname(fields), collapse=", "))
    biodb::logDebug0("fields VALUES = ", paste(unname(fields), collapse=", "))

    # Loop on all entries
    allIds <- .self$getEntryIds()
    biodb::logDebug0("ALL IDS = ", paste(allIds[1:10], collapse=", "))
    biodb::logDebug0("COUNT ALL IDS = ", length(allIds))
    prg <- biodb::Progress$new(biodb=.self$getBiodb(),
                               msg='Searching for entries.',
                               total=length(allIds))
    for (id in allIds) {

        # Get entry
        entry <- .self$getEntry(id)

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

getEntryPageUrl=function(id) {
    # Overrides super class' method.

    fct <- function(x) {
        u <- c(.self$getPropValSlot('urls', 'base.url'), 'metabolites', x)
        BiodbUrl$new(url=u)$toString()
    }

    return(vapply(id, fct, FUN.VALUE=''))
},

getEntryImageUrl=function(id) {
    # Overrides super class' method.

    fct <- function(x) {
        u <- c(.self$getPropValSlot('urls', 'base.url'), 'structures', x,
               'image.png')
        BiodbUrl$new(url=u)$toString()
    }

    return(vapply(id, fct, FUN.VALUE=''))
},

.doGetEntryContentRequest=function(id, concatenate=TRUE) {

    u <- c(.self$getPropValSlot('urls', 'base.url'), 'metabolites',
           paste(id, 'xml', sep='.'))
    url <- BiodbUrl$new(url=u)$toString()

    return(url)
},

.doDownload=function() {

    biodb::logInfo("Downloading HMDB metabolite database...")
    u <- c(.self$getPropValSlot('urls', 'base.url'), 'system', 'downloads',
           'current', 'hmdb_metabolites.zip')
    zip.url <- BiodbUrl$new(url=u)
    biodb::logInfo0("Downloading \"", zip.url$toString(), "\"...")
    sched <- .self$getBiodb()$getRequestScheduler()
    sched$downloadFile(url=zip.url, dest.file=.self$getDownloadPath())
},

.doExtractDownload=function() {

    biodb::logInfo0("Extracting content of downloaded',
                    ' HMDB metabolite database...")
    cch <- .self$getBiodb()$getPersistentCache()

    # Expand zip
    extract.dir <- cch$getTmpFolderPath()
    zip.path <- .self$getDownloadPath()
    biodb::logDebug0("Unzipping ", zip.path, "...")
    utils::unzip(zip.path, exdir=extract.dir)
    biodb::logDebug0("Unzipped ", zip.path, ".")

    # Search for extracted XML file
    files <- list.files(path=extract.dir)
    xml.file <- NULL
    if (length(files) == 0)
        biodb::error0("No XML file found in zip file \"",
                      .self$getDownloadPath(), "\".")
    else if (length(files) == 1)
        xml.file <- file.path(extract.dir, files)
    else {
        for (f in c('hmdb_metabolites.xml', 'hmdb_metabolites_tmp.xml'))
            if (f %in% files)
                xml.file <- file.path(extract.dir, f)
        if (is.null(xml.file))
            biodb::error0("More than one file found in zip file \"",
                        .self$getDownloadPath(), "\":",
                        paste(files, collapse=", "), ".")
    }
    if (is.null(xml.file))
        biodb::error("No XML file found in ZIP file.")
    biodb::logDebug0("Found XML file ", xml.file, " in ZIP file.")

    # Delete existing cache files
    biodb::logDebug('Delete existing entry files in cache system.')
    cch$deleteFiles(.self$getCacheId(),
                    ext=.self$getPropertyValue('entry.content.type'))

    # Extract entries
#    entryFiles <- .self$.extractEntriesFromXmlFile(xml.file, extract.dir)
    biodb::logDebug0('Extract entries from XML file "', xml.file,
                '", into "', extract.dir, '".')
    entryFiles <- extractXmlEntries(normalizePath(xml.file),
                                    normalizePath(extract.dir))

    # Move extracted files into cache
    ctype <- .self$getPropertyValue('entry.content.type')
    cch$moveFilesIntoCache(unname(entryFiles), cache.id=.self$getCacheId(), name=names(entryFiles),
                           ext=ctype)

    # Remove extracted XML database file
    biodb::logDebug('Delete extracted database.')
    unlink(xml.file)
},

.doGetEntryIds=function(max.results=NA_integer_) {

    ids <- NULL
    cch <- .self$getBiodb()$getPersistentCache()

    # Download
    .self$download()

    biodb::logDebug(".doGetEntryIds 10")
    if (.self$isDownloaded()) {

        biodb::logDebug(".doGetEntryIds 11")
        # Get IDs from cache
        ctype <- .self$getPropertyValue('entry.content.type')
        ids <- cch$listFiles(.self$getCacheId(),
                             ext=ctype, extract.name=TRUE)
        biodb::logDebug0("COUNT IDS = ", length(ids))

        # Filter out wrong IDs
        ids <- ids[grepl("^HMDB[0-9]+$", ids, perl=TRUE)]
    }

    return(ids)
}

))
