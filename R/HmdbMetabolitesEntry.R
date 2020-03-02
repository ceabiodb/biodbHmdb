#' HMDB Metabolites entry class.
#'
#' This is the entry class for the HMDB Metabolites database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
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
#' @export HmdbMetabolitesEntry
#' @exportClass HmdbMetabolitesEntry
HmdbMetabolitesEntry <- methods::setRefClass("HmdbMetabolitesEntry",
    contains="BiodbXmlEntry",

methods=list(

initialize=function(...) {

    callSuper(...)
},

.isParsedContentCorrect=function(parsed.content) {
    return(length(XML::getNodeSet(parsed.content, "//error")) == 0)
},

.parseFieldsStep2=function(parsed.content) {

    # Remove fields with empty string
    for (f in .self$getFieldNames()) {
        v <- .self$getFieldValue(f)
        if (is.character(v) && ! is.na(v) && v == '')
            .self$removeField(f)
    }

    # Correct InChIKey
    if (.self$hasField('INCHIKEY')) {
        v <- sub('^InChIKey=', '', .self$getFieldValue('INCHIKEY'), perl=TRUE)
        .self$setFieldValue('INCHIKEY', v)
    }

    # Synonyms
    synonyms <- XML::xpathSApply(parsed.content, "//synonym", XML::xmlValue)
    if (length(synonyms) > 0)
        .self$appendFieldValue('name', synonyms)
}

))
