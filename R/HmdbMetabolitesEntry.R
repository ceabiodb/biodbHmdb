#' HMDB Metabolites entry class.
#'
#' This is the entry class for the HMDB Metabolites database.
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
#' @importFrom R6 R6Class
#' @export
HmdbMetabolitesEntry <- R6::R6Class("HmdbMetabolitesEntry",
inherit=biodb::BiodbXmlEntry,

public=list(

#' @description
#' New instance initializer. Connector classes must not be instantiated
#' directly. Instead, you must use the createConn() method of the factory class.
#' @param ... All parameters are passed to the super class initializer.
#' @return Nothing.
initialize=function(...) {
    super$initialize(...)
}
),

private=list(

doCheckParsedContent=function(parsed.content) {
    return(length(XML::getNodeSet(parsed.content, "//error")) == 0)
}

,doParseFieldsStep2=function(parsed.content) {

    # Remove fields with empty string
    for (f in self$getFieldNames()) {
        v <- self$getFieldValue(f)
        if (is.character(v) && ! is.na(v) && v == '')
            self$removeField(f)
    }

    # Correct InChIKey
    if (self$hasField('INCHIKEY')) {
        v <- sub('^InChIKey=', '', self$getFieldValue('INCHIKEY'), perl=TRUE)
        self$setFieldValue('INCHIKEY', v)
    }

    # Synonyms
    synonyms <- XML::xpathSApply(parsed.content, "//synonym", XML::xmlValue)
    if (length(synonyms) > 0)
        self$appendFieldValue('name', synonyms)

    return(invisible(NULL))
}
))
