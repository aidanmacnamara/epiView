#' parses out the gene names from a deliminated string
#'
#' Newline, tab, colon and semicolon and comma delimited lists are supported
#'
#' @param genes an input string to be parsed
#'
#' @return a character vector with one element for every gene
#' @export
#'
#' @examples
#' parseGeneNames("a b c")
#' parseGeneNames("a, b, c")
#' # etc.

parseGeneNames=function(genes){
  genes=gsub("\\n"," ",genes)
  genes=gsub("\\t"," ",genes)
  genes=gsub(","," ",genes)
  genes=gsub(";"," ",genes)
  genes=gsub(":"," ",genes)
  genes=gsub("\\s+"," ",genes)
  strsplit(genes," ")[[1]]
}
