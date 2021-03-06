#' assigns colours to text values using a matched list
#'
#' Compares a dictionary of search values against a list of elements
#' in a string vector to assign colour - or other other coding - to
#' each element. In particular, in turn, for each item, we cycle
#' through the dictionary (a list of words as a space deliminated string)
#' and return a colour taken from the name of the corresponding dictionary
#' entry if a match is detected. If multiple hits are detected, the first is
#' returned. For an illustration, see the examples.
#'
#' Typically, you might used to to assign named colours to labels on
#' the basis of a text match in their description, or to generate a
#' factor of arbitary levels (i.e. not explicit factor names) to be
#' used to assign colours (or other plotting attributes) automatically. Based upon
#' a function, colourLookup in a previous library.
#'
#' @param values the list of values to be coloured, e.g. labels on a
#'   plot
#' @param dict a list of vectors of single word values to be
#'   searched for in values. If the list is names, its names
#'   will be taken as the colours, otherwise a list of artifical
#'   names of the form colour.X will be used. If the list is omitted (length zero)
#'   a vector of the default.colour length length(values) is returned
#' @param ignore.case if true (default) case is ignored
#' @param default.colour ("black") the colour or value assigned to all non-matched values
#'
#' @return a vector of length values coloured by the name of the
#'   entry with the first hit in dict.
#' @export
#'
#' @examples
#'# using a named dictionary
#'
#'legendColourLookup(c("a big dog","lots of fish","a small cat","a cat-dog"),
#'             list(red="dog",
#'                  blue="cat"))
#'
#'# or
#'
#'legendColourLookup(c("a big Dog","lots of fish","a small cat","a cat-dog"),
#'              list(red="dog",
#'                   blue="cat"),
#'              default.colour = "none",
#'              ignore.case=F)
#'
#'# and without names
#'
#'legendColourLookup(c("a big dog","a small cat","a cat-dog"),
#'             list("dog",
#'                  "cat"))


legendColourLookup=function(values=NULL,dict,ignore.case=T,default.colour="black"){

  if (length(dict)>0){

    if (is.null(names(dict))){
      names(dict)=paste("colour",1:length(dict),sep=".")
    }

    colours=rep(default.colour,length(values))

    for (dictItemName in rev(names(dict))){
      dictItem=dict[dictItemName]
      dictItem=gsub(" ","",dictItem) # remove multiple blanks
      for (dictItemWord in strsplit(dictItem," ")){
        # dictItemName is a colour
        colours[grepl(dictItemWord,values,ignore.case = ignore.case)]=dictItemName
      }

    }
    colours
  } else {
    rep(default.colour,length(values))
  }


}
