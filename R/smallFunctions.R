#https://stackoverflow.com/questions/43368073/how-to-remove-common-parts-of-strings-in-a-character-vector-in-r
mf <- function(x) {
  xsplit = strsplit(x,split = '')
  xdfm <- as.data.frame(do.call(rbind, xsplit))
  res <- list()
  for (i in 1:ncol(xdfm)){
    if (!all(xdfm[,i] == xdfm[1,i])){
      res[[length(res)+1]] <- as.character(xdfm[,i])
    }
  }
  res <- as.data.frame(do.call(rbind,res))
  res <- apply(res,2,function(x) paste(x,collapse="_"))
  return(res)
}

addList <- function(element, arg) {
  if (!missingArg(symbol = arg)) {
    element[[deparse(substitute(arg))]] <- arg
    return(element)
  } else {
    return(element)
  }
}
