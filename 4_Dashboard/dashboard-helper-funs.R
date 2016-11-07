simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

capVect <- function(vect) {
  v <- vector(length = length(vect))
  for(i in 1:length(vect)) {
    v[[i]] <- simpleCap(vect[[i]])
  }
  return(v)
}

