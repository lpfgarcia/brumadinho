formulae <- function(header, tail) {
  aux = formula(paste(header, "~", paste(tail, collapse="+")))
  return(aux)
}

rename <- function(tech) {
  strsplit(as.character(tech), " ~ ")[[2]]
}

cfold <- function(data) {

  id = rep(folds(), length.out=nrow(data))

  tran = lapply(folds(), function(i) {
    subset(data, id %in% setdiff(folds(), i))
  })

  test = lapply(folds(), function(i) {
    subset(data, id %in% i)
  })

  tmp = list()
  tmp$data = data
  tmp$tran = tran
  tmp$test = test
  return(tmp)
}