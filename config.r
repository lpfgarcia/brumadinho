
require(e1071)
require(ggplot2)
require(kknn)
require(randomForest)
require(reshape2)

algorithms <- function() {
  c("RF", "SVR","DWNN", "RD", "DF")
}

folds <- function() {
  1:10
}

label <- function() {
  c("rx", "ry", "rz", "rw")
}
