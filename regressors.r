RF <- function(tec, tran, test) {
  model = randomForest(tec, tran)
  as.numeric(predict(model, test))
}

SVR <- function(tec, tran, test) {
  model = svm(tec, tran, scale=TRUE, type="eps-regression", kernel="radial")
  as.numeric(predict(model, test))
}

DWNN <- function(tec, tran, test) {
  model = kknn(tec, tran, test, kernel="gaussian")
  model$fitted.values
}

RD <- function(tec, tran, test) {
  rep(sample(tran[,rename(tec)], 1), nrow(test))
}

DF <- function(tec, tran, test) {
  rep(mean(tran[,rename(tec)]), nrow(test))
}

rmse <- function(tec, test, pred) {
  true = test[,rename(tec)]
  sqrt(sum((true - pred)^2)/length(true))
}

evaluation <- function(tec, tran, test) {

  sapply(algorithms(), function(r) {
    pred = eval(call(r, tec, tran, test))
    rmse(tec, test, pred)
  })
}
