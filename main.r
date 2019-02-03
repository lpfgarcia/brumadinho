

source("config.r")
source("regressors.r")
source("validation.r")

data = read.table("data/data.csv", sep=",", header=TRUE)

fold = cfold(data)

performance <- function(tran, test) {

  aux = lapply(label(), function(foo) {
    tec = formulae(foo, c("x", "y", "z"))
    evaluation(tec, tran, test)
  })

  aux = do.call("rbind", aux)
  return(aux)
}

oracle <- function(tran, test) {

  aux = lapply(folds(), function(i) {
    performance(tran[[i]], test[[i]])
  })

}

aux = oracle(fold$tran, fold$test)

rx = t(sapply(aux, "[", 1,))
ry = t(sapply(aux, "[", 2,))
rz = t(sapply(aux, "[", 3,))
rw = t(sapply(aux, "[", 4,))

result = rbind(rx,ry, rz, rw)
result = data.frame(result)
result$target = rep(label(), each=10)

default <- function() {

  par = element_text(size=14, colour="black")

  plot = theme_bw() + 
    theme(legend.position="top", 
      text=par, axis.text=par, strip.text=par, legend.text=par
    )

  return(plot)
}

boxplot <- function(result) {

  table = melt(result)
  table$group = c(rep("a", 120), rep("b", 80))

  pdf("boxplot.pdf", width=15, height=5)
    plot = ggplot(table, aes(x=variable, y=value, fill=group)) + 
      geom_boxplot(outlier.size=-1) + 
      facet_wrap(target ~ ., scales="free", ncol=4) + 
      default() + scale_fill_manual(values=c("white", "grey")) + 
      xlab("Regressores") + ylab("RMSE") + 
      guides(fill=FALSE)
    print(plot)
  dev.off()
}
