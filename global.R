library(shiny)
options(scipen = 4)

dataset.name <- "OptSum_SPX_635109439791671835"
file.dir <- paste0("DATA/", dataset.name, ".RData")
load(file.dir)
do.call("assign", args = list("options", get(dataset.name)))
options <- subset(options, !is.na(VOLATILITY))
