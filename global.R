options(repos = "https://cran.rstudio.com/")

libs <- c("shiny", "DT", "plyr", "dplyr", "reshape2")

available <- suppressWarnings(sapply(libs, require, character.only=TRUE))
inst.libs <- libs[available == FALSE]

if(length(inst.libs) != 0) {
  install.packages(inst.libs, dependencies = TRUE)
}

suppressWarnings(sapply(libs, require, character.only=TRUE))

synapseAvailable <- suppressWarnings(require("synapseClient", character.only=TRUE))

if (!synapseAvailable) {
  source('http://depot.sagebase.org/CRAN.R')
  pkgInstall(c("synapseClient"))
}

library(synapseClient)
synapseLogin()

source("lib.R")
source("load.R")
