library(shiny) 
library(DT)
library(plyr)
library(dplyr)
library(reshape2)

library(synapseClient)

synapseLogin()

# projectId <- "syn2787333"
# queryString <- sprintf("select id,study,group,platform,dataType,fileType from file where projectId=='%s'", projectId)
# dfData <- synQuery(queryString, 250)$collectAll()
# colnames(dfData) <- gsub('file\\.', '', colnames(dfData))
# save(dfData, file="dfData.RData")

load("dfData.RData")

colsUsed <- setdiff(colnames(dfData), "id")

makeDFTable <- function(df) {
  df %>%
    dplyr::select(-id, -variable) %>%
    dplyr::count(value) %>%
    dplyr::mutate(pct=sprintf("%s (%s)", n, round((n / sum(n)) * 100)))
    # dplyr::mutate(pct=round((n / sum(n)) * 100))
}

makeDFTable2 <- function(df) {
  df %>%
    dplyr::select(-id, -variable) %>%
    dplyr::count(value) %>%
    dplyr::mutate(pct=sprintf("%s (%s)", n, round((n / sum(n)) * 100)))
    # dplyr::mutate(pct=round((n / sum(n)) * 100))
}

dfOrig <- dlply(dfData %>% melt(id.vars="id"), .(variable), makeDFTable)

dfTableUpdate <- function(df, rows, myCol, newFilteredDF) {
  
  newDfOrig <- dlply(newFilteredDF %>% melt(id.vars="id"), .(variable), makeDFTable)
  
  myNewDfOrig <- left_join(dfOrig[[myCol]] %>% select(value),
                           newDfOrig[[myCol]])
  
  tmp <- myNewDfOrig %>% select(-pct)
  notSel <- which(!(tmp$value %in% newFilteredDF[, myCol]))
  tmp$n[notSel] <- 0
  
  tmp %>% 
    dplyr::mutate(pct=sprintf("%s (%s)", n, round((n / sum(n)) * 100)))
    # mutate(pct=round((n / sum(n)) * 100))
  
}
