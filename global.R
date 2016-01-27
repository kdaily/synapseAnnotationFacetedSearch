library(shiny) 
library(DT)
library(plyr)
library(dplyr)
library(reshape2)

library(synapseClient)

synapseLogin()

source("lib.R")

# projectId <- "syn2787333"
# queryString <- sprintf("select id,study,group,platform,dataType,fileType from file where projectId=='%s'", projectId)
# dfData <- synQuery(queryString, 250)$collectAll()
# colnames(dfData) <- gsub('file\\.', '', colnames(dfData))
# save(dfData, file="dfData.RData")
load("dfData.RData")
dfData <- dfData %>%
  mutate(id=sprintf("<a href='https://www.synapse.org/#!Synapse:%s'>%s</a>", id, id)) %>% 
  select(id, everything())

dfOrig <- dlply(dfData %>% melt(id.vars="id"), .(variable), makeDFTable)

colsUsed <- setdiff(colnames(dfData), "id")


