library(shiny) 
library(DT)
library(plyr)
library(dplyr)
library(reshape2)

library(synapseClient)

synapseLogin()

source("lib.R")

projectId <- "syn2787333"
colsUsed <- c("study","group","platform","dataType","fileType")

# queryString <- sprintf("select id,%s from file where projectId=='%s'", 
#                        paste(colsToUse, collapse=","),
#                        projectId)
# dfData <- synQuery(queryString, 250)$collectAll()
# colnames(dfData) <- gsub('file\\.', '', colnames(dfData))
# save(dfData, file="dfData.RData")
load("dfData.RData")
dfData <- dfData %>%
  mutate(id=sprintf("<a href='https://www.synapse.org/#!Synapse:%s'>%s</a>", id, id))

dfData <- dfData[, c("id", colsUsed)]

dfOrig <- dlply(dfData %>% melt(id.vars="id"), .(variable), makeDFTable)

dfOrig <- llply(dfOrig, function(x) x %>% arrange(desc(n)))

# colsUsed <- setdiff(colnames(dfData), "id")


