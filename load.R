projectId <- "syn2787333"
colsUsed <- c("study", "group", "assay", "assayTarget",
              "organism", "disease",
              "tissueType", "cellType", "treatmentType", 
              "dataType", "dataSubType", "fileType")

queryString <- sprintf("select id,%s from file where projectId=='%s'",
                       paste(colsUsed, collapse=","),
                       projectId)
dfData <- synQuery(queryString, 450)$collectAll()
colnames(dfData) <- gsub('file\\.', '', colnames(dfData))

# save(dfData, file="dfData.RData")
# load("dfData.RData")

dfData <- dfData %>%
  mutate(synapseid=sprintf("<a href='https://www.synapse.org/#!Synapse:%s' target='_blank'>%s</a>", id, id))

dfData <- dfData[, c("id", "synapseid", colsUsed)]

dfOrig <- dlply(dfData %>% melt(id.vars="id"), .(variable), makeDFTable)

dfOrig <- llply(dfOrig, function(x) x %>% arrange(desc(n)))

# colsUsed <- setdiff(colnames(dfData), "id")


