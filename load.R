projectId <- "syn2787333"

colsUsed <- c("study", "organism", "disease"#,
              # "tissueType", "cellType", "treatmentType",
              # "assay", "assayTarget",
              # "dataType", "dataSubType", 
              # "fileType"
              )

queryString <- sprintf("select id,%s from file where projectId=='%s'",
                       paste(colsUsed, collapse=","),
                       projectId)
dfData <- synQuery(queryString, 450)$collectAll()
colnames(dfData) <- gsub('file\\.', '', colnames(dfData))

# save(dfData, file="dfData.RData")
# load("dfData.RData")

dfData <- dfData %>%
  mutate(link=sprintf("https://www.synapse.org/#!Synapse:%s", id),
         synapseid=sprintf("<a href='%s' target='_blank'>%s</a>", link, id))

dfData <- dfData[, c("id", "synapseid", "link", colsUsed)]

dfOrig <- dlply(dfData %>% select(-synapseid, -link) %>% melt(id.vars="id"), .(variable), makeDFTable)

dfOrig <- llply(dfOrig, function(x) x %>% arrange(desc(n)))

# colsUsed <- setdiff(colnames(dfData), "id")


