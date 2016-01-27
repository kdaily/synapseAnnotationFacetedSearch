
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output, session) {
  
  # Save state of selected rows to compare to identify changes
  selectedRows <- reactiveValues()
  
  # Initialize the selected rows to NULL
  lapply(colsUsed, function(x) selectedRows[[x]] <- NULL)
  
  updateFilterTables <- reactive({
    
    lapply(colsUsed,
           function(x) {
             input[[paste0(x, '_rows_selected')]]})
    
    myFilteredDF <- isolate(filterMyTable())
    
    myVals <- isolate(reactiveValuesToList(selectedRows))
    
    # Check which rows selections are the same
    sameSelectedRows <- lapply(colsUsed,
                               function(x) {
                                 dplyr::setequal(input[[paste0(x, '_rows_selected')]],
                                                 myVals[[x]])
                               })
    
    names(sameSelectedRows) <- colsUsed
    
    # print(sprintf("you clicked on %s", names(which(sameSelectedRows != TRUE))))
    
    
    res <- lapply(colsUsed,
                  function(x) {
                    rows <- input[[paste0(x, '_rows_selected')]]
                    
                    
                    # The selected rows didn't change, so this table wasn't clicked.
                    # Hence, it's percentages should be updated reflecting the current
                    # filtered data
                    # if (sameSelectedRows[[x]] == TRUE) {
                    #   # print(sprintf("Row %s was not clicked, so updating", x))
                    
                    # There are rows selected - any other rows should be zero
                    if (!is.null(rows)) {
                      dfTableUpdate(dfOrig[[x]], rows, x, myFilteredDF)
                    }
                    # There are no rows currently selected
                    else {
                      dfTableUpdate(dfOrig[[x]], 1:nrow(dfOrig[[x]]), x, myFilteredDF)
                    }
                    
                    # }
                    # else {
                    #   dfOrig[[x]]
                    # }
                  })
    names(res) <- colsUsed
    
    res
  })
  
  myDF <- reactive({
    
    tmp <- updateFilterTables()
    
    newDFs <- lapply(colsUsed,
                     function(x) {
                       foo <- tmp[[x]] %>% select(-n)
                       colnames(foo) <- NULL
                       DT::renderDataTable(foo, server = FALSE,
                                           rownames=FALSE,
                                           options = list(
                                             lengthChange = FALSE, dom='t',
                                             ordering=FALSE,
                                             autoWidth = TRUE,
                                             nowrap=FALSE
                                           ))
                     })
    
    names(newDFs) <- colsUsed
    
    newDFs
  })
  
  observe({
    
    
    tmp <- myDF()
    
    lapply(colsUsed,
           function(x) {
             rows <- input[[paste0(x, '_rows_selected')]]
             output[[x]] <- tmp[[x]]
             myProxy <- DT::dataTableProxy(x, session)
             DT::selectRows(myProxy, rows)
             selectedRows[[x]] <- rows
           })
    
  })
  
  # render the table containing shiny inputs 
  output$x1 <- shiny::renderUI({
    lapply(colsUsed,
           dataTableOutput)
  })
  
  filterMyTable <- reactive({
    
    res <- dfData
    
    for (x in colsUsed){
      rows <- input[[paste0(x, '_rows_selected')]]
      
      if (!is.null(rows)) {
        cats <- dfOrig[[x]][rows, "value"]
        
        i <- which(res[, x] %in% cats$value)
        res <- res[i, ]
      }
      
    }
    
    res
    
  })
  
  output$x2 <- DT::renderDataTable(filterMyTable(),
                                   rownames=FALSE,
                                   options = list(
                                     lengthChange = FALSE, dom='tp',
                                     ordering=FALSE,
                                     autoWidth = TRUE,
                                     nowrap=FALSE
                                   ))
  
})
