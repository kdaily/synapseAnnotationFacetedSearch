
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output, session) {
  
  # Save state of selected rows to compare to identify changes
  selectedRows <- reactiveValues()
  
  # Save state of filtered data
  filteredData <- reactiveValues()
  
  # Initialize the selected rows to NULL
  lapply(colsUsed, function(x) selectedRows[[x]] <- NULL)
  
  # Update data in each of the filtering tables based on currently selected
  # things
  updateFilterTables <- reactive({
    
    lapply(colsUsed,
           function(x) {
             input[[paste0(x, '_rows_selected')]]})
    
    prevFilteredData <- isolate(filteredData[['current']])
    myFilteredDF <- isolate(filterMyTable())
    
    myVals <- isolate(reactiveValuesToList(selectedRows))
    
    # Check which rows selections are the same
    sameSelectedRows <- lapply(colsUsed,
                               function(x) {
                                 dplyr::setequal(input[[paste0(x, '_rows_selected')]],
                                                 myVals[[x]])
                               })
    
    names(sameSelectedRows) <- colsUsed
    
    res <- lapply(colsUsed,
                  function(x) {
                    rows <- input[[paste0(x, '_rows_selected')]]

                    if (nrow(myFilteredDF) == 0) {
                      useTable <- prevFilteredData
                    }
                    else {
                      useTable <- myFilteredDF
                    }
                    
                    # There are rows selected - any other rows should be zero
                    if (!is.null(rows)) {
                      useRows <- rows
                    }
                    # There are no rows currently selected
                    else {
                      useRows <- 1:nrow(dfOrig[[x]])
                    }

                    dfTableUpdate(dfOrig[[x]], useRows, x, useTable)
                    # }                  
                  })
    
    names(res) <- colsUsed
    
    res
  })
  
  myDF <- reactive({
    
    tmp <- updateFilterTables()
    
    myFilteredDF <- isolate(filterMyTable())
    
    newDFs <- lapply(colsUsed,
                     function(x) {
                       foo <- tmp[[x]] %>% select(-n)
                       # colnames(foo) <- NULL
                       DT::renderDataTable(foo, server = FALSE,
                                           rownames=FALSE,
                                           options = list(
                                             lengthChange = FALSE, dom='t',
                                             ordering=FALSE,
                                             autoWidth = TRUE,
                                             nowrap=FALSE
                                           ),
                                           escape=1)
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
    lapply(colsUsed, function(x) {list(h4(x), dataTableOutput(x))})
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
    
    filteredData[['current']] <- res
    
    res
    
  })
  
  output$x2 <- DT::renderDataTable(filterMyTable(),
                                   rownames=FALSE,
                                   options = list(
                                     lengthChange = FALSE, 
                                     pageLength=25,
                                     dom='tp',
                                     ordering=FALSE,
                                     autoWidth = TRUE,
                                     nowrap=FALSE
                                   ),
                                   escape=0)
  
})
