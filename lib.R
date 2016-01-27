barTxt <-'<div id="progress" class="graph"><div id="bar" style="width:%s%%"><p><span style="float: left; text-align: left">%s</span><span style="float: right; text-align: right">%s</span></p></div></div>'

makeDFTable <- function(df) {
  df %>%
    dplyr::select(-id, -variable) %>%
    dplyr::count(value) %>%
    dplyr::mutate(count=sprintf(barTxt, 100 - round((n / sum(n)) * 100), value, n))
    # dplyr::mutate(pct=sprintf("%s (%s)", n, round((n / sum(n)) * 100)))
  # dplyr::mutate(pct=round((n / sum(n)) * 100))
}

dfTableUpdate <- function(df, rows, myCol, newFilteredDF) {
  
  newDfOrig <- dlply(newFilteredDF %>% melt(id.vars="id"), .(variable), makeDFTable)
  
  myNewDfOrig <- left_join(df %>% select(value),
                           newDfOrig[[myCol]], by="value")
  
  tmp <- myNewDfOrig %>% select(-count)
  notSel <- which(!(tmp$value %in% newFilteredDF[, myCol]))
  tmp$n[notSel] <- 0
  
  tmp %>% 
    dplyr::mutate(count=sprintf(barTxt, 100 - round((n / sum(n)) * 100), value, n))
  # mutate(pct=round((n / sum(n)) * 100))
  
}
