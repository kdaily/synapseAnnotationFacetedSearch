makeDFTable <- function(df) {
  df %>%
    dplyr::select(-id, -variable) %>%
    dplyr::count(value) %>%
    dplyr::mutate(pct=sprintf("%s (%s)", n, round((n / sum(n)) * 100)))
  # dplyr::mutate(pct=round((n / sum(n)) * 100))
}

dfTableUpdate <- function(df, rows, myCol, newFilteredDF) {
  
  newDfOrig <- dlply(newFilteredDF %>% melt(id.vars="id"), .(variable), makeDFTable)
  
  myNewDfOrig <- left_join(dfOrig[[myCol]] %>% select(value),
                           newDfOrig[[myCol]], by="value")
  
  tmp <- myNewDfOrig %>% select(-pct)
  notSel <- which(!(tmp$value %in% newFilteredDF[, myCol]))
  tmp$n[notSel] <- 0
  
  tmp %>% 
    dplyr::mutate(pct=sprintf("%s (%s)", n, round((n / sum(n)) * 100)))
  # mutate(pct=round((n / sum(n)) * 100))
  
}
