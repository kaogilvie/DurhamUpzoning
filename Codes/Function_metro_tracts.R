#https://walker-data.com/2017/05/tigris-metros/

metro_tracts <- function(metro_name) {
  
  # First, identify which states intersect the metro area using the
  # `states` function in tigris
  st <- states(cb = TRUE)
  cb <- core_based_statistical_areas(cb = TRUE)
  metro <- filter(cb, grepl(metro_name, NAME))
  
  stcodes <- st[metro,]$STATEFP
  
  # Then, fetch the tracts, using rbind_tigris if there is more
  # than one state
  if (length(stcodes) > 1) {
    tr <- rbind_tigris(
      map(stcodes, function(x) {
        tracts(x, cb = TRUE)
      })
    )
  } else {
    tr <- tracts(x, cb = TRUE)
  }
  
  # Now, find out which tracts are within the metro area
  within <- st_within(tr, metro)
  
  within_lgl <- map_lgl(within, function(x) {
    if (length(x) == 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  # Finally, subset and return the output
  output <- tr[within_lgl,]
  
  return(output)
  
}