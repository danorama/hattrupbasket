#' 2-Point Field Goal Percentage (2P%) 
#'
#' Calculate 2-point-percentage (2P%) using the number of
#' sucessfull 2-pointer and total 2-point attempts
#' (2P/2PA)*100
#'
#' @param twoP total number of succesfull 2-pointers
#' @param twoPA number of 2-point attempts
#' @param pct TRUE for percentage of 100, FALSE for fraction of 1 
#' @return 2P% ( between 0 and 1 )
#' @export
calc_twoPpct <- function(twoP,twoPA,pct=TRUE){
  if(perc==TRUE){
    twoPpct <- (twoP/twoPA)*100
  }
  else {
    twoPpct <- (twoP/twoPA)
  }
  twoPpct
}