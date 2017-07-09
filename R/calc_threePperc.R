#' 3-Point Field Goal Percentage (3P%) 
#'
#' Calculate 3-point-percentage (3P%) using the number of 
#' sucessfull 3-pointers and total 3-point attempts
#' (3P/3PA)*100
#'
#' @param threeP total number of succesfull 2-pointers
#' @param threePA number of 2-point attempts
#' @param pct TRUE for percentage of 100, FALSE for fraction of 1 
#' @return 3P%
#' @export
calc_threePpct <- function(threeP,threePA,pct=TRUE){
  if(pct==TRUE){
    threePpct <- (threeP/threePA)*100
  }
  else {
    threePpct <- (threeP/threePA)
  }
  threePpct
}