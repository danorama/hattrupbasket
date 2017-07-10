#################################################
# hattrupbasket
# functions
#################################################

#' 2-Point Field Goal Percentage (2P%) -- calc2Ppct()
#'
#' Calculate 2-point-percentage (2P%) using the number of
#' sucessfull 2-pointer and total 2-point attempts
#' (2P/2PA)*100
#'
#' @param p2 total number of succesfull 2-pointers
#' @param p2a number of 2-point attempts
#' @param pct TRUE for percentage of 100, FALSE for fraction of 1 
#' @param digits number of digits
#' @return 2P% ( between 0 and 1 )
#' @export
pct2p <- function(p2,p2a,pct=TRUE,digits=1){
  # check arguments
  if (missing(p2) == TRUE) stop("missing argument: 'p2' (number of 2-pointers made)")
  if (missing(p2a) == TRUE) stop("missing argument: 'p2a' (number of 2-point attempts)")
  if (p2 < 0) stop("'p2' must be >= 0")
  if (p2a < 0) stop("'p2a' must be >= 0")
  if (p2 > p2a) stop("'p2' must be <= p2a")
  if (p2a == 0) warning("2-point attempts equals zero")
  # calc
  pct2p <- (p2/p2a)
  # format
  if(pct==TRUE){pct2p <- pct2p*100}
  pct2p <- format(pct2p, digit=digits)
  # return
  if (p2a == 0) {
    return(pct2p <- NA)
  } else {
    return(pct2p)  
  }
}

#' 3-Point Field Goal Percentage (3P%) -- calc3Ppct()
#'
#' Calculate 3-point-percentage (3P%) using the number of
#' sucessfull 3-pointer and total 3-point attempts
#' (3P/3PA)*100
#'
#' @param p3 total number of succesfull 2-pointers
#' @param p3a number of 2-point attempts
#' @param pct TRUE for percentage of 100, FALSE for fraction of 1 
#' @param digits number of digits
#' @return 3P% ( between 0 and 1 )
#' @export
pct3p <- function(p3,p3a,pct=TRUE,digits=1){
  # check arguments
  if (missing(p3) == TRUE) stop("missing argument: 'p3' (number of 3-pointers made)")
  if (missing(p3a) == TRUE) stop("missing argument: 'p3a' (number of 3-point attempts)")
  if (p3 < 0) stop("'p3' must be >= 0")
  if (p3a < 0) stop("'p3a' must be >= 0")
  if (p3 > p3a) stop("'p3' must be <= p3a")
  if (p3a == 0) warning("3-point attempts equals zero")
  # calc
  pct3p <- (p3/p3a)
  # format
  if(pct==TRUE){pct3p <- pct3p*100}
  pct3p <- format(pct3p, digit=digits)
  # return
  if (p3a == 0) {
    return(pct3p <- NA)
  } else {
    return(pct3p)  
  }
}

#' Field Goal Percentage (FG%) -- pctFG()
#'
#' Calculate field-goal-percentage (FGP%) using the number of 
#' sucessfull field goals and total field goal attempts
#' (FG/FGA)*100
#'
#' @param fg total number of field goals made (not points)
#' @param fga number of field goal attempts
#' @param pct TRUE for percentage of 100, FALSE for fraction of 1 
#' @return 3P%
#' @export
pctFG <- function(fg,fga,pct=TRUE,digits=1){
  # check arguments
  if (missing(fg) == TRUE) stop("missing argument: 'fg' (number of field-goals made)")
  if (missing(fga) == TRUE) stop("missing argument: 'fga' (number of field-goal-attempts)")
  if (fg < 0) stop("'fg' must be >= 0")
  if (fga < 0) stop("'fga' must be >= 0")
  if (fg > fga) stop("'fg' must be <= fga")
  if (fga == 0) warning("field-goal-attempts equals zero")
  # calc
  pctFG <- (fg/fga)
  # format
  if(perc==TRUE){pctFG <- pctFG*100}
  pctFG <- format(pctFG, digit=digits)
  # return
  if (fga == 0) {
    return(pctFG <- NA)
  } else {
    return(pctFG)  
  }
}

#' effective Field Goal Percentage (eFG%) -- eFGpct()
#'
#' Calculate effective/adjusted field-goal-percentage (FGP%) using the number of 
#' sucessfull field goals and total field goal attempts
#' ((FG+0.5*3P)/FGA)*100
#'




#' assist percentage (AST%) -- ASTpct
#'
#' Calculate assist percentage - Assist percentage is an 
#' estimate of the percentage of teammate field goals 
#' a player assisted while he was on the floor.
#' 
#' the formula is 
#' AST / (((MP / (Tm MP / 5)) * Tm FG) - FG)
#' 
#'
#' @param FG total number of field goals made (not points)
#' @param FGA number of field goal attempts
#' @param threeP number of 3-points made
#' @param pct TRUE for percentage of 100, FALSE for fraction of 1 
#' @return 3P%
#' @export
ASTpct <- function(AST,MP,TmMP,TmFG,FG,pct=TRUE,digits=1){
  if(pct==TRUE){
    ASTpct <- (AST/(((MP/(TmMP/5))*TmFG)-FG))*100
  }
  else {
    ASTpct <- (AST/(((MP/(TmMP/5))*TmFG)-FG))
  }
  ASTpct <- format(ASTpct, digit=digits)
  return(ASTpct)
}

