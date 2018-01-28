# packages ----------------------------------------------------------------

library(dplyr)
library(stringr)
library(stringi)
#library(tidyverse)
library(ggplot2)
library(scales)  # for percentage scales
library(ggradar)
library(scales)
library(tibble)
#install.packages("devtools")
library(devtools)
#devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
library(ggradar)
library(reshape2)


# functions ---------------------------------------------------------------

d2ms <- function(x) {
  sprintf("%02d:%02d", x %/%60, x %% 60)
}

#starter
is.starter <- function(txt) {stri_detect_regex(txt, 'Startet mit: #.*')}
is.starter.empty <- function(txt) {stri_detect_regex(txt, 'Startet mit:')}

#jumpball
is.jumpball <- function(txt) {stri_detect_regex(txt, '.hat den Sprungball')}
#possession
is.possession <- function(txt) {stri_detect_regex(txt, 'Ballbesitz für.')}
#exchange of player
is.newplayer <- function(txt) {stri_detect_regex(txt, '#.*.geht, #.*.kommt ins Spiel')}
#timeout
is.timeout <- function(txt) {stri_detect_regex(txt, 'Timeout')}

#layups
is.layup <- function(txt) {stri_detect_regex(txt, 'Korbleger')}
is.layup.m <- function(txt) {stri_detect_regex(txt, 'Korbleger durch.*.getroffen')}
is.layup.missed <- function(txt) {stri_detect_regex(txt, 'Korbleger durch.*.verworfen')}
is.layup.blocked <- function(txt) {stri_detect_regex(txt, 'Korbleger durch.*.geblockt')}

#2er
is.2PA <- function(txt) {stri_detect_regex(txt, '2er')}
is.2P.m <- function(txt) {stri_detect_regex(txt, '2er - Sprungwurf durch.*.getroffen')}
is.2P.missed <- function(txt) {stri_detect_regex(txt, '2er - Sprungwurf durch.*.verworfen')}
is.2P.blocked <- function(txt) {stri_detect_regex(txt, '2er - Sprungwurf durch.*.geblockt')}

#2er Tip In
is.2P.tipin <- function(txt) {stri_detect_regex(txt, 'Tip In')}
is.2P.tipin.m <- function(txt) {stri_detect_regex(txt, '2er - Tip In durch.*.getroffen')}
is.2P.tipin.missed <- function(txt) {stri_detect_regex(txt, '2er - Tip In durch.*.verworfen')}
is.2P.tipin.blocked <- function(txt) {stri_detect_regex(txt, '2er - Tip In durch.*.geblockt')}

#3er
is.3PA <- function(txt) {stri_detect_regex(txt, '3er')}
is.3P.m <- function(txt) {stri_detect_regex(txt, '3er durch.*.getroffen')}
is.3P.missed <- function(txt) {stri_detect_regex(txt, '3er durch.*.verworfen')}
is.3P.blocked <- function(txt) {stri_detect_regex(txt, '3er durch.*.geblockt')}

#freethrows
is.FT <- function(txt) {stri_detect_regex(txt, 'Freiwurf durch')}
is.FT.m <- function(txt) {stri_detect_regex(txt, 'Freiwurf durch.*.getroffen')}
is.FT.missed <- function(txt) {stri_detect_regex(txt, 'Freiwurf durch.*.verworfen')}
is.FT1.m <- function(txt) {stri_detect_regex(txt, '1. Freiwurf durch.*.getroffen')}
is.FT2.m <- function(txt) {stri_detect_regex(txt, '2. Freiwurf durch.*.getroffen')}
is.FT3.m <- function(txt) {stri_detect_regex(txt, '3. Freiwurf durch.*.getroffen')}
is.FT1.missed <- function(txt) {stri_detect_regex(txt, '1. Freiwurf durch.*.verworfen')}
is.FT2.missed <- function(txt) {stri_detect_regex(txt, '2. Freiwurf durch.*.verworfen')}
is.FT3.missed <- function(txt) {stri_detect_regex(txt, '3. Freiwurf durch.*.verworfen')}

#rebounds
is.orb <- function(txt) {stri_detect_regex(txt, 'Offensiver Rebound durch')}
is.drb <- function(txt) {stri_detect_regex(txt, 'Defensiver Rebound durch')}
is.orb.tm <- function(txt) {stri_detect_regex(txt, 'Offensiver Team-Rebound durch')}
is.drb.tm <- function(txt) {stri_detect_regex(txt, 'Defensiver Team-Rebound durch')}

#assists
is.assist <- function(txt) {stri_detect_regex(txt, 'Assist von.*.#')}

#turnover
is.turnover <- function(txt) {stri_detect_regex(txt, 'Turnover durch.*')}
is.turnover.ballhanding <- function(txt) {stri_detect_regex(txt, 'Turnover durch.*.#.*.- schlechte Ballbehandlung')}
is.turnover.pass <- function(txt) {stri_detect_regex(txt, 'Turnover durch.*.#.*.- schlechte Ballbehandlung')}
is.turnover.outofbounds <- function(txt) {stri_detect_regex(txt, 'Turnover durch.*.#.*.- Spieler außerhalb des Spielfeldes')}

#fouls
is.foul.made <- function(txt) {stri_detect_regex(txt, 'Foul durch.*.#')}
is.foul.draw <- function(txt) {stri_detect_regex(txt, 'Foul an.*.#')}
is.foul.2FT <- function(txt) {stri_detect_regex(txt, 'Foul durch.*.#.*.- 2 Freiwürfe')}
is.foul.1FT <- function(txt) {stri_detect_regex(txt, 'Foul durch.*.#.*.- 1 Freiwurf')}
is.foul.flagrant <- function(txt) {stri_detect_regex(txt, 'Unsportliches Foul durch.*.#')}
is.foul.technical <- function(txt) {stri_detect_regex(txt, 'Technisches Foul durch.*.#')}

#steals
is.steal <- function(txt) {stri_detect_regex(txt, 'Steal durch.*.#')}

#blocks
is.blocked <- function(txt) {stri_detect_regex(txt, 'wurde geblockt von')}

#dunking
is.dunk <- function(txt) {stri_detect_regex(txt, '2er - Dunk durch.*.#')}
is.alleyoopdunk <- function(txt) {stri_detect_regex(txt, '2er - Alley Oop Dunk durch.*.#')}

sep_playernumber <- function(playernumber) {
  string <- gsub("^.*?#","",playernumber) # Delete "#"
  number <- as.numeric(sapply(strsplit(string, " "), "[[", 1))
  player <- gsub("^.*? ","",playernumber) # get player name
  array = c(player, number)
  return(array)
}
#sep_playernumber("#23 Leon Sautier")


action2starter <- function(action) {
  action <- gsub("^.*?: ","",action) # Delete "Startet mit: "
  starter <- strsplit(action, ", ")
  starter <- unlist(starter)
  starter <- c(sep_playernumber(starter[1]),sep_playernumber(starter[2]),sep_playernumber(starter[3]),sep_playernumber(starter[4]),sep_playernumber(starter[5]))
  mat  <- matrix(unlist(starter), ncol=2, byrow=TRUE)
  df   <- as.data.frame(mat)
  names(df)[1]<-paste("player")
  names(df)[2]<-paste("number")
  starting5 <- df
  p1 <- starting5[1,]
  p2 <- starting5[2,]
  p3 <- starting5[3,]
  p4 <- starting5[4,]
  p5 <- starting5[5,]
  returnList <- list("starting5" = starting5, "p1" = p1, "p2" = p2, "p3" = p3, "p4" = p4, "p5" = p5)
}

name2init <- function(fullname){
  s <- strsplit(fullname, " ")
  sapply(s, function(x){
    toupper(paste(substring(x, 1, 1), collapse =""))
  })
}

#starting5 <- action2starter("Startet mit: #20 Leon Sautier, #23 Maurice Coach Tabandite, #17 Piet Mahler, #7 Sebastian Schaaf, #15 Marc Hall")
#starting5

### team DRB

calc_tm_DRB <- function(team,tbl){
  s_team <- subset(tbl, home_away == team, select = c(DRB))
  tmDRB <- sum(s_team$DRB)
  return(tmDRB)
}

get_tm_DRB2 <- function(home_away,home_DRB,away_DRB){
  if (home_away == "home"){
    tmDRB2 = home_DRB
  } else {
    tmDRB2 = away_DRB
  }
  return(tmDRB2)
}

### team ORB

calc_tm_ORB <- function(team,tbl){
  s_team <- subset(tbl, home_away == team, select = c(ORB))
  tmORB <- sum(s_team$ORB)
  return(tmORB)
}

get_tm_ORB2 <- function(home_away,home_ORB,away_ORB){
  if (home_away == "home"){
    tmORB2 = home_ORB
  } else {
    tmORB2 = away_ORB
  }
  return(tmORB2)
}


get_opp_DRB <- function(home_away,home_DRB,away_DRB){
  if (home_away == "home"){
    oppDRB = away_DRB
  } else {
    oppDRB = home_DRB
  }
  return(oppDRB)
}

get_opp_ORB <- function(home_away,home_ORB,away_ORB){
  if (home_away == "home"){
    oppORB = away_ORB
  } else {
    oppORB = home_ORB
  }
  return(oppORB)
}

calc_oppFGM <- function(game_FGM,tmFGM){
  oppFGM <- game_FGM - tmFGM
  return(oppFGM)
}

calc_oppFGA <- function(game_FGA,tmFGA){
  oppFGA <- game_FGA - tmFGA
  return(oppFGA)
}

calc_oppFTM <- function(game_FTM,tmFTM){
  oppFTM <- game_FTM - tmFTM
  return(oppFTM)
}

calc_opp3PM <- function(game_3PM,tm3PM){
  opp3PM <- game_3PM - tm3PM
  return(opp3PM)
}

calc_oppFTA <- function(game_FTA,tmFTA){
  oppFTA <- game_FTA - tmFTA
  return(oppFTA)
}

calc_oppTOV <- function(game_TOV,tmTOV){
  oppTOV <- game_TOV - tmTOV
  return(oppTOV)
}



#tbl.ORB <- aggregate(ORB ~ player_team, boxscore, sum)

## advanced stats

calc_ast2to <- function(AST,TOV){
  if (AST > 0) {
    ast_tov <- AST/TOV
    ast_tov = round(ast_tov, 3)
  } else {
    ast_tov = 0
  }
  return(ast_tov)
}

# Assist Ratio Formula=(Assists)*100)/ [(Field Goal Attempts)+(Free Throw Attempts*0.44)+(Assists)+(Turnovers)]

calc_ASTr <- function(AST,FGA,FTA,TOV){
  if (AST == 0) {
    ASTr = 0
  } else {
    ASTr <- (AST*100) / (FGA + FTA*0.44 + AST + TOV)
    ASTr <- round(ASTr, 3)
  }
  return(ASTr)
}

# 100 * (DRB * (Tm MP / 5)) / (MP * (Tm DRB + Opp ORB))

calc_DRBp <- function(DRB,tmMP,MP,tmDRB,oppORB){
  if (MP > 0){
    DRBp <- 100 * (DRB * (tmMP / 5)) / (MP * (tmDRB + oppORB))
    DRBp <- round(DRBp,3)
  } else {
    DRBp = ""
  }
  return(DRBp)
}

calc_ORBp <- function(ORB,tmMP,MP,tmORB,oppDRB){
  if (MP > 0){
    ORBp <- 100 * (ORB * (tmMP / 5)) / (MP * (tmORB + oppDRB))
    ORBp <- round(ORBp,3)
  } else {
    ORBp = ""
  }
  return(ORBp)
}

calc_POSS <- function(tmFGA,tmFTA,tmORB,tmDRB,oppDRB,tmFG,tmTOV,oppFGA,oppFTA,oppORB,oppFG,oppTOV){
  POSS <- 0.5 * ((tmFGA + 0.4 * tmFTA - 1.07 * (tmORB / (tmORB + oppDRB)) * (tmFGA - tmFG) + tmTOV) + (oppFGA + 0.4 * oppFTA - 1.07 * (oppORB / (oppORB + tmDRB)) * (oppFGA - oppFG) + oppTOV))
  return(round(POSS, 3))
}

calc_Pace <- function(tmPOSS,oppPOSS,tmMP){
  tm_PACE <- 40*((tmPOSS + oppPOSS)/(2*(tmMP/5)))
  return(round(tm_PACE, 3))
}

## 4 Factors

### Offense 4 factors

#Team Effective Field Goal Percentage
calc_tm_eFGp <- function(tmFGM,tm3PM,tmFGA){
  tm_eFGp <- (tmFGM + 0.5 * tm3PM)/tmFGA
  return(round(tm_eFGp,3))
}

#Turnover Rate=Turnovers/(Field Goal Attempts + 0.44*Free Throw Attempts + Turnovers)
calc_tm_TOVr <- function(tmFGA,tmFTA,tmTOV){
  tmTOVr <- tmTOV/(tmFGA + 0.44*tmFTA + tmTOV)
  return(round(tmTOVr, 3))
}

#Offensive Rebounding Percentage = (Offensive Rebounds)/[(Offensive Rebounds)+(Opponent’s Defensive Rebounds)]
calc_tm_ORBp <- function(tmORB,oppDRB){
  tmORBp <- tmORB/(tmORB+oppDRB)
  return(round(tmORBp, 3))
}


calc_tm_DRBp <- function(tmDRB,oppORB){
  tmDRBp <- tmDRB/(tmDRB+oppORB)
  return(round(tmDRBp, 3))
}

calc_opp_ORBp <- function(oppORB,tmDRB){
  oppORBp <- oppORB/(oppORB+tmDRB)
  return(round(oppORBp, 3))
}

calc_opp_DRBp <- function(oppDRB,tmORB){
  oppDRBp <- oppDRB/(oppDRB+tmORB)
  return(round(oppDRBp, 3))
}

#Free Throw Rate=(Free Throws Made)/(Field Goals Attempted) or Free Throws Attempted/Field Goals Attempted
calc_tm_FTr <- function(tmFTM,tmFGA){
  tmFTr <- tmFTM/(tmFGA)
  return(round(tmFTr, 3))
}

#FTM/Poss
calc_tm_FTMPOSSr <- function(tmFTM,tmPOSS){
  tmFTMPOSSr <- tmFTM/tmPOSS
  return(round(tmFTMPOSSr, 3))
}

calc_fourfactors.offense <- function(tm_eFGp,tmTOVr,tmORBp,tmFTr){
  fourfactors <- 0.4*tm_eFGp + 0.25*tmTOVr + 0.2*tmORBp + 0.15*tmFTr
  return(round(fourfactors, 3))
}

### defense 4 factors

#opp Effective Field Goal Percentage
calc_opp_eFGp <- function(oppFGM,opp3PM,oppFGA){
  opp_eFGp <- (oppFGM + 0.5 * opp3PM)/oppFGA
  return(round(opp_eFGp,3))
}

#Turnover Rate=Turnovers/(Field Goal Attempts + 0.44*Free Throw Attempts + Turnovers)
calc_opp_TOVr <- function(oppFGA,oppFTA,oppTOV){
  oppTOVr <- oppTOV/(oppFGA + 0.44*oppFTA + oppTOV)
  return(round(oppTOVr, 3))
}

#Offensive Rebounding Percentage = (Offensive Rebounds)/[(Offensive Rebounds)+(Opponent’s Defensive Rebounds)]
calc_tm_DRBp <- function(tmDRB,oppORB){
  tmDRBp <- tmDRB/(tmDRB+oppORB)
  return(round(tmDRBp, 3))
}

#Free Throw Rate=(Free Throws Made)/(Field Goals Attempted) or Free Throws Attempted/Field Goals Attempted
calc_opp_FTr <- function(oppFTM,oppFGA){
  oppFTr <- oppFTM/(oppFGA)
  return(round(oppFTr, 3))
}

calc_fourfactors.defense <- function(opp_eFGp,oppTOVr,tmDRBp,oppFTr){
  fourfactors <- 0.4*opp_eFGp + 0.25*oppTOVr + 0.2*tmDRBp + 0.15*oppFTr
  return(round(fourfactors, 3))
}
