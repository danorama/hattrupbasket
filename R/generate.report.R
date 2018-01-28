setwd("~/Projects/danorama/hamburgtowers/data/liveticker2csv/")

#install.packages('knitrBootstrap')
library(knitrBootstrap)
library(rmarkdown)

generate.gamereport <- function(rmd,gameID,folder){
  file_title = paste(folder,gameID,".html", sep='')
  render(rmd, bootstrap_document(
    #highlight='Github',
    theme='Simplex',
    theme.chooser=T,
    highlight.chooser=T,
    menu=T),
    output_file = file_title,
    params = list(
      dynamictitle=paste("Follow Up Analytics","Game",gameID),
      reportdate=Sys.Date(),
      lastgame=gameID
    ))
  return(file_title)
}

generate.gamereport("lastgame.Rmd",103600,"reports/")
generate.gamereport("lastgame.Rmd",102299,"reports/")
