#MICROSTRATEGY_BEGIN
#
#RVAR prod -input -string -vector
#
#RVAR rules -output -string -vector  #Metric Expression: RScript<_RScriptFile="C:\RDir\RAsocjacje.R", _InputNames="prod">(prod)
#if(exists("mstr.WorkingDir")) setwd(mstr.WorkingDir)  #Working Directory if executed by MicroStrategy
#
#MICROSTRATEGY_END
setwd("C://RDir")

library(knitr)
library(markdown)
#tryCatch for Exception Handling
mstr.ErrMsg <- tryCatch({
#knit2html('AsocjacjeHTML.Rmd', output ="C://inetpub//wwwroot//AsocjacjeReport.html", text = NULL, quiet = FALSE, envir = new.env(),  encoding = getOption("encoding"))
knit2html('AsocjacjeHTML.Rmd', output ="AsocjacjeReport.html",   envir = globalenv(), options=c('base64_images'), encoding='CP1250')
#knit('AsocjacjeHTML.Rmd', output ='C://inetpub//wwwroot//AsocjacjeReport.md', encoding='CP1250')
#markdownToHTML('C://inetpub//wwwroot//AsocjacjeReport.md', output ='C://inetpub//wwwroot//AsocjacjeReport.html')

rules<-c("Zakoñczono generacjê regu³ asocjacyjnych")
try(print("Success!"))
#If we made it here, no errors were caught
mstr.ErrMsg <- ""
#Catch block to report an error
}, error = function(err) {
  #Print error message if run from console
  try(print(err))
  #Return error message
  return(err$message)
})