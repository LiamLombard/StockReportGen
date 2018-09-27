library(rmarkdown)

renderDoc <- function()
{
  rmarkdown::render(fileNames[1])
}

lastUpdated <- function()
{
  latest <- lastUp
  for (file in fileNames) 
  {
    if(lastUp < file.mtime(file))
    {
      lastUp <- file.mtime(file)
    }
  }
  return(lastUp)
}


fileNames <- c("ReportGen.rmd", "processing.r", "settings.csv")
lastUp <- 0

lastUp <- lastUpdated()
latest <- lastUp
renderDoc()

while(1)
{
  lastUp <- lastUpdated()
  if(latest < lastUp)
  {
    latest <- lastUp
    renderDoc()
    Sys.sleep(5)
  }
  Sys.sleep(1)

}



