library(rmarkdown)

renderDoc <- function()
{
  rmarkdown::render(fileName)
}

fileName <- "ReportGen.rmd"

lastUpdated <- file.mtime(fileName)
renderDoc()

while(1)
{
  if(lastUpdated != file.mtime(fileName))
  {
    renderDoc()
    lastUpdated <- file.mtime(fileName)
  }
  Sys.sleep(5)
}

