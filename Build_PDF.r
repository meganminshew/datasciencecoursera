# Set working directory
setwd('C:\\Rstudio_working\\Regression')

# Load packages
require(knitr)
require(markdown)

# Create .md, .html, and .pdf files
knit("RM_CarMPG.Rmd")
markdownToHTML('RM_CarMPG.md', 'RM_CarMPG.html', options=c("use_xhml"))
system("pandoc -s RM_CarMPG.html -o RM_CarMPG.pdf")