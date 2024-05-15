## code to prepare `docTemplates` dataset goes here

file_path <- system.file('extdata/csv', 'docTemplates.csv', package = 'shinyReqPlanner')
docTemplates <- read.csv(file_path, encoding = 'UTF-8', stringsAsFactors = FALSE)

usethis::use_data(docTemplates, overwrite = TRUE)
