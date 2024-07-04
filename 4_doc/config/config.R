
DATAPATH_RAW <- "/html-file-information.csv"

## domains to look at: spreadsheet links
SPREADSHEET_PATH_GENERELL <- "https://docs.google.com/spreadsheets/d/1adwVWgBeZI70j0wHZsD6xho1jZTbJzXZ6RHtx86QJc8/"

SPREADSHEET_PATH_DOMAINS <- list()
SPREADSHEET_PATH_DOMAINS$World <- "Worldwide news"
SPREADSHEET_PATH_DOMAINS$Dutch <- "Dutch news"
SPREADSHEET_PATH_DOMAINS$German <-"German news"


## snippets to search for

SPREADSHEET_SNIPPETS_TO_SEARCH_FOR <- "788821906"

## annotation from research: currently for all graphics using annotation. The dot plot as well as the one for detailed info on a special domain
## with interview data

SPREADSHEET_ANNOTATION_DATA <- "https://docs.google.com/spreadsheets/d/1aEQAAh0UZlFImajCQ-E7cH-0nrlH8TGuxQt4PTbVCJE/"

SPREADSHEET_ANNOTATION <- list()
SPREADSHEET_ANNOTATION$World <- "international-domains"
SPREADSHEET_ANNOTATION$Dutch <- "nl-domains"
SPREADSHEET_ANNOTATION$German <- "de-domains"

PATH_TO_SHINY_DATA <- "shiny/data/"

library(re2)
SUBDOMAINS_TO_INKLUDE <- re2_regexp("news\\.sky|timesofindia.indiatimes")
SCRIPT_ENDINGS_TO_SEARCH_FOR <- re2_regexp("ajax(entry)?|\\.asp(x)?|\\.js(on|p|f)?|\\.php|\\.(f)?cgi|\\.do")

COMMENTS_IN_TAGS <- "comment|komment"
