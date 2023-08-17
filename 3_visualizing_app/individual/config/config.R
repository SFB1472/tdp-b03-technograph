
# CURRENT_SPHERE <- "German"

SELECT_MENU_SITE <- list()
SELECT_MENU_SITE$German <- c('augsburger-allgemeine', 'abendblatt', 'badische-zeitung', 'berliner-zeitung', 'bild', 'deutschlandfunk', 'faz', 'fr', 'freiepresse', 'freitag', 'ftd', 'handelsblatt', 'haz', 'hna', 'jungefreiheit', 'ksta', 'lvz', 'mads', 'mainpost', 'merkur', 'mopo', 'morgenpost', 'nwzonline', 'rheinpfalz', 'rp-online', 'stern', 'stuttgarter-nachrichten', 'stuttgarter-zeitung', 'sueddeutsche', 'swr', 'tagesspiegel', 'taz', 'tz', 'volksstimme', 'welt', 'wn', 'zdf', 'zeit')
SELECT_MENU_SITE$Dutch <- c('bd', 'bndestem', 'bnr', 'businessinsider', 'destentor', 'dutchnews', 'dvhn', 'ed', 'fd', 'geenstijl', 'gelderlander', 'groene', 'hpdetijd', 'jeugdjournaal', 'joop', 'metronieuws', 'ninefornews', 'nos', 'nrc', 'oneworld', 'parool', 'powned', 'pzc', 'sargasso', 'telegraaf', 'trouw', 'tubantia', 'vn', 'volkskrant', 'welingelichtekringen', 'ad', 'dagelijksestandaard', 'haarlemsdagblad', 'hartvannederland', 'lc', 'leidschdagblad', 'limburger', 'nhnieuws', 'nieuws', 'noordhollandsdagblad', 'nu', 'quotenet', 'rtlnieuws', 'tpo')
SELECT_MENU_SITE$World <- c('abc', 'bbc', 'cbsnews', 'channelnewsasia', 'chinadaily', 'cnbc', 'cnn', 'csmonitor', 'ctvnews', 'dailytelegraph', 'euronews', 'express', 'foxnews', 'globalnews', 'go', 'huffingtonpost', 'independent', 'indiatimes', 'latimes', 'mirror', 'news24', 'npr', 'nypost', 'nytimes', 'reuters', 'rt', 'scmp', 'sfgate', 'smh', 'sputniknews', 'thehindu', 'todayonline', 'usatoday', 'vox', 'washingtonpost', 'washingtontimes', 'wsj', 'aljazeera', 'buzzfeednews', 'cbc', 'nbcnews', 'rawstory', 'telegraph', 'thesun', 'time')


PATH_DATA_CONFIG <- "https://api.github.com/repos/SFB1472/ARCH_News_Comments/contents/config.yaml"

DATAPATH_RAW <- "/html-file-information.csv"
# DATAPATH_RAW_WORLD <- "https://jupyter.digitalmethodologies.center/user/b03/files/data/World/html-file-information.csv?token="
# DATAPATH_RAW_NL <- "https://jupyter.digitalmethodologies.center/user/b03/files/data/Dutch/html-file-information.csv?token="



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