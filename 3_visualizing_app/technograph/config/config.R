
# CURRENT_SPHERE <- "Dutch"

SELECT_MENU_SITE <- list()
SELECT_MENU_SITE$German <- c('abendblatt', 'augsburger-allgemeine', 'badische-zeitung', 'berliner-zeitung', 'bild', 'deutschlandfunk', 'faz', 'focus', 'fr', 'freiepresse', 'freitag', 'ftd', 'handelsblatt', 'haz', 'hna', 'jungefreiheit', 'ksta', 'lvz', 'mads', 'mainpost', 'merkur', 'mopo', 'morgenpost', 'nwzonline', 'rheinpfalz', 'rp-online', 'spiegel', 'stern', 'stuttgarter-nachrichten', 'stuttgarter-zeitung', 'sueddeutsche', 'swr', 'tagesspiegel', 'taz', 'tz', 'volksstimme', 'welt', 'wn', 'zdf', 'zeit')
SELECT_MENU_SITE$Dutch <- c('bd', 'bndestem', 'bnr', 'businessinsider', 'dagelijksestandaard', 'destentor', 'dutchnews', 'dvhn', 'ed', 'fd', 'geenstijl', 'gelderlander', 'groene', 'haarlemsdagblad', 'hpdetijd', 'jeugdjournaal', 'joop', 'leidschdagblad', 'limburger', 'metronieuws', 'ninefornews', 'noordhollandsdagblad', 'nos', 'nrc', 'oneworld', 'parool', 'powned', 'pzc', 'quotenet', 'sargasso', 'telegraaf', 'trouw', 'tubantia', 'vn', 'volkskrant', 'welingelichtekringen', '1limburg', 'ad', 'hartvannederland', 'lc', 'nhnieuws', 'nieuws', 'nu', 'rtlnieuws', 'tpo')
SELECT_MENU_SITE$World <- c('abc', 'bbc', 'cbc', 'cbsnews', 'channelnewsasia', 'chinadaily', 'cnbc', 'cnn', 'csmonitor', 'ctvnews', 'dailymail', 'dailytelegraph', 'euronews', 'express', 'foxnews', 'globalnews', 'go', 'huffingtonpost', 'independent', 'indiatimes', 'latimes', 'mirror', 'nbcnews', 'news24', 'npr', 'nypost', 'nytimes', 'rawstory', 'reuters', 'rt', 'scmp', 'sfgate', 'smh', 'sputniknews', 'telegraph', 'thehindu', 'todayonline', 'usatoday', 'vox', 'washingtonpost', 'washingtontimes', 'wsj', 'aljazeera', 'buzzfeednews', 'thesun', 'time')


load(file = "data/systems/df_snippets_per_month_domain.RData")
# load(file = "data/df_heatmaps_availability.RData")
load(file = "data/systems/df_system_lifetime.RData")

## https://stackoverflow.com/questions/62898726/how-to-refresh-rdata-objects-in-shiny-app

df_systems_color_legend <- read_csv(file = "data/helper/colors-systems.csv", show_col_types = FALSE)

df_timespan_month <- seq(ymd("1997-01-01"), ymd("2021-06-01"), by = "month") %>% as_tibble()
df_timespan_year <- seq(ymd("1996-01-01"), ymd("2021-06-01"), by = "year") %>% as_tibble()

year_breaks_for_plotting <- df_timespan_year %>% 
  mutate(years = year(value)) %>% 
  select(years) %>% pull(.)

ANNOTATION_IS_EMPTY <- TRUE




# PATH_DATA_CONFIG <- "https://api.github.com/repos/SFB1472/ARCH_News_Comments/contents/config.yaml"

# DATAPATH_RAW <- "/html-file-information.csv"
# DATAPATH_RAW_WORLD <- "https://jupyter.digitalmethodologies.center/user/b03/files/data/World/html-file-information.csv?token="
# DATAPATH_RAW_NL <- "https://jupyter.digitalmethodologies.center/user/b03/files/data/Dutch/html-file-information.csv?token="



## domains to look at: spreadsheet links
# SPREADSHEET_PATH_GENERELL <- "https://docs.google.com/spreadsheets/d/1adwVWgBeZI70j0wHZsD6xho1jZTbJzXZ6RHtx86QJc8/"
# 
# SPREADSHEET_PATH_DOMAINS <- list()
# SPREADSHEET_PATH_DOMAINS$World <- "Worldwide news"
# SPREADSHEET_PATH_DOMAINS$Dutch <- "Dutch news"
# SPREADSHEET_PATH_DOMAINS$German <-"German news"


## snippets to search for

SPREADSHEET_SNIPPETS_TO_SEARCH_FOR <- "788821906"

## annotation from research: currently used for two graphics on tab "perspective on aggregated data" and also used on the tab "perspective on domains" 
## copied to local data file system for reasons of performance and less frequent changes on the google sheets than expected

SPREADSHEET_ANNOTATION_DATA <- "data"

SPREADSHEET_ANNOTATION <- list()
SPREADSHEET_ANNOTATION$World <- "/World/technographer - manually added data points - international-domains.csv"
SPREADSHEET_ANNOTATION$Dutch <- "/Dutch/technographer - manually added data points - nl-domains.csv"
SPREADSHEET_ANNOTATION$German <- "/German/technographer - manually added data points - de-domains.csv"

# SPREADSHEET_SITES <- list()
# SPREADSHEET_SITES$World <- "data/World/"
# SPREADSHEET_SITES$Dutch <- 
# SPREADSHEET_SITES$German <- 