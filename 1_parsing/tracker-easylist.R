library(tidyverse)

# en - adservers -----------------------------------------------------

easylist_adservers <- read_lines("data/helper/tracker-snippets/easylist_adservers.txt") %>% enframe()

df_easylist_adservers <- easylist_adservers %>% 
  select(-name) %>% 
  mutate(
    snippets = str_remove(value, "\\^"),
    snippets = str_remove(snippets, "[||]{2,}"),
    snippets = str_remove(snippets, "[$].*$"),
    exclude = str_detect(snippets, "^[/]{1,}"),
    section = str_detect(value, "^!"))

snippets_to_search_for <- df_easylist_adservers %>% 
  filter(section == FALSE, exclude != TRUE) %>% 
  select(snippets) %>% distinct(snippets) %>% pull(snippets)

save(snippets_to_search_for, file="data/helper/tracker-snippets/easylist-adservers.RData")

# en - general block -------------------------------------------

easylist_general_block <- read_lines("data/helper/tracker-snippets/easylist_general_block.txt") %>% enframe()
df_easylist_general_block <- easylist_general_block %>% 
  select(-name) %>% 
  mutate(
    snippets = str_remove(value, "\\^"),
    snippets = str_remove(snippets, "[||]{2,}"),
    snippets = str_remove(snippets, "[$].*$"),
    snippets = str_remove(snippets, "^[://]{2,}"),
    snippets = str_remove(snippets, "^[|]{1,}"),
    # exclude = str_detect(snippets, "^[://]{2,}"),
    section = str_detect(value, "^!"),
    section_name = ifelse(section, value, NA)
  ) %>% 
  fill(section_name, .direction = "down")

snippets_to_search_for <- df_easylist_general_block %>% 
  filter(section == FALSE) %>% 
  select(snippets) %>% distinct(snippets) %>% pull(.)

save(snippets_to_search_for, file="data/1-parsing/tracker-easylist/easylist-gerneral-block.RData")

# en - third party --------------------------------------------

easylist_third_party <- read_lines("data/helper/tracker-snippets/easylist_thirdparty.txt") %>% enframe()
df_easylist_third_party <- easylist_third_party %>% 
  select(-name) %>% 
  mutate(
    snippets = str_remove(value, "\\^"),
    snippets = str_remove(snippets, "[||]{2,}"),
    snippets = str_remove(snippets, "[$].*$"),
    # snippets = str_remove(snippets, "^[://]{2,}"),
    # snippets = str_remove(snippets, "^[|]{1,}"),
    exclude = str_detect(snippets, "^[/]{1,}"),
    section = str_detect(value, "^!"),
    section_name = ifelse(section, value, NA)
  ) %>% 
  fill(section_name, .direction = "down")

snippets_to_search_for <- df_easylist_third_party %>% 
  filter(section == FALSE, !exclude) %>% 
  select(snippets) %>% distinct(snippets) %>% pull(.)

save(snippets_to_search_for, file="data/helper/tracker-snippets/easylist-thirdparty.RData")

# general hide ----------------------

easylist_general_hide <- read_lines("data/helper/tracker-snippets/easylist_general_hide.txt") %>% enframe()
df_easylist_general_hide <- easylist_general_hide %>% 
  select(-name) %>% 
  filter(!str_detect(value, "#@#")) %>% ## '#@#' bedeutet, dass diese elemente geladen werden sollen -> whitelist
  mutate(
    snippets = str_remove(value, "\\^|\\*"),
    snippets = str_remove(snippets, "(^##div\\[)|(^##a\\[)|(^##img\\[)|(^##span\\[)|(##iframe\\[)"),
    snippets = str_remove(snippets, "^[#]{2,}"),
    snippets = str_remove(snippets, "[$]"),
    snippets = str_remove_all(snippets, "[\\[\\]]"),
    # snippets = str_remove(snippets, "^[|]{1,}"),
    exclude = str_detect(snippets, "gdprAdTransparencyCogWheelButton"),
    section = str_detect(value, "^!"),
    section_name = ifelse(section, value, NA)
  ) %>% 
  fill(section_name, .direction = "down")

snippets_to_search_for <- df_easylist_general_hide %>% 
  filter(section == FALSE, !exclude) %>% 
  select(snippets) %>% 
  distinct(snippets) %>%
  pull(.)

save(snippets_to_search_for, file="data/helper/tracker-snippets/easylist-general-hide.RData")

# specific hide: to do ----------------------

easylist_specific_hide <- read_lines("data/helper/tracker-snippets/easylist/easylist_specific_hide.txt") %>% enframe()
df_easylist_specific_hide <- easylist_specific_hide %>% 
  select(-name) %>% 
  filter(!str_detect(value, "#@#")) %>% ## '#@#' bedeutet, dass diese elemente geladen werden sollen -> whitelist
  mutate(
    snippets = str_remove(value, "\\^|\\*"),
    snippets = str_remove(snippets, "(^##div\\[)|(^##a\\[)|(^##img\\[)|(^##span\\[)|(##iframe\\[)"),
    snippets = str_remove(snippets, "^[#]{2,}"),
    snippets = str_remove(snippets, "[$]"),
    snippets = str_remove_all(snippets, "[\\[\\]]"),
    # snippets = str_remove(snippets, "^[|]{1,}"),
    exclude = str_detect(snippets, "gdprAdTransparencyCogWheelButton"),
    section = str_detect(value, "^!"),
    section_name = ifelse(section, value, NA)
  ) %>% 
  fill(section_name, .direction = "down")

snippets_to_search_for <- df_easylist_specific_hide %>% 
  filter(section == FALSE, !exclude) %>% 
  select(snippets) %>% 
  distinct(snippets) %>%
  pull(.)

save(snippets_to_search_for, file="data/helper/tracker-snippets/easylist-specific-hide.RData")

# specific block: to do -------------------------------------------

easylist_specific_block <- read_lines("data/helper/tracker-snippets/easylist_specific_block.txt") %>% enframe()
df_easylist_specific_block <- easylist_specific_block %>% 
  select(-name) %>% 
  mutate(
    snippets = str_remove(value, "\\^"),
    snippets = str_remove(snippets, "[||]{2,}"),
    snippets = str_remove(snippets, "[$].*$"),
    snippets = str_remove(snippets, "^[://]{2,}"),
    snippets = str_remove(snippets, "^[|]{1,}"),
    # exclude = str_detect(snippets, "^[://]{2,}"),
    section = str_detect(value, "^!"),
    section_name = ifelse(section, value, NA)
  ) %>% 
  fill(section_name, .direction = "down")

snippets_to_search_for <- df_easylist_specific_block %>% 
  filter(section == FALSE) %>% 
  select(snippets) %>% distinct(snippets) %>% pull(.)

save(snippets_to_search_for, file="data/helper/tracker-snippets/easylist-specific-block.RData")

## ###teaser1[style^="width:autopx;"]

# de general block ----------------------

easylist_general_block_de <- read_lines("data/helper/tracker-snippets/easylist/easylistgermany_general_block.txt") %>% enframe()
df_easylist_general_block_de <- easylist_general_block_de %>% 
  select(-name) %>% 
  mutate(
    snippets = str_remove(value, "(\\^)|(\\*)"),
    # snippets = str_remove(snippets, "[||]{2,}"),
    snippets = str_remove(snippets, "[$].*$"),
    # snippets = str_remove(snippets, "^[://]{2,}"),
    # snippets = str_remove(snippets, "^[|]{1,}"),
    # exclude = str_detect(snippets, "^[://]{2,}"),
    section = str_detect(value, "^!"),
    section_name = ifelse(section, value, NA)
  ) %>% 
  fill(section_name, .direction = "down")

snippets_to_search_for <- df_easylist_general_block_de %>% 
  filter(section == FALSE) %>% 
  select(snippets) %>% distinct(snippets) %>% pull(.)

save(snippets_to_search_for, file="data/helper/tracker-snippets/easylist-gerneral-block_de.RData")

# de general hide ---------------------------

easylist_general_hide_de <- read_lines("data/helper/tracker-snippets/easylist/easylistgermany_general_hide.txt") %>% enframe()
df_easylist_general_hide_de <- easylist_general_hide_de %>% 
  select(-name) %>% 
  filter(!str_detect(value, "#@#")) %>% ## '#@#' bedeutet, dass diese elemente geladen werden sollen -> whitelist
  mutate(
    snippets = str_remove(value, "\\^|\\*"),
    snippets = str_remove(snippets, "(^##div\\[)|(^##a\\[)|(^##img\\[)|(^##span\\[)|(##iframe\\[)"),
    snippets = str_remove(snippets, "^[#]{2,}"),
    snippets = str_remove(snippets, "[$]"),
    snippets = str_remove(snippets, "\\s>.*$"),
    snippets = str_remove_all(snippets, "[\\[\\]]"),
    # snippets = str_remove(snippets, "^[|]{1,}"),
    # exclude = str_detect(snippets, "gdprAdTransparencyCogWheelButton"),
    section = str_detect(value, "^!"),
    section_name = ifelse(section, value, NA)
  ) %>% 
  fill(section_name, .direction = "down")

snippets_to_search_for <- df_easylist_general_hide_de %>% 
  filter(section == FALSE) %>% 
  select(snippets) %>% 
  distinct(snippets) %>%
  pull(.)

save(snippets_to_search_for, file="data/helper/tracker-snippets/easylist-general-hide_de.RData")

# de - adservers -----------------------------------------------------

easylist_adservers_de <- read_lines("data/helper/tracker-snippets/easylist/easylistgermany_adservers.txt") %>% enframe()

df_easylist_adservers_de <- easylist_adservers_de %>% 
  select(-name) %>% 
  mutate(
    snippets = str_remove(value, "\\^"),
    snippets = str_remove(snippets, "[||]{2,}"),
    snippets = str_remove(snippets, "[$].*$"),
    exclude = str_detect(snippets, "^[/]{1,}"),
    section = str_detect(value, "^!"))

snippets_to_search_for <- df_easylist_adservers_de %>% 
  filter(section == FALSE, exclude != TRUE) %>% 
  select(snippets) %>% distinct(snippets) %>% pull(snippets)

save(snippets_to_search_for, file="data/helper/tracker-snippets/easylist-adservers-de.RData")

# de - third party--------------------------------------------
easylist_third_party_de <- read_lines("data/helper/tracker-snippets/easylist/easylistgermany_thirdparty.txt") %>% enframe()
df_easylist_third_party_de <- easylist_third_party_de %>% 
  select(-name) %>% 
  mutate(
    snippets = str_remove(value, "\\^"),
    snippets = str_remove(snippets, "[||]{2,}"),
    snippets = str_remove(snippets, "[$].*$"),
    # snippets = str_remove(snippets, "^[://]{2,}"),
    # snippets = str_remove(snippets, "^[|]{1,}"),
    exclude = str_detect(snippets, "^[/]{1,}"),
    section = str_detect(value, "^!"),
    section_name = ifelse(section, value, NA)
  ) %>% 
  fill(section_name, .direction = "down")

snippets_to_search_for <- df_easylist_third_party_de %>% 
  filter(section == FALSE, !exclude) %>% 
  select(snippets) %>% distinct(snippets) %>% pull(.)

save(snippets_to_search_for, file="data/helper/tracker-snippets/easylist-thirdparty-de.RData")

# merkzettel ---------------
# regex-patterns, die rausgefiltert werden
# endungen: $document, $popup, $script, $xmlhttprequest, $third-party, $subdocument, $domain, $image

# combine all tracker snippets in one list

load(file = "data/helper/tracker-snippets/easylist-thirdparty-de.RData")
all_snippets <- snippets_to_search_for %>% enframe()
load(file = "data/helper/tracker-snippets/easylist-thirdparty.RData")
merge_snippets <- snippets_to_search_for %>% enframe()
all_snippets <- all_snippets %>% bind_rows(., merge_snippets)
load(file = "data/helper/tracker-snippets/tracker-domains.RData")
merge_snippets <- snippets_to_search_for %>% enframe()
all_snippets <- all_snippets %>% bind_rows(., merge_snippets)
load(file = "data/helper/tracker-snippets/tracker-pattern.RData")
merge_snippets <- snippets_to_search_for %>% enframe()
all_snippets <- all_snippets %>% bind_rows(., merge_snippets)
load(file = "data/helper/tracker-snippets/easylist-gerneral-block.RData")
merge_snippets <- snippets_to_search_for %>% enframe()
all_snippets <- all_snippets %>% bind_rows(., merge_snippets)
load(file = "data/helper/tracker-snippets/easylist-gerneral-block_de.RData")
merge_snippets <- snippets_to_search_for %>% enframe()
all_snippets <- all_snippets %>% bind_rows(., merge_snippets)
load(file = "data/helper/tracker-snippets/easylist-general-hide.RData")
merge_snippets <- snippets_to_search_for %>% enframe()
all_snippets <- all_snippets %>% bind_rows(., merge_snippets)
load(file = "data/helper/tracker-snippets/easylist-general-hide_de.RData")
merge_snippets <- snippets_to_search_for %>% enframe()
all_snippets <- all_snippets %>% bind_rows(., merge_snippets)
load(file = "data/helper/tracker-snippets/easylist-adservers.RData")
merge_snippets <- snippets_to_search_for %>% enframe()
all_snippets <- all_snippets %>% bind_rows(., merge_snippets)
load(file = "data/helper/tracker-snippets/easylist-adservers-de.RData")
merge_snippets <- snippets_to_search_for %>% enframe()
all_snippets <- all_snippets %>% bind_rows(., merge_snippets)

all_snippets <- all_snippets %>% 
  # select(-name) %>% 
  distinct() %>% pull(.)
save(all_snippets, file = "data/helper/tracker-snippets/all-snippets.RData")
