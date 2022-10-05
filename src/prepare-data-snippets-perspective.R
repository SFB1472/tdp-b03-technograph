library(tidyverse)
library(lubridate)
library(ggiraph)
source("src/config-graphics.R")

regex_year_only <- "^[\\d]{4,4}$"
regex_month_name <- "^[a-zA-Z]{1,}"
regex_full_date <- "^\\d{2,2}\\.\\d{2,2}\\.\\d{4,4}"



# to be changed for reading directly from google
# https://docs.google.com/spreadsheets/d/13s-hrOrP0kYrfWJEx-BD675JF51CKN9PBhO9M_lay_U/edit#gid=241642220
raw_vis_data <- read.csv("data/22-01-11-Commenting Technology Museum - List of commenting systems.csv") %>% 
  # column name -> name_product bei Aktualisierung
  select(name, starts_with("date_"))

df_raw_vis_data <- raw_vis_data %>% 
  mutate(
      start_date = case_when(
           str_detect(date_introductionfounded, regex_month_name) ~ my(date_introductionfounded),
           str_detect(date_introductionfounded, regex_full_date) ~ dmy(date_introductionfounded),
           str_detect(date_introductionfounded, regex_year_only) ~ as_date(paste0(date_introductionfounded,"01","01")),
           TRUE ~ NA_Date_
         ),
      unsafe_start = case_when(
           str_detect(date_introductionfounded, regex_month_name) ~ "month",
           str_detect(date_introductionfounded, regex_year_only) ~ "year",
           TRUE ~ "none"
           ),
      date_discontinued = trimws(date_discontinued),
      end_date = case_when(
        str_detect(date_discontinued, "current") ~ today(),
        str_detect(date_discontinued, regex_full_date) ~ dmy(date_discontinued),
        str_detect(date_discontinued, regex_year_only) ~ as_date(paste0(date_discontinued,"01","01")),
        str_detect(date_discontinued, "^[\\d]{1,2}\\s{1,}[a-zA-Z]{1,}\\s{1,}[\\d]{4,4}") ~ dmy(date_discontinued),
        str_detect(date_discontinued, "^[a-zA-Z]{1,}\\s{1,}[\\d]{4,4}") ~ my(date_discontinued),
        (date_discontinued == "") ~ today(),
        TRUE ~ today()
      ),
      unsafe_end = case_when(
        str_detect(date_discontinued, "current") ~ "none",
        str_detect(date_discontinued, regex_full_date) ~ "none",
        str_detect(date_discontinued, regex_year_only) ~ "year",
        str_detect(date_discontinued, "^[\\d]{1,2}\\s{1,}[a-zA-Z]{1,}\\s{1,}[\\d]{4,4}") ~ "none",
        str_detect(date_discontinued, "^[a-zA-Z]{1,}\\s{1,}[\\d]{4,4}") ~ "month",
        (date_discontinued == "") ~ "unknown",
        TRUE ~ "special"
      ),
      secure_start = case_when(
        unsafe_start == "month" ~ ymd(start_date) %m+% months(1),
        unsafe_start == "year"~ start_date %m+% years(1),
        unsafe_start == "none"~ start_date
      ),
      secure_end = case_when(
        unsafe_end == "month" ~ ymd(end_date) %m+% months(-1),
        unsafe_end == "year"~ end_date %m+% years(-1),
        unsafe_end %in% c("none", "unknown", "special") ~ end_date
      ),
      time_span = interval( start_date, end_date) %>% as.duration(.) %>% as.period(., unit = "years")
      )

save(df_raw_vis_data, file= paste0(path_to_shinydata, "df_raw_vis_data.RData"))
load(paste0(path_to_shinydata, "df_raw_vis_data.RData"))

from_to <- df_raw_vis_data %>% 
  select(start_date, end_date) %>% 
  summarise(start = min(start_date, na.rm = TRUE), end = max(end_date, na.rm = TRUE)) %>% 
  mutate(start = year(start),
         end = year(end))

seq_years_ <- seq(as.numeric(from_to$start), as.numeric(from_to$end), by = 1) %>% as.character(.)

df_raw_vis_data[,seq_years_] = NA

df_system_lifetime <- df_raw_vis_data %>% 
  filter(!is.na(secure_start), !is.na(time_span)) %>% 
  arrange(desc(time_span)) %>% 
  mutate(start_year_unsecure = year(start_date) %>% as.numeric(.),
         end_year_unsecure = year(end_date) %>% as.numeric(.),
         start_year = year(secure_start) %>% as.numeric(.),
         end_year = year(secure_end) %>% as.numeric(.),
         sorting_lifetime = row_number()) %>% 
  select(name, sorting_lifetime, time_span, start_year_unsecure, end_year_unsecure, start_year, end_year, as.character(from_to$start):ncol(.)) %>% 
  distinct() %>% 
  pivot_longer(., as.character(from_to$start):as.character(from_to$end), names_to = "check_years", values_to = "year") %>% 
  mutate(check_years = as.numeric(check_years),
    year = (check_years >= start_year & check_years <= end_year),
    # year = ifelse(year == FALSE, NA, year),#) %>% 
    year_unsecure = (check_years >= start_year_unsecure & check_years <= end_year_unsecure),
    year_unsecure = ifelse(year_unsecure == year, FALSE, year_unsecure)) %>%
  pivot_longer(., year:year_unsecure, names_to = "cat", values_to = "present") %>%
  select(-start_year,-start_year_unsecure, -end_year, -end_year_unsecure) %>% 
  mutate(present = ifelse(present == FALSE, NA, 1))

## false values rausfiltern, für obere grafik nur values printen, für untere categorie wichtig.

save(df_system_lifetime, file = paste0(path_to_shinydata, "df_system_lifetime.RData"))


plot <- df_raw_vis_data %>% 
  filter(!is.na(name), name != "") %>% 
  arrange((time_span)) %>% 
  head(50) %>% #View()
  ggplot(., aes()) +
  geom_rect_interactive(aes(xmin = start_date, xmax = secure_start, ymin = 0.5, ymax = -0.5, tooltip = name, fill = "timespan_unsafe")) +
  geom_rect_interactive( aes(xmin = secure_start, xmax = secure_end, ymin = 0.5, ymax = -0.5, tooltip = name)) +
  geom_rect_interactive(aes(xmin = secure_end, xmax = end_date, ymin = 0.5, ymax = -0.5, tooltip = name, fill = "timespan_unsafe")) +
  facet_wrap(~reorder(name, time_span), ncol = 1) +
  scale_x_date() +
  theme_b03_box_timeline

girafe(ggobj = plot)

#check on enddates! sieht so aus, als wäre das nicht ganz korrekt, dass alle so lange laufen. 
## wie viele haben ein "special" bei unsafe date?

# darstellung finden um alle systeme zu zeigen. 


