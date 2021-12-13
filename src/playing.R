library(tidyverse)
library(lubridate)
library(ggiraph)

regex_year_only <- "^[\\d]{4,4}$"
regex_month_name <- "^[a-zA-Z]{1,}"
regex_full_date <- "^\\d{2,2}\\.\\d{2,2}\\.\\d{4,4}"

# to be changed for reading directly from google
raw_vis_data <- read.csv("data/Commenting Technology Museum - List of commenting systems.csv") %>% 
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
      indicator = case_when(
        unsafe_start != "none" ~ 1,
        unsafe_start == "none" ~ 2,
        unsafe_end %in% c("month", "year") ~ 3
      )
      )

plot <- df_raw_vis_data %>% 
  filter(!is.na(name), name != "") %>% 
  head(50) %>% #View()
  ggplot(., aes()) +
  geom_rect_interactive(aes(xmin = start_date, xmax = secure_start, ymin = 0.5, ymax = -0.5, tooltip = name, fill = "timespan_unsafe")) +
  geom_rect_interactive( aes(xmin = secure_start, xmax = secure_end, ymin = 0.5, ymax = -0.5, tooltip = name)) +
  geom_rect_interactive(aes(xmin = secure_end, xmax = end_date, ymin = 0.5, ymax = -0.5, tooltip = name, fill = "timespan_unsafe")) +
  facet_wrap(~name, ncol = 1) +
  scale_x_date() +
  theme_b03_box_timeline

girafe(ggobj = plot)



