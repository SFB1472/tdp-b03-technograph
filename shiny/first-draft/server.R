library(shiny)
library(quanteda)
library(tidyverse)
library(fuzzyjoin)

raw_data <- read_csv("data/Disqus-Blog-disqus-blog.csv") %>% 
  select(id = `web-scraper-order`, date = date_blogpost, title = `title_blogpost...3`, text = text_blogpost) %>% 
  mutate(check_keyword = str_detect(text, "moderat"))

## https://stackoverflow.com/questions/53813211/ngram-refer-back-to-docname-in-quanteda


toks_disqus <- tokens(corpus_disqus, remove_punct = TRUE)
head(toks_disqus)

toks_disqus_lower <- tokens_group(tokens_tolower(toks_disqus), groups = id)

# toks_disqus_cleaned <- tokens_group(tokens_remove(toks_disqus_lower, pattern = stopwords("en")), groups = id)

words_to_extract %>% 
  mutate(look_up = str_split(word, "\\|"))

get_context_words <- function(inputCat){
  print(inputCat)
  # chat_messages_big
  # inputCat <- "blacklist|filter"
  search_words <- str_split(inputCat, "\\|")
  print((search_words))
  kw <- kwic(toks_disqus_lower, pattern = search_words[[1]]) %>% 
    as_tibble() %>% 
    select(-from, -to)
  
}

shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot


  printhateindex <- reactive({
    get_context_words(input$searchWord)
    })
    output$printhateindex <- DT::renderDataTable(printhateindex(), options = list(pageLength = 30, row_number =FALSE))

  
  
  # values <- reactiveValues(get_hate_data = data.frame(x = NA, y = NA))
  # 
  # observeEvent(list(input$searchWord),{
  #   values$df_embedding_data <- isolate({
  #     get_hate_data(input$searchWord)
  #   })
  # })
  # 
  # printhateindex <- reactive({print_hate_table(values$df_embedding_data, input$searchWord)})
  # output$printembeddings <- DT::renderDataTable({printhateindex()})
  # 
  # printOneHitWonder <- reactive({print_OneHitWonder(input$searchWord, input$number_words)})
  # output$printOneHitWonder <- renderPlot({printOneHitWonder()})

  ################
  #### 2. Tab ####
  ################
  table_caution <- reactive({
    "DISCLAMER: Für diese Darstellung wurden nur rund 18.000 Nachrichten kodiert und für die Kodierung das Spreadsheet in seinem unvollständigen Zustand eingesetzt. Deshalb: diese Daten sind noch nicht aussagekräftig, nicht im mindesten."
  })
  output$table_caution <- renderText(table_caution())
  
  printhateLines <- reactive({print_hate_lines()})
  output$printhateLines <- renderPlot({printhateLines()})
  
  printhatebarsfacets <- reactive({print_hate_faceted()})
  output$printhatebarsfacets <- renderPlot({printhatebarsfacets()})
  
    
    
    
    
  # 
  # strongestEmbeddingsPerPeriod <- reactive({
  #   get_strongestEmbeddingsPerPeriod(input$searchWord_2, input$input_period)
  # })
  # output$strongestEmbeddingsPerPeriod <- DT::renderDataTable(strongestEmbeddingsPerPeriod(), options = list(pageLength = 30))
  # 
  # ################
  # #### 3. Tab ####
  # ################  
  # 
  # printembeddingsLines <- reactive({print_histo_lines(input$searchWord3)})
  # output$printembeddingsLines <- renderPlot({printembeddingsLines()})

})
