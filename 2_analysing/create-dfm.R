library(tidyverse)
library(DBI)
library(RPostgres)
library(quanteda)
library(quanteda.textstats)
library(urltools)
library(tidygraph)
library(ggraph)
library(igraph)
library(ggiraph)
library(visNetwork)

source("config/config-secret-local.R")

SPHERE_FOR_SHEET <- "German"

con <- dbConnect(RPostgres::Postgres(), 
                 dbname = dsn_database,
                 host = dsn_hostname, 
                 port = dsn_port,
                 user = dsn_uid, 
                 password = dsn_pwd
)

df_tags_pathes_cleaned <- dbGetQuery(conn = con, paste0("SELECT tc.id_sha1_group, s.site, s.url, s.crawl_date, tc.tag, tc.attr, tc.value_cleaned_whitelist FROM tag_context tc INNER JOIN sites s ON tc.sha1 = s.sha1 WHERE tc.sphere = '",SPHERE_FOR_SHEET,"'"))


# creating hashes for cleaned_whitelist iteration ------------------------------------------------------------------

prep_context_cleaned_whitelist <- df_tags_pathes_cleaned %>%
  mutate(subdomain = urltools::domain(url) %>% suffix_extract(.) %>% select(subdomain) %>% pull(.),
         site_subdomain = paste(site, subdomain, sep = "_") %>% as.character(.)) %>%
  distinct(tag, attr, value_cleaned_whitelist, id_sha1_group, .keep_all = TRUE) %>% 
  # select(id_sha1_group, tag, attr, value_cleaned_whitelist) %>%
  # group_by(id_sha1_group) %>% 
  arrange(id_sha1_group, tag, attr, value_cleaned_whitelist) %>% #View()
  # ungroup() %>% # View()
  mutate(content = paste(tag, attr, value_cleaned_whitelist)) %>% 
  # select(id_sha1_group, content) %>% 
  reframe(contents = paste(content, collapse = " "), .by = c(id_sha1_group, crawl_date, site_subdomain)) #%>% #,


site_subdomain_list <- prep_context_cleaned_whitelist %>% select(site_subdomain) %>% distinct() %>% pull(.)

# walk(site_subdomain_list, function(i){
  print(i)
  # corpus_whitelist <- prep_context_cleaned_whitelist %>% filter( site_subdomain == i) %>% corpus(., docid_field="id_sha1_group", text_field = "contents")  
  corpus_whitelist <- prep_context_cleaned_whitelist %>% filter( site_subdomain == "sueddeutsche_www") %>% corpus(., docid_field="id_sha1_group", text_field = "contents")
  toks_whitelist <- corpus_whitelist %>% tokens()
  dfm_whitelist <- dfm(toks_whitelist)
  
  simi_whitelist <- textstat_simil(dfm_whitelist, method = "cosine", margin = "documents") %>% 
    as_tibble() %>% 
    mutate(site_subdomain = "sueddeutsche_www") %>% 
    rename(id_sha1_group_1 = document1, id_sha1_group_2 = document2)
  # return(simi_whitelist)
  
  # save(simi_whitelist, file=paste0("data/2-analysing/",SPHERE_FOR_SHEET,"/similarities/sim-",i,".RData"))
# })


# dbWriteTable(conn = con, name = "context_hashed", value = df_hashed_context_cleaned_whitelist, append = TRUE)


load(file="data/2-analysing/German/similarities/sim-welt_www.RData")

subdomain_ <- "welt_www"

df_graph_context <- dbGetQuery(conn = con, paste0("SELECT tc.id_sha1_group, s.site, s.url, s.crawl_date, tc.context_path_cleaned FROM tag_context tc INNER JOIN sites s ON tc.sha1 = s.sha1 WHERE tc.sphere = '",SPHERE_FOR_SHEET,"' AND s.site ='welt'"))


context_data <- df_graph_context %>% 
  mutate(subdomain = urltools::domain(url) %>% suffix_extract(.) %>% select(subdomain) %>% pull(.),
         site_subdomain = paste(site, subdomain, sep = "_") %>% as.character(.)) %>% #View()
  filter(site_subdomain == subdomain_) %>% 
  distinct() %>% 
  mutate(month= month(crawl_date),
         year = year(crawl_date),
         simple_date = paste0(year,"-", month, "-01")) %>% 
  arrange(simple_date) %>% 
  mutate(same = cur_group_id(), .by = c(context_path_cleaned, simple_date),
         simple_time = as.numeric(as.POSIXct(simple_date, format="%Y-%m-%d"))) %>% 
  reframe(., id = first(id_sha1_group), .by = c(context_path_cleaned, simple_time))

nodes_ <- simi_whitelist %>% 
  select(id_sha1_group_1) %>% 
  distinct() %>% 
  filter(id_sha1_group_1 %in% context_data$id) %>% 
  left_join(., context_data, by = c("id_sha1_group_1" = "id")) %>% 
  select(id = id_sha1_group_1, label = context_path_cleaned, timestamp = simple_time)

clipr::write_clip(nodes_)

edges_ <- simi_whitelist %>% 
  filter(id_sha1_group_1 %in% context_data$id) %>% 
  filter(id_sha1_group_2 %in% context_data$id) %>% 
  rename(source = id_sha1_group_1, target = id_sha1_group_2, weight = cosine) %>% 
  filter(weight > 0.59) %>%
  mutate(source = as.character(source),
         target = as.character(target)) %>% 
  arrange(source)

edges_ %>% select(revCheck) %>% unique() %>% nrow()

clipr::write_clip(edges_)

nodes_ %>% filter(is.na(id_sha1_group_1)) %>% View()

graph_data <- as_tbl_graph(edges_, directed = F) %>% 
  activate(nodes) %>% #as_tibble() %>% #View()
  left_join(., context_data, by = c("name" = "id"), copy = T) %>% # View()
  mutate(
    degree = igraph::degree(.),
    # indegree = igraph::degree(., mode = "in"),
    # outdegree = igraph::degree(., mode = "out"),
    # strength = igraph::strength(.),
    # between_ = igraph::betweenness(.),
    # close = igraph::closeness(.),
    # title = context_path_cleaned,
    group = context_path_cleaned
  ) #%>% 

vis_graph_data <- toVisNetworkData(graph_data)

graph_data %>% as_tibble() %>% View()

sim_network_welt <- visNetwork(nodes = vis_graph_data$nodes, edges = vis_graph_data$edges, height = "500px", width = "100%") %>%
  # visIgraphLayout(layout = "layout_nicely", randomSeed = 1105) %>%
  visIgraphLayout(layout = "layout_with_fr", randomSeed = 3014) %>%
  visOptions(
    highlightNearest = list(
      enabled = TRUE,
      degree = 1,
      hover = TRUE,
      labelOnly = TRUE
    ), autoResize = TRUE,
    nodesIdSelection = list(enabled = TRUE, values = "context_path_cleaned"), # value from vectir names
    selectedBy = "context_path_cleaned" # dropdowns by group
  ) %>%
  # visLayout(randomSeed = 0104, improvedLayout = T) %>%
  visEdges(color = list("color"= "#444444", "opacity" = 0.1)) %>%
  visNodes(size = 5) %>% 
  visPhysics(
    # solver = "hierarchicalRepulsion",
    forceAtlas2Based = list(gravitationalConstant = 20)
  ) %>%
  # visPhysics(solver = "hierarchicalRepulsion") %>%
  visInteraction(navigationButtons = TRUE)


sim_network_welt


mini_nodes <- nodes_ %>% filter(label == "div {'class': ['tab'],")

mini_edges <- edges_ %>% filter(source %in% group_test$id, weight == 1) #%>% View()

edges_ %>% filter(source == "8a83d71086f2ae5b41a2d9c387d33586e0183c9b_4" | target == "8a83d71086f2ae5b41a2d9c387d33586e0183c9b_4") %>% 
  left_join(., nodes_, by = c("target" = "id")) %>% View()

mini_graph_data <- as_tbl_graph(mini_edges, directed = F) %>% 
  activate(nodes) %>% #as_tibble() %>% #View()
  left_join(., context_data, by = c("name" = "id"), copy = T) %>% # View()
  mutate(
    degree = igraph::degree(.),
    # indegree = igraph::degree(., mode = "in"),
    # outdegree = igraph::degree(., mode = "out"),
    # strength = igraph::strength(.),
    # between_ = igraph::betweenness(.),
    # close = igraph::closeness(.),
    # title = context_path_cleaned,
    group = context_path_cleaned
  ) #%>% 

mini_vis_graph_data <- toVisNetworkData(mini_graph_data)

mini_sim_network_welt <- visNetwork(nodes = mini_vis_graph_data$nodes, edges = mini_vis_graph_data$edges, height = "500px", width = "100%") %>%
  # visIgraphLayout(layout = "layout_nicely", randomSeed = 1105) %>%
  visIgraphLayout(layout = "layout_with_fr", randomSeed = 3014) %>%
  visOptions(
    highlightNearest = list(
      enabled = TRUE,
      degree = 1,
      hover = TRUE,
      labelOnly = TRUE
    ), autoResize = TRUE,
    nodesIdSelection = list(enabled = TRUE, values = "context_path_cleaned"), # value from vectir names
    selectedBy = "context_path_cleaned" # dropdowns by group
  ) %>%
  # visLayout(randomSeed = 0104, improvedLayout = T) %>%
  visEdges(color = list("color"= "#444444", "opacity" = 0.1)) %>%
  visNodes(size = 5) %>% 
  visPhysics(
    # solver = "hierarchicalRepulsion",
    forceAtlas2Based = list(gravitationalConstant = 20)
  ) %>%
  # visPhysics(solver = "hierarchicalRepulsion") %>%
  visInteraction(navigationButtons = TRUE)


mini_sim_network_welt
