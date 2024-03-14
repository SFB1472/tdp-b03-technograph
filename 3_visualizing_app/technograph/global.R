library(pool)
library(RPostgres)
source("../../shiny-conf/config-secret.R")

# CURRENT_SPHERE <- "German"

pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = dsn_database,
  host = dsn_hostname,
  user = dsn_uid,
  password = dsn_pwd
)

## issue with pool in complex shiny apps: https://github.com/rstudio/pool/issues/58
## https://stackoverflow.com/questions/64563750/using-pool-in-an-r-shiny-package