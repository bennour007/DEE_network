## cIn this script we hamonise the external data
## the objective isto make sure that we have metrics of complexity 
## measure of networks
## including the CB data 



## Software topics 


topics <- read_csv(here::here("data", "eci_software", "data", "inputs", "topics.csv")) %>%
  filter( iso2_code != "EU" )


topics %>%
  group_by(topic, country = iso2_code, year) %>%
  summarise(
    num_pushers = sum(num_pushers)
  ) %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(
    mat = map(
      data, 
      \(x){
        cs <- sort(unique(x$country))   # union of BOTH columns
        
        x %>% 
          complete(country = cs, fill = list(value = 0)) %>% 
          pivot_wider(names_from = topic, values_from = num_pushers, values_fill = 0) %>%
          column_to_rownames("country") %>%
          as.matrix()
      }
    ),
    
    complexity = map(
      mat, 
      \(x){
        # rca_bin <- EconGeo::rca(x, binary = TRUE)
        rcomp <- EconGeo::morc(mat =  EconGeo::rca(x, binary = TRUE)) %>% 
          as.data.frame() %>%
          rownames_to_column("country") %>% 
          mutate(
            country = countrycode(
              country, 
              , origin = "iso2c", destination = "iso3c"
            )
          ) %>% 
          # countrycode(country, origin = "iso2c", destination = "iso3c") %>% 
          rename(soft_comp = 2)
        return(rcomp)
      }
    )
  ) -> soft_comp 

soft_comp %>% 
  unnest(c("complexity")) %>% 
  ungroup() %>% 
  filter(year %in% c(2020, 2021)) %>% 
  select(year, country, soft_comp) -> soft_comp_compact

soft_comp_compact %>% 
  write_csv(here::here("data", "soft_comp20-21.csv"))

## this wont work with the current data, because the time range is 2020 to 2023. not good.
# loading libraries

pacman::p_load(
  tidyverse, 
  tidygraph,
  igraph, 
  fixest, 
  marginaleffects,
  gt,
  countrycode,
  PerformanceAnalytics,
  WDI
  # enaR
)

gc()

## loading data 

trade_hq <- read_csv(here::here("data", "all_country_trade_hq_intervals.csv"))

## crunch base 


cb_data <- readxl::read_xlsx(here::here("data", "CB_firms.xlsx"))

adj_mat <- trade_hq %>% 
  # group_by(year, tech_group) %>% 
  # select(1,2, category, value = trade_value_pred) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(value = sum(trade_value_pred)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(
    A = map(
      data, 
      \(x){
        cs <- sort(unique(c(x$iso_o, x$iso_d)))   # union of BOTH columns
        
        x %>% 
          complete(iso_o = cs, iso_d = cs, fill = list(value = 0)) %>% 
          pivot_wider(names_from = "iso_d", values_from = "value", values_fill = 0) %>% 
          column_to_rownames(var = "iso_o") %>% 
          as.matrix() 
      }
    )
  )


nets <- adj_mat %>% 
  mutate(
    
    
    cyc2     = map(A, ~ diag(.x %*% .x)),                              # direct round-trip
    cyc_katz = map(
      A, 
      ~ diag(solve(diag(nrow(.x)) - 0.85/max(abs(eigen(.x)$values)) * .x)) - 1
    ), 
    
    Net = map(
      A, 
      ~ tidygraph::as_tbl_graph(.x, directed = T)
    ),
    mets = map(
      Net, 
      ~ .x %>% 
        activate(nodes) %>%
        mutate(
          eigen = centrality_eigen(
            directed = T, 
            scale = T
          ),
          broker   = centrality_betweenness(
            weights = 1 / E(.)$weight, 
            directed = TRUE, 
            normalized = T
          )
        ) %>% 
        as_tibble()
    )
  )

nets_full <- nets %>% 
  unnest(c(cyc2, cyc_katz, mets)) %>% 
  ungroup() %>% 
  select(year, country = name, cyc2, cyc_katz, broker, eigen) %>% 
  left_join(cb_data, by = c("country", "year" = "Year"))


nets_full %>% 
  write_csv(here::here("data", "nets_full.csv"))


nets %>% 
  ungroup() %>% 
  select(year, Net) %>% 
  write_rds(here::here("data", "network_countries.rds"))
