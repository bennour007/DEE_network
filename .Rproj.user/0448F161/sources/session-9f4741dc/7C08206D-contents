## This script is supposed to do a very simple thing:

## construct the data from rraw sources, then return full raw data set to be used for regresions. 


## loading libraries

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

DEE_data <- readxl::read_excel("~/Research/DEE_esteban/DEE_Index_ 2017_2022_short - Copy.xlsx", sheet = 3)


nets_full <- read_csv(here::here("data", "nets_full.csv"))

## setting up tech groups 

trade_hq <- trade_hq %>%
  mutate(
    tech_group = case_when(
      category %in% c(
        "cloud-computing",
        "operating-system",
        "web-hosting",
        "file-hosting-service",
        "Cybersecurity",
        "payment-service",
        "data-licensing"
      ) ~ "infrastructure",
      
      category %in% c(
        "online-marketplace",
        "digital-advertising",
        "online-ride-hailing",
        "online-travel-market",
        "online-food-ordering"
      ) ~ "platform",
      
      category %in% c(
        "software_enterprise-software_business-intelligence-software",
        "software_enterprise-software_customer-relationship-management-software",
        "software_enterprise-software_enterprise-resource-planning-software",
        "software_enterprise-software_supply-chain-management-software",
        "software_enterprise-software_other-enterprise-software",
        "software_productivity-software_creative-software",
        "software_productivity-software_office-software",
        "software_productivity-software_administrative-software",
        "software_productivity-software_collaboration-software"
      ) ~ "productive",
      
      category %in% c(
        "digital-media_digital-music_music-streaming",
        "digital-media_video-games_gaming-networks",
        "digital-media_video-games_pc-games",
        "digital-media_video-on-demand_video-streaming-svod",
        "games",
        "apps",
        "online-gambling",
        "digital-media_epublishing_ebooks",
        "eservices_dating-services_online-dating",
        "eservices_online-education"
      ) ~ "user_facing",
      
      TRUE ~ "other"
    )
  )

# trade_hq %>% 
#   count(tech_group)
#

## function to compute the finn's cycle index


leon_diag <- function(data){
  # first we take the bilateral trade (each category? yes)
  # that's our input, country of origin, desitination, value (normalised by sum of the value of origin)
  # we take that input and calculate the leontief matrix 
  # inverse of (identity - our input matrix)
  L <- solve(diag(nrow(data)) - data)
  # normalise the diagonal element st: Lii -1 / Lii
  Ln <- (diag(L) - 1)/ diag(L)
  # then we use another function to calculate the the weighted average, although we might not need to. 
  return(Ln)
}


# tmp and tmp2 are basically the same logic for different aggregations

# to make things more organised, I will consttruct two data sets 

# Data-full-dyadic and data full country

# I will also refrain from normalising anything here.

# and make sure to actually construct the networks before hand 


################################################################################
## DYADIC DATA, COUNTRY TECH_GREOUP PAIRS -> Not a priority here 
################################################################################


# dyad_trade <- trade_hq %>% 
#   # group_by(year, tech_group) %>% 
#   # select(1,2, category, value = trade_value_pred) %>% 
#   group_by(year, tech_group, iso_o, iso_d) %>% 
#   summarise(value = sum(trade_value_pred)) %>% 
#   ungroup() %>% 
#   group_by(year, tech_group) %>% 
#   nest() %>% 
#   mutate(
#     A = map(
#       data, 
#       \(x){
#         cs <- sort(unique(c(x$iso_o, x$iso_d)))   # union of BOTH columns
#         
#         x %>% 
#           complete(iso_o = cs, iso_d = cs, fill = list(value = 0)) %>% 
#           pivot_wider(names_from = "iso_d", values_from = "value", values_fill = 0) %>% 
#           column_to_rownames(var = "iso_o") %>% 
#           as.matrix() 
#       }
#     ),
#     A = map(
#       A,
#       \(x){
#         m <- x / rowSums(x, na.rm = T)
#         mm <- replace_values(m, from = NaN, to = 0)
#         return(mm)
#         # replace_values(x, from =, to = 0)
#       }
#     )
#   )
# 
# dyad_trade_full <- dyad_trade %>% 
#   mutate(
#     L = map(A, ~ {
#       v <- leon_diag(.x)
#       tibble(country = rownames(.x),   # pull names from the matrix, not the vector
#              L = as.numeric(v))
#     })
#   )
# 
# # tmp2[1, "L"] %>% unnest()
# 
# dyad_dci <- dyad_trade_full %>% 
#   unnest("L") %>% 
#   ungroup() %>% 
#   select(year, country, tech_group, DCI = L) 
# 
# 

################################################################################
## COUNTRY DATA, AGGREGATION
################################################################################


country_trade <- trade_hq %>% 
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
    ),
    A = map(
      A,
      \(x){
        # maybe we gotta try the claude part here? (base R as an example)
        m <- x / rowSums(x, na.rm = T)
        mm <- replace_values(m, from = NaN, to = 0)
        return(mm)
        # replace_values(x, from =, to = 0)
      }
    )
  )

country_trade_full <- country_trade %>% 
  mutate(
    L = map(A, ~ {
      v <- leon_diag(.x)
      tibble(country = rownames(.x),   # pull names from the matrix, not the vector
             L = as.numeric(v))
    })
  )

# tmp2[1, "L"] %>% unnest()

country_dci <- country_trade_full %>% 
  unnest("L") %>% 
  ungroup() %>% 
  select(year, country, DCI = L) 




dependency_metrics_c <- trade_hq %>%
  # --- supplier shares: country = importer (iso_d), supplier = iso_o ---
  group_by(country = iso_d,  year, supplier = iso_o) %>%
  summarise(M = sum(trade_value_pred), .groups = "drop") %>%   # sum categories within group
  group_by(country,  year) %>%
  mutate(
    s = M / sum(M),
    n_suppliers = n()
  ) %>%
  summarise(
    n_suppliers = first(n_suppliers),
    HHI      = sum(s^2),
    HHI_clean = if (first(n_suppliers) > 1)
      (sum(s^2) - 1/first(n_suppliers)) / (1 - 1/first(n_suppliers)) else 1,
    entropy  = -sum(s * log(s)),
    entropy_clean = if (first(n_suppliers) > 1) (-sum(s * log(s))) / log(first(n_suppliers)) else 0,
    .groups = "drop"
  )


net_reliance_c <- bind_rows(
  trade_hq %>% transmute(country = iso_d, tech_group, year, M = trade_value_pred, X = 0),
  trade_hq %>% transmute(country = iso_o, tech_group, year, M = 0, X = trade_value_pred)
) %>%
  group_by(country, year) %>%
  summarise(M = sum(M), X = sum(X), dep_m = sum(M) / (sum(M) + sum(X)), .groups = "drop")

dependency_metrics_full_c <- net_reliance_c %>% 
  left_join(
    dependency_metrics_c, 
    by = c("country", "year")
  ) %>% 
  left_join(
    country_dci,
    by = c("country", "year")
  ) %>% 
  # group_by( year) %>% 
  # mutate(
  #   across(
  #     c(dep_m, n_suppliers, HHI, entropy, DCI, entropy_clean, HHI_clean),
  #     ~ scale(.x)[,1],
  #     .names = "{.col}_norm"
  #   )
  # ) %>% 
  ungroup()




# Download GDP total (current US$)
gdp_total <- WDI(
  country = "all",
  indicator = "NY.GDP.PCAP.KD",
  start = min(trade_hq$year, na.rm = TRUE),
  end   = max(trade_hq$year, na.rm = TRUE),
  extra = F
)

gdp_total_clean <- gdp_total %>%
  transmute(
    country = iso3c,
    year = year,
    GDP = NY.GDP.PCAP.KD
  ) %>%
  filter(!is.na(country), !is.na(year))


# import shares from each importer's perspective, per group-year
shares_c <- trade_hq %>%
  group_by(iso_d, year, iso_o) %>%
  summarise(M = sum(trade_value_pred), .groups = "drop_last") %>%
  group_by(iso_d, year) %>%
  mutate(s = M / sum(M)) %>%                      # s[importer <- supplier]
  ungroup() %>%
  select(importer = iso_d, supplier = iso_o, year, s)

asymmetry_c <- shares_c %>%
  # bring in the reverse share: supplier's dependence on importer
  left_join(
    shares_c %>% select(importer2 = importer, supplier2 = supplier,
                      year, s_rev = s),
    by = c("importer" = "supplier2", "supplier" = "importer2",
           "year")
  ) %>%
  mutate(s_rev = coalesce(s_rev, 0)) %>%          # no reverse flow = 0
  group_by(country = importer, year) %>%
  summarise(
    asym = sum(s * (s - s_rev)),                  # weighted net exposure, [-1,1]
    .groups = "drop"
  )



################################################################################
## 
################################################################################


full_data_c <- dependency_metrics_full_c %>% 
  left_join(gdp_total_clean) %>% 
  full_join(
    DEE_data %>% 
      mutate(
        country = countryname(Country, destination = "iso3c"),
        year = Year
      ) %>% 
      select(country, year,  DEE, DTE, DMSP, DUC, DTI)
  ) %>% 
  left_join(
    asymmetry_c
  ) %>% 
  left_join(
    nets_full,
    by = c("year", "country")
  ) %>% 
  # group_by(year) %>% 
  # mutate(
  #   across(
  #     c(DEE, DUC, DTE, DMSP, DTI, asym),
  #     ~ scale(.x)[,1],
  #     .names = "{.col}_norm"
  #   )
  # ) %>% 
  ungroup() 

full_data_c %>%  write_csv(here::here("data", "full_data_c.csv"))
