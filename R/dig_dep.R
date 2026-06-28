
setwd("D:/2_Modul University/3.Digital networks papers/")
#Import DEE data
# Import the CSV files
dee_data <- read.csv("D:/2_Modul University/Data DEE/Dashboard and Plots DEE/deematerials/DEE_DATA_WTIME.csv")
gdp_capita <- read.csv("D:/2_Modul University/Data DEE/Dashboard and Plots DEE/deematerials/GDP_Capita_2022.csv")
country_info <- read.csv("D:/2_Modul University/Data DEE/Dashboard and Plots DEE/deematerials/Country_Income_Region.csv")

# Apply the str() function to inspect their structure
str(dee_data)
str(gdp_capita)
str(country_info)

# Add Year column to gdp_capita
gdp_capita$Year <- 2022

# Merge gdp_capita with dee_data on Country and Year
dee_merged <- merge(dee_data, gdp_capita, by = c("Country", "Year"), all.x = TRUE)

# Merge country_info with the previously merged dataframe on Country
dee_final <- merge(dee_merged, country_info, by = "Country", all.x = TRUE)

# Check the structure of the final merged data
str(dee_final)

View(dee_final)
unique(dee_final$Country)

str(dee_final)

c(
  "D:/2_Modul University/3.Digital networks papers/all_country_trade_hq_intervals.csv",
  "D:/2_Modul University/3.Digital networks papers/all_country_trade_ot_intervals.csv"
)


# Load necessary library
library(dplyr)

# Import HQ-based trade data
trade_hq <- read.csv("D:/2_Modul University/3.Digital networks papers/all_country_trade_hq_intervals.csv")
unique(trade_hq$category)

# Import OT-based trade data
trade_ot <- read.csv("D:/2_Modul University/3.Digital networks papers/all_country_trade_ot_intervals.csv")

str(dee_final)
View(trade_hq)
str(trade_ot)

unique(trade_hq$category)


# -------------------------
# 4. CHOOSE TRADE DATASET
# -------------------------
trade_use <- trade_hq
# trade_use <- trade_ot

trade_use <- trade_use %>%
  rename(
    country_exporter = iso_o,
    country_importer = iso_d,
    Year = year,
    category_raw = category,
    trade_value = trade_value_pred
  )

# -------------------------
# 5. CLASSIFY INTO 4 GROUPS
# -------------------------
trade_use <- trade_use %>%
  mutate(
    tech_group = case_when(
      category_raw %in% c(
        "cloud-computing",
        "operating-system",
        "web-hosting",
        "file-hosting-service",
        "Cybersecurity",
        "payment-service",
        "data-licensing"
      ) ~ "infrastructure",
      
      category_raw %in% c(
        "online-marketplace",
        "digital-advertising",
        "online-ride-hailing",
        "online-travel-market",
        "online-food-ordering"
      ) ~ "platform",
      
      category_raw %in% c(
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
      
      category_raw %in% c(
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

# =========================================================
# 6. BUILD DEPENDENCE MEASURES USING ALL CATEGORIES
# =========================================================

# -------------------------
# 6A. COUNTRY-YEAR-CATEGORY-TECH TRADE
# -------------------------
imports_tech_full <- trade_use %>%
  group_by(country = country_importer, Year, category_raw, tech_group) %>%
  summarise(M = sum(trade_value, na.rm = TRUE), .groups = "drop")

exports_tech_full <- trade_use %>%
  group_by(country = country_exporter, Year, category_raw, tech_group) %>%
  summarise(X = sum(trade_value, na.rm = TRUE), .groups = "drop")

country_tech_full <- full_join(
  imports_tech_full,
  exports_tech_full,
  by = c("country", "Year", "category_raw", "tech_group")
) %>%
  mutate(
    M = ifelse(is.na(M), 0, M),
    X = ifelse(is.na(X), 0, X)
  )
View(country_tech_full)
# -------------------------
# 6B. TRADE DEPENDENCE
# TD = M / (M + X)
# -------------------------
country_tech_full <- country_tech_full %>%
  mutate(
    TD = ifelse((M + X) > 0, M / (M + X), NA_real_),
    TD = formatC(TD, format = "f", digits = 4)
  )

# -------------------------
# 6C. NET IMPORT DEPENDENCE
# NetDep = (M - X) / GDP
# -------------------------
country_tech_full <- country_tech_full %>%
  left_join(
    dee_final %>% select(country, Year, GDP) %>% distinct(),
    by = c("country", "Year")
  ) %>%
  mutate(
    NetDep = ifelse(!is.na(GDP) & GDP > 0, (M - X) / GDP, NA_real_)
  )

str(trade_use)
str(country_tech_full)


install.packages("WDI")
library(WDI)
library(dplyr)

# Download GDP total (current US$)
gdp_total <- WDI(
  country = "all",
  indicator = "NY.GDP.MKTP.CD",
  start = min(trade_use$Year, na.rm = TRUE),
  end   = max(trade_use$Year, na.rm = TRUE),
  extra = TRUE
)

gdp_total_clean <- gdp_total %>%
  transmute(
    country = iso3c,
    Year = year,
    GDP = NY.GDP.MKTP.CD
  ) %>%
  filter(!is.na(country), !is.na(Year))

# Total trade by country-year
trade_country_year <- trade_use %>%
  group_by(country = country_importer, Year) %>%
  summarise(total_imports = sum(trade_value, na.rm = TRUE), .groups = "drop") %>%
  full_join(
    trade_use %>%
      group_by(country = country_exporter, Year) %>%
      summarise(total_exports = sum(trade_value, na.rm = TRUE), .groups = "drop"),
    by = c("country", "Year")
  ) %>%
  mutate(
    total_imports = coalesce(total_imports, 0),
    total_exports = coalesce(total_exports, 0),
    total_trade = total_imports + total_exports
  )


# Trade / GDP
trade_gdp <- trade_country_year %>%
  left_join(gdp_total_clean, by = c("country", "Year")) %>%
  mutate(
    trade_over_gdp = ifelse(!is.na(GDP) & GDP > 0, total_trade / GDP, NA_real_)
  )


View(trade_gdp)

View(trade_country_year)

#########################################VC@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(stringr)
# -------------------------
# 1. GDP
# -------------------------
gdp_total_clean <- gdp_total %>%
  transmute(
    country = iso3c,
    Year = year,
    GDP = NY.GDP.MKTP.CD
  ) %>%
  filter(!is.na(country), !is.na(Year))

# -------------------------
# 2. Total trade by country-year
# -------------------------
trade_country_year <- trade_use %>%
  group_by(country = country_importer, Year) %>%
  summarise(
    total_imports = sum(trade_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  full_join(
    trade_use %>%
      group_by(country = country_exporter, Year) %>%
      summarise(
        total_exports = sum(trade_value, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("country", "Year")
  ) %>%
  mutate(
    total_imports = coalesce(total_imports, 0),
    total_exports = coalesce(total_exports, 0),
    total_trade = total_imports + total_exports
  )

# -------------------------
# 3. VC by country-year (ALL VC, not early/later)
# -------------------------
VCDATA <- read.csv("D:/2_Modul University/Data DEE 2026/VC_Data_2026/CB_funding_20251003_743821.csv")
ISO3UP  <- read.csv("D:/2_Modul University/Crunchbase/Venture data/final_updated_country_iso3.csv")

iso_manual <- tibble::tribble(
  ~country, ~ISO3,
  "Anguilla", "AIA",
  "Bhutan", "BTN",
  "Brunei Darussalam", "BRN",
  "Burundi", "BDI",
  "Congo, Democratic Republic of", "COD",
  "Cook Islands", "COK",
  "Cape Verde", "CPV",
  "Chad", "TCD",
  "Djibouti", "DJI",
  "Gambia", "GMB",
  "Greenland", "GRL",
  "Grenada", "GRD",
  "Guyana", "GUY",
  "Macao", "MAC",
  "Malawi", "MWI",
  "Mauritania", "MRT",
  "North Korea", "PRK",
  "Northern Mariana Islands", "MNP",
  "Saint Lucia", "LCA",
  "Swaziland", "SWZ",
  "Tajikistan", "TJK",
  "United States Minor Outlying Islands", "UMI"
)

VC_All <- VCDATA %>%
  mutate(
    Year = as.integer(substr(announced_on, 1, 4)),
    money_raised_usd = as.numeric(money_raised_usd),
    country = na_if(str_trim(country), "")
  ) %>%
  filter(Year >= min(trade_use$Year, na.rm = TRUE),
         Year <= max(trade_use$Year, na.rm = TRUE)) %>%
  group_by(country, Year) %>%
  summarise(
    VC_Funding_USD = sum(money_raised_usd, na.rm = TRUE),
    VC_Rounds = n_distinct(id),
    .groups = "drop"
  )

VC_All_ISO3 <- VC_All %>%
  left_join(ISO3UP, by = c("country" = "Country")) %>%
  left_join(iso_manual, by = "country", suffix = c("", "_manual")) %>%
  mutate(ISO3 = coalesce(ISO3, ISO3_manual)) %>%
  select(-ISO3_manual)

# -------------------------
# 4. Trade + GDP + VC
# -------------------------
trade_gdp <- trade_country_year %>%
  left_join(gdp_total_clean, by = c("country", "Year")) %>%
  left_join(
    VC_All_ISO3 %>%
      transmute(
        country = ISO3,
        Year,
        VC_Funding_USD,
        VC_Rounds
      ),
    by = c("country", "Year")
  ) %>%
  mutate(
    trade_over_gdp = ifelse(!is.na(GDP) & GDP > 0, total_trade / GDP, NA_real_),
    VC_Funding_USD = coalesce(VC_Funding_USD, 0),
    VC_Rounds = coalesce(VC_Rounds, 0)
  )

################################################number of firms

library(dplyr)
library(purrr)
library(tidyr)

# -------------------------
# 1. Read and merge all company files
# -------------------------
file_paths <- c(
  "D:/2_Modul University/Data DEE/Indicators/Data_Crunch_Indicator/CRUNCHBASE_company_Asia1.csv",
  "D:/2_Modul University/Data DEE/Indicators/Data_Crunch_Indicator/CRUNCHBASE_company_Asia2.csv",
  "D:/2_Modul University/Data DEE/Indicators/Data_Crunch_Indicator/CRUNCHBASE_company_Europe1_20241222_514009.csv",
  "D:/2_Modul University/Data DEE/Indicators/Data_Crunch_Indicator/CRUNCHBASE_company_Europe2_20241222_335483.csv",
  "D:/2_Modul University/Data DEE/Indicators/Data_Crunch_Indicator/CRUNCHBASE_company_ 3_continents20241222_283264.csv",
  "D:/2_Modul University/Data DEE/Indicators/Data_Crunch_Indicator/CRUNCHBASE_company_20241222_US1_593388.csv",
  "D:/2_Modul University/Data DEE/Indicators/Data_Crunch_Indicator/CRUNCHBASE_company_20241223_US2_514978.csv",
  "D:/2_Modul University/Data DEE/Indicators/Data_Crunch_Indicator/CRUNCHBASE_company_20241223_US3_194276.csv"
)

merged_data <- file_paths %>%
  map_dfr(read.csv, stringsAsFactors = FALSE)

all_firms <- merged_data %>%
  select(id, founded_on, country) %>%
  mutate(
    country = trimws(tolower(country)),
    founded_year = as.integer(substr(founded_on, 1, 4))
  ) %>%
  filter(!is.na(country), country != "")


all_firms <- all_firms %>%
  mutate(
    founded_year = case_when(
      is.na(founded_year) ~ 2016,
      founded_year < 2016 ~ 2016,
      founded_year > 2024 ~ NA_integer_,
      TRUE ~ founded_year
    )
  ) %>%
  filter(!is.na(founded_year))

all_firms_unique <- all_firms %>%
  distinct(id, country, founded_year)

firms_by_year <- all_firms_unique %>%
  group_by(country, founded_year) %>%
  summarise(
    new_cb_firms = n(),
    .groups = "drop"
  )

all_years <- min(trade_gdp$Year, na.rm = TRUE):max(trade_gdp$Year, na.rm = TRUE)

firms_panel <- firms_by_year %>%
  complete(country, founded_year = all_years, fill = list(new_cb_firms = 0)) %>%
  arrange(country, founded_year) %>%
  group_by(country) %>%
  mutate(
    CB_Firms_Cumulative = cumsum(new_cb_firms)
  ) %>%
  ungroup() %>%
  rename(Year = founded_year)


ISO3UP <- read.csv("D:/2_Modul University/Crunchbase/Venture data/final_updated_country_iso3.csv",
                   stringsAsFactors = FALSE)

ISO3UP <- ISO3UP %>%
  mutate(country = trimws(tolower(Country))) %>%
  select(country, ISO3)

iso_manual <- tibble::tribble(
  ~country, ~ISO3,
  "åland islands", "ALA",
  "anguilla", "AIA",
  "antigua and barbuda", "ATG",
  "bhutan", "BTN",
  "brunei darussalam", "BRN",
  "burundi", "BDI",
  "cape verde", "CPV",
  "central african republic", "CAF",
  "chad", "TCD",
  "cocos (keeling) islands", "CCK",
  "comoros", "COM",
  "congo, democratic republic of", "COD",
  "cook islands", "COK",
  "cuba", "CUB",
  "djibouti", "DJI",
  "equatorial guinea", "GNQ",
  "eritrea", "ERI",
  "falkland islands", "FLK",
  "french guiana", "GUF",
  "french polynesia", "PYF",
  "gambia", "GMB",
  "greenland", "GRL",
  "grenada", "GRD",
  "guadeloupe", "GLP",
  "guam", "GUM",
  "guinea-bissau", "GNB",
  "guyana", "GUY",
  "macao", "MAC",
  "malawi", "MWI",
  "maldives", "MDV",
  "martinique", "MTQ",
  "mauritania", "MRT",
  "mayotte", "MYT",
  "micronesia, federated states of", "FSM",
  "montserrat", "MSR",
  "norfolk island", "NFK",
  "north korea", "PRK",
  "northern mariana islands", "MNP",
  "palau", "PLW",
  "saint-martin (france)", "MAF",
  "saint barthélemy", "BLM",
  "saint lucia", "LCA",
  "san marino", "SMR",
  "sao tome and principe", "STP",
  "solomon islands", "SLB",
  "st. pierre and miquelon", "SPM",
  "suriname", "SUR",
  "svalbard and jan mayen islands", "SJM",
  "swaziland", "SWZ",
  "tajikistan", "TJK",
  "timor-leste", "TLS",
  "tonga", "TON",
  "turkmenistan", "TKM",
  "turks and caicos islands", "TCA",
  "tuvalu", "TUV",
  "united states minor outlying islands", "UMI",
  "western sahara", "ESH"
)

firms_panel_iso3 <- firms_panel %>%
  left_join(ISO3UP, by = "country") %>%
  left_join(iso_manual, by = "country", suffix = c("", "_manual")) %>%
  mutate(ISO3 = coalesce(ISO3, ISO3_manual)) %>%
  select(country, ISO3, Year, new_cb_firms, CB_Firms_Cumulative)


missing_cb_iso3 <- firms_panel_iso3 %>%
  filter(is.na(ISO3)) %>%
  distinct(country)

missing_cb_iso3

trade_gdp <- trade_gdp %>%
  left_join(
    firms_panel_iso3 %>%
      transmute(
        country = ISO3,
        Year,
        new_cb_firms,
        CB_Firms_Cumulative
      ),
    by = c("country", "Year")
  ) %>%
  mutate(
    new_cb_firms = coalesce(new_cb_firms, 0),
    CB_Firms_Cumulative = coalesce(CB_Firms_Cumulative, 0)
  )


trade_gdp <- trade_gdp %>%
  mutate(
    VC_over_GDP = ifelse(!is.na(GDP) & GDP > 0,
                         VC_Funding_USD / GDP,
                         NA_real_),
    
    VC_Rounds_over_CB_Firms = ifelse(CB_Firms_Cumulative > 0,
                                     VC_Rounds / CB_Firms_Cumulative,
                                     NA_real_),
    
    VC_Funding_over_CB_Firms = ifelse(CB_Firms_Cumulative > 0,
                                      VC_Funding_USD / CB_Firms_Cumulative,
                                      NA_real_)
  )


str(country_tech_full)

str(trade_gdp)
str(trade_use)
##############################Dependency at different levels
# -------------------------
trade_use <- trade_use %>%
  mutate(
    tech_group = case_when(
      category_raw %in% c(
        "cloud-computing",
        "operating-system",
        "web-hosting",
        "file-hosting-service",
        "Cybersecurity",
        "payment-service",
        "data-licensing"
      ) ~ "infrastructure",
      
      category_raw %in% c(
        "online-marketplace",
        "digital-advertising",
        "online-ride-hailing",
        "online-travel-market",
        "online-food-ordering"
      ) ~ "platform",
      
      category_raw %in% c(
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
      
      category_raw %in% c(
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

# Optional: if you want the grouped dataset to contain ONLY the 4 core groups
trade_use_4groups <- trade_use %>%
  filter(tech_group %in% c("infrastructure", "platform", "productive", "user_facing"))

# -------------------------
# 2. GDP PANEL FROM trade_gdp
# -------------------------
gdp_panel <- trade_gdp %>%
  select(country, Year, GDP) %>%
  distinct()

# -------------------------
# 3. FUNCTION: DEPENDENCY MEASURES
# TD     = M / (M + X)
# NetDep = (M - X) / GDP
# -------------------------
calc_dependency <- function(data, group_vars, gdp_data) {
  
  imports <- data %>%
    group_by(across(all_of(c("country_importer", "Year", group_vars)))) %>%
    summarise(
      M = sum(trade_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(country = country_importer)
  
  exports <- data %>%
    group_by(across(all_of(c("country_exporter", "Year", group_vars)))) %>%
    summarise(
      X = sum(trade_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(country = country_exporter)
  
  full_join(
    imports,
    exports,
    by = c("country", "Year", group_vars)
  ) %>%
    mutate(
      M = coalesce(M, 0),
      X = coalesce(X, 0)
    ) %>%
    left_join(gdp_data, by = c("country", "Year")) %>%
    mutate(
      TD = ifelse((M + X) > 0, M / (M + X), NA_real_),
      TD = round(TD, 4),
      NetDep = ifelse(!is.na(GDP) & GDP > 0, (M - X) / GDP, NA_real_)
    )
}

# -------------------------
# 4. FUNCTION: HHI
# HHI based on import concentration across supplier countries
# HHI = sum(share_supplier^2)
# -------------------------
calc_hhi <- function(data, group_vars) {
  
  data %>%
    group_by(across(all_of(c("country_importer", "Year", group_vars, "country_exporter")))) %>%
    summarise(
      import_val = sum(trade_value, na.rm = TRUE),
    ) %>%
    rename(
      country = country_importer,
      supplier = country_exporter
    ) %>%
    group_by(across(all_of(c("country", "Year", group_vars)))) %>%
    mutate(
      total_imports_hhi = sum(import_val, na.rm = TRUE),
      share_supplier = ifelse(total_imports_hhi > 0, import_val / total_imports_hhi, NA_real_)
    ) %>%
    summarise(
      HHI = sum(share_supplier^2, na.rm = TRUE),
      n_suppliers = n_distinct(supplier),
      total_imports_hhi = first(total_imports_hhi),
      .groups = "drop"
    )
}

# =========================================================
# 5. DATASET 1: CATEGORY-RAW LEVEL
# one row per country-Year-category_raw-tech_group
# =========================================================
dep_category <- calc_dependency(
  data = trade_use,
  group_vars = c("category_raw", "tech_group"),
  gdp_data = gdp_panel
)

hhi_category <- calc_hhi(
  data = trade_use,
  group_vars = c("category_raw", "tech_group")
)

dependency_hhi_category <- dep_category %>%
  left_join(
    hhi_category,
    by = c("country", "Year", "category_raw", "tech_group")
  ) %>%
  arrange(country, Year, tech_group, category_raw)

dependency_hhi_category

# =========================================================
# 6. DATASET 2: TECH-GROUP LEVEL
# one row per country-Year-tech_group
# uses only the 4 main groups
# =========================================================
dep_group <- calc_dependency(
  data = trade_use_4groups,
  group_vars = c("tech_group"),
  gdp_data = gdp_panel
)

hhi_group <- calc_hhi(
  data = trade_use_4groups,
  group_vars = c("tech_group")
)

dependency_hhi_group <- dep_group %>%
  left_join(
    hhi_group,
    by = c("country", "Year", "tech_group")
  ) %>%
  arrange(country, Year, tech_group)

dependency_hhi_group

# =========================================================
# 7. DATASET 3: TOTAL DIGITAL LEVEL
# one row per country-Year across all technologies
# uses all digital trade in trade_use
# =========================================================
dep_total <- calc_dependency(
  data = trade_use,
  group_vars = character(0),
  gdp_data = gdp_panel
)

hhi_total <- calc_hhi(
  data = trade_use,
  group_vars = character(0)
)

dependency_hhi_total <- dep_total %>%
  left_join(
    hhi_total,
    by = c("country", "Year")
  ) %>%
  mutate(
    category_raw = "Total",
    tech_group = "Total"
  ) %>%
  select(
    country, Year, category_raw, tech_group,
    M, X, GDP, TD, NetDep, HHI, n_suppliers, total_imports_hhi
  ) %>%
  arrange(country, Year)



str(dependency_hhi_category)
str(dependency_hhi_group)
str(dependency_hhi_total)
str(trade_gdp)

###############################################################################

library(dplyr)

# =========================================================
# 1. MERGE trade_gdp WITH dependency_hhi_total
#    Keep only the dependency/concentration variables you need
#    Drop M, X, duplicate GDP, category_raw, tech_group
# =========================================================
trade_gdp_dep <- trade_gdp %>%
  left_join(
    dependency_hhi_total %>%
      select(country, Year, TD, NetDep, HHI, n_suppliers),
    by = c("country", "Year")
  )

# =========================================================
# 2. MANUAL COUNTRY CLASSIFICATIONS (ISO3)
#    One country can belong to multiple groups
#    These are hard-coded vectors, no package used
# =========================================================

# --- European Union (current members in your sample)
eu_countries <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA",
  "DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD",
  "POL","PRT","ROU","SVK","SVN","ESP","SWE"
)

# --- OECD members in your sample
oecd_countries <- c(
  "AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST","FIN","FRA",
  "DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN","KOR","LVA","LTU","LUX",
  "MEX","NLD","NZL","NOR","POL","PRT","SVK","SVN","ESP","SWE","CHE","TUR",
  "GBR","USA"
)

# --- All Europe (broad geographic Europe, not only EU/OECD)
all_europe_countries <- c(
  "ALB","AND","AUT","BEL","BGR","BIH","BLR","CHE","CYP","CZE","DEU","DNK",
  "ESP","EST","FIN","FRA","FRO","GBR","GRC","HRV","HUN","IRL","ISL","ITA",
  "LUX","LVA","LTU","MDA","MKD","MLT","NLD","NOR","POL","PRT","ROU","RUS",
  "SMR","SRB","SVK","SVN","SWE","TUR","UKR"
)

# --- Latin America & Caribbean
latam_countries <- c(
  "ATG","ARG","BHS","BLZ","BOL","BRA","BRB","CHL","COL","CRI","CUB","DMA",
  "DOM","ECU","GRD","GTM","GUY","HND","HTI","JAM","KNA","LCA","MEX","NIC",
  "PAN","PER","PRY","SLV","SUR","TTO","URY","VCT","VEN"
)

# --- Africa
africa_countries <- c(
  "AGO","BDI","BEN","BFA","BWA","CAF","CIV","CMR","COG","COM","CPV","DJI",
  "DZA","EGY","ETH","GAB","GHA","GIN","GMB","GNB","GNQ","KEN","LBR","LBY",
  "LSO","MAR","MDG","MLI","MOZ","MRT","MUS","MWI","NAM","NER","NGA","RWA",
  "SDN","SEN","SLE","SOM","STP","SWZ","SYC","TCD","TGO","TUN","TZA","UGA",
  "ZAF","ZWE"
)

# --- Asia (including Middle East + Central Asia + East/South/Southeast Asia)
asia_countries <- c(
  "AFG","ARE","ARM","AZE","BGD","BHR","BRN","BTN","CHN","FSM","GEO","HKG",
  "IDN","IND","IRN","IRQ","ISR","JOR","JPN","KAZ","KGZ","KHM","KIR","KOR",
  "KWT","LAO","LBN","LKA","MAC","MDV","MMR","MNG","MYS","NPL","OMN","PAK",
  "PHL","PNG","QAT","SAU","SGP","SLB","SYR","THA","TJK","TKM","TON","TUV",
  "TWN","UZB","VNM","VUT","WSM","YEM"
)

# --- Special single-country flags
usa_country   <- "USA"
china_country <- "CHN"

# =========================================================
# 3. FUNCTION TO BUILD ONE MULTI-LABEL STRING PER COUNTRY
# =========================================================
make_country_group <- function(iso3) {
  groups <- character(0)
  
  if (iso3 %in% eu_countries)         groups <- c(groups, "European Union")
  if (iso3 %in% oecd_countries)       groups <- c(groups, "OECD")
  if (iso3 %in% all_europe_countries) groups <- c(groups, "ALL_Europe")
  if (!(iso3 %in% oecd_countries))    groups <- c(groups, "Non_OECD")
  if (iso3 == usa_country)            groups <- c(groups, "USA")
  if (iso3 == china_country)          groups <- c(groups, "China")
  if (iso3 %in% latam_countries)      groups <- c(groups, "LatAm")
  if (iso3 %in% africa_countries)     groups <- c(groups, "Africa")
  if (iso3 %in% asia_countries)       groups <- c(groups, "Asia")
  
  if (length(groups) == 0) {
    return(NA_character_)
  } else {
    return(paste(unique(groups), collapse = ", "))
  }
}

# =========================================================
# 4. ADD Country_group COLUMN
# =========================================================
trade_gdp_dep <- trade_gdp_dep %>%
  mutate(
    Country_group = vapply(country, make_country_group, character(1))
  )

View(trade_gdp_dep)
str(dependency_hhi_category)
str(dependency_hhi_group)
str(dependency_hhi_total)

# 1. trade_gdp with dependency + country groups
write.csv(
  trade_gdp_dep,
  "D:/2_Modul University/3.Digital networks papers/trade_gdp_dep.csv",
  row.names = FALSE
)

# 2. Category-level dependency + HHI
write.csv(
  dependency_hhi_category,
  "D:/2_Modul University/3.Digital networks papers/dependency_hhi_category.csv",
  row.names = FALSE
)

# 3. Tech-group-level dependency + HHI
write.csv(
  dependency_hhi_group,
  "D:/2_Modul University/3.Digital networks papers/dependency_hhi_group.csv",
  row.names = FALSE
)

# 4. Total-level dependency + HHI
write.csv(
  dependency_hhi_total,
  "D:/2_Modul University/3.Digital networks papers/dependency_hhi_total.csv",
  row.names = FALSE
)

#######################################################Descriptive statistics

str(trade_gdp_dep)


library(dplyr)
library(tidyr)
library(ggplot2)

# =========================================================
# 1. SELECT ONLY NUMERIC VARIABLES FOR DESCRIPTIVES
# =========================================================
num_vars <- trade_gdp_dep %>%
  select(where(is.numeric))

# =========================================================
# FUNCTION: DESCRIPTIVE STATS
# =========================================================
desc_stats <- function(df) {
  df %>%
    summarise(across(everything(),
                     list(
                       mean = ~mean(.x, na.rm = TRUE),
                       sd = ~sd(.x, na.rm = TRUE),
                       min = ~min(.x, na.rm = TRUE),
                       q1 = ~quantile(.x, 0.25, na.rm = TRUE),
                       median = ~median(.x, na.rm = TRUE),
                       q3 = ~quantile(.x, 0.75, na.rm = TRUE),
                       max = ~max(.x, na.rm = TRUE),
                       n = ~sum(!is.na(.x))
                     ),
                     .names = "{.col}_{.fn}"
    )) %>%
    pivot_longer(everything(),
                 names_to = c("Variable", ".value"),
                 names_sep = "_")
}

# =========================================================
# 2. DESCRIPTIVE – FULL DATASET (ALL YEARS)
# =========================================================
desc_full <- desc_stats(num_vars)

write.csv(
  desc_full,
  "D:/2_Modul University/3.Digital networks papers/desc_full.csv",
  row.names = FALSE
)

# =========================================================
# 3. DESCRIPTIVE – BY REGION (ALL YEARS)
# =========================================================
desc_region <- trade_gdp_dep %>%
  group_by(Country_group) %>%
  summarise(across(where(is.numeric),
                   list(
                     mean = ~mean(.x, na.rm = TRUE),
                     sd = ~sd(.x, na.rm = TRUE),
                     min = ~min(.x, na.rm = TRUE),
                     q1 = ~quantile(.x, 0.25, na.rm = TRUE),
                     median = ~median(.x, na.rm = TRUE),
                     q3 = ~quantile(.x, 0.75, na.rm = TRUE),
                     max = ~max(.x, na.rm = TRUE),
                     n = ~sum(!is.na(.x))
                   ),
                   .names = "{.col}_{.fn}"
  ))

write.csv(
  desc_region,
  "D:/2_Modul University/3.Digital networks papers/desc_region.csv",
  row.names = FALSE
)

# =========================================================
# 4. DESCRIPTIVE – FULL DATASET BY YEAR
# =========================================================
desc_year <- trade_gdp_dep %>%
  group_by(Year) %>%
  summarise(across(where(is.numeric),
                   list(
                     mean = ~mean(.x, na.rm = TRUE),
                     sd = ~sd(.x, na.rm = TRUE),
                     min = ~min(.x, na.rm = TRUE),
                     q1 = ~quantile(.x, 0.25, na.rm = TRUE),
                     median = ~median(.x, na.rm = TRUE),
                     q3 = ~quantile(.x, 0.75, na.rm = TRUE),
                     max = ~max(.x, na.rm = TRUE),
                     n = ~sum(!is.na(.x))
                   ),
                   .names = "{.col}_{.fn}"
  ))

write.csv(
  desc_year,
  "D:/2_Modul University/3.Digital networks papers/desc_year.csv",
  row.names = FALSE
)

# =========================================================
# 5. DESCRIPTIVE – BY REGION AND YEAR
# =========================================================
desc_region_year <- trade_gdp_dep %>%
  group_by(Country_group, Year) %>%
  summarise(across(where(is.numeric),
                   list(
                     mean = ~mean(.x, na.rm = TRUE),
                     sd = ~sd(.x, na.rm = TRUE),
                     min = ~min(.x, na.rm = TRUE),
                     q1 = ~quantile(.x, 0.25, na.rm = TRUE),
                     median = ~median(.x, na.rm = TRUE),
                     q3 = ~quantile(.x, 0.75, na.rm = TRUE),
                     max = ~max(.x, na.rm = TRUE),
                     n = ~sum(!is.na(.x))
                   ),
                   .names = "{.col}_{.fn}"
  ))

write.csv(
  desc_region_year,
  "D:/2_Modul University/3.Digital networks papers/desc_region_year.csv",
  row.names = FALSE
)

# =========================================================
# 6. EVOLUTION PLOTS (2016–2021)
# =========================================================

# Select key variables to plot (you can adjust)
vars_to_plot <- c("TD", "NetDep", "HHI", "n_suppliers", "trade_over_gdp")

# -------------------------
# 6A. FULL DATASET EVOLUTION
# -------------------------
for (var in vars_to_plot) {
  
  p <- trade_gdp_dep %>%
    group_by(Year) %>%
    summarise(value = mean(.data[[var]], na.rm = TRUE)) %>%
    ggplot(aes(x = Year, y = value)) +
    geom_line() +
    geom_point() +
    labs(
      title = paste("Evolution of", var, "(Global Average)"),
      y = var
    ) +
    theme_minimal()
  
  ggsave(
    filename = paste0("D:/2_Modul University/3.Digital networks papers/plot_", var, "_global.png"),
    plot = p,
    width = 7,
    height = 5
  )
}

# -------------------------
# 6B. EVOLUTION BY REGION
# -------------------------
for (var in vars_to_plot) {
  
  p <- trade_gdp_dep %>%
    group_by(Country_group, Year) %>%
    summarise(value = mean(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = Year, y = value, linetype = Country_group)) +
    geom_line() +
    labs(
      title = paste("Evolution of", var, "by Region"),
      y = var
    ) +
    theme_minimal()
  
  ggsave(
    filename = paste0("D:/2_Modul University/3.Digital networks papers/plot_", var, "_regions.png"),
    plot = p,
    width = 8,
    height = 6
  )
}

library(dplyr)
library(tidyr)
library(ggplot2)

# =========================================================
# 1. CREATE SEPARATE GROUP DUMMIES
# =========================================================
trade_gdp_dep_groups <- trade_gdp_dep %>%
  mutate(
    European_Union = ifelse(grepl("European Union", Country_group, fixed = TRUE), 1, 0),
    OECD           = ifelse(grepl("OECD", Country_group, fixed = TRUE) & !grepl("Non_OECD", Country_group, fixed = TRUE), 1, 0),
    ALL_Europe     = ifelse(grepl("ALL_Europe", Country_group, fixed = TRUE), 1, 0),
    Non_OECD       = ifelse(grepl("Non_OECD", Country_group, fixed = TRUE), 1, 0),
    USA_group      = ifelse(grepl("USA", Country_group, fixed = TRUE), 1, 0),
    China_group    = ifelse(grepl("China", Country_group, fixed = TRUE), 1, 0),
    LatAm          = ifelse(grepl("LatAm", Country_group, fixed = TRUE), 1, 0),
    Africa         = ifelse(grepl("Africa", Country_group, fixed = TRUE), 1, 0),
    Asia           = ifelse(grepl("Asia", Country_group, fixed = TRUE), 1, 0)
  )

# =========================================================
# 2. RESHAPE TO LONG FORMAT: ONE ROW PER COUNTRY-YEAR-GROUP
# =========================================================
trade_gdp_dep_long <- trade_gdp_dep_groups %>%
  pivot_longer(
    cols = c(European_Union, OECD, ALL_Europe, Non_OECD, USA_group, China_group, LatAm, Africa, Asia),
    names_to = "Region",
    values_to = "in_group"
  ) %>%
  filter(in_group == 1) %>%
  mutate(
    Region = recode(
      Region,
      European_Union = "European Union",
      OECD = "OECD",
      ALL_Europe = "ALL_Europe",
      Non_OECD = "Non_OECD",
      USA_group = "USA",
      China_group = "China",
      LatAm = "LatAm",
      Africa = "Africa",
      Asia = "Asia"
    )
  )

# =========================================================
# 3. DESCRIPTIVE STATS BY SEPARATE REGION
# =========================================================
desc_region_separate <- trade_gdp_dep_long %>%
  group_by(Region) %>%
  summarise(across(where(is.numeric),
                   list(
                     mean = ~mean(.x, na.rm = TRUE),
                     sd = ~sd(.x, na.rm = TRUE),
                     min = ~min(.x, na.rm = TRUE),
                     q1 = ~quantile(.x, 0.25, na.rm = TRUE),
                     median = ~median(.x, na.rm = TRUE),
                     q3 = ~quantile(.x, 0.75, na.rm = TRUE),
                     max = ~max(.x, na.rm = TRUE),
                     n = ~sum(!is.na(.x))
                   ),
                   .names = "{.col}_{.fn}"
  ))

write.csv(
  desc_region_separate,
  "D:/2_Modul University/3.Digital networks papers/desc_region_separate.csv",
  row.names = FALSE
)

# =========================================================
# 4. DESCRIPTIVE STATS BY SEPARATE REGION AND YEAR
# =========================================================
desc_region_year_separate <- trade_gdp_dep_long %>%
  group_by(Region, Year) %>%
  summarise(across(where(is.numeric),
                   list(
                     mean = ~mean(.x, na.rm = TRUE),
                     sd = ~sd(.x, na.rm = TRUE),
                     min = ~min(.x, na.rm = TRUE),
                     q1 = ~quantile(.x, 0.25, na.rm = TRUE),
                     median = ~median(.x, na.rm = TRUE),
                     q3 = ~quantile(.x, 0.75, na.rm = TRUE),
                     max = ~max(.x, 0.75, na.rm = TRUE),
                     n = ~sum(!is.na(.x))
                   ),
                   .names = "{.col}_{.fn}"
  ))

write.csv(
  desc_region_year_separate,
  "D:/2_Modul University/3.Digital networks papers/desc_region_year_separate.csv",
  row.names = FALSE
)

# =========================================================
# 5. EVOLUTION PLOTS FOR ENTIRE DATASET
# =========================================================
vars_to_plot <- c("TD", "NetDep", "HHI", "n_suppliers", "trade_over_gdp")

for (var in vars_to_plot) {
  
  p_global <- trade_gdp_dep %>%
    group_by(Year) %>%
    summarise(value = mean(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = Year, y = value)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = paste("Evolution of", var, "- Full Dataset"),
      x = "Year",
      y = var
    ) +
    theme_minimal()
  
  ggsave(
    filename = paste0("D:/2_Modul University/3.Digital networks papers/plot_", var, "_global.png"),
    plot = p_global,
    width = 8,
    height = 5
  )
}

# =========================================================
# 6. EVOLUTION PLOTS BY SEPARATE REGION
# =========================================================
for (var in vars_to_plot) {
  
  p_region <- trade_gdp_dep_long %>%
    group_by(Region, Year) %>%
    summarise(value = mean(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = Year, y = value, color = Region)) +
    geom_line(size = 1) +
    geom_point(size = 1.8) +
    labs(
      title = paste("Evolution of", var, "by Region"),
      x = "Year",
      y = var,
      color = "Region"
    ) +
    theme_minimal()
  
  ggsave(
    filename = paste0("D:/2_Modul University/3.Digital networks papers/plot_", var, "_by_region.png"),
    plot = p_region,
    width = 9,
    height = 6
  )
}

# =========================================================
# 7. OPTIONAL: CHECK
# =========================================================
View(trade_gdp_dep_long)
str(trade_gdp_dep_long)
unique(trade_gdp_dep_long$Region)




str(dependency_hhi_group)
str(trade_gdp_dep)
str(dependency_hhi_category)


# Packages
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)
library(purrr)

# =========================
# 1. Build country-group lookup from trade_gdp_dep
# =========================

# Keep one mapping per country, even if repeated over years
country_group_lookup <- trade_gdp_dep %>%
  select(country, Country_group) %>%
  distinct() %>%
  filter(!is.na(Country_group), Country_group != "")

# Optional check:
# See whether any country has inconsistent group assignments
country_group_check <- country_group_lookup %>%
  group_by(country) %>%
  summarise(n_groups_strings = n_distinct(Country_group), .groups = "drop") %>%
  filter(n_groups_strings > 1)

if (nrow(country_group_check) > 0) {
  warning("Some countries have more than one Country_group string in trade_gdp_dep. The code will merge all listed groups for each country.")
}

# Expand comma-separated groups into long format, clean spaces
country_groups_long <- country_group_lookup %>%
  mutate(Country_group = str_split(Country_group, ",")) %>%
  unnest(Country_group) %>%
  mutate(Country_group = str_trim(Country_group)) %>%
  filter(Country_group != "") %>%
  distinct(country, Country_group)

# Convert to dummy columns (1/0)
country_group_dummies <- country_groups_long %>%
  mutate(value = 1L) %>%
  pivot_wider(
    names_from = Country_group,
    values_from = value,
    values_fill = 0
  )

# Make column names safe for Excel/R
names(country_group_dummies) <- names(country_group_dummies) %>%
  make.names(unique = TRUE)

# =========================
# 2. Helper function to add dummy columns to any table
# =========================

add_country_groups <- function(df, lookup_df) {
  df %>%
    left_join(lookup_df, by = "country")
}

# =========================
# 3. Apply to the three datasets
# =========================

# Country-level file
trade_gdp_dep_country_level <- trade_gdp_dep %>%
  select(-Country_group) %>%   # remove old comma-separated column
  add_country_groups(country_group_dummies)

# Tech-level file
dependency_hhi_group_tech_level <- dependency_hhi_group %>%
  add_country_groups(country_group_dummies)

# Tech-group-level / category-level file
dependency_hhi_category_tech_group_level <- dependency_hhi_category %>%
  add_country_groups(country_group_dummies)

# =========================
# 4. Save as Excel files
# =========================

write_xlsx(trade_gdp_dep_country_level, "country_level.xlsx")
write_xlsx(dependency_hhi_group_tech_level, "raw_tech_level.xlsx")
write_xlsx(dependency_hhi_category_tech_group_level, "tech_group_level.xlsx")



n_distinct(dependency_hhi_category_tech_group_level$category_raw)



str(dependency_hhi_category_tech_group_level) 

dependency_hhi_category_tech_group_level %>%
  group_by(Year) %>%
  summarise(
    n_techs = n_distinct(category_raw),
    .groups = "drop"
  )

# #############################################################################
# #############################################################################
# ##                                                                         ##
# ##   PAPER PIPELINE: DCI (Finn) · EEDR · TVI · TABLES · MODELS · CLUSTERS   ##
# ##   Appended after the "DEE and continuation" marker.                     ##
# ##                                                                         ##
# ##   This block is SELF-CONTAINED. It reuses objects already built above:  ##
# ##     trade_use                 (exporter/importer/Year/category_raw/     ##
# ##                                 tech_group/trade_value)                 ##
# ##     trade_gdp_dep             (country-Year: TD, HHI, trade_over_gdp,    ##
# ##                                 GDP, Country_group, ...)                 ##
# ##     dependency_hhi_group      (country-Year-tech_group: TD, HHI)        ##
# ##     dee_final                 (country-Year: DEE index + components)    ##
# ##     eu_countries              (ISO3 vector defined earlier)             ##
# ##                                                                         ##
# ##   The downstream "old" exploratory code further below (category HHI,    ##
# ##   DTE placeholder regressions, KET network) can be deleted - this       ##
# ##   block supersedes it.                                                  ##
# #############################################################################
# #############################################################################

# install.packages(c("fixest","writexl","tibble","igraph","ggplot2"))  # run once if missing
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(purrr)
library(writexl)
library(fixest)
library(igraph)
library(ggplot2)

# =========================================================
# 0. CONFIG  ===  SET THESE TO YOUR REAL COLUMN NAMES  ====
# =========================================================
# Inspect what is actually in dee_final and set the names below.
print(names(dee_final))

# --- Main dependent variable: the DEE / Digital Entrepreneurial Ecosystem index
DEE_var <- "DEE"          # <-- CHANGE to the exact DEE column name in dee_final

# --- DEE sub-components used for the robustness models (Attack 1 fix).
#     Set to the real component column names; leave as character(0) to skip.
DEE_components <- c()     # e.g. c("Entrepreneurial_Activity","Digital_Infrastructure","Platforms")

# --- The FOUR DEE PILLARS used as SEPARATE outcomes (the core redesign).
#     Names on the left are labels used in output; values on the right MUST be
#     the exact column names in dee_final. Edit the right-hand side.
DEE_pillars <- c(
  DTE  = "DTE",     # Digital Technology Entrepreneurship
  DMSP = "DMSP",    # Digital Multi-Sided Platforms / market-support
  DUC  = "DUC",     # Digital User Conditions / adoption
  DTI  = "DTI"      # Digital Technology Infrastructure
)

# --- GDP per capita column (used as a control). If you only have GDP level,
#     set GDPpc_var <- NA and the code will build a rough per-capita proxy if
#     Population exists; otherwise it falls back to log(GDP).
GDPpc_var <- "GDP_per_capita"   # <-- CHANGE if named differently (or NA)

# --- Direction of "Openness" inside the Technology Vulnerability Index.
#     TRUE  = openness is PROTECTIVE (more connected/diversified -> inverted)
#     FALSE = openness is a RISK (more exposed -> not inverted)
OPENNESS_PROTECTIVE <- TRUE

# =========================================================
# 1. CORE FUNCTION: DIGITAL CYCLING INDEX (DCI) + FCI
#    Finn Cycling Index adapted to a digital-trade network.
#
#    Nodes      = countries.
#    Z[i,j]     = digital trade flow exporter i -> importer j.
#    x[j]       = total digital throughput of j (exports_j + imports_j),
#                 used as the "total output" proxy. Because column sums of A
#                 = imports_j / (imports_j + exports_j) < 1, (I - A) is
#                 invertible (no singularity).
#    A[i,j]     = Z[i,j] / x[j]     (Braun a_ij = w_ij / x_j)
#    L          = (I - A)^-1        (Leontief inverse, all direct + indirect)
#    lhat_i     = (L[i,i] - 1)/L[i,i]   (node cycling index = "DCI_i")
#    FCI        = sum_i lhat_i x_i / sum_i x_i   (economy-wide scalar)
#
#    NOTE: the bilateral data has no diagonal (no domestic digital production),
#    so the diagonal of Z is 0 and DCI captures MULTILATERAL round-trip
#    embeddedness (i -> j -> ... -> i), not domestic recirculation. This is the
#    international-network reading of the Finn index; see paper limitations.
# =========================================================
compute_dci_one <- function(flows, nodes, domestic = NULL) {
  # flows    : data.frame with country_exporter, country_importer, w (cross-border)
  # domestic : OPTIONAL named numeric vector (names = node ISO3) of within-country
  #            self-supply for this tech/year. If supplied, it fills the diagonal
  #            Z[i,i] and DCI then captures DOMESTIC recirculation / self-supply
  #            (a sovereignty reading) on top of multilateral round-trips.
  #            Build it from data on the SAME scale as trade_value, e.g.
  #              Z_ii = domestic_market_i - imports_i, or
  #              Z_ii = domestic_production_i - exports_i.
  flows <- flows %>%
    filter(country_exporter %in% nodes,
           country_importer %in% nodes,
           country_exporter != country_importer,
           w > 0)
  n <- length(nodes)
  if (n < 3 || nrow(flows) == 0) {
    return(tibble(country = nodes, DCI = NA_real_, throughput = 0,
                  FCI_economywide = NA_real_))
  }
  Z <- matrix(0, n, n, dimnames = list(nodes, nodes))
  Z[cbind(match(flows$country_exporter, nodes),
          match(flows$country_importer, nodes))] <- flows$w
  # ---- optional domestic diagonal (within-country self-supply) ----
  if (!is.null(domestic)) {
    d <- domestic[nodes]
    d[is.na(d)] <- 0
    d[d < 0]    <- 0          # negative self-supply is meaningless -> 0
    diag(Z)     <- d
  }
  out_i <- rowSums(Z)                 # gross sales (incl. domestic)
  in_j  <- colSums(Z)                 # gross purchases (incl. domestic)
  x     <- out_i + in_j - diag(Z)     # throughput, diagonal counted once
  xsafe <- ifelse(x > 0, x, 1)
  A     <- sweep(Z, 2, xsafe, "/")    # column sums < 1 -> (I - A) invertible
  L     <- tryCatch(solve(diag(n) - A), error = function(e) NULL)
  if (is.null(L)) {
    return(tibble(country = nodes, DCI = NA_real_, throughput = x,
                  FCI_economywide = NA_real_))
  }
  dL   <- diag(L)
  lhat <- (dL - 1) / dL
  lhat[!is.finite(lhat)] <- 0
  lhat[lhat < 0] <- 0
  fci  <- if (sum(x) > 0) sum(lhat * x) / sum(x) else NA_real_
  tibble(country = nodes, DCI = as.numeric(lhat),
         throughput = as.numeric(x), FCI_economywide = fci)
}

# Wrapper: run compute_dci_one for every Year (optionally restricting nodes,
# e.g. to EU-27, and optionally pre-filtered to a tech group).
compute_dci <- function(trade_df, node_universe = NULL, domestic_df = NULL) {
  # domestic_df : OPTIONAL data.frame with columns country, Year, w_domestic.
  #   Leave NULL (default) for the cross-border-only DCI. Template to build it:
  #
  #   domestic_df <- domestic_market %>%            # your domestic market sizes
  #     left_join(imports_by_country_year, by = c("country","Year")) %>%
  #     transmute(country, Year,
  #               w_domestic = pmax(market_size - imports, 0))
  #
  years <- sort(unique(trade_df$Year))
  purrr::map_dfr(years, function(yr) {
    d <- trade_df %>% filter(Year == yr)
    fl <- d %>%
      group_by(country_exporter, country_importer) %>%
      summarise(w = sum(trade_value, na.rm = TRUE), .groups = "drop")
    nodes <- if (is.null(node_universe)) {
      sort(unique(c(fl$country_exporter, fl$country_importer)))
    } else {
      sort(node_universe)
    }
    dom <- NULL
    if (!is.null(domestic_df)) {
      dd  <- domestic_df %>% filter(Year == yr)
      dom <- setNames(dd$w_domestic, dd$country)
    }
    compute_dci_one(fl, nodes, domestic = dom) %>% mutate(Year = yr)
  })
}

# =========================================================
# 2. GLOBAL DCI  (all countries, full digital network)
# =========================================================
dci_global <- compute_dci(trade_use) %>%
  rename(DCI_Global = DCI,
         throughput_Global = throughput,
         FCI_Global = FCI_economywide)

# Economy-wide FCI per year (one scalar per year)
fci_global_year <- dci_global %>%
  distinct(Year, FCI_Global)
print(fci_global_year)

# =========================================================
# 3. PER-TECH-GROUP DCI  (one network per tech group per year)
# =========================================================
core_groups <- c("infrastructure", "platform", "productive", "user_facing")

dci_group <- purrr::map_dfr(core_groups, function(g) {
  compute_dci(trade_use %>% filter(tech_group == g)) %>%
    mutate(tech_group = g)
}) %>%
  rename(DCI_group = DCI)

# =========================================================
# 4. EUROPEAN DCI  (EU-27 subnetwork only)
#    eu_countries was defined earlier in the script.
# =========================================================
dci_eu <- compute_dci(
  trade_use %>% filter(country_exporter %in% eu_countries,
                       country_importer %in% eu_countries),
  node_universe = eu_countries
) %>%
  rename(DCI_EU = DCI,
         throughput_EU = throughput,
         FCI_EU = FCI_economywide)

# =========================================================
# 5. EEDR  -  Extra-European Dependency Ratio (EU importers)
#    EEDR_i = (digital imports of i from NON-EU) / (all digital imports of i)
# =========================================================
eedr <- trade_use %>%
  filter(country_importer %in% eu_countries) %>%
  group_by(country = country_importer, Year) %>%
  summarise(
    imports_total  = sum(trade_value, na.rm = TRUE),
    imports_nonEU  = sum(trade_value[!(country_exporter %in% eu_countries)],
                         na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(EEDR = ifelse(imports_total > 0, imports_nonEU / imports_total, NA_real_))

# Optional: EEDR by technology group (kept for appendix / robustness)
eedr_group <- trade_use %>%
  filter(country_importer %in% eu_countries, tech_group %in% core_groups) %>%
  group_by(country = country_importer, Year, tech_group) %>%
  summarise(
    imports_total = sum(trade_value, na.rm = TRUE),
    imports_nonEU = sum(trade_value[!(country_exporter %in% eu_countries)],
                        na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(EEDR = ifelse(imports_total > 0, imports_nonEU / imports_total, NA_real_))

# =========================================================
# 6. GROUP-LEVEL OPENNESS  (digital openness within each tech group)
#    Openness_group = (M_group + X_group) / GDP
# =========================================================
gdp_lookup <- trade_gdp_dep %>% select(country, Year, GDP) %>% distinct()

openness_group <- dependency_hhi_group %>%
  left_join(gdp_lookup, by = c("country", "Year")) %>%
  mutate(
    trade_group = coalesce(M, 0) + coalesce(X, 0),
    Openness_group = ifelse(!is.na(GDP) & GDP > 0, trade_group / GDP, NA_real_)
  ) %>%
  select(country, Year, tech_group, Openness_group)

# =========================================================
# 7. TECHNOLOGY VULNERABILITY INDEX (TVI)  - per country-Year-tech_group
#    Dimensions: TD (dependency), HHI (concentration),
#                Openness_group, DCI_group.
#    Higher TD, higher HHI  -> MORE vulnerable.
#    Higher DCI (embeddedness) and (by default) higher openness
#                            -> LESS vulnerable (inverted).
#    NOTE: TVI is a ROBUSTNESS / visualisation device. The four dimensions
#    are kept SEPARATE as the main analysis (see models below).
# =========================================================
mm <- function(v) {                     # min-max scaling to [0,1]
  rng <- range(v, na.rm = TRUE)
  if (!is.finite(rng[1]) || diff(rng) == 0) return(rep(NA_real_, length(v)))
  (v - rng[1]) / (rng[2] - rng[1])
}

tvi_base <- dependency_hhi_group %>%
  select(country, Year, tech_group, TD, HHI) %>%
  left_join(openness_group, by = c("country", "Year", "tech_group")) %>%
  left_join(dci_group %>% select(country, Year, tech_group, DCI_group),
            by = c("country", "Year", "tech_group")) %>%
  mutate(TD = as.numeric(TD))

tvi_scored <- tvi_base %>%
  mutate(
    s_TD   = mm(TD),
    s_HHI  = mm(HHI),
    s_OPEN = if (OPENNESS_PROTECTIVE) 1 - mm(Openness_group) else mm(Openness_group),
    s_DCI  = 1 - mm(DCI_group)
  ) %>%
  rowwise() %>%
  mutate(
    TVI = mean(c(s_TD, s_HHI, s_OPEN, s_DCI), na.rm = TRUE)
  ) %>%
  ungroup()

# Quartile classes: Low / Medium / High / Very High
tvi_cuts <- quantile(tvi_scored$TVI, probs = c(.25, .5, .75), na.rm = TRUE)
tvi_scored <- tvi_scored %>%
  mutate(
    TVI_class = case_when(
      is.na(TVI)            ~ NA_character_,
      TVI <= tvi_cuts[1]    ~ "Low",
      TVI <= tvi_cuts[2]    ~ "Medium",
      TVI <= tvi_cuts[3]    ~ "High",
      TRUE                  ~ "Very High"
    ),
    TVI_class = factor(TVI_class,
                       levels = c("Low", "Medium", "High", "Very High"))
  )

# =========================================================
# 7b. BROKERAGE  -  betweenness centrality in the digital network
#     A country is a "broker" when many shortest digital-trade paths pass
#     through it (gateway position: e.g. Ireland, Netherlands, Singapore).
#     Weighted betweenness: edge weight = flow, so the path "cost" (distance)
#     is 1/flow (a stronger tie = a shorter hop). Normalised for comparability.
# =========================================================
compute_brokerage_one <- function(flows, nodes) {
  flows <- flows %>%
    filter(country_exporter %in% nodes, country_importer %in% nodes,
           country_exporter != country_importer, w > 0)
  if (length(nodes) < 3 || nrow(flows) == 0) {
    return(tibble(country = nodes, Brokerage = NA_real_))
  }
  g <- graph_from_data_frame(
    flows %>% transmute(from = country_exporter, to = country_importer, weight = w),
    directed  = TRUE,
    vertices  = data.frame(name = nodes)
  )
  b <- betweenness(g, directed = TRUE,
                   weights = 1 / E(g)$weight, normalized = TRUE)
  tibble(country = names(b), Brokerage = as.numeric(b))
}

compute_brokerage <- function(trade_df, node_universe = NULL) {
  years <- sort(unique(trade_df$Year))
  purrr::map_dfr(years, function(yr) {
    fl <- trade_df %>% filter(Year == yr) %>%
      group_by(country_exporter, country_importer) %>%
      summarise(w = sum(trade_value, na.rm = TRUE), .groups = "drop")
    nodes <- if (is.null(node_universe)) {
      sort(unique(c(fl$country_exporter, fl$country_importer)))
    } else sort(node_universe)
    compute_brokerage_one(fl, nodes) %>% mutate(Year = yr)
  })
}

# Global brokerage (full world network)
brokerage_global <- compute_brokerage(trade_use) %>%
  rename(Brokerage_Global = Brokerage)

# EU brokerage (EU-27 subnetwork)
brokerage_eu <- compute_brokerage(
  trade_use %>% filter(country_exporter %in% eu_countries,
                       country_importer %in% eu_countries),
  node_universe = eu_countries
) %>%
  rename(Brokerage_EU = Brokerage)

# =========================================================
# 8. ASSEMBLE THE FOUR PAPER TABLES
#    Reporting year: latest year in the trade data (change if you prefer
#    a panel or a different reference year).
# =========================================================
report_year <- max(trade_use$Year, na.rm = TRUE)

dee_lookup <- dee_final %>%
  transmute(country, Year, DEE = .data[[DEE_var]]) %>%
  distinct()

# Pillar lookup: rename the real pillar columns to the labels DTE/DMSP/DUC/DTI
pillar_cols   <- DEE_pillars[unname(DEE_pillars) %in% names(dee_final)]
pillar_lookup <- dee_final %>%
  select(country, Year, all_of(unname(pillar_cols))) %>%
  distinct()
names(pillar_lookup)[match(unname(pillar_cols), names(pillar_lookup))] <- names(pillar_cols)

# ---- TABLE 1: country indicators (all countries) ----
#      Dependency, HHI, Openness, Global DCI, Brokerage, DTE, DMSP, DUC, DTI
table1_country <- trade_gdp_dep %>%
  filter(Year == report_year) %>%
  transmute(country, Year,
            Dependency = as.numeric(TD),
            HHI,
            Openness = trade_over_gdp) %>%
  left_join(dci_global %>% filter(Year == report_year) %>%
              select(country, DCI_Global),
            by = "country") %>%
  left_join(brokerage_global %>% filter(Year == report_year) %>%
              select(country, Brokerage_Global),
            by = "country") %>%
  left_join(dee_lookup %>% filter(Year == report_year) %>%
              select(country, DEE),
            by = "country") %>%
  left_join(pillar_lookup %>% filter(Year == report_year) %>% select(-Year),
            by = "country") %>%
  arrange(country)

# ---- TABLE 2: TVI by technology group (wide: groups as columns) ----
table2_tvi <- tvi_scored %>%
  filter(Year == report_year) %>%
  select(country, tech_group, TVI) %>%
  pivot_wider(names_from = tech_group, values_from = TVI) %>%
  select(country, any_of(c("user_facing", "productive",
                           "platform", "infrastructure"))) %>%
  arrange(country)

# ---- TABLE 3: TVI classes by technology group ----
table3_tvi_class <- tvi_scored %>%
  filter(Year == report_year) %>%
  select(country, tech_group, TVI_class) %>%
  pivot_wider(names_from = tech_group, values_from = TVI_class) %>%
  select(country, any_of(c("user_facing", "productive",
                           "platform", "infrastructure"))) %>%
  arrange(country)

# ---- TABLE 4: EU countries only ----
#      Dependency, EEDR, HHI, European DCI, Brokerage_EU, DTE, DMSP, DUC, DTI
table4_eu <- trade_gdp_dep %>%
  filter(Year == report_year, country %in% eu_countries) %>%
  transmute(country, Year,
            Dependency = as.numeric(TD),
            HHI) %>%
  left_join(eedr %>% filter(Year == report_year) %>% select(country, EEDR),
            by = "country") %>%
  left_join(dci_eu %>% filter(Year == report_year) %>%
              select(country, DCI_EU),
            by = "country") %>%
  left_join(brokerage_eu %>% filter(Year == report_year) %>%
              select(country, Brokerage_EU),
            by = "country") %>%
  left_join(dee_lookup %>% filter(Year == report_year) %>%
              select(country, DEE),
            by = "country") %>%
  left_join(pillar_lookup %>% filter(Year == report_year) %>% select(-Year),
            by = "country") %>%
  select(country, Year, Dependency, EEDR, HHI, DCI_EU, Brokerage_EU,
         DEE, any_of(names(DEE_pillars))) %>%
  arrange(country)

# ---- export ----
write_xlsx(
  list(
    Table1_Country      = table1_country,
    Table2_TVI_group    = table2_tvi,
    Table3_TVI_classes  = table3_tvi_class,
    Table4_EU           = table4_eu,
    FCI_by_year         = fci_global_year
  ),
  "paper_tables.xlsx"
)

# =========================================================
# 9. ANALYSIS PANEL  (country-Year)  for the regressions
#    Dependency, HHI, Openness, DCI_Global, Brokerage_Global,
#    EEDR, DCI_EU, Brokerage_EU, DEE + 4 pillars, controls.
# =========================================================
panel <- dee_final %>%
  left_join(trade_gdp_dep %>%
              transmute(country, Year,
                        Dependency = as.numeric(TD),
                        HHI,
                        Openness = trade_over_gdp,
                        GDP_level = GDP),
            by = c("country", "Year")) %>%
  left_join(dci_global %>% select(country, Year, DCI_Global),
            by = c("country", "Year")) %>%
  left_join(brokerage_global %>% select(country, Year, Brokerage_Global),
            by = c("country", "Year")) %>%
  left_join(eedr %>% select(country, Year, EEDR),
            by = c("country", "Year")) %>%
  left_join(dci_eu %>% select(country, Year, DCI_EU),
            by = c("country", "Year")) %>%
  left_join(brokerage_eu %>% select(country, Year, Brokerage_EU),
            by = c("country", "Year")) %>%
  mutate(EU = as.integer(country %in% eu_countries))

# Ensure the pillar columns are available under their LABELS (DTE/DMSP/DUC/DTI).
# dee_final already carries the raw pillar columns; only create a label column
# when the label differs from the raw name (avoids a self-join name clash).
for (lab in names(pillar_cols)) {
  raw <- unname(pillar_cols[lab])
  if (!(lab %in% names(panel)) && raw %in% names(panel)) {
    panel[[lab]] <- panel[[raw]]
  }
}

# GDP per capita control (use real column if present, else proxies)
if (!is.na(GDPpc_var) && GDPpc_var %in% names(panel)) {
  panel$GDPpc <- panel[[GDPpc_var]]
} else if ("Population" %in% names(panel)) {
  panel$GDPpc <- panel$GDP_level / panel$Population
} else {
  panel$GDPpc <- panel$GDP_level     # falls back to GDP level; logged below
}
panel <- panel %>% mutate(log_GDPpc = log(ifelse(GDPpc > 0, GDPpc, NA)))

stopifnot(DEE_var %in% names(panel))
panel$DEE <- panel[[DEE_var]]

# Which pillar outcomes are actually present
pillar_labels <- intersect(names(DEE_pillars), names(panel))

# Helper: run feols for several outcomes that share one RHS + FE/cluster
run_outcomes <- function(data, outcomes, rhs) {
  setNames(lapply(outcomes, function(y) {
    feols(as.formula(paste0("`", y, "` ~ ", rhs, " | country + Year")),
          data = data, cluster = ~country)
  }), outcomes)
}

# =========================================================
# 10. ALL-COUNTRY MODELS  -  one per outcome (Models 1-4 + aggregate DEE)
#     Outcome ~ Dependency + HHI + Openness + DCI_Global + Brokerage_Global
# =========================================================
rhs_global   <- "Dependency + HHI + Openness + DCI_Global + Brokerage_Global + log_GDPpc"
models_global <- run_outcomes(panel, c("DEE", pillar_labels), rhs_global)
etable(models_global, digits = 4)

# =========================================================
# 11. MODERATION MODELS  (Models 5-7 generalised to all pillars)
#     (a) Dependency x DCI_Global   - embeddedness moderates dependency
#     (b) Dependency x Brokerage_Global - gateway position moderates dependency
# =========================================================
mod_dci <- run_outcomes(panel, c("DEE", pillar_labels),
                        "Dependency * DCI_Global + HHI + log_GDPpc")
mod_brk <- run_outcomes(panel, c("DEE", pillar_labels),
                        "Dependency * Brokerage_Global + HHI + log_GDPpc")
etable(mod_dci, digits = 4)
etable(mod_brk, digits = 4)

# =========================================================
# 12. EU MODELS  -  one per outcome, EU sample
#     Outcome ~ EEDR + HHI + DCI_EU + Brokerage_EU
# =========================================================
rhs_eu     <- "EEDR + HHI + DCI_EU + Brokerage_EU + log_GDPpc"
models_eu  <- run_outcomes(panel %>% filter(EU == 1),
                           c("DEE", pillar_labels), rhs_eu)
etable(models_eu, digits = 4)

# EU moderation (Dependency x European DCI)
mod_eu <- run_outcomes(panel %>% filter(EU == 1), c("DEE", pillar_labels),
                       "Dependency * DCI_EU + HHI + log_GDPpc")
etable(mod_eu, digits = 4)

# =========================================================
# 13. DYNAMIC / LAGGED MODELS  -  network_t -> outcome_{t+1}
#     Moves toward "network position precedes ecosystem performance".
# =========================================================
panel_dyn <- panel %>%
  arrange(country, Year) %>%
  group_by(country) %>%
  mutate(across(all_of(c("DEE", pillar_labels)),
                ~ dplyr::lead(.x), .names = "lead_{.col}")) %>%
  ungroup()

lead_outcomes <- paste0("lead_", c("DEE", pillar_labels))
models_dyn <- run_outcomes(panel_dyn, lead_outcomes, rhs_global)
etable(models_dyn, digits = 4)

# =========================================================
# 14. ROBUSTNESS: DEE COMPONENTS (if you also have legacy components)
# =========================================================
component_models <- list()
for (cmp in DEE_components) {
  if (cmp %in% names(panel)) {
    f <- as.formula(
      paste0("`", cmp, "` ~ ", rhs_global, " | country + Year")
    )
    component_models[[cmp]] <- feols(f, data = panel, cluster = ~country)
  }
}
if (length(component_models) > 0) etable(component_models, digits = 4)

# =========================================================
# 15. COUNTRY TYPOLOGY  (cluster analysis; country means over sample)
#     Named quadrants on Dependency x DCI, k-means cross-check,
#     and the Brokerage x Dependency view.
# =========================================================
cluster_data <- panel %>%
  group_by(country) %>%
  summarise(
    Dependency = mean(Dependency, na.rm = TRUE),
    DCI        = mean(DCI_Global, na.rm = TRUE),
    Brokerage  = mean(Brokerage_Global, na.rm = TRUE),
    HHI        = mean(HHI, na.rm = TRUE),
    Openness   = mean(Openness, na.rm = TRUE),
    DEE        = mean(DEE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(is.finite(Dependency), is.finite(DCI), is.finite(DEE))

dep_med <- median(cluster_data$Dependency, na.rm = TRUE)
dci_med <- median(cluster_data$DCI, na.rm = TRUE)
brk_med <- median(cluster_data$Brokerage, na.rm = TRUE)

cluster_data <- cluster_data %>%
  mutate(
    quadrant = case_when(
      DCI >  dci_med & Dependency <= dep_med ~ "Embedded Leaders",        # high DCI, low dep
      DCI >  dci_med & Dependency >  dep_med ~ "Gateway Economies",       # high DCI, high dep
      DCI <= dci_med & Dependency <= dep_med ~ "Sovereign but Isolated",  # low DCI, low dep
      TRUE                                   ~ "Vulnerable Periphery"     # low DCI, high dep
    )
  )

quadrant_summary <- cluster_data %>%
  group_by(quadrant) %>%
  summarise(n = n(), mean_DEE = mean(DEE, na.rm = TRUE),
            mean_Dependency = mean(Dependency), mean_DCI = mean(DCI),
            mean_Brokerage = mean(Brokerage), .groups = "drop")
print(quadrant_summary)

set.seed(123)
km_mat <- scale(cluster_data %>% select(Dependency, DCI, Brokerage, HHI, Openness))
km <- kmeans(km_mat, centers = 4, nstart = 25)
cluster_data$kcluster <- factor(km$cluster)

kmeans_summary <- cluster_data %>%
  group_by(kcluster) %>%
  summarise(n = n(),
            Dependency = mean(Dependency), DCI = mean(DCI),
            Brokerage = mean(Brokerage), HHI = mean(HHI),
            Openness = mean(Openness), DEE = mean(DEE), .groups = "drop")
print(kmeans_summary)

write_xlsx(
  list(country_clusters = cluster_data,
       quadrant_summary = quadrant_summary,
       kmeans_summary   = kmeans_summary),
  "country_clusters.xlsx"
)

# =========================================================
# 16. FIGURES
# =========================================================
# Figure 3: Dependency vs DCI quadrants (point size = DEE)
fig3 <- ggplot(cluster_data,
               aes(x = Dependency, y = DCI, size = DEE, colour = quadrant)) +
  geom_hline(yintercept = dci_med, linetype = "dashed") +
  geom_vline(xintercept = dep_med, linetype = "dashed") +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = country), size = 2.5, vjust = -1, show.legend = FALSE) +
  labs(title = "Dependency vs Digital Cycling Index (DCI)",
       x = "Digital trade dependency", y = "Global DCI (embeddedness)",
       colour = "Typology", size = "DEE") +
  theme_minimal()
ggsave("fig3_dependency_vs_dci.png", fig3, width = 9, height = 6)

# Figure 4: Brokerage vs Dependency (gateway economies sit upper-right)
fig4 <- ggplot(cluster_data,
               aes(x = Dependency, y = Brokerage, size = DEE, colour = DEE)) +
  geom_point(alpha = 0.85) +
  geom_text(aes(label = country), size = 2.5, vjust = -1, show.legend = FALSE) +
  scale_colour_viridis_c() +
  labs(title = "Brokerage vs Dependency",
       x = "Digital trade dependency", y = "Brokerage (betweenness)",
       colour = "DEE", size = "DEE") +
  theme_minimal()
ggsave("fig4_brokerage_vs_dependency.png", fig4, width = 9, height = 6)

# =========================================================
# 17. (OPTIONAL) export tidy regression tables to disk
# =========================================================
# etable(models_global, file = "models_global.html",  digits = 4)
# etable(mod_dci, mod_brk, file = "models_moderation.html", digits = 4)
# etable(models_eu, file = "models_eu.html", digits = 4)
# etable(models_dyn, file = "models_dynamic.html", digits = 4)

# #############################################################################
# ##   END PAPER PIPELINE                                                    ##
# #############################################################################
















































# -------------------------
# 6D. HHI AT CATEGORY LEVEL
# HHI = sum_j (M_ij / M_i)^2
# -------------------------
hhi_data_full <- trade_use %>%
  group_by(country = country_importer, Year, category_raw, tech_group, supplier = country_exporter) %>%
  summarise(import_val = sum(trade_value, na.rm = TRUE), .groups = "drop") %>%
  group_by(country, Year, category_raw, tech_group) %>%
  mutate(
    total_imports = sum(import_val, na.rm = TRUE),
    share_supplier = ifelse(total_imports > 0, import_val / total_imports, NA_real_)
  ) %>%
  summarise(
    HHI = sum(share_supplier^2, na.rm = TRUE),
    .groups = "drop"
  )

country_tech_full <- country_tech_full %>%
  left_join(
    hhi_data_full,
    by = c("country", "Year", "category_raw", "tech_group")
  )

# =========================================================
# 7. REGRESSION MODELS AFTER DEPENDENCE TYPES
# CATEGORY-LEVEL, ALL DATA
# =========================================================

DTE_var <- "DTE"
possible_controls <- c("GDP", "GDP_per_capita", "Population", "Internet_Users", "Human_Capital")

# Reshape category-level TD
dep_td_wide <- country_tech_full %>%
  select(country, Year, category_raw, TD) %>%
  pivot_wider(
    names_from = category_raw,
    values_from = TD,
    names_prefix = "TD_"
  )

# Reshape category-level NetDep
dep_netdep_wide <- country_tech_full %>%
  select(country, Year, category_raw, NetDep) %>%
  pivot_wider(
    names_from = category_raw,
    values_from = NetDep,
    names_prefix = "NetDep_"
  )

# Reshape category-level HHI
dep_hhi_wide <- country_tech_full %>%
  select(country, Year, category_raw, HHI) %>%
  pivot_wider(
    names_from = category_raw,
    values_from = HHI,
    names_prefix = "HHI_"
  )

# Merge into panels
panel_td <- dee_final %>%
  left_join(dep_td_wide, by = c("country", "Year"))

panel_netdep <- dee_final %>%
  left_join(dep_netdep_wide, by = c("country", "Year"))

panel_hhi <- dee_final %>%
  left_join(dep_hhi_wide, by = c("country", "Year"))

controls_td <- possible_controls[possible_controls %in% names(panel_td)]
controls_netdep <- possible_controls[possible_controls %in% names(panel_netdep)]
controls_hhi <- possible_controls[possible_controls %in% names(panel_hhi)]

if (DTE_var %in% names(panel_td)) {
  
  # TD model using all category variables
  td_vars <- grep("^TD_", names(panel_td), value = TRUE)
  td_vars <- setdiff(td_vars, controls_td)
  
  formula_td <- as.formula(
    paste0(
      DTE_var, " ~ ",
      paste(c(td_vars, controls_td), collapse = " + "),
      " | country + Year"
    )
  )
  
  model_TD <- feols(
    formula_td,
    data = panel_td,
    cluster = ~country
  )
  
  print(summary(model_TD))
} else {
  message("Replace DTE_var with the correct dependent variable name.")
}

if (DTE_var %in% names(panel_netdep)) {
  
  # NetDep model using all category variables
  netdep_vars <- grep("^NetDep_", names(panel_netdep), value = TRUE)
  netdep_vars <- setdiff(netdep_vars, controls_netdep)
  
  formula_netdep <- as.formula(
    paste0(
      DTE_var, " ~ ",
      paste(c(netdep_vars, controls_netdep), collapse = " + "),
      " | country + Year"
    )
  )
  
  model_NetDep <- feols(
    formula_netdep,
    data = panel_netdep,
    cluster = ~country
  )
  
  print(summary(model_NetDep))
}

if (DTE_var %in% names(panel_hhi)) {
  
  # HHI model using all category variables
  hhi_vars <- grep("^HHI_", names(panel_hhi), value = TRUE)
  hhi_vars <- setdiff(hhi_vars, controls_hhi)
  
  formula_hhi <- as.formula(
    paste0(
      DTE_var, " ~ ",
      paste(c(hhi_vars, controls_hhi), collapse = " + "),
      " | country + Year"
    )
  )
  
  model_HHI <- feols(
    formula_hhi,
    data = panel_hhi,
    cluster = ~country
  )
  
  print(summary(model_HHI))
}

# =========================================================
# 8. AGGREGATE TO 4 TECHNOLOGY GROUPS
# ONLY HERE DO WE MOVE TO 4 GROUPS
# =========================================================

dep_group <- country_tech_full %>%
  filter(tech_group %in% c("infrastructure", "platform", "productive", "user_facing")) %>%
  group_by(country, Year, tech_group) %>%
  summarise(
    M = sum(M, na.rm = TRUE),
    X = sum(X, na.rm = TRUE),
    
    TD = ifelse(sum(M + X, na.rm = TRUE) > 0,
                sum(M, na.rm = TRUE) / sum(M + X, na.rm = TRUE),
                NA_real_),
    
    GDP = dplyr::first(na.omit(GDP)),
    NetDep = ifelse(!is.na(GDP) & GDP > 0,
                    (sum(M, na.rm = TRUE) - sum(X, na.rm = TRUE)) / GDP,
                    NA_real_),
    .groups = "drop"
  )

# Group-level HHI
hhi_group <- trade_use %>%
  filter(tech_group %in% c("infrastructure", "platform", "productive", "user_facing")) %>%
  group_by(country = country_importer, Year, tech_group, supplier = country_exporter) %>%
  summarise(import_val = sum(trade_value, na.rm = TRUE), .groups = "drop") %>%
  group_by(country, Year, tech_group) %>%
  mutate(
    total_imports = sum(import_val, na.rm = TRUE),
    share_supplier = ifelse(total_imports > 0, import_val / total_imports, NA_real_)
  ) %>%
  summarise(
    HHI = sum(share_supplier^2, na.rm = TRUE),
    .groups = "drop"
  )

dep_group <- dep_group %>%
  left_join(hhi_group, by = c("country", "Year", "tech_group"))

# =========================================================
# 9. REGRESSION MODEL AFTER 4 GROUPS
# =========================================================

dep_group_baseline <- dep_group %>%
  select(country, Year, tech_group, TD) %>%
  pivot_wider(
    names_from = tech_group,
    values_from = TD,
    names_prefix = "TD_"
  )

panel_4groups <- dee_final %>%
  left_join(dep_group_baseline, by = c("country", "Year"))

controls_4groups <- possible_controls[possible_controls %in% names(panel_4groups)]

if (DTE_var %in% names(panel_4groups)) {
  
  formula_4groups <- as.formula(
    paste0(
      DTE_var, " ~ ",
      paste(c("TD_infrastructure", "TD_platform", "TD_productive", "TD_user_facing", controls_4groups), collapse = " + "),
      " | country + Year"
    )
  )
  
  model_4groups <- feols(
    formula_4groups,
    data = panel_4groups,
    cluster = ~country
  )
  
  print(summary(model_4groups))
}

# =========================================================
# 10. BUILD KET NETWORK FROM ALL CATEGORIES
# =========================================================

country_tech_network <- country_tech_full %>%
  mutate(
    T = M + X
  ) %>%
  select(country, Year, category_raw, M, X, T)

# Main KET definition: shares based on M + X
country_tech_mx <- country_tech_network %>%
  group_by(country, Year) %>%
  mutate(
    total_T = sum(T, na.rm = TRUE),
    share_mx = ifelse(total_T > 0, T / total_T, 0)
  ) %>%
  ungroup()

tech_wide_mx <- country_tech_mx %>%
  select(country, Year, category_raw, share_mx) %>%
  pivot_wider(
    names_from = category_raw,
    values_from = share_mx,
    values_fill = 0
  )

tech_matrix_mx <- tech_wide_mx %>%
  select(-country, -Year)

corr_mx <- cor(tech_matrix_mx, use = "pairwise.complete.obs")
corr_mx[corr_mx < 0] <- 0
diag(corr_mx) <- 0

g_mx <- graph_from_adjacency_matrix(
  corr_mx,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

tech_centrality_mx <- data.frame(
  technology = V(g_mx)$name,
  strength = strength(g_mx, weights = E(g_mx)$weight),
  betweenness = betweenness(g_mx, directed = FALSE, weights = 1 / E(g_mx)$weight),
  eigenvector = eigen_centrality(g_mx, directed = FALSE, weights = E(g_mx)$weight)$vector,
  pagerank = page_rank(g_mx, directed = FALSE, weights = E(g_mx)$weight)$vector
)

tech_centrality_mx <- tech_centrality_mx %>%
  mutate(
    z_strength = as.numeric(scale(strength)),
    z_betweenness = as.numeric(scale(betweenness)),
    z_eigenvector = as.numeric(scale(eigenvector)),
    z_pagerank = as.numeric(scale(pagerank)),
    KET_index_mx = z_strength + z_betweenness + z_eigenvector + z_pagerank
  ) %>%
  arrange(desc(KET_index_mx))

ket_cutoff_mx <- quantile(tech_centrality_mx$KET_index_mx, 0.75, na.rm = TRUE)

tech_centrality_mx <- tech_centrality_mx %>%
  mutate(
    KET_flag_mx = ifelse(KET_index_mx >= ket_cutoff_mx, 1, 0)
  )

ket_list_mx <- tech_centrality_mx %>%
  filter(KET_flag_mx == 1) %>%
  pull(technology)

# =========================================================
# 11. REGRESSION MODEL AFTER KET
# =========================================================

dep_ket_mx <- country_tech_network %>%
  mutate(
    KET_type = ifelse(category_raw %in% ket_list_mx, "KET", "nonKET")
  ) %>%
  group_by(country, Year, KET_type) %>%
  summarise(
    M = sum(M, na.rm = TRUE),
    X = sum(X, na.rm = TRUE),
    TD = ifelse(sum(M + X, na.rm = TRUE) > 0,
                sum(M, na.rm = TRUE) / sum(M + X, na.rm = TRUE),
                NA_real_),
    .groups = "drop"
  ) %>%
  select(country, Year, KET_type, TD) %>%
  pivot_wider(
    names_from = KET_type,
    values_from = TD,
    names_prefix = "TD_"
  )

panel_ket <- dee_final %>%
  left_join(dep_ket_mx, by = c("country", "Year"))

controls_ket <- possible_controls[possible_controls %in% names(panel_ket)]

if (DTE_var %in% names(panel_ket)) {
  
  formula_ket <- as.formula(
    paste0(
      DTE_var, " ~ ",
      paste(c("TD_KET", "TD_nonKET", controls_ket), collapse = " + "),
      " | country + Year"
    )
  )
  
  model_ket <- feols(
    formula_ket,
    data = panel_ket,
    cluster = ~country
  )
  
  print(summary(model_ket))
  print(wald(model_ket, "TD_KET = TD_nonKET"))
}

# =========================================================
# 12. EXPORT RESULTS
# =========================================================

write.csv(country_tech_full, "country_technology_dependence_all_categories.csv", row.names = FALSE)

write.csv(dep_td_wide, "dependence_td_all_categories_wide.csv", row.names = FALSE)
write.csv(dep_netdep_wide, "dependence_netdep_all_categories_wide.csv", row.names = FALSE)
write.csv(dep_hhi_wide, "dependence_hhi_all_categories_wide.csv", row.names = FALSE)

write.csv(panel_td, "panel_td_all_categories.csv", row.names = FALSE)
write.csv(panel_netdep, "panel_netdep_all_categories.csv", row.names = FALSE)
write.csv(panel_hhi, "panel_hhi_all_categories.csv", row.names = FALSE)

write.csv(dep_group, "dependence_by_4groups_long.csv", row.names = FALSE)
write.csv(panel_4groups, "panel_4groups.csv", row.names = FALSE)

write.csv(tech_centrality_mx, "technology_network_centrality_mplusx.csv", row.names = FALSE)
write.csv(dep_ket_mx, "dependence_ket_main_mplusx.csv", row.names = FALSE)
write.csv(panel_ket, "panel_ket_main_mplusx.csv", row.names = FALSE)







