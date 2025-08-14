#==============================================================================#
# Dataset Creation Training Code                                               #
#==============================================================================#

# --- Load packages & set working directory ------------------------------------

# Installs `pacman` package if not already installed
if (!require("pacman")) install.packages("pacman")

# Load packages (& install those not already installed)
pacman::p_load(this.path, readxl, janitor, dplyr, psych)

# Set working directory to current folder
setwd(here())

# --- Load data ----------------------------------------------------------------

# Load revenues dataset
rev <- read_xlsx(
  "data/Finances AFR Revenues 2021-2022.xlsx",
  sheet = "2021-22 Rev per ADM"
)

# Load locales dataset
locale <- read_xlsx(
  "data/Urban Centric and Metro Centric Locale Codes.xlsx", 
  sheet = "SY1819 LEA Locale"
)

# --- Merge data frames --------------------------------------------------------

# Convert rev AUN to character to prep for merge
rev$AUN <- as.character(rev$AUN)

# Merge datasets
df <- left_join(rev, locale, by = "AUN")

# --- Clean & prep data --------------------------------------------------------

# Clean names
df <- clean_names(df)

# Drop extra rows
df <- filter(df, !is.na(aun))

# Drop unnecessary columns
df <- select(df, -lea_name, -county_name, -contains("total_rank"))

# Recode locale variable into set of dummy variables
df <- df |> 
  mutate(
    suburb = ifelse(grepl("Suburb", urban_centric_locale_district), 1, 0),
    city   = ifelse(grepl("City", urban_centric_locale_district), 1, 0),
    rural  = ifelse(grepl("Rural", urban_centric_locale_district), 1, 0)
  )

# Create total revenue variable
df <- df |> 
  mutate(total_rev = x2021_22_average_daily_membership * total_revenue_per_adm)

# --- Descriptive statistics ---------------------------------------------------

# Create grouping variable indicating districts in our sample
set.seed(6)
df$in_sample <- rbinom(500, 1, .1)
## ^Don't worry about this code, just for example purposes

# Compute summary stats
summary_stats <- df |> 
  group_by(in_sample) |> 
  summarize(
    rev_mean   = mean(total_revenue_per_adm, na.rm = TRUE),
    rev_sd     = sd(total_revenue_per_adm, na.rm = TRUE),
    suburb_pct = mean(suburb, na.rm = TRUE),
    city_pct   = mean(city, na.rm = TRUE),
    rural_pct  = mean(rural, na.rm = TRUE)
  )

# Print summary stats table
summary_stats

# Another approach to quickly get descriptive stats table
describe(df)

# --- Statistical tests of group differences -----------------------------------

# Linear regression for diffs in total rev per student
model_results_rev <- lm(total_revenue_per_adm ~ in_sample, data = df)
summary(model_results_rev)

# Logistic regression for diffs in locale percentages
model_results_suburb <- glm(suburb ~ in_sample, data = df, family = "binomial")
model_results_city   <- glm(city ~ in_sample, data = df, family = "binomial")
model_results_rural  <- glm(rural ~ in_sample, data = df, family = "binomial")
summary(model_results_suburb)
summary(model_results_city)
summary(model_results_rural)
