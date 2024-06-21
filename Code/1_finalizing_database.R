### importing data
### times series of hospitalized cases, temp, stratas, and dengue cases
library(tidyverse)
library(tidylog)
library(vroom)

# 20,212,424
db    <- vroom('Data/dengue_t2m_stratas_2010_2019.csv.xz')
# 2,123,095
cases <- vroom('Data/dengue_cases_confirmed_muni_2000_2020_complete.csv.xz')


# binding cases, deriving moving average
db <-
db %>% 
    left_join(cases %>% select(date = date_symptoms,
                               code_muni,
                               confirmed),
              by = c('code_muni', 'date'))

db <- db %>%  mutate(confirmed = replace_na(confirmed, 0))
           

db <- db %>% 
    arrange(code_muni, date) %>% 
    group_by(code_muni) %>% 
    mutate(cases_confirmed_7ma  = zoo::rollmean(confirmed, k=7,  fill = NA, align = 'right'),
           cases_confirmed_14ma = zoo::rollmean(confirmed, k=14, fill = NA, align = 'right'))


db <- db %>% 
    select(date, code_muni, code_state,
           Cases, temp_mean, cases_confirmed_7ma)

# 20,312,424 / 6
arrow::write_parquet(x = db, sink = 'Data/db_to_analysis.parquet')
