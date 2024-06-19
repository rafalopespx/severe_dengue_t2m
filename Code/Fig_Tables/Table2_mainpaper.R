








# Loading databases
## Br
br      <- vroom("Outputs/Brazil/results_rr_main_br_residence.csv.xz")
perc_br_50 <- vroom('Outputs/Brazil/results_lag_main_br_residence.csv.xz') %>% 
            filter(percentile == 50) %>% distinct(region,value)
perc_br_95 <- vroom('Outputs/Brazil/results_lag_main_br_residence.csv.xz') %>% 
            filter(percentile == 95) %>% distinct(region,value)

# region
reg <- vroom("Outputs/Region/results_rr_main_region_residence.csv.xz")
perc_reg50 <- vroom('Outputs/Region/results_lag_main_region_residence.csv.xz') %>% 
    filter(percentile == 50) %>% distinct(value, region) 
perc_reg95 <- vroom('Outputs/Region/results_lag_main_region_residence.csv.xz') %>% 
    filter(percentile == 95) %>% distinct(value, region) 


table_p50 <- 
bind_rows(
    br  %>% left_join(perc_br_50)  %>% filter(value == temp),
    reg %>% left_join(perc_reg50) %>% filter(value == temp)
) %>% 
    mutate(
        estimate_p50 = paste0(
            format(round(estimate, 3), nsmall = 3),
            " (",
            format(round(conf.low, 3), nsmall = 3),
            "-",
            format(round(conf.hig, 3), nsmall = 3),
            ")"),
    ) %>% 
    select(region, mht, estimate_p50)


table_p95 <- 
    bind_rows(
        br  %>% left_join(perc_br_95)  %>% filter(value == temp),
        reg %>% left_join(perc_reg95) %>% filter(value == temp)
    ) %>% 
    mutate(
        estimate_p95 = paste0(
            format(round(estimate, 3), nsmall = 3),
            " (",
            format(round(conf.low, 3), nsmall = 3),
            "-",
            format(round(conf.hig, 3), nsmall = 3),
            ")"),
    ) %>% 
    select(region, mht, estimate_p95)


table_p50 %>% 
    left_join(table_p95) %>% 
    mutate(region = factor(region, levels = c('br', 'N', 'NE', 'CW', 'SE', 'S'), 
                                   labels = c('Brazil', 'North', 'Northeast', 'Center-West',
                                              'Southeast', 'South'))) %>% 
    arrange(region) %>% write_csv('Outputs/Tables/table2_main_paper.csv')
