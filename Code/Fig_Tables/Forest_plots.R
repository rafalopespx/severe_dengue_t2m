#### Forest plot with all analysis

library(tidyverse)
library(tidylog)
library(grid)
library(gridExtra)


fct_case_when <- function(...) {
    args <- as.list(match.call())
    levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
    levels <- levels[!is.na(levels)]
    factor(dplyr::case_when(...), levels=levels)
}


main <- vroom::vroom('Outputs/Brazil/results_rr_main_br_residence.csv.xz')
s1   <- vroom::vroom('Outputs/Brazil/results_rr_s1_br_residence.csv.xz')
s2   <- vroom::vroom('Outputs/Brazil/results_rr_s2_br_residence.csv.xz')
s3   <- vroom::vroom('Outputs/Brazil/results_rr_s3_br_residence.csv.xz')


mainreg <- vroom::vroom('Outputs/Region/results_rr_main_region_residence.csv.xz')
s1reg   <- vroom::vroom('Outputs/Region/results_rr_s1_region_residence.csv.xz')
s2reg   <- vroom::vroom('Outputs/Region/results_rr_s2_region_residence.csv.xz')
s3reg   <- vroom::vroom('Outputs/Region/results_rr_s3_region_residence.csv.xz')

## Br
perc_br_50 <- vroom('Outputs/Brazil/results_lag_main_br_residence.csv.xz') %>% 
    filter(percentile == 50) %>% distinct(region,value)
perc_br_95 <- vroom('Outputs/Brazil/results_lag_main_br_residence.csv.xz') %>% 
    filter(percentile == 95) %>% distinct(region,value)

# region
perc_reg50 <- vroom('Outputs/Region/results_lag_main_region_residence.csv.xz') %>% 
    filter(percentile == 50) %>% distinct(value, region) 
perc_reg95 <- vroom('Outputs/Region/results_lag_main_region_residence.csv.xz') %>% 
    filter(percentile == 95) %>% distinct(value, region) 


db_br_50 <-
  bind_rows(
    main %>% mutate(model = 'Main analysis'),
    s1   %>% mutate(model = 'Sensitivity 1'),
    s2   %>% mutate(model = 'Sensitivity 2'),
    s3   %>% mutate(model = 'Sensitivity 3')) %>% 
    left_join(perc_br_50)  %>% filter(value == temp) %>% mutate(percentile = 50)
    
db_br_95 <-
    bind_rows(
        main %>% mutate(model = 'Main analysis'),
        s1   %>% mutate(model = 'Sensitivity 1'),
        s2   %>% mutate(model = 'Sensitivity 2'),
        s3   %>% mutate(model = 'Sensitivity 3')) %>% 
    left_join(perc_br_95)  %>% filter(value == temp) %>% mutate(percentile = 95)


db_reg_50 <-
    bind_rows(
        mainreg %>% mutate(model = 'Main analysis'),
        s1reg   %>% mutate(model = 'Sensitivity 1'),
        s2reg   %>% mutate(model = 'Sensitivity 2'),
        s3reg   %>% mutate(model = 'Sensitivity 3')) %>% 
    left_join(perc_reg50)  %>% filter(value == temp) %>% mutate(percentile = 50)

db_reg_95 <-
    bind_rows(
        mainreg %>% mutate(model = 'Main analysis'),
        s1reg   %>% mutate(model = 'Sensitivity 1'),
        s2reg   %>% mutate(model = 'Sensitivity 2'),
        s3reg   %>% mutate(model = 'Sensitivity 3')) %>% 
    left_join(perc_reg95)  %>% filter(value == temp) %>% mutate(percentile = 95)


bind_rows(
    db_br_50, db_br_95,
    db_reg_50, db_reg_95
) %>% 
    mutate(
        text = paste0(
            format(round(estimate, 3), nsmall = 3),
            " (",
            format(round(conf.low, 3), nsmall = 3),
            "-",
            format(round(conf.hig, 3), nsmall = 3),
            ")"),
    ) ->db
  



db <-
  db %>% 
  mutate(
    submodel = fct_case_when(
        region == 'br'~ 'Brazil',
        region == 'N' ~ "North",
        region == 'NE'~ "Northeast", 
        region == 'CW'~ "Center-West", 
        region == 'SE'~ "Southeast", 
        region == 'S' ~ "South"))
        

## 50th

models_int_plot_50 <-
  db %>% 
    filter(percentile==50) %>% 
  select(submodel, model, estimate, conf.low, conf.hig,
         text, mht) %>% 
  arrange(submodel) %>% 
  mutate(order = 
           c(2:5,7:10,12:15,17:20,22:25, 27:30),
         model = paste0("     ", model),
         bold  = 1)

add_lines <-
  tibble(
    levels = c("Brazil", 'North', 'Northeast','Center-West','Southeast', 'South'),
    order = 
      c(1,5,11,16,21,26),
    bold = 2
  )

models_int_plot_50 <-
  bind_rows(models_int_plot_50, add_lines) %>% 
  arrange(order)

models_int_plot_50 <-
  models_int_plot_50 %>% 
  mutate(
    mht = case_when(
    levels == "Brazil" ~ "MHT*", TRUE ~ as.character(round(mht,1))),
    text = case_when(
      levels == "Brazil" ~ "     RR (95% CI)", TRUE ~ text))

models_int_plot_50$yval <- seq(nrow(models_int_plot_50), 1, by = -1)


p1 <- 
  models_int_plot_50 %>% 
  ggplot() + 
  theme_bw() + 
  aes(x = estimate, xmin = conf.low, xmax = conf.hig, y = yval, col = model, fill = model) + 
  geom_errorbarh(
    height = 0.4, size = 1.0) + 
  geom_point(size = 3, shape = 15) + 
  geom_vline(xintercept = 1) + 
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    panel.border = element_blank(),
    legend.position = 'none',
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 12, colour = "black")
  )  +
  xlab("\n Relative Risk (95% CI)") +
  scale_x_continuous(
    # trans="log",
    breaks = c(1,2,3,4,5),
  )
p1

# labels, could be extended to show more information
p2 <-
  models_int_plot_50 %>% 
  ggplot() + 
  theme_bw() + 
  aes(y = yval) + 
  geom_text(aes(label = levels, 
                x = 0,
                fontface = bold, hjust = 0)) + 
  geom_text(aes(label = model, 
                x = 0.02,
                fontface = bold, hjust = 0)) + 
  geom_text(aes(label = mht, 
                x = 0.5,
                fontface = bold, hjust = 0)) + 
  geom_text(aes(label = text, 
                x = 0.7,
                fontface = bold, hjust = 0)) + 
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank() 
  ) +
  xlim(0,1)
p2  



gg50th <- grid.arrange(gridExtra::gtable_cbind(ggplotGrob(p2), ggplotGrob(p1), 
                                  size = "max"))
gg50th

ggsave(filename = "Outputs/Plots/S3_forest_plot_at_50th.png",
       plot = gg50th,
       width = 9,
       height = 11,
       dpi = 100)

ggsave(filename = "~/Dropbox/Rafael_Paixao/Article_Dengue_hosp/IJHEH/Figures/Supplementary/S3_forest_plot_at_50th.png",
       plot = gg50th,
       width = 16,
       height = 11,
       dpi = 100)

### 95th

models_int_plot_95 <-
    db %>% 
    filter(percentile==95) %>% 
    select(submodel, model, estimate, conf.low, conf.hig,
           text, mht) %>% 
    arrange(submodel) %>% 
    mutate(order = 
               c(2:5,7:10,12:15,17:20,22:25, 27:30),
           model = paste0("     ", model),
           bold  = 1)

add_lines <-
    tibble(
        levels = c("Brazil", 'North', 'Northeast','Center-West','Southeast', 'South'),
        order = 
            c(1,5,11,16,21,26),
        bold = 2
    )

models_int_plot_95 <-
    bind_rows(models_int_plot_95, add_lines) %>% 
    arrange(order)

models_int_plot_95 <-
    models_int_plot_95 %>% 
    mutate(
        mht = case_when(
            levels == "Brazil" ~ "MHT*", TRUE ~ as.character(round(mht,1))),
        text = case_when(
            levels == "Brazil" ~ "     RR (95% CI)", TRUE ~ text))

models_int_plot_95$yval <- seq(nrow(models_int_plot_95), 1, by = -1)


p3 <- 
    models_int_plot_95 %>% 
    ggplot() + 
    theme_bw() + 
    aes(x = estimate, xmin = conf.low, xmax = conf.hig, y = yval, col = model, fill = model) + 
    geom_errorbarh(
        height = 0.4, size = 1.0) + 
    geom_point(size = 3, shape = 15) + 
    geom_vline(xintercept = 1) + 
    theme(
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank(),
        legend.position = 'none',
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12, colour = "black")
    )  +
    xlab("\n Relative Risk (95% CI)") +
    scale_x_continuous(
        # trans="log",
        breaks = c(1,2,3,4,5),
    )
p3

# labels, could be extended to show more information
p4 <-
    models_int_plot_95 %>% 
    ggplot() + 
    theme_bw() + 
    aes(y = yval) + 
    geom_text(aes(label = levels, 
                  x = 0,
                  fontface = bold, hjust = 0)) + 
    geom_text(aes(label = model, 
                  x = 0.02,
                  fontface = bold, hjust = 0)) + 
    geom_text(aes(label = mht, 
                  x = 0.5,
                  fontface = bold, hjust = 0)) + 
    geom_text(aes(label = text, 
                  x = 0.7,
                  fontface = bold, hjust = 0)) + 
    theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank() 
    ) +
    xlim(0,1)
p4  



# gg95th <- grid.arrange(gridExtra::gtable_cbind(ggplotGrob(p2), ggplotGrob(p1), 
#                                                size = "max"))
# gg95th
# 
# ggsave(filename = "Outputs/Plots/S3_forest_plot_at_50th.png",
#        plot = gg50th,
#        width = 9,
#        height = 11,
#        dpi = 100)


gg95th <- grid.arrange(gridExtra::gtable_cbind(ggplotGrob(p4), 
                                               ggplotGrob(p3), 
                                               size = "max"))
gg95th

ggsave(filename = "Outputs/Plots/S4_forest_plot_at_95th.png",
       plot = gg95th,
       width = 9,
       height = 11,
       dpi = 100)

ggsave(filename = "~/Dropbox/Rafael_Paixao/Article_Dengue_hosp/IJHEH/Figures/S4_forest_plot_at_95th.png",
       plot = gg95th,
       width = 16,
       height = 9,
       dpi = 100)

