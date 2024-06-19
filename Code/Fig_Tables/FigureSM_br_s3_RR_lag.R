rm(list=ls())
gc()

### Loading packages
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(vroom)){install.packages("vroom"); library(vroom)}
if(!require(patchwork)){install.packages("patchwork"); library(patchwork)}
if(!require(geofacet)){install.packages("geofacet"); library(geofacet)}
library(tidylog)


# Loading databases
## Lag and Overall
RRVal_lag_list <- vroom("Outputs/Brazil/results_lag_s3_br_residence.csv.xz")
RR_overall_list <- vroom("Outputs/Brazil/results_rr_s3_br_residence.csv.xz")
## Quantiles t2m
quantiles_t2m<-vroom("Outputs/quantiles_temp_mean.csv.xz")


RRVal_lag_list <- RRVal_lag_list %>% 
    rename(idE = percentile, 
           RR = estimate, LowRR = conf.low, HighRR = conf.hig)
RR_overall_list <- RR_overall_list %>% 
    rename(temp_mean = temp,
           RR = estimate, LowRR = conf.low, HighRR = conf.hig)

## Dose-response Plot Meta
metaMHT<-RR_overall_list$temp_mean[RR_overall_list$RR ==1]
ptmean<-quantile(RR_overall_list$temp_mean, probs = (1:99)/100, na.rm = T)
p50<-ptmean[50]
p95<-ptmean[95]

xlab<-pretty(RR_overall_list$temp_mean)
ylab<-pretty(c(RR_overall_list$LowRR, RR_overall_list$HighRR))
plot_overall<-RR_overall_list %>% 
    ggplot(aes(temp_mean, RR)) + 
    geom_hline(yintercept = 1, size = 0.5) +
    geom_vline(xintercept = c(ptmean[50],
                              ptmean[95]), 
               size = 0.5,colour=c("#4575b4","#d73027"),linetype="dashed") +
    geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.3) +
    geom_line(colour="#cb181d",size=1) +
    geom_point(aes(metaMHT,1),shape = 21, fill = "white", size = 3, colour="#cb181d",
               show.legend = FALSE) +
    scale_x_continuous(breaks = xlab) +
    scale_y_continuous(breaks = ylab) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    labs(x = "Mean Temperature [ºC]", 
         y = "Dengue Hosp. RR",
         # title="Temperature and Dengue Hospitalization",
         subtitle="Cumulative Effects by Temperature Degrees"
    )
plot_overall

ggsave(plot = plot_overall, 
       filename = "Outputs/Plots/plot_overall_br_s3.png", 
       width = 11, 
       height = 9, 
       dpi = 200)

# Percentile Plots Meta
## Plot effects on the P50
ylab<-RRVal_lag_list %>% 
    filter(idE == 50)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p50<-RRVal_lag_list %>%
    filter(idE == 50 & lag %in% seq(0,7,1)) %>%
    ggplot(aes(x=lag, y=RR, ymin = LowRR, ymax = HighRR)) + 
    geom_hline(yintercept = 1, size = 0.5) +
    geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR),
                   size = .8, show.legend = FALSE, colour="#d73027") +
    geom_point(shape = 21, fill = "white", size = 2,show.legend = FALSE) +
    scale_x_continuous(breaks = seq(0, 21, 2)) +
    scale_y_continuous(breaks = ylab) +
    labs(x = "", 
         y = "Dengue Hosp. RR ",
         # title= paste0("Effect on P0.05"),
         subtitle="Lag Effects on 50th Temperature Percentile"
    )+
    theme_minimal()+
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
plot_p50

## Plot effects over P0.95
ylab<-RRVal_lag_list %>% 
    filter(idE == 95)
ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
plot_p95<-RRVal_lag_list %>%
    filter(idE == 95 & lag %in% seq(0,7,1)) %>% 
    ggplot(aes(x=lag, y=RR, ymin = LowRR, ymax = HighRR)) + 
    geom_hline(yintercept = 1, size = 0.5) +
    geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR),
                   size = .8, show.legend = FALSE, colour="#d73027") +
    geom_point(shape = 21, fill = "white", size = 2,show.legend = FALSE) +
    scale_x_continuous(breaks = seq(0, 21, 2)) +
    scale_y_continuous(breaks = ylab) +
    labs(x = "lag (days)", 
         y = "Dengue Hosp. RR ",
         # title= paste0("Effect on P0.95"),
         subtitle="Lag Effects on 95th Temperature Percentile"
    )+
    theme_bw()+
    theme_minimal()
plot_p95

## Plot all together
plot_percent<-(plot_p50 / plot_p95)+
    plot_annotation(title = "Lag Effects", tag_levels = 'a')+
    plot_layout(guides = 'collect')
plot_percent

ggsave(filename = "Outputs/Plots/percentil_effects_br_s3.png", 
       plot = plot_percent, 
       width = 11, 
       height = 9, 
       dpi = 200)

## Figure - paper
library(patchwork)

plot_figureSM<-(plot_overall | (plot_p50 / plot_p95))+
    plot_layout(guides = "collect")+
    plot_annotation(
        # title = "Meta-analysis Results", 
        tag_levels = c("A", "B", "C"), theme = theme_minimal())
plot_figureSM

ggsave(filename = "Outputs/Plots/figure_2_br_s3_analysis.png", 
       plot = plot_figureSM, 
       width = 11, 
       height = 9, 
       dpi = 200)
