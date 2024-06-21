rm(list=ls())
gc()

### Loading packages
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(vroom)){install.packages("vroom"); library(vroom)}
if(!require(patchwork)){install.packages("patchwork"); library(patchwork)}
if(!require(geofacet)){install.packages("geofacet"); library(geofacet)}
library(tidylog)


fct_case_when <- function(...) {
    args <- as.list(match.call())
    levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
    levels <- levels[!is.na(levels)]
    factor(dplyr::case_when(...), levels=levels)
}


# Loading databases
## Lag and Overall
RRVal_lag_list <- vroom("Outputs/Region/results_lag_main_region_residence.csv.xz")
RR_overall_list <- vroom("Outputs/Region/results_rr_main_region_residence.csv.xz")
## Quantiles t2m
quantiles_t2m<-vroom("Outputs/quantiles_temp_mean.csv.xz")


RRVal_lag_list <- RRVal_lag_list %>% 
    rename(idE = percentile, 
           RR = estimate, LowRR = conf.low, HighRR = conf.hig)
RR_overall_list <- RR_overall_list %>% 
    rename(temp_mean = temp,
           RR = estimate, LowRR = conf.low, HighRR = conf.hig)

RRVal_lag_list <- RRVal_lag_list %>% 
mutate(region = fct_case_when(
    region == 'N' ~ "North",
    region == 'NE'~ "Northeast", 
    region == 'CW'~ "Center-West", 
    region == 'SE'~ "Southeast", 
    region == 'S' ~ "South"))


RR_overall_list <- RR_overall_list %>% 
    mutate(region = fct_case_when(
        region == 'N' ~"North",
        region == 'NE'~ "Northeast", 
        region == 'CW'~ "Center-West", 
        region == 'SE'~ "Southeast", 
        region == 'S' ~ "South"))


regions_names <- unique(RR_overall_list$region)

## Dose-respostas by Region
metaMHT_region<-RR_overall_list %>% 
    filter(RR == 1) %>% 
    select(temp_mean, region)

ptmean_region<-RR_overall_list |> 
    reframe(p50 = quantile(temp_mean, probs = 0.50),
            p95 = quantile(temp_mean, probs = 0.95),
            .by = region) %>% 
    mutate(
        region = as.character(region),
        region = factor(region,levels=c("North", "Northeast", "Center-West", "Southeast", "South"))) %>% 
    arrange(region)




## Patchwork to make the traced lines of the p50 and p95 on each region plots

xlab<-pretty(RR_overall_list$temp_mean)
ylab<-pretty(c(RR_overall_list$LowRR, RR_overall_list$HighRR))
plot_overall_region<-RR_overall_list %>% 
    ggplot(aes(temp_mean, RR)) + 
    geom_hline(yintercept = 1, size = 0.5) +
    geom_vline(data = ptmean_region,
               aes(xintercept = p50),
               size = 0.5,colour=rep(c("#4575b4"),5),linetype="dashed") +
    geom_vline(data = ptmean_region,
               aes(xintercept = p95),
               size = 0.5,colour=rep(c("#d73027"),5),linetype="dashed") +
    geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.5) +
    geom_line(colour="#cb181d",size=1) +
    geom_point(data = metaMHT_region, aes(temp_mean,1),
               shape = 21,
               fill = "white",
               size = 2,
               colour="#cb181d",
               show.legend = FALSE) +
    scale_x_continuous(breaks = xlab) +
    # scale_y_continuous(breaks = ylab) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    labs(x = "Mean Temperature [ºC] ", 
         y = "Dengue Hosp. RR",
         title="Temperature and Dengue Hospitalization",
         subtitle=paste0("2010-2019"))+
    facet_wrap(region~., scales = "free")
plot_overall_region

ggsave(plot = plot_overall_region,
       filename = paste0("Outputs/Plots/fig3_plot_overall_regions_all_regions_main.png"),
       width = 16,
       height = 9,
       dpi = 200)

ggsave(plot = plot_overall_region,
       filename = paste0("~/Dropbox/Rafael_Paixao/Article_Dengue_hosp/IJHEH/Figures/fig3_plot_overall_regions_all_regions_main.png"),
       width = 16,
       height = 9,
       dpi = 200)

## Lags Plots by Region

## patchwork plots
plot_figure_region<-vector("list", 2)
## Overall Effects and 95th,5th lags effect
# for each region in a separate plot
for (i in 1:5) {
    overall_data_region<-RR_overall_list %>% 
        filter(region == regions_names[i])
    lag_data<-RRVal_lag_list %>% 
        filter(region == regions_names[i])
    
    ## Overall Plot
    MHT_region<-overall_data_region %>% 
        filter(RR == 1) %>% 
        select(temp_mean, region)
    
    ptmean = distinct(lag_data %>% select(idE, value)) %>% pull(value)
    
    xlab<-pretty(overall_data_region$temp_mean)
    ylab<-pretty(c(overall_data_region$LowRR, overall_data_region$HighRR))
    plot_overall_region<-overall_data_region %>% 
        ggplot(aes(temp_mean, RR)) + 
        geom_hline(yintercept = 1, size = 0.5) +
        geom_vline(xintercept = ptmean[1], 
                   size = 0.5,colour=c("#4575b4"),linetype="dashed") +
        geom_vline(xintercept = ptmean[2], 
                   size = 0.5,colour=c("#d73027"),linetype="dashed") +
        geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.5) +
        geom_line(colour="#cb181d",size=1) +
        geom_point(data = MHT_region, aes(x=temp_mean,y=1),
                   shape = 21,
                   fill = "white",
                   size = 2,
                   colour="#cb181d",
                   show.legend = FALSE) +
        scale_x_continuous(breaks = xlab) +
        scale_y_continuous(breaks = ylab) +
        theme_bw() +
        theme(panel.grid.minor = element_blank()) +
        labs(x = "Mean Temperature [ºC]", 
             y = "Dengue Hosp. RR",
             subtitle="Cumulative Effects by Temp. Degrees")
    plot_overall_region
    
    # Percentile Plots Meta
    ## Plot effects over P0.05
    ylab<-lag_data %>% 
        filter(idE == 50)
    ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
    plot_p50<-lag_data %>%
        filter(idE == 50 & lag %in% seq(0,7, 1)) %>% 
        ggplot(aes(x=lag, y=RR, ymin = LowRR, ymax = HighRR)) + 
        geom_hline(yintercept = 1, size = 0.5) +
        geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR),
                       size = .8, show.legend = FALSE, colour="#d73027") +
        geom_point(shape = 21, fill = "white", size = 2,show.legend = FALSE) +
        # scale_x_continuous(breaks = seq(0, 7, 1)) +
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
    ylab<-lag_data %>% 
        filter(idE == 95)
    ylab<-pretty(c(ylab$LowRR, ylab$HighRR))
    plot_p95<-lag_data %>%
        filter(idE == 95 & lag %in% seq(0,7, 1)) %>% 
        ggplot(aes(x=lag, y=RR, ymin = LowRR, ymax = HighRR)) + 
        geom_hline(yintercept = 1, size = 0.5) +
        geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR),
                       size = .8, show.legend = FALSE, colour="#d73027") +
        geom_point(shape = 21, fill = "white", size = 2,show.legend = FALSE) +
        scale_x_continuous(breaks = seq(0, 7, 1)) +
        scale_y_continuous(breaks = ylab) +
        labs(x = "lag (days)", 
             y = "Dengue Hosp. RR ",
             # title= paste0("Effect on P0.95"),
             subtitle="Lag Effects on 95th Temperature Percentile"
        )+
        theme_bw()+
        theme_minimal()
    plot_p95
    
    plot_figure_region[[i]]<-(plot_overall_region | (plot_p50 / plot_p95))+
        plot_layout(guides = "collect",
                    widths = c(2,1))+
        plot_annotation(title = paste0("Results ", regions_names[i], " Region"), 
                        tag_levels = c("A", "B", "C"), 
                        theme = theme_minimal())
    
    ggsave(filename = paste0("Outputs/Plots/figure_SM_", regions_names[i], "_region_meta_analysis.png"),
           plot = plot_figure_region[[i]],
           width = 16,
           height = 9, 
           dpi = 100)
    
    ggsave(filename = paste0("~/Dropbox/Rafael_Paixao/Article_Dengue_hosp/IJHEH/Figures/Supplementary/figure_SM_", regions_names[i], "_region_meta_analysis.png"),
           plot = plot_figure_region[[i]],
           width = 16,
           height = 9, 
           dpi = 100)
}
