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
RRVal_lag_list <- vroom("Outputs/State/results_lag_main_state_residence.csv.xz")
RR_overall_list <- vroom("Outputs/State/results_rr_main_state_residence.csv.xz")
## Quantiles t2m
quantiles_t2m<-vroom("Outputs/quantiles_temp_mean.csv.xz")


areas <-
    tibble(
        code_state = c(11,12,13,14,15,16,17, ## Code number for the states in the North Region
                   21,22,23,24,25,26,27,28,29, ## Code number for the states in the Northeast Region
                   31,32,33,35, ## Code number for the states in the Southeast Region
                   41,42,43, ## Code number for the states in the South Region
                   50,51,52,53), ## Code number for the states in the Center-West Region
        
        abbrev_state = c("RO","AC","AM","RR","PA","AP","TO", ## Abbreviation for the states in the North Region
           "MA","PI","CE","RN","PB","PE","AL","SE","BA", ## Abbreviation for the states in the Northeast Region
           "MG","ES","RJ","SP", ## Abbreviation for the states in the Southeast Region
           "PR","SC","RS", ## Abbreviation for the states in the South Region
           "MS","MT","GO","DF")) %>%
    mutate(
    region = case_when(
        code_state %in% c(11,12,13,14,15,16,17)       ~ 'N',
        code_state %in% c(21,22,23,24,25,26,27,28,29) ~ 'NE',
        code_state %in% c(31,32,33,35)                ~ 'SE',
        code_state %in% c(41,42,43)                   ~ 'S',
        code_state %in% c(50,51,52,53)                ~ 'CW'
    ))

RRVal_lag_list <- RRVal_lag_list %>% 
    rename(code_state = region, idE = percentile, 
           RR = estimate, LowRR = conf.low, HighRR = conf.hig) %>% left_join(areas)
RR_overall_list <- RR_overall_list %>% 
    rename(code_state = region, temp_mean = temp,
           RR = estimate, LowRR = conf.low, HighRR = conf.hig) %>% left_join(areas)

## Plot looping
plot_final<-vector("list", 27)
plot_list<-vector("list", 27)
names_stacked_plot<-unique(areas$abbrev_state)
stacked_levels<-length(names_stacked_plot)

## RR 95% t2m
RR_95_05_overall<-vector("list", stacked_levels)
names(RR_95_05_overall)<-names_stacked_plot

for (i in 1:stacked_levels) {
    quantile_state<-quantiles_t2m %>% 
        filter(abbrev_state == names_stacked_plot[i]) %>% 
        as.data.frame()
    # creating plot_list
    ## 
    RR_lag<-RRVal_lag_list %>% 
        filter(abbrev_state == names_stacked_plot[i] & idE %in% c(50, 95))
    
    RR_overall<-RR_overall_list %>% 
        filter(abbrev_state == names_stacked_plot[i])
    
    ## RR 95% t2m dist
    ## 
    RR_95_05_overall[[i]]<-RR_overall %>% 
        filter(temp_mean >= quantile_state$q05) %>% 
        filter(temp_mean <= quantile_state$q95)
    
    # Plot RR Lag
    ## 
    ylab<-pretty(c(RR_lag$LowRR,RR_lag$HighRR))
    
    plot_rr_lag<-RR_lag %>% 
        ggplot(aes(lag, RR, ymin =LowRR , ymax = HighRR)) + 
        geom_hline(yintercept = 1, size = 0.25) +
        geom_linerange(aes(x = lag, y = RR, ymin = LowRR, ymax = HighRR, colour = as.factor(idE)),
                       size = .8, show.legend = FALSE) +
        geom_point(shape = 21, fill = "white", size = 2, aes(colour = as.factor(idE)),
                   show.legend = FALSE) +
        scale_x_continuous(breaks = seq(0, 21, 2)) +
        # scale_y_continuous(breaks = ylab) +
        scale_colour_manual(values = c( "#4575b4","#d73027")) +
        labs(x = "lag (days)", y = "Dengue Hosp. RR ", title = names_stacked_plot[i])+
        facet_wrap(vars(idE),nrow=2)+
        theme_minimal()
    
    # Plot RR Overall
    ## Absolute
    xlab<-pretty(RR_overall$temp_mean)
    ylab<-pretty(c(RR_overall$LowRR,RR_overall$HighRR))
    mht<-RR_overall$temp_mean[which.min(RR_overall$RR)]
    
    plot_rr_overall<-RR_overall %>% 
        ggplot(aes(temp_mean, RR)) + 
        geom_hline(yintercept = 1, size = 0.5) +
        geom_vline(xintercept = c(quantile_state$q05,quantile_state$q95), 
                   size = 0.5,colour=c("#4575b4","#d73027"),linetype="dashed") +
        geom_ribbon(aes(ymin = LowRR, 
                        ymax = HighRR),
                    fill="grey80",alpha=0.2) +
        geom_line(colour="#cb181d",size=1) +
        geom_point(aes(x = mht, y = 1),shape = 21, fill = "white", size = 2, colour="#cb181d",
                   show.legend = FALSE) +
        scale_x_continuous(breaks = xlab) +
        scale_y_continuous(breaks = ylab) +
        theme_minimal()+
        theme(panel.grid.minor = element_blank()) +
        labs(x = "Mean Temperature [ºC]", y = "Dengue Hosp. RR", title = names_stacked_plot[i])
    
    plot_list[[i]]<-list(rr_lag = plot_rr_lag, 
                         rr_overall = plot_rr_overall)
    
    # Patchwork Plot to be saved
    plot_final[[i]]<-(plot_rr_lag | plot_rr_overall)+
        plot_layout(guides = "collect")+
        plot_annotation(tag_levels = "a")
    
    # PAY ATTENTION TO THE NAME OF FILES YOU ARE SAVING, 
    # MAKE SURE IT IS THE CORRECT, TO NOT SUBSTITUTE FOR AN EXISTING ONE!
    ggsave(paste0("Outputs/Plots/main_", names_stacked_plot[i], "_state_overall_effects.png"),
           plot = plot_final[[i]],
           width = 9,
           height = 7,
           dpi = 300)
}

## RR 95% t2m
RR_95_05_overall<-RR_95_05_overall %>% 
    bind_rows()

vroom_write(file = "Outputs/Tables/RR_95_05_t2m_overall_main_state.csv.xz", 
            RR_95_05_overall)

## Overall Brasil Plot
## Absolute
MHT_states<-RR_overall_list %>% 
    filter(RR == 1) %>% 
    select(temp_mean, abbrev_state)

xlab<-pretty(RR_overall_list$temp_mean)
# ylab<-pretty(c(RR_overall_list$LowRR, RR_overall_list$HighRR))
plot_overall_state<-RR_overall_list %>% 
    ggplot(aes(temp_mean, RR)) + 
    geom_line(aes(y=1, x=temp_mean))+
    geom_ribbon(aes(ymin = LowRR,ymax = HighRR),fill="grey80",alpha=0.5) +
    geom_line(colour="#cb181d",size=1) +
    geom_point(data = MHT_states, aes(temp_mean,1),
               shape = 21,
               fill = "white",
               size = 2,
               colour="#cb181d",
               show.legend = FALSE) +
    # scale_x_continuous(breaks = xlab) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    labs(x = "Mean Temperature [ºC]", 
         y = "Dengue Hosp. RR",
         title="Temperature and Dengue Hospitalization",
         subtitle=paste0("Overall by State, 2010-2019"))+
    facet_geo(abbrev_state~., grid = "br_states_grid1", scales = "free")

plot_overall_state

ggsave(paste0("Outputs/Plots/main_all_state_overall_effects.png"),
       plot = plot_overall_state,
       width = 9,
       height = 7,
       dpi = 300)