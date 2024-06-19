## Clean the environment
rm(list=ls())
gc()

### Loading packages
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(vroom)){install.packages("vroom"); library(vroom)}
if(!require(dlnm)){install.packages("dlnm"); library(dlnm)}
if(!require(splines)){install.packages("splines"); library(splines)}
if(!require(gnm)){install.packages("gnm"); library(gnm)}
if(!require(arrow)){install.packages("arrow"); library(arrow)}


## Loading the cases, by residence
# 20,312,424 / 6
db <-read_parquet("Data/db_to_analysis.parquet")

## Setting up and month_city_dow strata
db <- 
  db %>% 
  mutate(
    month = month(date),
    dow   = wday(date),
    year  = year(date),
  )

gc()

# Brazil 

data<- db |> 
  arrange(code_muni, date) |>
  data.table::as.data.table()

rm(db)

#### strata
data[, stratum:=factor(paste(code_muni, month, dow, sep=":"))]

#### Parameters for cb

# data <- data %>% filter(month %in% c(12, 1:6))

# argvar <- list(fun="bs", knots=quantile(data$temp_mean, c(50,90)/100, na.rm=T), degree=2, int=FALSE)


knotsper <- equalknots(data$temp_mean, nk = 3)
varfun   <- 'ns'

nlag  <- 7  # lag days
xlag  <- 0:nlag
lagnk <- 2   # lag knots
klag  <- logknots(nlag,lagnk, int=TRUE)
lagfun<- 'ns'

argvar   <- list(fun=varfun, knots=knotsper, int=FALSE)
arglag   <- list(fun=lagfun, knots=klag,     int=TRUE)

group    <- factor(data$code_muni)

cbtemp   <- crossbasis(x      = data$temp_mean,
                       group  = group, 
                       lag    = nlag,
                       argvar = argvar,
                       arglag = arglag
) 

tpred_state <- quantile(data$temp_mean, probs=(1:99)/100, na.rm=T)

gc()
    
    ## eliminating strata with zero events to not inflate SE
    data[,  keep:=sum(Cases)>0, by=stratum]
    
    
    ## DLNM
    formula.gnm<- paste0('Cases', ' ~ cbtemp  + ns(date, df = 7*10)')
    
    model.gnm<-gnm(as.formula(formula.gnm), 
                   eliminate = stratum, 
                   data=data, 
                   family = quasipoisson(link = "log"), 
                   subset = keep)  
    
    
    ## Cross-pred
    pred.gnm<-crosspred(cbtemp,model.gnm, at=tpred_state) 
    
    mht.gnm <-pred.gnm$predvar[which.min(pred.gnm$allRRfit)] 
    
    predcen.gnm<-crosspred(basis = cbtemp, model = model.gnm, model.link = 'log',
                           at=tpred_state, cen=mht.gnm)
    
    temp_rr <- tibble( estimate = predcen.gnm$allRRfit,
                       conf.low = predcen.gnm$allRRlow,
                       conf.hig = predcen.gnm$allRRhigh,
                       temp     = predcen.gnm$predvar,
                       mht      = mht.gnm,
                       region   = 'br')
    
    
    
    ## For the lag 
    
    
    red_lag_50<-crossreduce(cbtemp, model.gnm, 
                            type = "var", 
                            cen = 10.7, ## 
                            value = tpred_state[50])
    
    red_lag_95<-crossreduce(cbtemp, model.gnm, 
                            type = "var", 
                            cen = 10.7, ## 
                            value = tpred_state[95])
    
    
    temp_lag50 <- tibble(estimate = red_lag_50$RRfit,
                         conf.low = red_lag_50$RRlow,
                         conf.hig = red_lag_50$RRhigh,
                         lag      = 0:7,
                         cen      = 10.7,
                         value    = tpred_state[50])
    
    temp_lag95 <- tibble(estimate = red_lag_95$RRfit,
                         conf.low = red_lag_95$RRlow,
                         conf.hig = red_lag_95$RRhigh,
                         lag      = 0:7,
                         cen      = 10.7,
                         value    = tpred_state[95])
    
    
    temp_lag <- 
      bind_rows(
        temp_lag50 %>% mutate(percentile = 50),
        temp_lag95 %>% mutate(percentile = 95),
      ) %>% 
      mutate(region   = 'br')

results_rr   <- temp_rr
results_lag  <- temp_lag

##saving
vroom_write(results_rr,   file = "Outputs/Brazil/results_rr_s1_br_residence.csv.xz")
vroom_write(results_lag,  file = "Outputs/Brazil/results_lag_s1_br_residence.csv.xz")

