library(dplyr)
library(missForest)
library(tidyverse)
library(readxl)

bweight_missing <- read_excel()

bweight_missing$married %>% is.na() %>% mean()

is.na(bweight_missing_logit_imp$married)

# Testep que variables estan siginificativamente asociadas a la falta
# del dato en la variable 'married' (osea que mi variable dependiente es is.na(married))
model_imp_logit = glm(is.na(as.factor(married))~.,
                      data = bweight_missing_logit_imp, family = 'binomial')
 
#Eliminar las variables estadisticamente significativas en la fala (es decir, )
bweight_missing_logit_imp = bweight_missing %>% select(-smoke, -cigsper)

model_imp_logit = glm(as.factor(married)~.,
                      data=bweight_missing_logit_imp, family='binomial')

bweight_miss=bweight_missing[is.na(bweight_missing$married),]

bweight_miss['married_prob'] = predict(model_imp_logit.bweight_miss, type=)