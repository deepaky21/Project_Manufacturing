file = "C:/Users/h8man/Downloads/Project3/product_train.csv"
product_train = read.csv(file,stringsAsFactors = F)
file = "C:/Users/h8man/Downloads/Project3/product_test.csv"
product_test = read.csv(file,stringsAsFactors = F)
library(dplyr)
library(tidyr)
product_test$data = "test"
product_train$data = "train"
product_test$went_on_backorder = NA
product = rbind(product_train,product_test)
glimpse(product)
names(product)[sapply(product,function(x) is.character(x))]
table(product$potential_issue)
table(product$deck_risk)
table(product$ppap_risk)
table(product$stop_auto_buy)
table(product$rev_stop)
table(product$went_on_backorder)
table(product$oe_constraint)

product = product%>%
  mutate(potential_issue = as.numeric(ifelse(potential_issue == "Yes",1,0)),
         deck_risk = as.numeric(ifelse(deck_risk == "Yes",1,0)),
         ppap_risk = as.numeric(ifelse(ppap_risk == "Yes",1,0)),
         rev_stop = as.numeric(ifelse(rev_stop == "Yes",1,0)),
         went_on_backorder = as.numeric(ifelse(went_on_backorder == "Yes",1,0)),
         oe_constraint = as.numeric(ifelse(oe_constraint == "Yes",1,0)),
         stop_auto_buy = as.numeric(ifelse(stop_auto_buy == "Yes",1,0)))
glimpse(product)
names(product)[sapply(product, function(x) is.character(x))]
names(product)[sapply(product, function(x) is.integer(x))]
product = product%>%
  mutate(national_inv = as.numeric(national_inv),
         lead_time = as.numeric(lead_time),
         in_transit_qty = as.numeric(in_transit_qty),
         forecast_3_month  = as.numeric(forecast_3_month),
         forecast_6_month = as.numeric(forecast_6_month),
         forecast_9_month = as.numeric(forecast_9_month),
         sales_1_month = as.numeric(sales_1_month),
         sales_3_month = as.numeric(sales_3_month),
         sales_6_month = as.numeric(sales_6_month),
         sales_9_month = as.numeric(sales_9_month),
         min_bank = as.numeric(min_bank),
         pieces_past_due = as.numeric(pieces_past_due),
         local_bo_qty = as.numeric(local_bo_qty))
names(product)[sapply(product, function(x) is.integer(x))]
lapply(product,function(x) sum(is.na(x)))
product_train = product%>%
  filter(data == "train")%>%
  select(-data)
product_test = product%>%
  filter(data == "test")%>%
  select(-data,-went_on_backorder)
# it seems I'm done with data preparation part.
set.seed(2)
s = sample(1:nrow(product_train),0.7*nrow(product_train))
product_train1 = product_train[s,]
product_train2 = product_train[-s,]
library(car)
for_vif = lm(went_on_backorder ~.-sku,data = product_train1)
sort(vif(for_vif),decreasing = T)[1:3]
for_vif = lm(went_on_backorder ~.-sku-forecast_6_month,data = product_train1)
sort(vif(for_vif),decreasing = T)[1:3]
for_vif = lm(went_on_backorder ~.-sku-forecast_6_month-sales_6_month,data = product_train1)
sort(vif(for_vif),decreasing = T)[1:3]
for_vif = lm(went_on_backorder ~.-sku-forecast_6_month-sales_6_month-forecast_9_month,data = product_train1)
sort(vif(for_vif),decreasing = T)[1:3]
for_vif = lm(went_on_backorder ~.-sku-forecast_6_month-sales_6_month-forecast_9_month
             -sales_9_month,data = product_train1)
sort(vif(for_vif),decreasing = T)[1:3]
for_vif = lm(went_on_backorder ~.-sku-forecast_6_month-sales_6_month-forecast_9_month
             -sales_9_month-sales_1_month,data = product_train1)
sort(vif(for_vif),decreasing = T)[1:3]
for_vif = lm(went_on_backorder ~.-sku-forecast_6_month-sales_6_month-forecast_9_month
             -sales_9_month-sales_1_month-perf_12_month_avg,data = product_train1)
sort(vif(for_vif),decreasing = T)[1:3]
for_vif = lm(went_on_backorder ~.-sku-forecast_6_month-sales_6_month-forecast_9_month
             -sales_9_month-sales_1_month-perf_12_month_avg-min_bank,data = product_train1)
sort(vif(for_vif),decreasing = T)[1:3]
for_vif = lm(went_on_backorder ~.-sku-forecast_6_month-sales_6_month-forecast_9_month
             -sales_9_month-sales_1_month-perf_12_month_avg-min_bank-forecast_3_month ,data = product_train1)
sort(vif(for_vif),decreasing = T)[1:3]
# I'm with it also.
log_fit = glm(went_on_backorder ~.-sku-forecast_6_month-sales_6_month-forecast_9_month
              -sales_9_month-sales_1_month-perf_12_month_avg-min_bank,data = product_train1,family = "binomial")
log_fit = step(log_fit)
formula(log_fit)
log_fit = glm(went_on_backorder ~ national_inv + lead_time + in_transit_qty + 
                forecast_3_month + perf_6_month_avg 
                 + deck_risk + rev_stop,data = product_train1,family = "binomial")
summary(log_fit)
library(pROC)
citation("pROC")
value.predict = predict(log_fit,newdata = product_train2,type = "response")
auc_score = auc(roc(product_train2$went_on_backorder,value.predict))
auc_score
prop.table(table(product_train$went_on_backorder))