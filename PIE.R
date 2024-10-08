# Price Increase Effectiveness
# April 8, 2024

library(openxlsx)
library(dplyr)
library(tidyr)
library(astsa)
library(car)
library(mgcv)
library(ggplot2)
library(CausalImpact)
library(MASS)
library(pscl)
library(emmeans)
library(rrtable)


library(seasonal)
library(x13binary)
library(forecast)
#library(fpp3)


#################### Metric Function ######################
Metrics_fun = function(x,y) {
  AE= abs(x-y)  # Absolute difference
  AE.mean = mean(AE)  # Mean AE
  AE.se = sd(AE)  # standard error of AE
  SE = AE^2  # Squared AE
  MSE = mean(SE)  # Mean of squared error
  RMSE = sqrt(MSE) # Root of MSE
  RMSE.se = sd(SE) # Standard error of SE
  Metrics = data.frame(AE.mean = AE.mean,
                       AE.se = AE.se,
                       RMSE=RMSE)
  Metrics
}
##############################################################

##################### Replace NAs for Numeric Variables ####################
NA_Replace_Num_fun = function(x) {
  for (i in 1:length(x)) {
    if (is.na(x[1])) {
      x[1] = x[!is.na(x)][1]
    }
    else if (is.na(x[i])) {
    x[i] = x[i-1]
    }
  }
  x
}
############################################################################


######################## Shelf price vs week ############################
Price_fun = function(Store_ID) {
  df = Black_Box_df %>%
    dplyr::filter(store.id_ == Store_ID)  %>%
    as.data.frame()
  
  ggplot(df,aes(x=Week_No,y=cy.gallo.shelf.price,group=1))+
    geom_point()+geom_line()+
    labs(title=paste("Store ",Store_ID,sep=""))+
    scale_x_continuous(breaks=df$Week_No,
                       labels=df$Week_No)+
    theme(axis.text.x=element_text(hjust=1,vjust=0.5,angle=90))
  
}
####################################################################

############################## Simulation Function ################
Simulation_Fun_Outputs = function(Store_ID,         # Numeric store ID
                                  Pre_Period,       # c(A,B)
                                  Post_Period,      # c(C,D)
                                  Simulation_Grid,  # Grid with two variables
                                  horizon) {        # No. of period to forecast
  df = Black_Box_df %>%
    dplyr::filter(store.id_ == Store_ID) %>%
    as.data.frame()
  
  CausalImpact_Model = CausalImpact(zoo(df[,c("cy.gallo.unit.sales",
                                              "cy.gallo.shelf.price",
                                              "cy.gallo.comp.shelf.gap")]),
                                    model.args=list(niter=2000,nseasons=52,
                                                    standardize.data=F),
                                    pre.period=Pre_Period,
                                    post.period=Post_Period)
  
  Output_df = matrix(NA,nrow=nrow(Simulation_Grid),ncol=9)
  for (i in 1:nrow(Output_df)) {
    cy.gallo.shelf.price = Simulation_Grid[i,1] 
    cy.gallo.comp.shelf.gap = Simulation_Grid[i,2]
    BSTS_Predicted = predict(CausalImpact_Model$model$bsts.model,
                             horizon=horizon,
                             burn=300,
                             newdata=data.frame(cy.gallo.shelf.price=rep(cy.gallo.shelf.price,horizon),
                                                cy.gallo.comp.shelf.gap=rep(cy.gallo.comp.shelf.gap,horizon)))
    Impact_Summary <- CausalImpact(bsts.model = CausalImpact_Model$model$bsts.model,
                                   post.period.response = BSTS_Predicted$mean)
    
    Output_df[i,1] = unique(df$sto_state)
    Output_df[i,2] = Store_ID
    Output_df[i,3] = Simulation_Grid[i,1]
    Output_df[i,4] = Simulation_Grid[i,3]
    Output_df[i,5] = Simulation_Grid[i,2]
    Output_df[i,6] = base::round(Impact_Summary$summary[1,1],0)
    Output_df[i,7] = base::round(Impact_Summary$summary[1,6],0)
    Output_df[i,8] = base::round(Impact_Summary$summary[1,15],2)
    Output_df[i,9] = base::round(mean(Impact_Summary$model$bsts.model$coefficients[,2]),2)
  }
  Output_df = as.data.frame(Output_df)
  names(Output_df) = c("State","Store_ID","cy.gallo.shelf.price","cy.comp.shelf.price",
                       "cy.gallo.comp.shelf.gap",
                       "Post-Period Sales/WK", "Post-Pre Diff/WK","P",
                       "cy.gallo.shelf.price.coef")
  Output_df = Output_df %>%
    dplyr::mutate(Store_ID = as.numeric(Store_ID),
                  cy.gallo.shelf.price = as.numeric(cy.gallo.shelf.price),
                  cy.comp.shelf.price = as.numeric(cy.comp.shelf.price),
                  cy.gallo.comp.shelf.gap = as.numeric(cy.gallo.comp.shelf.gap),
                  `Post-Period Sales/WK` = as.numeric(`Post-Period Sales/WK`),
                  `Post-Pre Diff/WK` = as.numeric(`Post-Pre Diff/WK`),
                  P = as.numeric(P),
                  cy.gallo.shelf.price.coef = as.numeric(cy.gallo.shelf.price.coef))
  Output_df
  }


Simulation_Grid = data.frame(cy.gallo.shelf.price=c(18,18,18,19,19,19),
                             cy.gallo.comp.shelf.gap=c(-1,0,1,0,1,2)) %>%
  dplyr::mutate(cy.comp.shelf.price = cy.gallo.shelf.price-cy.gallo.comp.shelf.gap) %>%
  as.data.frame()


Simulation_Fun_Outputs(29210,c(1,147),c(148,165),Simulation_Grid,18)
Simulation_Fun_Outputs(18759,c(36,123),c(124,165),Simulation_Grid,42)






##############################################################################
# 1. data
## 1.1. raw data
setwd("C:\\Users\\YCHEN\\OneDrive - E & J Gallo Winery\\Desktop\\WIP\\2024-April-Mykel Garton-Price increase effectiveness-Revenue Management")
load("Black_Box_df.RData")
load("Black_Box_raw.RData")

load("Stores_Price_Increase_State_df.RData")
load("Stores_Price_Increase_Mykel_vector.RData")



(file = list.files(pattern="^Black.{0,100}.csv"))
Black_Box_raw = read.csv(file)

str(Black_Box_raw)
names(Black_Box_raw)

apply(Black_Box_raw[,c("gallo_brand_","comp_brand_","store.id_")],2,unique)


Black_Box_raw = Black_Box_raw %>%
  dplyr::mutate(period_id=as.Date(period_id)) %>%
  dplyr::arrange(store.id_,period_id) %>%
  as.data.frame()


Period_ID_Week_No = data.frame(period_id = sort(unique(Black_Box_raw$period_id)),
                               Week_No = 1:165)

unique(Black_Box_raw$cy.gallo.ad.desc)
unique(Black_Box_raw$cy.gallo.display)
unique(Black_Box_raw$cy.comp.ad.desc)
unique(Black_Box_raw$cy.comp.display)

## 1.2. cleaned data
Black_Box_df = Black_Box_raw %>%
  dplyr::left_join(Period_ID_Week_No,by="period_id") %>%
  dplyr::mutate(cy.gallo.dollar.sales=NA_Replace_Num_fun(cy.gallo.dollar.sales),
                cy.gallo.unit.sales=NA_Replace_Num_fun(cy.gallo.unit.sales),
                cy.gallo.shelf.price=NA_Replace_Num_fun(cy.gallo.shelf.price),
                cy.gallo.ad.desc = as.numeric(as.factor(cy.gallo.ad.desc)),
                cy.gallo.display=as.numeric(as.factor(cy.gallo.display)),
                
                py.gallo.dollar.sales=NA_Replace_Num_fun(py.gallo.dollar.sales),
                py.gallo.unit.sales=NA_Replace_Num_fun(py.gallo.unit.sales),
                py.gallo.shelf.price=NA_Replace_Num_fun(py.gallo.shelf.price),
                
                cy.comp.dollar.sales=NA_Replace_Num_fun(cy.comp.dollar.sales),
                cy.comp.unit.sales=NA_Replace_Num_fun(cy.comp.unit.sales),
                cy.comp.shelf.price=NA_Replace_Num_fun(cy.comp.shelf.price),
                cy.comp.ad.desc = as.numeric(as.factor(cy.comp.ad.desc)),
                cy.comp.display=as.numeric(as.factor(cy.comp.display)),
                
                py.comp.dollar.sales=NA_Replace_Num_fun(py.comp.dollar.sales),
                py.comp.unit.sales=NA_Replace_Num_fun(py.comp.unit.sales),
                py.comp.shelf.price=NA_Replace_Num_fun(py.comp.shelf.price),
                
                cy.gallo.comp.shelf.gap=NA_Replace_Num_fun(cy.gallo.comp.shelf.gap),
                py.gallo.comp.shelf.gap=NA_Replace_Num_fun(py.gallo.comp.shelf.gap),
                ) %>%
  as.data.frame()
  

save(Black_Box_df,file="Black_Box_df.RData")
save(Black_Box_raw,file="Black_Box_raw.RData")

# 2. store
## 2.1. all stores
store_all = sort(unique(Black_Box_df$store.id_))  



## 2.2. stores with mean cy.gallo.unit.sales > 20
store_cy.gallo.unit.sales_20_and_plus = Black_Box_df %>%
  dplyr::group_by(sto_state,store.id_) %>%
  summarize(mean.cy.gallo.unit.sales = mean(cy.gallo.unit.sales)) %>%
  dplyr::arrange(desc(mean.cy.gallo.unit.sales)) %>%
  as.data.frame() %>%
  dplyr::filter(mean.cy.gallo.unit.sales >20) %>%
  as.data.frame() 


length(store_all)  # 2224
str(store_cy.gallo.unit.sales_20_and_plus)  # 688 stores

### 2.1.1. export graphs by store (cy.gallo.shelf.price over week)
plot1 = Price_fun(store_cy.gallo.unit.sales_20_and_plus$store.id_[1])
plot1

plot2docx(plot1)
for (i in 2:((dim(store_cy.gallo.unit.sales_20_and_plus)[1]-1))) (
  plot2docx(Price_fun(store_cy.gallo.unit.sales_20_and_plus$store.id_[i]),
            append=T)
)


## 2.3. stores with structural price increase/decrease (mean(cy.gallo.unit.sales > 20))
(Store_Price_Increase_Decrease_file = list.files(pattern="^stores with"))

Store_Price_Increase_Decrease_df = read.xlsx(Store_Price_Increase_Decrease_file,
                                         sheet="Sheet1",startRow=2)
str(Store_Price_Increase_Decrease_df)

Stores_Price_Increase = sort(Store_Price_Increase_Decrease_df$Stores_Price_Increase)
Stores_Price_Decrease = sort(Store_Price_Increase_Decrease_df$Stores_Price_Decrease)



## 2.4. store by state
Stores_Price_Increase_State_df = Black_Box_df %>%
  dplyr::filter(store.id_ %in% Stores_Price_Increase) %>%
  dplyr::select(store.id_, sto_state) %>%
  dplyr::filter(!duplicated(store.id_)) %>%
  as.data.frame()
Stores_Price_Increase_State_df
save(Stores_Price_Increase_State_df,file="Stores_Price_Increase_State_df.RData")

### 2.4.1. GA stores
plot1 = Price_fun(Stores_Price_Increase[1])
plot2docx(plot1)
for (i in 2:length(Stores_Price_Increase[1:62])) {
  plot2docx(Price_fun(Stores_Price_Increase[i]),
            append=T)
}

### 2.4.2. KY stores
Price_fun(Stores_Price_Increase[63])
Price_fun(Stores_Price_Increase[64])
Price_fun(Stores_Price_Increase[65])


### 2.4.3. MI store 66-81
for (i in 66:81) {
  print(Price_fun(Stores_Price_Increase[i]))
}


## 2.5. stores with price increase provided with Mykel
Stores_Price_Increase_Mykel_file = "stores_w_structural_price_increases.xlsx"

Stores_Price_Increase_Mykel_vector = read.xlsx(Stores_Price_Increase_Mykel_file,
                                               sheet="Sheet1")
str(Stores_Price_Increase_Mykel_vector)

Stores_Price_Increase_Mykel_vector = Stores_Price_Increase_Mykel_vector %>%
  dplyr::pull(Store)

save(Stores_Price_Increase_Mykel_vector,file="Stores_Price_Increase_Mykel_vector.RData")


############################### Individual Stores ############################
############################### May 24 #######################################
# store 211
Store_211_df = Black_Box_df %>%
  dplyr::filter(store.id_ == "211") %>%
  as.data.frame()

str(Store_211_df)
names(Store_211_df)

unique(Store_211_df$comp_brand_)


## EDA
hist(Store_211_df$cy.gallo.unit.sales,nclass=20)
Store_211_df$cy.gallo.unit.sales

hist(Store_211_df$cy.comp.unit.sales,nclass=20)


ggplot(Store_211_df,aes(x=Week_No,y=cy.gallo.unit.sales,group=1))+
  geom_point()+geom_line()+
  geom_point(data=Store_211_df,aes(x=Week_No,y=cy.comp.unit.sales),
             color="blue")+
  geom_line(data=Store_211_df,aes(x=Week_No,y=cy.comp.unit.sales),
             color="blue")+
  scale_x_continuous(breaks=Store_211_df$Week_No,
                     labels=Store_211_df$Week_No)+
  theme(axis.text.x=element_text(angle=90,size=5,vjust=0.5))+
  annotate("text",x=Store_211_df[9,"Week_No"],y=12,
           label="Bota Box",color="blue")+
  annotate("text",x=Store_211_df[10,"Week_No"],y=0,
           label="Black Box")+
  labs(x="Date",y="CY Unit Sales")+
  geom_vline(xintercept=Store_211_df[27,"Week_No"],lty=2,color="red")+
  geom_vline(xintercept=Store_211_df[113,"Week_No"],lty=2,color="red")+
  annotate("text",x=Store_211_df[27,"Week_No"],y=15.5,label="Week 27",
           color="red")+
  annotate("text",x=Store_211_df[113,"Week_No"],y=15.5,label="Week 113",
           color="red")
  
cor(Store_211_df$cy.gallo.unit.sales,Store_211_df$cy.gallo.shelf.price)
cor(Store_211_df$cy.gallo.unit.sales,Store_211_df$cy.comp.unit.sales)
cor(Store_211_df$cy.gallo.shelf.price,Store_211_df$cy.comp.shelf.price)


Store_211_df %>%
  ggplot(aes(x=Week_No,y=cy.gallo.shelf.price,group=1))+
  geom_point()+geom_line()+
  scale_x_continuous(breaks=Store_211_df$Week_No,
                     labels=Store_211_df$Week_No)+
  theme(axis.text.x=element_text(angle=90,size=5,vjust=0.5))+
  geom_vline(xintercept=Store_211_df[27,"Week_No"],lty=2,color="red")+
  geom_vline(xintercept=Store_211_df[113,"Week_No"],lty=2,color="red")+
  annotate("text",x=Store_211_df[27,"Week_No"],y=15.5,label="Week 27",
           color="red")+
  annotate("text",x=Store_211_df[113,"Week_No"],y=15.5,label="Week 113",
           color="red")
  
Store_211_df %>%
  ggplot(aes(x=Week_No,y=cy.gallo.unit.sales,group=1))+
  geom_point()+geom_line()+
  scale_x_continuous(breaks=Store_211_df$Week_No,
                     labels=Store_211_df$Week_No)+
  theme(axis.text.x=element_text(angle=90,size=5,vjust=0.5))+
  geom_smooth(method="lm",se=F)+
  geom_smooth(method="loess",se=F,color="red")+
  geom_vline(xintercept=Store_211_df[27,"Week_No"],lty=2,color="red")+
  geom_vline(xintercept=Store_211_df[113,"Week_No"],lty=2,color="red")+
  annotate("text",x=Store_211_df[27,"Week_No"],y=11,label="Week 27",
           color="red")+
  annotate("text",x=Store_211_df[113,"Week_No"],y=11,label="Week 113",
           color="red")

Store_211_df$Price_Period = c(rep(1,27),rep(2,86),rep(3,52))

Store_211_df %>%
  ggplot(aes(x=jitter(cy.gallo.shelf.price),y=cy.gallo.unit.sales,
             color=as.character(Price_Period)))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  guides(color=guide_legend(title="Price Period"))

Store_211_df %>%
  ggplot(aes(x=jitter(cy.gallo.shelf.price),y=cy.gallo.unit.sales))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  guides(color=guide_legend(title="Price Period"))



## bsts
names(Store_211_df)

### model 1
ss = AddLocalLinearTrend(list(),Store_211_df$cy.gallo.unit.sales)
bsts_model1 = bsts(Store_211_df$cy.gallo.unit.sales,
                   state.specification=ss,
                #       family="poisson",
                   niter=1000)
plot(bsts_model1)

### model 2
ss = AddLocalLinearTrend(list(),Store_211_df$cy.gallo.unit.sales)
ss = AddAutoAr(ss,Store_211_df$cy.gallo.unit.sales)
bsts_model2 = bsts(Store_211_df$cy.gallo.unit.sales,
                   state.specification=ss,
                   #       family="poisson",
                   niter=1000)
plot(bsts_model2)

CompareBstsModels(list("Model 1" = bsts_model1,
                       "Model 2" = bsts_model2),
                  colors = c("black", "red")) # Model 1 is the best


### model 3
ss = AddLocalLinearTrend(list(),Store_211_df$cy.gallo.unit.sales)
ss = AddSeasonal(ss,Store_211_df$cy.gallo.unit.sales,
                 nseasons=4,season.duration=1)
bsts_model3 = bsts(Store_211_df$cy.gallo.unit.sales,
                   state.specification=ss,
                   #    family="poisson",
                   niter=1000)

### model 4
ss = AddLocalLinearTrend(list(),Store_211_df$cy.gallo.unit.sales)
ss = AddSeasonal(ss,Store_211_df$cy.gallo.unit.sales,
                 nseasons=4,season.duration=1)
ss = AddSeasonal(ss,Store_211_df$cy.gallo.unit.sales,
                 nseasons=52,season.duration=1)
bsts_model4 = bsts(Store_211_df$cy.gallo.unit.sales,
                   state.specification=ss,
                   #    family="poisson",
                   niter=1000)

CompareBstsModels(list("Model 1" = bsts_model1,
                       "Model 2" = bsts_model2,
                       "Model 3" = bsts_model3,
                       "Model 4" = bsts_model4),
                  colors = c("black", "red", "blue","orange")) # Model 1 is the best


### model 5
ss = AddLocalLinearTrend(list(),Store_211_df$cy.gallo.unit.sales)
bsts_model5 = bsts(cy.gallo.unit.sales~cy.gallo.shelf.price+ 
                     py.gallo.unit.sales+py.gallo.shelf.price+
                     cy.comp.unit.sales+cy.comp.shelf.price+
                     py.comp.unit.sales+py.comp.shelf.price,
                   state.specification=ss,
                   #    family="poisson",
                   niter=1000,
                   data=Store_211_df)
plot(bsts_model5)
plot(bsts_model5,"coef",burn=200)
summary(bsts_model5)

PlotBstsCoefficients(bsts_model5,burn=200)
PlotBstsComponents(bsts_model5,burn=200)


### model 6
ss = AddLocalLinearTrend(list(),Store_211_df$cy.gallo.unit.sales)
ss = AddDynamicRegression(ss,cy.gallo.unit.sales~cy.gallo.shelf.price+ 
                            py.gallo.unit.sales+py.gallo.shelf.price+
                            cy.comp.unit.sales+cy.comp.shelf.price+
                            py.comp.unit.sales+py.comp.shelf.price,
                          data=Store_211_df)
bsts_model6 = bsts(Store_211_df$cy.gallo.unit.sales,
                   state.specification=ss,
                   #   family="poisson",
                   niter=1000,
                   data=Store_211_df)
plot(bsts_model6)
summary(bsts_model6)


### model comparison
CompareBstsModels(list("Model 1" = bsts_model1,
                       "Model 5" = bsts_model5,
                       "Model 6" = bsts_model6),
                  colors = c("black", "red", "blue")) # model 1 is best
                                                      



## gls/lme
names(Store_211_df)
unique(Store_211_df$cy.gallo.ad.desc)
unique(Store_211_df$cy.gallo.display)
table(Store_211_df$cy.gallo.ad.desc)

Store_211_gls0 = gls(cy.gallo.unit.sales~Week_No+as.factor(Price_Period)+
                       as.factor(cy.gallo.ad.desc)+
                       cy.gallo.shelf.price+
                       cy.comp.shelf.price+
                       cy.comp.unit.sales+
                       py.gallo.shelf.price+
                       py.comp.shelf.price,
                     method="ML",
                     data=Store_211_df)
summary(Store_211_gls0)
acf2(residuals(Store_211_gls0),max.lag=80)
plot(Store_211_gls0,residuals(.,type="normalized")~fitted(.),
     type=c("p","smooth"))
plot(Store_211_gls0,residuals(.,type="normalized")~Price_Period,
     type=c("p","smooth"))
qqPlot(residuals(Store_211_gls0,type="normalized"))

pairs(emmeans(Store_211_gls0,"Price_Period"))


Store_211_gls1 = gls(cy.gallo.unit.sales~as.factor(Price_Period)+
                       py.gallo.shelf.price,
                    # method="ML",
                     data=Store_211_df)
anova(Store_211_gls0,Store_211_gls1)
summary(Store_211_gls1)
acf2(residuals(Store_211_gls1),max.lag=80)
plot(Store_211_gls1,residuals(.,type="normalized")~fitted(.),
     type=c("p","smooth"))
plot(Store_211_gls1,residuals(.,type="normalized")~Price_Period,
     type=c("p","smooth"))
qqPlot(residuals(Store_211_gls1,type="normalized"),ylab="Residuals")

pairs(emmeans(Store_211_gls1,"Price_Period"))

Store_211_df %>%
#  dplyr::filter(py.gallo.shelf.price >15.5) %>%
  ggplot(aes(x=py.gallo.shelf.price,y=cy.gallo.unit.sales))+
  geom_point()+
  geom_smooth(method="lm",se=F)



xtabs(~Price_Period+cy.gallo.ad.desc,data=Store_211_df)



Store_211_lme = lme(cy.gallo.unit.sales~Week_No+
                      cy.gallo.shelf.price+
                      cy.comp.shelf.price+
                      cy.comp.unit.sales+
                      py.gallo.shelf.price+
                      py.comp.shelf.price,
                    method="ML",
                    random=list(Price_Period=~1),
                    data=Store_211_df)
summary(Store_211_lme)
plot(Store_211_lme,residuals(.,type="normalized")~fitted(.),
     type=c("p","smooth"))
qqPlot(residuals(Store_211_lme,type="normalized"),ylab="Residual")
acf2(residuals(Store_211_lme,type="normalized"),max.lag=80)
ranef(Store_211_lme)

Store_211_lme_pred = predict(Store_211_lme,newdata=Store_211_df)
plot(x=1:165,y=Store_211_df$cy.gallo.unit.sales,type="o")
points(x=1:165,y=Store_211_lme_pred,col="red")
lines(x=1:165,y=Store_211_lme_pred,col="red")


Store_211_lme1 = lme(cy.gallo.unit.sales~Week_No,
                  #   method="ML",
                     random=list(Price_Period=~1),
                     data=Store_211_df)
anova(Store_211_lme,Store_211_lme1)
summary(Store_211_lme1)
plot(Store_211_lme1,residuals(.,type="normalized")~fitted(.),
     type=c("p","smooth"))
qqPlot(residuals(Store_211_lme1,type="normalized"),ylab="Residual")
acf2(residuals(Store_211_lme1,type="normalized"),max.lag=80)
ranef(Store_211_lme1)




## count
### poisson
acf2(Store_211_df$cy.gallo.unit.sales)
spectrum(Store_211_df$cy.gallo.unit.sales,log="no")
spectrum(Store_211_df$cy.gallo.unit.sales,log="no",plot=F)

Store_211_Poisson = glm(cy.gallo.unit.sales~Week_No+as.factor(Price_Period)+
                          as.factor(cy.gallo.ad.desc)+
                          cy.gallo.shelf.price+
                          cy.comp.shelf.price+
                          cy.comp.unit.sales+
                          py.gallo.shelf.price+
                          py.comp.shelf.price,
                        family="quasipoisson",
                        data=Store_211_df)
summary(Store_211_Poisson)
acf2(residuals(Store_211_Poisson),max.lag=80)

Store_211_Poisson1 = glm(cy.gallo.unit.sales~as.factor(Price_Period)+
                          py.gallo.shelf.price,
                        family="quasipoisson",
                        data=Store_211_df)
anova(Store_211_Poisson,Store_211_Poisson1)
summary(Store_211_Poisson1)
acf2(residuals(Store_211_Poisson1),max.lag=80)

pairs(emmeans(Store_211_Poisson1,"Price_Period"))

### nb
Store_211_nb = glm.nb(cy.gallo.unit.sales~Week_No+as.factor(Price_Period)+
                        as.factor(cy.gallo.ad.desc)+
                        cy.gallo.shelf.price+
                        cy.comp.shelf.price+
                        cy.comp.unit.sales+
                        py.gallo.shelf.price+
                        py.comp.shelf.price,
                      data=Store_211_df)
summary(Store_211_nb)
acf2(residuals(Store_211_nb),max.lag=80)
pchisq(Store_211_nb$deviance,Store_211_nb$df.residual,lower.tail=F)

Store_211_nb1 = glm.nb(cy.gallo.unit.sales~as.factor(Price_Period)+
                         py.gallo.shelf.price,
                       data=Store_211_df)
anova(Store_211_nb,Store_211_nb1)
summary(Store_211_nb1)
acf2(residuals(Store_211_nb1),max.lag=80)
pchisq(Store_211_nb1$deviance,Store_211_nb1$df.residual,lower.tail=F)


### zero inflated nb
Store_211_zeroinfl_nb = zeroinfl(cy.gallo.unit.sales~as.factor(Price_Period)+
                                   py.gallo.shelf.price,
                        dist="negbin",
                      data=Store_211_df)
summary(Store_211_zeroinfl_nb)
acf2(residuals(Store_211_zeroinfl_nb),max.lag=80)
pchisq(Store_211_zeroinfl_nb$deviance,Store_211_zeroinfl_nb$df.residual,lower.tail=F)



## CausalImpact
### w/o predictors
Store_211_1_High = c(1,27)
Store_211_2_Low = c(28,113)
Store_211_3_High = c(114,165)

Store_211_High_To_Low = CausalImpact(Store_211_df[,"cy.gallo.unit.sales"],
                                     pre.period=Store_211_1_High,
                                     post.period=Store_211_2_Low)
Store_211_High_To_Low = CausalImpact(zoo(Store_211_df[,c("cy.gallo.unit.sales",
                                                         "cy.gallo.shelf.price")]),
                                     pre.period=Store_211_1_High,
                                     post.period=Store_211_2_Low)
Store_211_High_To_Low = CausalImpact(zoo(Store_211_df[,c("cy.gallo.unit.sales",
                                              #           "cy.gallo.shelf.price",
                                                         "cy.comp.unit.sales")]),
                                     pre.period=Store_211_1_High,
                                     post.period=Store_211_2_Low)
Store_211_High_To_Low = CausalImpact(zoo(Store_211_df[,c("cy.gallo.unit.sales",
                                                       #  "cy.gallo.shelf.price",
                                                         "cy.comp.unit.sales",
                                                         "py.gallo.shelf.price",
                                                         "py.comp.shelf.price")]),
                                     pre.period=Store_211_1_High,
                                     post.period=Store_211_2_Low)

plot(Store_211_High_To_Low)
plot(Store_211_High_To_Low$model$bsts.model,"coef")
summary(Store_211_High_To_Low)

Store_211_Low_To_High = CausalImpact(Store_211_df[,"cy.gallo.unit.sales"],
                                     pre.period=Store_211_2_Low,
                                     post.period=Store_211_3_High)
Store_211_Low_To_High = CausalImpact(zoo(Store_211_df[,c("cy.gallo.unit.sales",
                                                  "Price_Period")]),
                                     pre.period=Store_211_2_Low,
                                     post.period=Store_211_3_High)
Store_211_Low_To_High = CausalImpact(zoo(Store_211_df[,c("cy.gallo.unit.sales",
                                                         "cy.gallo.shelf.price")]),
                                     pre.period=Store_211_2_Low,
                                     post.period=Store_211_3_High)
Store_211_Low_To_High = CausalImpact(zoo(Store_211_df[,c("cy.gallo.unit.sales",
                                                         "cy.gallo.shelf.price",
                                                         "cy.comp.unit.sales")]),
                                     pre.period=Store_211_2_Low,
                                     post.period=Store_211_3_High)
Store_211_Low_To_High = CausalImpact(zoo(Store_211_df[,c("cy.gallo.unit.sales",
                                                         "cy.gallo.shelf.price",
                                                         "cy.comp.unit.sales",
                                                         "py.gallo.shelf.price",
                                                         "py.comp.shelf.price")]),
                                     pre.period=Store_211_2_Low,
                                     post.period=Store_211_3_High)
Store_211_Low_To_High = CausalImpact(zoo(Store_211_df[,c("cy.gallo.unit.sales",
                                                         "cy.gallo.shelf.price",
                                                         
                                                         "py.gallo.unit.sales",
                                                         "py.gallo.shelf.price",
                                                         
                                                         "cy.comp.unit.sales",
                                                         "cy.comp.shelf.price",
                                                         
                                                         "py.comp.unit.sales",
                                                         "py.comp.shelf.price",
                                                         
                                                         "cy.gallo.comp.shelf.gap",
                                                         "py.gallo.comp.shelf.gap",
                                                         
                                                         "cy.gallo.ad.desc",
                                                         "cy.gallo.display",
                                                         
                                                         "cy.comp.ad.desc",
                                                         "cy.comp.display")]),
                                     pre.period=Store_211_2_Low,
                                     post.period=Store_211_3_High)

plot(Store_211_Low_To_High)
plot(Store_211_Low_To_High$model$bsts.model,"coef")
summary(Store_211_Low_To_High)
str(Store_211_Low_To_High)

Store_211_Low_To_High$series %>%
  ggplot(aes(x=response,y=point.pred))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  geom_smooth(method="loess",se=F)

Store_211_Low_To_High$series %>%
  ggplot(aes(x=1:165,y=response))+
  geom_point()+geom_line()+
  geom_point(aes(y=point.pred),color="red")+
  geom_line(aes(y=point.pred),color="red")

with(Store_211_Low_To_High$series[28:165,],cor(response,point.pred))


Store_211_df %>%
  ggplot(aes(x=Week_No,y=py.gallo.shelf.price))+
  geom_point()+geom_line()+
  geom_smooth(method="lm",se=F)+
  geom_point(aes(y=cy.gallo.unit.sales),color="red")+
  geom_line(aes(y=cy.gallo.unit.sales),color="red")+
  labs(y="cy.gallo.unit.sales (red)/py.gallo.shelf.price (black)")

Store_211_df %>%
  ggplot(aes(x=jitter(py.gallo.shelf.price),y=cy.gallo.unit.sales))+
  geom_point()+
  geom_smooth(method="lm",se=F)


Store_211_Pre_Period = Store_211_df[28:165,]
dim(Store_211_Pre_Period)
Store_211_Pre_Period[87:138,"cy.gallo.unit.sales"] = NA

Store_211_Post_Period_Response = Store_211_df$cy.gallo.unit.sales[114:165]

ss = AddLocalLevel(list(),Store_211_Pre_Period$cy.gallo.unit.sales)
ss = AddAutoAr(ss,Store_211_Pre_Period$cy.gallo.unit.sales)
Store_211_Low_To_High_bsts = bsts(cy.gallo.unit.sales~cy.gallo.shelf.price+
                     py.gallo.unit.sales+py.gallo.shelf.price+
                     cy.comp.unit.sales+cy.comp.shelf.price+
                     py.comp.unit.sales+py.comp.shelf.price+
                     cy.gallo.comp.shelf.gap+py.gallo.comp.shelf.gap+
                     cy.gallo.ad.desc+cy.gallo.display+
                     cy.comp.ad.desc+cy.comp.display,
                   state.specification=ss,
                #       family="poisson",
                   niter=1000,
                   data=Store_211_Pre_Period)

Store_211_Low_To_High_impact <- CausalImpact(bsts.model = Store_211_Low_To_High_bsts,
                       post.period.response = Store_211_Post_Period_Response)

plot(Store_211_Low_To_High_impact)
summary(Store_211_Low_To_High_impact)




## ts decompose
names(Store_211_df)

decompose(ts(Store_211_df[47:165,]$cy.gallo.unit.sales),filter=c(0,3))

Store_211_SMA5 = SMA(ts(Store_211_df[47:165,]$cy.gallo.unit.sales),n=3)
plot.ts(Store_211_SMA5)




################## store 29202 (June 5, 2024)
## data and EDA
Store_29202_df = Black_Box_df %>%
  dplyr::filter(store.id_ == 29202) %>%
  as.data.frame()

Store_29202_df %>%
  dplyr::select(store.id_,sto_banner,size_,sto_state,cluster_) %>%
  as.data.frame() %>%
  head(n=1)

with(Store_29202_df,cor(cy.gallo.unit.sales,cy.gallo.shelf.price))
with(Store_29202_df,cor(log(cy.gallo.unit.sales),log(cy.gallo.shelf.price)))

with(Store_29202_df,cor(cy.gallo.unit.sales,cy.gallo.comp.shelf.gap))


Black_Box_raw %>%
  dplyr::filter(store.id_ == 29202) %>%
  ggplot(aes(x=period_id,y=cy.gallo.shelf.price))+
  geom_point()+geom_line()
Black_Box_raw %>%
  dplyr::filter(store.id_ == 29202) %>%
  ggplot(aes(x=period_id,y=cy.gallo.unit.sales))+
  geom_point()+geom_line()
Black_Box_raw %>%
  dplyr::filter(store.id_ == 29202) %>%
  ggplot(aes(x=cy.gallo.shelf.price,y=cy.gallo.unit.sales))+
  geom_point()+
  geom_smooth(method="lm",se=F)

Black_Box_df %>%
  dplyr::filter(store.id_ == 29202) %>%
  ggplot(aes(x=Week_No,y=cy.gallo.shelf.price))+
  geom_point()+geom_line()+
  scale_x_continuous(breaks=1:165,labels=1:165)+
  theme(axis.text.x=element_text(angle=90))+
  geom_vline(xintercept=147,lty=2,color="red")+
  annotate("text",x=145,y=18,label="Week 147", color="red",
           angle=90,size=3)

Black_Box_df %>%
  dplyr::filter(store.id_ == 29202) %>%
  ggplot(aes(x=Week_No,y=cy.gallo.unit.sales))+
  geom_point()+geom_line()+
  scale_x_continuous(breaks=1:165,labels=1:165)+
  theme(axis.text.x=element_text(angle=90))+
  geom_vline(xintercept=147,lty=2,color="red")+
  annotate("text",x=145,y=40,label="Week 147", color="red",
           angle=90,size=3)

Black_Box_df %>%
  dplyr::filter(store.id_ == 29202) %>%
  ggplot(aes(x=cy.gallo.shelf.price,y=cy.gallo.unit.sales))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  annotate("text",x=18,y=40,label="r = -0.34")

Black_Box_df %>%
  dplyr::filter(store.id_ == 29202) %>%
  ggplot(aes(x=cy.gallo.comp.shelf.gap,y=cy.gallo.unit.sales))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  annotate("text",x=1,y=40,label="r = -0.06")


## ts decompose
Store_29202_SMA = SMA(Store_29202_df$cy.gallo.unit.sales,n=5)
plot.ts(Store_29202_SMA)


## default model
names(Store_29202_df)
Store_29202_df$Week_No


spectrum(Store_29202_df$cy.gallo.unit.sales,log="no",plot=T)
spectrum(Store_29202_df$cy.gallo.unit.sales,log="no",spans=c(3,3),plot=F)

spectrum(Store_29202_df$cy.gallo.unit.sales[1:147],log="no",spans=c(3,3),plot=T)
spectrum(Store_29202_df$cy.gallo.unit.sales[1:147],log="no",spans=c(3,3),plot=F)


Store_29202_Low_Price_Period = c(1,147)
Store_29202_High_Price_Period = c(148,165)

Store_29202_Low_To_High_Default1 = CausalImpact(zoo(Store_29202_df[,c("cy.gallo.unit.sales",
                                                                      "cy.gallo.shelf.price",
                                                                      
                                                                      "py.gallo.unit.sales",
                                                                      "py.gallo.shelf.price",
                                                                      
                                                                      "cy.comp.unit.sales",
                                                                      "cy.comp.shelf.price",
                                                                      
                                                                      "py.comp.unit.sales",
                                                                      "py.comp.shelf.price",
                                                                      
                                                                      "cy.gallo.comp.shelf.gap",
                                                                      "py.gallo.comp.shelf.gap",
                                                                      
                                                                      "cy.gallo.ad.desc",
                                                                      "cy.gallo.display",
                                                                      
                                                                      "cy.comp.ad.desc",
                                                                      "cy.comp.display")]),
                                                model.args=list(niter=5000,nseasons=52),
                                                pre.period=Store_29202_Low_Price_Period,
                                                post.period=Store_29202_High_Price_Period)

plot(Store_29202_Low_To_High_Default1)
plot(Store_29202_Low_To_High_Default1$model$bsts.model,"coef")
summary(Store_29202_Low_To_High_Default1)
str(Store_29202_Low_To_High_Default1$model$bsts.model)


with(Store_29202_df[1:147,],cor(py.comp.shelf.price,cy.gallo.comp.shelf.gap))

Store_29202_df[1:147,] %>%
  ggplot(aes(x=Week_No,y=cy.gallo.comp.shelf.gap,group=1))+
  geom_point()+geom_line()+
  xlim(c(0,165))+
  geom_point(data=Store_29202_df[148:165,],aes(y=cy.gallo.comp.shelf.gap),
             color="red")+
  geom_line(data=Store_29202_df[148:165,],aes(y=cy.gallo.comp.shelf.gap),
            color="red")

mean(Store_29202_df[1:147,"cy.gallo.comp.shelf.gap"])
mean(Store_29202_df[148:165,"cy.gallo.comp.shelf.gap"])


Store_29202_Low_To_High_Default = CausalImpact(zoo(Store_29202_df[,c("cy.gallo.unit.sales",
                                                                     "cy.gallo.shelf.price",
                                                                     "cy.gallo.comp.shelf.gap")]),
                                               model.args=list(niter=3000,nseasons=52,
                                                               standardize.data=F),
                                               pre.period=Store_29202_Low_Price_Period,
                                               post.period=Store_29202_High_Price_Period)

plot(Store_29202_Low_To_High_Default)
plot(Store_29202_Low_To_High_Default$model$bsts.model,"coef")
summary(Store_29202_Low_To_High_Default)
str(Store_29202_Low_To_High_Default$model$bsts.model)
Store_29202_Low_To_High_Default$model$bsts.model$coefficients
Store_29202_Low_To_High_Default$model$bsts.model$coefficients[,2]
hist(Store_29202_Low_To_High_Default$model$bsts.model$coefficients[,2],
     nclass=50)

summary(Store_29202_Low_To_High_Default$model$bsts.model$coefficients[,2])


### simulation
#### batch simulation
Store_29202_Simulation_Grid = data.frame(expand.grid(cy.gallo.shelf.price=17:22,
                                                     cy.gallo.comp.shelf.gap=-2:2)) %>%
  dplyr::mutate(cy.comp.shelf.price = cy.gallo.shelf.price-cy.gallo.comp.shelf.gap) %>%
  as.data.frame()

Store_29202_Simulation_Outputs = Simulation_Fun_Outputs(29202,
                                                        c(1,147),
                                                        c(148,165),
                                                        Store_29202_Simulation_Grid,
                                                        18)


Store_29202_Simulation_Outputs %>%
  ggplot(aes(x=cy.gallo.shelf.price,y=`Average Effect`,color=as.character(cy.comp.shelf.price)))+
  geom_point() + geom_line()+
  guides(color=guide_legend("cy.comp.shelf.price"))+
  labs(y="Cases Increase or Decrease/wk")+
  annotate("text",x=Store_29202_Simulation_Outputs$cy.gallo.shelf.price+0.1,
           y=Store_29202_Simulation_Outputs$`Average Effect`+2.5,
           label=paste("gap = ",Store_29202_Simulation_Outputs$cy.gallo.comp.shelf.gap,sep=""),size=3)+
  annotate("text",x=Store_29202_Simulation_Outputs$cy.gallo.shelf.price+0.1,
           y=Store_29202_Simulation_Outputs$`Average Effect`+1,
           label=paste("P = ",Store_29202_Simulation_Outputs$P,sep=""),size=3)


Store_29202_Simulation_Outputs %>%
  ggplot(aes(x=cy.gallo.shelf.price,y=`Cumulative Effect`,color=as.character(cy.comp.shelf.price)))+
  geom_point() + geom_line()+
  guides(color=guide_legend("cy.comp.shelf.price"))+
  labs(y="Cumulative Cases Increase or Decrease (Post Periods) ")+
  annotate("text",x=Store_29202_Simulation_Outputs$cy.gallo.shelf.price+0.1,
           y=Store_29202_Simulation_Outputs$`Cumulative Effect`+40,
           label=paste("gap = ",Store_29202_Simulation_Outputs$cy.gallo.comp.shelf.gap,sep=""),size=3)+
  annotate("text",x=Store_29202_Simulation_Outputs$cy.gallo.shelf.price+0.1,
           y=Store_29202_Simulation_Outputs$`Cumulative Effect`+15,
           label=paste("P = ",Store_29202_Simulation_Outputs$P,sep=""),size=3)




#### cy.gallo.shelf.price = 17 cy.comp.shelf.price = 17
Store_29202_Low_To_High_Default_pred_case1 = predict(Store_29202_Low_To_High_Default$model$bsts.model,
                                                      horizon=18,
                                                      newdata=data.frame(cy.gallo.shelf.price=rep(17,18),
                                                                         cy.gallo.comp.shelf.gap=rep(0,18)))


Store_29202_Low_To_High_impact_case1 <- CausalImpact(bsts.model = Store_29202_Low_To_High_Default$model$bsts.model,
                                                      post.period.response = Store_29202_Low_To_High_Default_pred_case1$mean)
plot(Store_29202_Low_To_High_impact_case1)
summary(Store_29202_Low_To_High_impact_case1)

#### cy.gallo.shelf.price = 18 cy.comp.shelf.price = 17
Store_29202_Low_To_High_Default_pred_case2 = predict(Store_29202_Low_To_High_Default$model$bsts.model,
                                                     horizon=18,
                                                     newdata=data.frame(cy.gallo.shelf.price=rep(18,18),
                                                                        cy.gallo.comp.shelf.gap=rep(1,18)))


Store_29202_Low_To_High_impact_case2 <- CausalImpact(bsts.model = Store_29202_Low_To_High_Default$model$bsts.model,
                                                     post.period.response = Store_29202_Low_To_High_Default_pred_case2$mean)
plot(Store_29202_Low_To_High_impact_case2)
summary(Store_29202_Low_To_High_impact_case2)


#### cy.gallo.shelf.price = 19 cy.comp.shelf.price = 18
Store_29202_Low_To_High_Default_pred_case3 = predict(Store_29202_Low_To_High_Default$model$bsts.model,
                                                     horizon=18,
                                                     newdata=data.frame(cy.gallo.shelf.price=rep(19,18),
                                                                        cy.gallo.comp.shelf.gap=rep(1,18)))


Store_29202_Low_To_High_impact_case3 <- CausalImpact(bsts.model = Store_29202_Low_To_High_Default$model$bsts.model,
                                                     post.period.response = Store_29202_Low_To_High_Default_pred_case3$mean)
plot(Store_29202_Low_To_High_impact_case3)
summary(Store_29202_Low_To_High_impact_case3)




#### cy.gallo.shelf.price = 19 cy.comp.shelf.price = 17
Store_29202_Low_To_High_Default_pred_case4 = predict(Store_29202_Low_To_High_Default$model$bsts.model,
                                                     horizon=18,
                                                     newdata=data.frame(cy.gallo.shelf.price=rep(19,18),
                                                                        cy.gallo.comp.shelf.gap=rep(2,18)))


Store_29202_Low_To_High_impact_case4 <- CausalImpact(bsts.model = Store_29202_Low_To_High_Default$model$bsts.model,
                                                     post.period.response = Store_29202_Low_To_High_Default_pred_case4$mean)
plot(Store_29202_Low_To_High_impact_case4)
summary(Store_29202_Low_To_High_impact_case4)



#### cy.gallo.shelf.price = 19 cy.comp.shelf.price = 19
Store_29202_Low_To_High_Default_pred_case5 = predict(Store_29202_Low_To_High_Default$model$bsts.model,
                                                     horizon=18,
                                                     newdata=data.frame(cy.gallo.shelf.price=rep(19,18),
                                                                        cy.gallo.comp.shelf.gap=rep(-1,18)))


Store_29202_Low_To_High_impact_case5 <- CausalImpact(bsts.model = Store_29202_Low_To_High_Default$model$bsts.model,
                                                     post.period.response = Store_29202_Low_To_High_Default_pred_case5$mean)
plot(Store_29202_Low_To_High_impact_case5)
summary(Store_29202_Low_To_High_impact_case5)
Store_29202_Low_To_High_impact_case5$summary
Store_29202_Low_To_High_impact_case5$summary[1,6]


################## store 29210 (June 13, 2024)
## data and EDA
Store_29210_df = Black_Box_df %>%
  dplyr::filter(store.id_ == 29210) %>%
  as.data.frame()

Store_29210_df %>%
  dplyr::select(store.id_,sto_banner,size_,sto_state,cluster_) %>%
  as.data.frame() %>%
  head(n=1)

with(Store_29210_df,cor(cy.gallo.unit.sales,cy.gallo.shelf.price))
with(Store_29210_df,cor(log(cy.gallo.unit.sales),log(cy.gallo.shelf.price)))

with(Store_29210_df,cor(cy.gallo.unit.sales,cy.gallo.comp.shelf.gap))


Black_Box_df %>%
  dplyr::filter(store.id_ == 29210) %>%
  ggplot(aes(x=Week_No,y=cy.gallo.shelf.price))+
  geom_point()+geom_line()+
  scale_x_continuous(breaks=1:165,labels=1:165)+
  theme(axis.text.x=element_text(angle=90))+
  geom_vline(xintercept=147,lty=2,color="red")+
  annotate("text",x=145,y=18,label="Week 147", color="red",
           angle=90,size=3)

Black_Box_df %>%
  dplyr::filter(store.id_ == 29210) %>%
  ggplot(aes(x=Week_No,y=cy.gallo.unit.sales))+
  geom_point()+geom_line()+
  scale_x_continuous(breaks=1:165,labels=1:165)+
  theme(axis.text.x=element_text(angle=90))+
  geom_vline(xintercept=147,lty=2,color="red")+
  annotate("text",x=145,y=40,label="Week 147", color="red",
           angle=90,size=3)


## default model
names(Store_29210_df)

spectrum(Store_29210_df$cy.gallo.unit.sales,log="no",plot=T)
spectrum(Store_29210_df$cy.gallo.unit.sales,log="no",plot=F)

spectrum(Store_29210_df$cy.gallo.unit.sales[1:147],log="no",spans=c(3,3),plot=T)
spectrum(Store_29210_df$cy.gallo.unit.sales[1:147],log="no",spans=c(3,3),plot=F)


Store_29210_Low_Price_Period = c(1,147)
Store_29210_High_Price_Period = c(148,165)

Store_29210_Low_To_High_Default1 = CausalImpact(zoo(Store_29210_df[,c("cy.gallo.unit.sales",
                                                                      "cy.gallo.shelf.price",
                                                                      
                                                                      "py.gallo.unit.sales",
                                                                      "py.gallo.shelf.price",
                                                                      
                                                                      "cy.comp.unit.sales",
                                                                      "cy.comp.shelf.price",
                                                                      
                                                                      "py.comp.unit.sales",
                                                                      "py.comp.shelf.price",
                                                                      
                                                                      "cy.gallo.comp.shelf.gap",
                                                                      "py.gallo.comp.shelf.gap",
                                                                      
                                                                      "cy.gallo.ad.desc",
                                                                      "cy.gallo.display",
                                                                      
                                                                      "cy.comp.ad.desc",
                                                                      "cy.comp.display")]),
                                                model.args=list(niter=5000,nseasons=52),
                                                pre.period=Store_29210_Low_Price_Period,
                                                post.period=Store_29210_High_Price_Period)

plot(Store_29210_Low_To_High_Default1)
plot(Store_29210_Low_To_High_Default1$model$bsts.model,"coef")
summary(Store_29210_Low_To_High_Default1)
str(Store_29210_Low_To_High_Default1$model$bsts.model)


with(Store_29210_df[1:147,],cor(py.comp.shelf.price,cy.gallo.comp.shelf.gap))

Store_29210_df[1:147,] %>%
  ggplot(aes(x=Week_No,y=cy.gallo.comp.shelf.gap,group=1))+
  geom_point()+geom_line()+
  xlim(c(0,165))+
  geom_point(data=Store_29210_df[148:165,],aes(y=cy.gallo.comp.shelf.gap),
             color="red")+
  geom_line(data=Store_29210_df[148:165,],aes(y=cy.gallo.comp.shelf.gap),
            color="red")

mean(Store_29210_df[1:147,"cy.gallo.comp.shelf.gap"])
mean(Store_29210_df[148:165,"cy.gallo.comp.shelf.gap"])


Store_29210_Low_To_High_Default = CausalImpact(zoo(Store_29210_df[,c("cy.gallo.unit.sales",
                                                                     "cy.gallo.shelf.price",
                                                                     "cy.gallo.comp.shelf.gap")]),
                                               model.args=list(niter=2000,nseasons=52,
                                                               standardize.data=F),
                                               pre.period=Store_29210_Low_Price_Period,
                                               post.period=Store_29210_High_Price_Period)

plot(Store_29210_Low_To_High_Default)
plot(Store_29210_Low_To_High_Default$model$bsts.model,"coef")
summary(Store_29210_Low_To_High_Default)
str(Store_29210_Low_To_High_Default$model$bsts.model)
Store_29210_Low_To_High_Default$model$bsts.model$coefficients[,2]
Store_29210_Low_To_High_Default$summary[1,1]

### simulation
#### batch simulation
Store_29210_Simulation_Grid = data.frame(expand.grid(cy.gallo.shelf.price=17:22,
                                                     cy.gallo.comp.shelf.gap=-2:2)) %>%
  dplyr::mutate(cy.comp.shelf.price = cy.gallo.shelf.price-cy.gallo.comp.shelf.gap) %>%
  as.data.frame()

Store_29210_Simulation_Outputs = Simulation_Fun_Outputs(29210,
                                                        c(1,147),
                                                        c(148,165),
                                                        Store_29202_Simulation_Grid,
                                                        18)

Store_29210_Simulation_Outputs

Store_29210_Simulation_Outputs %>%
  ggplot(aes(x=cy.gallo.shelf.price,y=`Average Effect`,color=as.character(cy.comp.shelf.price)))+
  geom_point() + geom_line()+
  guides(color=guide_legend("cy.comp.shelf.price"))+
  labs(y="Cases Increase or Decrease/wk")+
  annotate("text",x=Store_29210_Simulation_Outputs$cy.gallo.shelf.price+0.1,
           y=Store_29210_Simulation_Outputs$`Average Effect`+2.5,
           label=paste("gap = ",Store_29210_Simulation_Outputs$cy.gallo.comp.shelf.gap,sep=""),size=3)+
  annotate("text",x=Store_29210_Simulation_Outputs$cy.gallo.shelf.price+0.1,
           y=Store_29210_Simulation_Outputs$`Average Effect`+1,
           label=paste("P = ",Store_29210_Simulation_Outputs$P,sep=""),size=3)


Store_29210_Simulation_Outputs %>%
  ggplot(aes(x=cy.gallo.shelf.price,y=`Cumulative Effect`,color=as.character(cy.comp.shelf.price)))+
  geom_point() + geom_line()+
  guides(color=guide_legend("cy.comp.shelf.price"))+
  labs(y="Cumulative Cases Increase or Decrease (Post Periods) ")+
  annotate("text",x=Store_29210_Simulation_Outputs$cy.gallo.shelf.price+0.1,
           y=Store_29210_Simulation_Outputs$`Cumulative Effect`+40,
           label=paste("gap = ",Store_29210_Simulation_Outputs$cy.gallo.comp.shelf.gap,sep=""),size=3)+
  annotate("text",x=Store_29210_Simulation_Outputs$cy.gallo.shelf.price+0.1,
           y=Store_29210_Simulation_Outputs$`Cumulative Effect`+15,
           label=paste("P = ",Store_29210_Simulation_Outputs$P,sep=""),size=3)


############ store 29509 (June 5, 2024)
## data and EDA
Store_29509_df = Black_Box_df %>%
  dplyr::filter(store.id_ == 29509) %>%
  as.data.frame()

Store_29509_df %>%
  dplyr::select(store.id_,sto_banner,size_,sto_state,cluster_) %>%
  as.data.frame() %>%
  head(n=1)

with(Store_29509_df,cor(cy.gallo.unit.sales,cy.gallo.shelf.price))
with(Store_29509_df,cor(log(cy.gallo.unit.sales),log(cy.gallo.shelf.price)))

with(Store_29509_df,cor(cy.gallo.unit.sales,cy.gallo.comp.shelf.gap))


Black_Box_raw %>%
  dplyr::filter(store.id_ == 29509) %>%
  ggplot(aes(x=period_id,y=cy.gallo.shelf.price))+
  geom_point()+geom_line()
Black_Box_raw %>%
  dplyr::filter(store.id_ == 29509) %>%
  ggplot(aes(x=period_id,y=cy.gallo.unit.sales))+
  geom_point()+geom_line()
Black_Box_raw %>%
  dplyr::filter(store.id_ == 29509) %>%
  ggplot(aes(x=cy.gallo.shelf.price,y=cy.gallo.unit.sales))+
  geom_point()+
  geom_smooth(method="lm",se=F)

Black_Box_df %>%
  dplyr::filter(store.id_ == 29509) %>%
  ggplot(aes(x=Week_No,y=cy.gallo.shelf.price))+
  geom_point()+geom_line()+
  scale_x_continuous(breaks=1:165,labels=1:165)+
  theme(axis.text.x=element_text(angle=90))+
  geom_vline(xintercept=147,lty=2,color="red")+
  annotate("text",x=145,y=18,label="Week 147", color="red",
           angle=90,size=3)

Black_Box_df %>%
  dplyr::filter(store.id_ == 29509) %>%
  ggplot(aes(x=Week_No,y=cy.gallo.unit.sales))+
  geom_point()+geom_line()+
  scale_x_continuous(breaks=1:165,labels=1:165)+
  theme(axis.text.x=element_text(angle=90))+
  geom_vline(xintercept=147,lty=2,color="red")+
  annotate("text",x=145,y=90,label="Week 147", color="red",
           angle=90,size=3)

Black_Box_df %>%
  dplyr::filter(store.id_ == 29509) %>%
  ggplot(aes(x=cy.gallo.shelf.price,y=cy.gallo.unit.sales))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  annotate("text",x=18,y=90,label="r = -0.27")

Black_Box_df %>%
  dplyr::filter(store.id_ == 29509) %>%
  ggplot(aes(x=cy.gallo.comp.shelf.gap,y=cy.gallo.unit.sales))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  annotate("text",x=1,y=90,label="r = -0.24")



## default model
names(Store_29509_df)
Store_29509_df$Week_No

plot(ts(Store_29509_df$cy.gallo.unit.sales[1:147]))
plot(ts(log(Store_29509_df$cy.gallo.unit.sales[1:147])))
plot(ts(diff(log(Store_29509_df$cy.gallo.unit.sales[1:147]))))

acf2(ts(Store_29509_df$cy.gallo.unit.sales[1:147]))
acf2(ts(log(Store_29509_df$cy.gallo.unit.sales[1:147])))

tseries::adf.test(ts(Store_29509_df$cy.gallo.unit.sales[1:147]))
tseries::adf.test(ts(log(Store_29509_df$cy.gallo.unit.sales[1:147])))


### using StructTS
Store_29509_StructTS_fit = StructTS(Store_29509_df$cy.gallo.unit.sales[1:147],
                                  #  fixed=c(10,NA),
                                  #  init = c(10,1),
                                    type="level")
Store_29509_StructTS_fit

var(Store_29509_df$cy.gallo.unit.sales[1:147])

plot(Store_29509_df$cy.gallo.unit.sales[1:147],type="o")
lines(fitted(Store_29509_StructTS_fit),col="red")

tsdiag(Store_29509_StructTS_fit)


spectrum(Store_29509_df$cy.gallo.unit.sales,log="no",plot=T)
spectrum(Store_29509_df$cy.gallo.unit.sales,log="no",spans=c(3,3),plot=F)

spectrum(Store_29509_df$cy.gallo.unit.sales[1:147],log="no",spans=c(3,3),plot=T)
spectrum(Store_29509_df$cy.gallo.unit.sales[1:147],log="no",spans=c(3,3),plot=F)


Store_29509_Low_Price_Period = c(1,147)
Store_29509_High_Price_Period = c(148,165)

Store_29509_Low_To_High_Default1 = CausalImpact(zoo(Store_29509_df[,c("cy.gallo.unit.sales",
                                                             "cy.gallo.shelf.price",
                                                            
                                                              "py.gallo.unit.sales",
                                                             "py.gallo.shelf.price",
                                                             
                                                             "cy.comp.unit.sales",
                                                             "cy.comp.shelf.price",
                                                             
                                                             "py.comp.unit.sales",
                                                             "py.comp.shelf.price",
                                                             
                                                             "cy.gallo.comp.shelf.gap",
                                                             "py.gallo.comp.shelf.gap",
                                                             
                                                             "cy.gallo.ad.desc",
                                                             "cy.gallo.display",
                                                             
                                                             "cy.comp.ad.desc",
                                                             "cy.comp.display")]),
                                                model.args=list(niter=1000,nseasons=52),
                                       pre.period=Store_29509_Low_Price_Period,
                                       post.period=Store_29509_High_Price_Period)

plot(Store_29509_Low_To_High_Default1)
plot(Store_29509_Low_To_High_Default1$model$bsts.model,"coef")
summary(Store_29509_Low_To_High_Default1)
str(Store_29509_Low_To_High_Default1$model$bsts.model)
Store_29509_Low_To_High_Default1$model$bsts.model$coefficients

PlotMarginalInclusionProbabilities(Store_29509_Low_To_High_Default1$model$bsts.model$coefficients)$inclusion.prob
PlotMarginalInclusionProbabilities(Store_29509_Low_To_High_Default1$model$bsts.model$coefficients)$positive.prob


with(Store_29509_df[1:147,],cor(py.comp.shelf.price,cy.gallo.comp.shelf.gap))

Store_29509_df[1:147,] %>%
  ggplot(aes(x=Week_No,y=cy.gallo.comp.shelf.gap,group=1))+
  geom_point()+geom_line()+
  xlim(c(0,165))+
  geom_point(data=Store_29509_df[148:165,],aes(y=cy.gallo.comp.shelf.gap),
             color="red")+
  geom_line(data=Store_29509_df[148:165,],aes(y=cy.gallo.comp.shelf.gap),
             color="red")
  
mean(Store_29509_df[1:147,"cy.gallo.comp.shelf.gap"])
mean(Store_29509_df[148:165,"cy.gallo.comp.shelf.gap"])


Store_29509_Low_To_High_Default = CausalImpact(zoo(Store_29509_df[,c("cy.gallo.unit.sales",
                                                             "cy.gallo.comp.shelf.gap")]),
                                               model.args=list(niter=1000,nseasons=52,
                                                               standardize.data=F),
                                             pre.period=Store_29509_Low_Price_Period,
                                       post.period=Store_29509_High_Price_Period)

plot(Store_29509_Low_To_High_Default)
plot(Store_29509_Low_To_High_Default$model$bsts.model,"coef")
summary(Store_29509_Low_To_High_Default)
str(Store_29509_Low_To_High_Default$model$bsts.model)
Store_29509_Low_To_High_Default$model$bsts.model$coefficients


Store_29509_Low_To_High_Default2 = CausalImpact(zoo(Store_29509_df[,c("cy.gallo.unit.sales",
                                                                     "cy.gallo.shelf.price",
                                                                     "cy.gallo.comp.shelf.gap")]),
                                               model.args=list(niter=1000,nseasons=52,
                                                               standardize.data=F),
                                               pre.period=Store_29509_Low_Price_Period,
                                               post.period=Store_29509_High_Price_Period)

plot(Store_29509_Low_To_High_Default2)
plot(Store_29509_Low_To_High_Default2$model$bsts.model,"coef")
summary(Store_29509_Low_To_High_Default2)
str(Store_29509_Low_To_High_Default2$model$bsts.model)

Store_29509_Low_To_High_Default2_pred = predict(Store_29509_Low_To_High_Default2$model$bsts.model,
                                                      horizon=18,burn=300,seed=123,
                                                      newdata=Store_29509_df[148:165,c("cy.gallo.shelf.price","cy.gallo.comp.shelf.gap")])
Metrics_fun(Store_29509_Low_To_High_Default2_pred$mean,
            Store_29509_df[148:165,"cy.gallo.unit.sales"])

mean(Store_29509_df$cy.gallo.unit.sales)

### simulation
#### batch process
Store_29509_Simulation_Grid = data.frame(expand.grid(cy.gallo.shelf.price=17:22,
                                                     cy.gallo.comp.shelf.gap=-2:2)) %>%
  dplyr::mutate(cy.comp.shelf.price = cy.gallo.shelf.price-cy.gallo.comp.shelf.gap) %>%
  as.data.frame()

Store_29509_Simulation_Outputs = Simulation_Fun_Outputs(29509,
                                                        c(1,147),
                                                        c(148,165),
                                                        Store_29202_Simulation_Grid,
                                                        18)

Store_29509_Simulation_Outputs %>%
  ggplot(aes(x=cy.gallo.shelf.price,y=`Average Effect`,color=as.character(cy.comp.shelf.price)))+
  geom_point() + geom_line()+
  guides(color=guide_legend("cy.comp.shelf.price"))+
  labs(y="Cases Increase or Decrease/wk")+
  annotate("text",x=Store_29509_Simulation_Outputs$cy.gallo.shelf.price+0.1,
           y=Store_29509_Simulation_Outputs$`Average Effect`+3.5,
           label=paste("gap = ",Store_29509_Simulation_Outputs$cy.gallo.comp.shelf.gap,sep=""),size=3)+
  annotate("text",x=Store_29509_Simulation_Outputs$cy.gallo.shelf.price+0.1,
           y=Store_29509_Simulation_Outputs$`Average Effect`+1.5,
           label=paste("P = ",Store_29509_Simulation_Outputs$P,sep=""),size=3)

Store_29509_Simulation_Outputs %>%
  ggplot(aes(x=cy.gallo.shelf.price,y=`Cumulative Effect`,color=as.character(cy.comp.shelf.price)))+
  geom_point() + geom_line()+
  guides(color=guide_legend("cy.comp.shelf.price"))+
  labs(y="Cumulative Cases Increase or Decrease (Post Periods) ")+
  annotate("text",x=Store_29509_Simulation_Outputs$cy.gallo.shelf.price+0.1,
           y=Store_29509_Simulation_Outputs$`Cumulative Effect`+55,
           label=paste("gap = ",Store_29509_Simulation_Outputs$cy.gallo.comp.shelf.gap,sep=""),size=3)+
  annotate("text",x=Store_29509_Simulation_Outputs$cy.gallo.shelf.price+0.1,
           y=Store_29509_Simulation_Outputs$`Cumulative Effect`+20,
           label=paste("P = ",Store_29509_Simulation_Outputs$P,sep=""),size=3)




#### cy.gallo.shelf.price = 17, cy.gallo.comp.shelf.gap = 0
Store_29509_Low_To_High_Default2_pred_case1 = predict(Store_29509_Low_To_High_Default2$model$bsts.model,
                                                      horizon=18,
                                                      newdata=data.frame(cy.gallo.shelf.price = rep(17,18),
                                                                         cy.gallo.comp.shelf.gap=rep(0,18)))
Store_29509_Low_To_High_impact_case1 <- CausalImpact(bsts.model = Store_29509_Low_To_High_Default2$model$bsts.model,
                                                      post.period.response = Store_29509_Low_To_High_Default2_pred_case1$mean)
plot(Store_29509_Low_To_High_impact_case1)
summary(Store_29509_Low_To_High_impact_case1)

#### cy.gallo.shelf.price = 18, cy.gallo.comp.shelf.gap = 1
Store_29509_Low_To_High_Default2_pred_case2 = predict(Store_29509_Low_To_High_Default2$model$bsts.model,
                                                      horizon=18,
                                                      newdata=data.frame(cy.gallo.shelf.price = rep(18,18),
                                                                         cy.gallo.comp.shelf.gap=rep(1,18)))
Store_29509_Low_To_High_impact_case2 <- CausalImpact(bsts.model = Store_29509_Low_To_High_Default2$model$bsts.model,
                                                     post.period.response = Store_29509_Low_To_High_Default2_pred_case2$mean)
plot(Store_29509_Low_To_High_impact_case2)
summary(Store_29509_Low_To_High_impact_case2)

#### cy.gallo.shelf.price = 18, cy.gallo.comp.shelf.gap = 0
Store_29509_Low_To_High_Default2_pred_case3 = predict(Store_29509_Low_To_High_Default2$model$bsts.model,
                                                      horizon=18,
                                                      newdata=data.frame(cy.gallo.shelf.price = rep(18,18),
                                                                         cy.gallo.comp.shelf.gap=rep(0,18)))
Store_29509_Low_To_High_impact_case3 <- CausalImpact(bsts.model = Store_29509_Low_To_High_Default2$model$bsts.model,
                                                     post.period.response = Store_29509_Low_To_High_Default2_pred_case3$mean)
plot(Store_29509_Low_To_High_impact_case3)
summary(Store_29509_Low_To_High_impact_case3)

#### cy.gallo.shelf.price = 17, cy.gallo.comp.shelf.gap = -1
Store_29509_Low_To_High_Default2_pred_case4 = predict(Store_29509_Low_To_High_Default2$model$bsts.model,
                                                      horizon=18,
                                                      newdata=data.frame(cy.gallo.shelf.price = rep(17,18),
                                                                         cy.gallo.comp.shelf.gap=rep(-1,18)))
Store_29509_Low_To_High_impact_case4 <- CausalImpact(bsts.model = Store_29509_Low_To_High_Default2$model$bsts.model,
                                                     post.period.response = Store_29509_Low_To_High_Default2_pred_case4$mean)
plot(Store_29509_Low_To_High_impact_case4)
summary(Store_29509_Low_To_High_impact_case4)





## customized models
names(Store_29509_Pre_Period_df)
sarima(Store_29509_Pre_Period_df$cy.gallo.unit.sales,2,0,0,1,0,0,S=10,
       xreg=Store_29509_Pre_Period_df[,c(30)])

Store_29509_Pre_Period_df = Store_29509_df
Store_29509_Pre_Period_df[148:165,"cy.gallo.unit.sales"] = NA
Store_29509_Post_Period_Response = Store_29509_df[148:165,"cy.gallo.unit.sales"]

### bsts0
ss = AddLocalLevel(list(),Store_29509_Pre_Period_df$cy.gallo.unit.sales)
Store_29509_Low_To_High_bsts0 = bsts(cy.gallo.unit.sales~cy.gallo.shelf.price+
                                       py.gallo.unit.sales+py.gallo.shelf.price+
                                       cy.comp.unit.sales+cy.comp.shelf.price+
                                       py.comp.unit.sales+py.comp.shelf.price+
                                       cy.gallo.comp.shelf.gap+py.gallo.comp.shelf.gap+
                                       cy.gallo.ad.desc+cy.gallo.display+
                                       cy.comp.ad.desc+cy.comp.display,
                                     state.specification=ss,
                                     niter=1000,
                                #     family="poisson",
                               #      expected.model.size=3,
                                     data=Store_29509_Pre_Period_df)

plot(Store_29509_Low_To_High_bsts0,"coef")


Store_29509_Low_To_High_bsts0_pred = predict(Store_29509_Low_To_High_bsts0,
                                             newdata=Store_29509_Pre_Period_df[148:165,])
Metrics_fun(Store_29509_Low_To_High_bsts0_pred$mean,
            Store_29509_Post_Period_Response)

### bsts1
ss = AddLocalLevel(list(),Store_29509_Pre_Period_df$cy.gallo.unit.sales)
ss = AddAutoAr(ss,Store_29509_Pre_Period_df$cy.gallo.unit.sales)
Store_29509_Low_To_High_bsts1 = bsts(cy.gallo.unit.sales~cy.gallo.shelf.price+
                                       py.gallo.unit.sales+py.gallo.shelf.price+
                                       cy.comp.unit.sales+cy.comp.shelf.price+
                                       py.comp.unit.sales+py.comp.shelf.price+
                                       cy.gallo.comp.shelf.gap+py.gallo.comp.shelf.gap+
                                       cy.gallo.ad.desc+cy.gallo.display+
                                       cy.comp.ad.desc+cy.comp.display,
                                     state.specification=ss,
                                     #       family="poisson",
                                     niter=1000,
                                     data=Store_29509_Pre_Period_df)
Store_29509_Low_To_High_bsts1_pred = predict(Store_29509_Low_To_High_bsts1,
                                             newdata=Store_29509_Pre_Period_df[148:165,])
Metrics_fun(Store_29509_Low_To_High_bsts1_pred$mean,
            Store_29509_Post_Period_Response)

plot(Store_29509_Low_To_High_bsts1)
plot(Store_29509_Low_To_High_bsts1,"coef")
str(Store_29509_Low_To_High_bsts1)

### bsts2
str(Store_29509_Low_To_High_Default$model$bsts.model)


ss = AddLocalLevel(list(),Store_29509_Pre_Period_df$cy.gallo.unit.sales,
                   sigma.prior=SdPrior(sigma.guess=0.114,sample.size=32))
ss = AddAutoAr(ss,Store_29509_Pre_Period_df$cy.gallo.unit.sales)
ss = AddSeasonal(ss,Store_29509_Pre_Period_df$cy.gallo.unit.sales,nseasons=52)
Store_29509_Low_To_High_bsts2 = bsts(cy.gallo.unit.sales~cy.gallo.comp.shelf.gap,
                                     state.specification=ss,
                                     niter=10000,
                                     data=Store_29509_Pre_Period_df)
Store_29509_Low_To_High_bsts2 = bsts(cy.gallo.unit.sales~cy.gallo.shelf.price+
                                       cy.comp.shelf.price,
                                     state.specification=ss,
                                     niter=10000,
                                     data=Store_29509_Pre_Period_df)
plot(Store_29509_Low_To_High_bsts2)
plot(Store_29509_Low_To_High_bsts2,"coef")


Store_29509_Low_To_High_Default = CausalImpact(zoo(Store_29509_df[,c("cy.gallo.unit.sales",
                                                                     "cy.gallo.comp.shelf.gap")]),
                                               model.args=list(niter=1000,nseasons=52,
                                                               standardize.data=F),
                                               pre.period=Store_29509_Low_Price_Period,
                                               post.period=Store_29509_High_Price_Period)



Metrics_fun(Store_29509_Low_To_High_bsts_fitted[148:165],
            Store_29509_df$cy.gallo.unit.sales[148:165])



Store_29509_Low_To_High_bsts2_pred = predict(Store_29509_Low_To_High_bsts2,
                                             burn=300,
                                             newdata=Store_29509_Pre_Period_df[148:165,])
Metrics_fun(Store_29509_Low_To_High_bsts2_pred$mean,
            Store_29509_Post_Period_Response)

CompareBstsModels(list("Model 0" = Store_29509_Low_To_High_bsts0,
                       "Model 1" = Store_29509_Low_To_High_bsts1,
                       "Model 2" = Store_29509_Low_To_High_bsts2),
                  color=c("black","red","blue"))   

plot(148:165,Store_29509_df$cy.gallo.unit.sales[148:165],type="o",
     xlab="Week No",  ylab="cy.gallo.unit.sales")
points(148:165,Store_29509_Low_To_High_bsts2_pred$mean,col="red")
lines(148:165,Store_29509_Low_To_High_bsts2_pred$mean,col="red")
text(x=155,y=67,labels = "original")
text(x=151,y=60,labels = "predicted",col="red")


### bsts3
ss = AddLocalLinearTrend(list(),Store_29509_Pre_Period_df$cy.gallo.unit.sales)
ss = AddAutoAr(ss,Store_29509_Pre_Period_df$cy.gallo.unit.sales)
Store_29509_Low_To_High_bsts3 = bsts(cy.gallo.unit.sales~cy.gallo.shelf.price+
                                       py.gallo.unit.sales+py.gallo.shelf.price+
                                       cy.comp.unit.sales+cy.comp.shelf.price+
                                       py.comp.unit.sales+py.comp.shelf.price+
                                       cy.gallo.comp.shelf.gap+py.gallo.comp.shelf.gap+
                                       cy.gallo.ad.desc+cy.gallo.display+
                                       cy.comp.ad.desc+cy.comp.display,
                                     state.specification=ss,
                                     niter=1000,
                                     data=Store_29509_Pre_Period_df)
CompareBstsModels(list("Model 1" = Store_29509_Low_To_High_bsts1,
                       "Model 2" = Store_29509_Low_To_High_bsts2,
                       "Model 3" = Store_29509_Low_To_High_bsts3),
                  color=c("black","red","orange"))  # model 1 or model 2

CompareBstsModels(list("Model 1" = Store_29509_Low_To_High_bsts1,
                       "Model 2" = Store_29509_Low_To_High_bsts2),
                  color=c("black","red"))


### bsts4
ss = AddLocalLinearTrend(list(),Store_29509_Pre_Period_df$cy.gallo.unit.sales)
ss = AddAutoAr(ss,Store_29509_Pre_Period_df$cy.gallo.unit.sales)
ss = AddSeasonal(ss,nseasons=52,Store_29509_Pre_Period_df$cy.gallo.unit.sales)
Store_29509_Low_To_High_bsts4 = bsts(cy.gallo.unit.sales~cy.gallo.shelf.price+
                                       py.gallo.unit.sales+py.gallo.shelf.price+
                                       cy.comp.unit.sales+cy.comp.shelf.price+
                                       py.comp.unit.sales+py.comp.shelf.price+
                                       cy.gallo.comp.shelf.gap+py.gallo.comp.shelf.gap+
                                       cy.gallo.ad.desc+cy.gallo.display+
                                       cy.comp.ad.desc+cy.comp.display,
                                     state.specification=ss,
                                     niter=1000,
                                     data=Store_29509_Pre_Period_df)


CompareBstsModels(list("Model 1" = Store_29509_Low_To_High_bsts1,
                       "Model 2" = Store_29509_Low_To_High_bsts2,
                       "Model 3" = Store_29509_Low_To_High_bsts3,
                       "Model 4" = Store_29509_Low_To_High_bsts4),
                  color=c("black","red","blue","orange"))  



Store_29509_Low_To_High_bsts0_pred = predict(Store_29509_Low_To_High_bsts0,
                                                  horizon = 19,
                                                  newdata=Store_29509_Pre_Period_df[148:165,])
Store_29509_Low_To_High_bsts1_pred = predict(Store_29509_Low_To_High_bsts1,
                                                  horizon = 19,
                                                  newdata=Store_29509_Pre_Period_df[148:165,])
Store_29509_Low_To_High_bsts2_pred = predict(Store_29509_Low_To_High_bsts2,
                                                  horizon = 19,
                                             newdata=Store_29509_Pre_Period_df[148:165,])
Store_29509_Low_To_High_bsts3_pred = predict(Store_29509_Low_To_High_bsts3,
                                                  horizon = 19,
                                                  newdata=Store_29509_Pre_Period_df[148:165,])
Store_29509_Low_To_High_bsts4_pred = predict(Store_29509_Low_To_High_bsts4,
                                                  horizon = 19,
                                                  newdata=Store_29509_Pre_Period_df[148:165,])

Metrics_fun(Store_29509_Low_To_High_bsts0_pred$mean,
            Store_29509_Post_Period_Response)
Metrics_fun(Store_29509_Low_To_High_bsts1_pred$mean,
            Store_29509_Post_Period_Response)
Metrics_fun(Store_29509_Low_To_High_bsts2_pred$mean,
            Store_29509_Post_Period_Response)
Metrics_fun(Store_29509_Low_To_High_bsts3_pred$mean,
            Store_29509_Post_Period_Response)
Metrics_fun(Store_29509_Low_To_High_bsts4_pred$mean,
            Store_29509_Post_Period_Response)



plot(x=148:165,y=Store_29509_Post_Period_Response,type="o",
     ylab="cy.gallo.unit.sales",xlab="Week_No")
points(x=148:165,y=Store_29509_Low_To_High_bsts0_pred$mean,col="red")
lines(x=148:165,y=Store_29509_Low_To_High_bsts0_pred$mean,col="red")
points(x=148:165,y=Store_29509_Low_To_High_bsts1_pred$mean,col="blue")
lines(x=148:165,y=Store_29509_Low_To_High_bsts1_pred$mean,col="blue")
points(x=148:165,y=Store_29509_Low_To_High_bsts2_pred$mean,col="orange")
lines(x=148:165,y=Store_29509_Low_To_High_bsts2_pred$mean,col="orange")
points(x=148:165,y=Store_29509_Low_To_High_bsts3_pred$mean,col="green")
lines(x=148:165,y=Store_29509_Low_To_High_bsts3_pred$mean,col="green")
points(x=148:165,y=Store_29509_Low_To_High_bsts4_pred$mean,col="cyan")
lines(x=148:165,y=Store_29509_Low_To_High_bsts4_pred$mean,col="cyan")


### causal impact of customer models (bsts2, basically reproduced the default model)
Store_29509_Low_To_High_impact1 <- CausalImpact(bsts.model = Store_29509_Low_To_High_bsts2,
                                               post.period.response = Store_29509_Post_Period_Response)
Store_29509_Low_To_High_impact2 <- CausalImpact(bsts.model = Store_29509_Low_To_High_Default$model$bsts.model,
                                               post.period.response = Store_29509_Post_Period_Response) 

plot(Store_29509_Low_To_High_impact1)
plot(Store_29509_Low_To_High_impact2)

summary(Store_29509_Low_To_High_impact1)
summary(Store_29509_Low_To_High_impact2)



######################## by State #########################################
Stores_Price_Increase_State_df

# 1. VA 
Store_Price_Increase_VA = Stores_Price_Increase_State_df %>%
  dplyr::filter(sto_state == "VA") %>%
  dplyr::select(store.id_) %>%
  dplyr::pull(store.id_)

Price_fun("29202")


Simulation_Grid = data.frame(cy.gallo.shelf.price=c(18,18,18,19,19,19),
                             cy.gallo.comp.shelf.gap=c(-1,0,1,0,1,2)) %>%
  dplyr::mutate(cy.comp.shelf.price = cy.gallo.shelf.price-cy.gallo.comp.shelf.gap) %>%
  as.data.frame()


Store_VA_Simulation_list = list()
for (i in 1:length(Store_Price_Increase_VA)) {
  Store_VA_Simulation_list[[i]] = Simulation_Fun_Outputs(Store_Price_Increase_VA[i],
                                                         c(1,147),
                                                         c(148,165),
                                                         Simulation_Grid,
                                                         18)
}

Store_VA_Simulation_df = do.call(rbind,Store_VA_Simulation_list)
Store_VA_Simulation_df

Store_VA_Simulation_df %>%
  dplyr::distinct(Store_ID,cy.gallo.shelf.price.coef) %>%
  dplyr::arrange(Store_ID) %>%
  ggplot(aes(x=as.character(Store_ID),y=cy.gallo.shelf.price.coef,group=1))+
  geom_point()+geom_line()+
  geom_hline(yintercept=0,lty=2,color="red")+
  theme(axis.text.x=element_text(angle=90,vjust=0.5))




Store_VA_Simulation_df %>%
  ggplot(aes(x=cy.gallo.shelf.price,y=`Post-Pre Diff/WK`,
             color=as.character(cy.comp.shelf.price)))+
  geom_point() +
  geom_smooth(method="lm",se=F)+
  geom_hline(yintercept=0,lty=2,color="red")+
  scale_x_continuous(breaks=17:22,labels=17:22)+
  labs(title="State: VA (45 Stores)",
       x = "Target Gallo Shelf Price (from $17)",
       y = "Cases Increase (or Decrease)/wk")+
  guides(color=guide_legend("cy.comp.shelf.price"))


Store_VA_Simulation_Final = 
  Store_VA_Simulation_df %>%
  dplyr::group_by(cy.gallo.shelf.price,cy.comp.shelf.price) %>%
  dplyr::summarize(n = n(),
                   `Post-Period Sales/WK.ave` = signif(mean(`Post-Period Sales/WK`),2),
                   `Post-Period Sales/WK.sd` = signif(sd(`Post-Period Sales/WK`)/sqrt(n),2),
                   `Post-Pre Diff/WK.ave` = signif(mean(`Post-Pre Diff/WK`),2),
                   `Post-Pre Diff/WK.sd` = signif(sd(`Post-Pre Diff/WK`)/sqrt(n),2),
                   mean.coef = signif(mean(cy.gallo.shelf.price.coef),2),
                   sd.coef = signif(sd(cy.gallo.shelf.price.coef)/sqrt(n),2)) %>%
  dplyr::mutate(`Revenue Change/WK` = `Post-Period Sales/WK.ave`*cy.gallo.shelf.price-
                  (`Post-Period Sales/WK.ave`-`Post-Pre Diff/WK.ave`)*17,
                `Revenue Change/WK_95_CI_Upper` = (`Post-Period Sales/WK.ave`+2*`Post-Pre Diff/WK.sd`)*cy.gallo.shelf.price-
                  (`Post-Period Sales/WK.ave`-`Post-Pre Diff/WK.ave`)*17,
                `Revenue Change/WK_95_CI_Lower` = (`Post-Period Sales/WK.ave`-2*`Post-Pre Diff/WK.sd`)*cy.gallo.shelf.price-
                  (`Post-Period Sales/WK.ave`-`Post-Pre Diff/WK.ave`)*17) %>%
  as.data.frame()

Store_VA_Simulation_Final


ggplot(Store_VA_Simulation_Final,
       aes(x=cy.gallo.shelf.price,y=`Post-Pre Diff/WK.ave`,
           color=as.character(cy.comp.shelf.price)))+
  geom_point() +
  geom_smooth(method="lm",se=F)+
  geom_hline(yintercept=0,lty=2,color="red")+
  scale_x_continuous(breaks=17:22,labels=17:22)+
  labs(title="State: VA (45 Stores)",
       x = "Target Gallo Shelf Price (from $17)",
       y = "Cases Increase (or Decrease)/wk")+
  guides(color=guide_legend("cy.comp.shelf.price"))


ggplot(Store_VA_Simulation_Final,
       aes(x=cy.gallo.shelf.price,y=`Post-Pre Diff/WK.ave`,
           fill=as.character(cy.comp.shelf.price)))+
  geom_bar(stat="identity",position=position_dodge(),width=0.5) +
  geom_errorbar(aes(ymin=`Post-Pre Diff/WK.ave`-2*`Post-Pre Diff/WK.sd`,
                    ymax=`Post-Pre Diff/WK.ave`+2*`Post-Pre Diff/WK.sd`),width=0.2,
                position=position_dodge(0.5))+
  labs(title="State: VA (45 Stores)",
       x = "Target Gallo Shelf Price (from $17)",
       y = "Post-Pre Diff in Case/WK (+/- 2 SE)")+
  guides(fill=guide_legend(title="cy.comp.shelf.price"))+
  scale_x_continuous(breaks=18:19,labels=18:19)


ggplot(Store_VA_Simulation_Final,
       aes(x=cy.gallo.shelf.price,y=`Revenue Change/WK`,
           fill=as.character(cy.comp.shelf.price)))+
  geom_bar(stat="identity",position=position_dodge(),width=0.5) +
  geom_errorbar(aes(ymin=`Revenue Change/WK_95_CI_Lower`,
                    ymax=`Revenue Change/WK_95_CI_Upper`),width=0.2,
                position=position_dodge(0.5))+
  labs(title="State: VA (45 Stores)",
       x = "Target Gallo Shelf Price (from $17)",
       y = "Revenue Change/wk (+/- 2 SE)")+
  guides(fill=guide_legend(title="cy.comp.shelf.price"))+
  scale_x_continuous(breaks=18:19,labels=18:19)



# 2. MI 
Store_Price_Increase_MI = Stores_Price_Increase_State_df %>%
  dplyr::filter(sto_state == "MI") %>%
  dplyr::select(store.id_) %>%
  dplyr::pull(store.id_)


Price_fun("18277")

for (i in 1:length(Store_Price_Increase_MI)) {
  print(Price_fun(Store_Price_Increase_MI[i]))
}


Black_Box_df %>%
  dplyr::filter(store.id_ == 18749) %>%
  ggplot(aes(x=Week_No,y=cy.gallo.shelf.price,group=1))+
  geom_point()+geom_line()+
  labs(title = "Store 18749 (MI)")+
  geom_vline(xintercept=36,lty=2,color="red")+
  geom_vline(xintercept=123,lty=2,color="red")+
  scale_x_continuous(breaks = 1:165,labels=1:165)+
  theme(axis.text.x=element_text(angle=90,vjust=0.5))+
  annotate("text",x=37,y=18,label="WK 36",color="red",angle=90,size=2.5)+
  annotate("text",x=122,y=18,label="WK 123",color="red",angle=90,size=2.5)

Black_Box_df %>%
  dplyr::filter(store.id_ == 18749) %>%
  ggplot(aes(x=Week_No,y=cy.gallo.unit.sales,group=1))+
  geom_point()+geom_line()+
  labs(title = "Store 18277 (MI)")+
  geom_vline(xintercept=36,lty=2,color="red")+
  geom_vline(xintercept=123,lty=2,color="red")+
  scale_x_continuous(breaks = 1:165,labels=1:165)+
  theme(axis.text.x=element_text(angle=90,vjust=0.5))+
  annotate("text",x=37,y=18,label="WK 36",color="red",angle=90,size=2.5)+
  annotate("text",x=122,y=18,label="WK 123",color="red",angle=90,size=2.5)

Black_Box_df %>%
  dplyr::filter(store.id_ == 18749) %>%
  dplyr::pull(cy.gallo.unit.sales)


Store_Price_Increase_MI = Stores_Price_Increase_State_df %>%
  dplyr::filter(sto_state == "MI") %>%
  dplyr::select(store.id_) %>%
  dplyr::filter(store.id_ != 18749) %>%
  dplyr::pull(store.id_)
Store_Price_Increase_MI

Simulation_Grid = data.frame(cy.gallo.shelf.price=c(18,18,18,19,19,19),
                             cy.gallo.comp.shelf.gap=c(-1,0,1,0,1,2)) %>%
  dplyr::mutate(cy.comp.shelf.price = cy.gallo.shelf.price-cy.gallo.comp.shelf.gap) %>%
  as.data.frame()


Store_MI_Simulation_list = list()
for (i in 1:length(Store_Price_Increase_MI)) {
  Store_MI_Simulation_list[[i]] = Simulation_Fun_Outputs(Store_Price_Increase_MI[i],
                                                         c(36,123),
                                                         c(124,165),
                                                         Simulation_Grid,
                                                         42)
}

Store_MI_Simulation_df = do.call(rbind,Store_MI_Simulation_list)
Store_MI_Simulation_df

Store_MI_Simulation_df %>%
  dplyr::distinct(Store_ID,cy.gallo.shelf.price.coef) %>%
  dplyr::arrange(Store_ID) %>%
  ggplot(aes(x=as.character(Store_ID),y=cy.gallo.shelf.price.coef,group=1))+
  geom_point()+geom_line()+
  geom_hline(yintercept=0,lty=2,color="red")+
  theme(axis.text.x=element_text(angle=90,vjust=0.5))



Store_MI_Simulation_df %>%
  ggplot(aes(x=cy.gallo.shelf.price,y=`Post-Pre Diff/WK`,
             color=as.character(cy.comp.shelf.price)))+
  geom_point() +
  geom_smooth(method="lm",se=F)+
  geom_hline(yintercept=0,lty=2,color="red")+
  scale_x_continuous(breaks=17:22,labels=17:22)+
  labs(title="State: MI (45 Stores)",
       x = "Target Gallo Shelf Price (from $17)",
       y = "Cases Increase (or Decrease)/wk")+
  guides(color=guide_legend("cy.comp.shelf.price"))


Store_MI_Simulation_Final = 
  Store_MI_Simulation_df %>%
  dplyr::group_by(cy.gallo.shelf.price,cy.comp.shelf.price) %>%
  dplyr::summarize(n = n(),
                   `Post-Period Sales/WK.ave` = signif(mean(`Post-Period Sales/WK`),2),
                   `Post-Period Sales/WK.sd` = signif(sd(`Post-Period Sales/WK`)/sqrt(n),2),
                   `Post-Pre Diff/WK.ave` = signif(mean(`Post-Pre Diff/WK`),2),
                   `Post-Pre Diff/WK.sd` = signif(sd(`Post-Pre Diff/WK`)/sqrt(n),2),
                   mean.coef = signif(mean(cy.gallo.shelf.price.coef),2),
                   sd.coef = signif(sd(cy.gallo.shelf.price.coef)/sqrt(n),2)) %>%
  dplyr::mutate(`Revenue Change/WK` = `Post-Period Sales/WK.ave`*cy.gallo.shelf.price-
                  (`Post-Period Sales/WK.ave`-`Post-Pre Diff/WK.ave`)*17,
                `Revenue Change/WK_95_CI_Upper` = (`Post-Period Sales/WK.ave`+2*`Post-Pre Diff/WK.sd`)*cy.gallo.shelf.price-
                  (`Post-Period Sales/WK.ave`-`Post-Pre Diff/WK.ave`)*17,
                `Revenue Change/WK_95_CI_Lower` = (`Post-Period Sales/WK.ave`-2*`Post-Pre Diff/WK.sd`)*cy.gallo.shelf.price-
                  (`Post-Period Sales/WK.ave`-`Post-Pre Diff/WK.ave`)*17) %>%
  as.data.frame()

Store_MI_Simulation_Final


ggplot(Store_MI_Simulation_Final,
       aes(x=cy.gallo.shelf.price,y=`Post-Pre Diff/WK.ave`,
           color=as.character(cy.comp.shelf.price)))+
  geom_point() +
  geom_smooth(method="lm",se=F)+
  geom_hline(yintercept=0,lty=2,color="red")+
  scale_x_continuous(breaks=17:22,labels=17:22)+
  labs(title="State: MI (45 Stores)",
       x = "Target Gallo Shelf Price (from $17)",
       y = "Cases Increase (or Decrease)/wk")+
  guides(color=guide_legend("cy.comp.shelf.price"))


ggplot(Store_MI_Simulation_Final,
       aes(x=cy.gallo.shelf.price,y=`Post-Pre Diff/WK.ave`,
           fill=as.character(cy.comp.shelf.price)))+
  geom_bar(stat="identity",position=position_dodge(),width=0.5) +
  geom_errorbar(aes(ymin=`Post-Pre Diff/WK.ave`-2*`Post-Pre Diff/WK.sd`,
                    ymax=`Post-Pre Diff/WK.ave`+2*`Post-Pre Diff/WK.sd`),width=0.2,
                position=position_dodge(0.5))+
  labs(title="State: MI (45 Stores)",
       x = "Target Gallo Shelf Price (from $17)",
       y = "Post-Pre Diff in Case/WK (+/- 2 SE)")+
  guides(fill=guide_legend(title="cy.comp.shelf.price"))+
  scale_x_continuous(breaks=18:19,labels=18:19)


ggplot(Store_MI_Simulation_Final,
       aes(x=cy.gallo.shelf.price,y=`Revenue Change/WK`,
           fill=as.character(cy.comp.shelf.price)))+
  geom_bar(stat="identity",position=position_dodge(),width=0.5) +
  geom_errorbar(aes(ymin=`Revenue Change/WK_95_CI_Lower`,
                    ymax=`Revenue Change/WK_95_CI_Upper`),width=0.2,
                position=position_dodge(0.5))+
  labs(title="State: MI (45 Stores)",
       x = "Target Gallo Shelf Price (from $17)",
       y = "Revenue Change/wk (+/- 2 SE)")+
  guides(fill=guide_legend(title="cy.comp.shelf.price"))+
  scale_x_continuous(breaks=18:19,labels=18:19)


# 3. IN
## 3.0. store 21869 (IN)
Black_Box_df %>%
  dplyr::filter(store.id_ == 21869) %>%
  ggplot(aes(x=Week_No,y=cy.gallo.shelf.price,group=1))+
  geom_point()+geom_line()+
  labs(title = "Store 21869 (IN)")+
  geom_vline(xintercept=47,lty=2,color="red")+
  geom_vline(xintercept=114,lty=2,color="red")+
  scale_x_continuous(breaks = 1:165,labels=1:165)+
  theme(axis.text.x=element_text(angle=90,vjust=0.5))+
  annotate("text",x=48,y=17.5,label="WK 47",color="red",angle=90,size=2.5)+
  annotate("text",x=115,y=17.5,label="WK 114",color="red",angle=90,size=2.5)

Black_Box_df %>%
  dplyr::filter(store.id_ == 21869) %>%
  ggplot(aes(x=Week_No,y=cy.comp.shelf.price,group=1))+
  geom_point()+geom_line()+
  labs(title = "Store 21869 (IN)")+
  geom_vline(xintercept=47,lty=2,color="red")+
  geom_vline(xintercept=114,lty=2,color="red")+
  scale_x_continuous(breaks = 1:165,labels=1:165)+
  theme(axis.text.x=element_text(angle=90,vjust=0.5))+
  annotate("text",x=48,y=17.5,label="WK 47",color="red",angle=90,size=2.5)+
  annotate("text",x=115,y=17.5,label="WK 114",color="red",angle=90,size=2.5)


Black_Box_df %>%
  dplyr::filter(store.id_ == 21869) %>%
  ggplot(aes(x=Week_No,y=cy.gallo.unit.sales,group=1))+
  geom_point()+geom_line()+
  labs(title = "Store 21869 (IN)")+
  geom_vline(xintercept=47,lty=2,color="red")+
  geom_vline(xintercept=114,lty=2,color="red")+
  scale_x_continuous(breaks = 1:165,labels=1:165)+
  theme(axis.text.x=element_text(angle=90,vjust=0.5))+
  annotate("text",x=48,y=17.5,label="WK 47",color="red",angle=90,size=2.5)+
  annotate("text",x=115,y=17.5,label="WK 114",color="red",angle=90,size=2.5)


IN_21869_df = Black_Box_df %>%
  dplyr::filter(store.id_ == 21869) %>%
  as.data.frame()

### 3.0.1. Pre- and Post-
IN_21869_bsts = CausalImpact(zoo(IN_21869_df[,c(17,18,26)]),
                                 pre.period=c(47,113),
                                 post.period=c(114,165))


IN_21869_bsts
plot(IN_21869_bsts)
plot(IN_21869_bsts$model$bsts.model,"coefficient")

apply(IN_21869_bsts$model$bsts.model$coefficients,2,mean)










### 3.0.2. Pre1- and Pre2-
IN_21869_bsts_Pre = CausalImpact(zoo(IN_21869_df[,c(17,18,26)]),
                             pre.period=c(47,89),
                             post.period=c(90,113))


IN_21869_bsts_Pre 
plot(IN_21869_bsts_Pre )
plot(IN_21869_bsts_Pre $model$bsts.model,"coefficient")

apply(IN_21869_bsts_Pre $model$bsts.model$coefficients,2,mean)


IN_21869_SMA = SMA(ts(IN_21869_df$cy.gallo.unit.sales),n=8)
plot(IN_21869_SMA)
IN_21869_SMA

IN_21869_Unit_Sale_ts = ts(IN_21869_df[,"cy.gallo.unit.sales"],
                           frequency=4,start=c(2000,1))
IN_21869_STL = stl(IN_21869_Unit_Sale_ts,s.window="periodic")
plot(IN_21869_STL)



### 3.0.3. varying post-period length


CausalImpact_Varying_Pre_Post_fun = function(pre.period,post.period) {
  bsts_model = CausalImpact(zoo(IN_21869_df[,c(17,18,26)]),
                                 pre.period=pre.period,
                                 post.period=post.period)
  
   Absolute_Effect_Per_WK = bsts_model$summary[1,6]
   cy_gallo_shelf_price_coef = apply(bsts_model$model$bsts.model$coefficients,2,mean)[2]
   cy_comp_shelf_price_coef = apply(bsts_model$model$bsts.model$coefficients,2,mean)[3]
   output = c(round(Absolute_Effect_Per_WK,1),
              round(cy_gallo_shelf_price_coef,1),
              round(cy_comp_shelf_price_coef,1))
   output
   }


CausalImpact_Varying_Pre_Post_fun(pre.period = c(47,113),
                                  post.period=c(114,150))



post_period_ending_wk = seq(120,165,5)
CausalImpact_Varying_Pre_Post_output = matrix(NA,
                                              nrow=length(post_period_ending_wk),
                                              ncol=4)
for (i in 1:length(post_period_ending_wk)) {
  pre.period = c(47,113)
  post.period = c(114,post_period_ending_wk[i])
  bsts_model = CausalImpact(zoo(IN_21869_df[,c(17,18,26)]),
                            pre.period=pre.period,
                            post.period=post.period)
  CausalImpact_Varying_Pre_Post_output[i,1] = post_period_ending_wk[i]
  CausalImpact_Varying_Pre_Post_output[i,2] = bsts_model$summary[1,6]
  CausalImpact_Varying_Pre_Post_output[i,3] = apply(bsts_model$model$bsts.model$coefficients,2,mean)[2]
  CausalImpact_Varying_Pre_Post_output[i,4] = apply(bsts_model$model$bsts.model$coefficients,2,mean)[3]
  CausalImpact_Varying_Pre_Post_output
  }

CausalImpact_Varying_Pre_Post_output = as.data.frame(CausalImpact_Varying_Pre_Post_output)
names(CausalImpact_Varying_Pre_Post_output) = c("No_Post_Period_WK",
                                                "Absolute_Effect_Per_WK",
                                                "cy_gallo_shelf_price_coef",
                                                "cy_comp_shelf_price_coef")
 
CausalImpact_Varying_Pre_Post_output


CausalImpact_Varying_Pre_Post_output %>%
  ggplot(aes(x=No_Post_Period_WK,y=Absolute_Effect_Per_WK))+
  geom_point() + geom_line() +
  scale_y_continuous(breaks=seq(0,7,0.5),labels=seq(0,7,0.5))+
  coord_cartesian(ylim=c(0,7))+
  annotate("text",x=CausalImpact_Varying_Pre_Post_output$No_Post_Period_WK,
           y=CausalImpact_Varying_Pre_Post_output$Absolute_Effect_Per_WK+0.3,
           label=round(CausalImpact_Varying_Pre_Post_output$Absolute_Effect_Per_WK,1))



### 3.0.4. gls
str(IN_21869_df)
IN_21869_df$Period = c(rep(1,46),rep(2,67),rep(3,52))

IN_21869_gls = gls(cy.gallo.unit.sales~factor(Period)+cy.gallo.shelf.price+
                     cy.comp.shelf.price,
                   correlation=corARMA(form=~Week_No,p=1,q=1),
                   weights=varPower(),
                   data=IN_21869_df[47:165,])
summary(IN_21869_gls)

plot(IN_21869_gls,resid(.,type="normalized")~fitted(.),
     type=c("p","smooth"))
plot(IN_21869_gls,resid(.,type="normalized")~cy.gallo.shelf.price,
     type=c("p","smooth"))
plot(IN_21869_gls,resid(.,type="normalized")~cy.comp.shelf.price,
     type=c("p","smooth"))
acf2(residuals(IN_21869_gls,type="normalized"),max.lag=50)



## 3.1. selected stores
Store_Price_Increase_IN = Stores_Price_Increase_State_df %>%
  dplyr::filter(sto_state == "IN") %>%
  dplyr::select(store.id_) %>%
  dplyr::pull(store.id_)


Price_fun("21215")

for (i in 1:length(Store_Price_Increase_IN)) {
  print(Price_fun(Store_Price_Increase_IN[i]))
}




Simulation_Grid = data.frame(cy.gallo.shelf.price=c(18,18,18,19,19,19),
                             cy.gallo.comp.shelf.gap=c(-1,0,1,0,1,2)) %>%
  dplyr::mutate(cy.comp.shelf.price = cy.gallo.shelf.price-cy.gallo.comp.shelf.gap) %>%
  as.data.frame()

Store_Price_Increase_IN = Store_Price_Increase_IN[-1]
Store_IN_Simulation_list = list()
for (i in 1:length(Store_Price_Increase_IN)) {
  Store_IN_Simulation_list[[i]] = Simulation_Fun_Outputs(Store_Price_Increase_IN[i],
                                                         c(47,113),
                                                         c(114,165),
                                                         Simulation_Grid,
                                                         52)
}

Store_IN_Simulation_df = do.call(rbind,Store_IN_Simulation_list)
Store_IN_Simulation_df

Store_IN_Simulation_df %>%
  dplyr::distinct(Store_ID,cy.gallo.shelf.price.coef) %>%
  dplyr::arrange(Store_ID) %>%
  ggplot(aes(x=as.character(Store_ID),y=cy.gallo.shelf.price.coef,group=1))+
  geom_point()+geom_line()+
  geom_hline(yintercept=0,lty=2,color="red")+
  theme(axis.text.x=element_text(angle=90,vjust=0.5))


Store_IN_Simulation_df %>%
  ggplot(aes(x=cy.gallo.shelf.price,y=`Post-Pre Diff/WK`,
             color=as.character(cy.comp.shelf.price)))+
  geom_point() +
  geom_smooth(method="lm",se=F)+
  geom_hline(yintercept=0,lty=2,color="red")+
  scale_x_continuous(breaks=17:22,labels=17:22)+
  labs(title="State: IN (7 Stores)",
       x = "Target Gallo Shelf Price (from $17)",
       y = "Cases Increase (or Decrease)/wk")+
  guides(color=guide_legend("cy.comp.shelf.price"))


Store_IN_Simulation_Final = 
  Store_IN_Simulation_df %>%
  dplyr::group_by(cy.gallo.shelf.price,cy.comp.shelf.price) %>%
  dplyr::summarize(n = n(),
                   `Post-Period Sales/WK.ave` = signif(mean(`Post-Period Sales/WK`),2),
                   `Post-Period Sales/WK.sd` = signif(sd(`Post-Period Sales/WK`)/sqrt(n),2),
                   `Post-Pre Diff/WK.ave` = signif(mean(`Post-Pre Diff/WK`),2),
                   `Post-Pre Diff/WK.sd` = signif(sd(`Post-Pre Diff/WK`)/sqrt(n),2),
                   mean.coef = signif(mean(cy.gallo.shelf.price.coef),2),
                   sd.coef = signif(sd(cy.gallo.shelf.price.coef)/sqrt(n),2)) %>%
  dplyr::mutate(`Revenue Change/WK` = `Post-Period Sales/WK.ave`*cy.gallo.shelf.price-
                  (`Post-Period Sales/WK.ave`-`Post-Pre Diff/WK.ave`)*17,
                `Revenue Change/WK_95_CI_Upper` = (`Post-Period Sales/WK.ave`+2*`Post-Pre Diff/WK.sd`)*cy.gallo.shelf.price-
                  (`Post-Period Sales/WK.ave`-`Post-Pre Diff/WK.ave`)*17,
                `Revenue Change/WK_95_CI_Lower` = (`Post-Period Sales/WK.ave`-2*`Post-Pre Diff/WK.sd`)*cy.gallo.shelf.price-
                  (`Post-Period Sales/WK.ave`-`Post-Pre Diff/WK.ave`)*17) %>%
  as.data.frame()

Store_IN_Simulation_Final


ggplot(Store_IN_Simulation_Final,
       aes(x=cy.gallo.shelf.price,y=`Post-Pre Diff/WK.ave`,
           color=as.character(cy.comp.shelf.price)))+
  geom_point() +
  geom_smooth(method="lm",se=F)+
  geom_hline(yintercept=0,lty=2,color="red")+
  scale_x_continuous(breaks=17:22,labels=17:22)+
  labs(title="State: IN (7 Stores)",
       x = "Target Gallo Shelf Price (from $17)",
       y = "Cases Increase (or Decrease)/wk")+
  guides(color=guide_legend("cy.comp.shelf.price"))


ggplot(Store_IN_Simulation_Final,
       aes(x=cy.gallo.shelf.price,y=`Post-Pre Diff/WK.ave`,
           fill=as.character(cy.comp.shelf.price)))+
  geom_bar(stat="identity",position=position_dodge(),width=0.5) +
  geom_errorbar(aes(ymin=`Post-Pre Diff/WK.ave`-2*`Post-Pre Diff/WK.sd`,
                    ymax=`Post-Pre Diff/WK.ave`+2*`Post-Pre Diff/WK.sd`),width=0.2,
                position=position_dodge(0.5))+
  labs(title="State: IN (7 Stores)",
       x = "Target Gallo Shelf Price (from $17)",
       y = "Post-Pre Diff in Case/WK (+/- 2 SE)")+
  guides(fill=guide_legend(title="cy.comp.shelf.price"))+
  scale_x_continuous(breaks=18:19,labels=18:19)


ggplot(Store_IN_Simulation_Final,
       aes(x=cy.gallo.shelf.price,y=`Revenue Change/WK`,
           fill=as.character(cy.comp.shelf.price)))+
  geom_bar(stat="identity",position=position_dodge(),width=0.5) +
  geom_errorbar(aes(ymin=`Revenue Change/WK_95_CI_Lower`,
                    ymax=`Revenue Change/WK_95_CI_Upper`),width=0.2,
                position=position_dodge(0.5))+
  labs(title="State: IN (7 Stores)",
       x = "Target Gallo Shelf Price (from $17)",
       y = "Revenue Change/wk (+/- 2 SE)")+
  guides(fill=guide_legend(title="cy.comp.shelf.price"))+
  scale_x_continuous(breaks=18:19,labels=18:19)


## 3.2. all stores?
### 3.2.1. graphs
str(Black_Box_df)

Stores_IN_df = Black_Box_df %>%
  dplyr::filter(store.id_ %in% Stores_Price_Increase_Mykel_vector) %>%
  dplyr::filter(sto_state == "IN") %>%
  as.data.frame()

Stores_IN = Black_Box_df %>%
  dplyr::filter(store.id_ %in% Stores_Price_Increase_Mykel_vector) %>%
  dplyr::filter(sto_state == "IN") %>%
  dplyr::select(store.id_) %>%
  dplyr::pull() %>%
  unique()

Store_IN_165_WKS = Stores_IN_df %>%
  dplyr::group_by(store.id_) %>%
  summarize(n = n()) %>%
  dplyr::filter(n == 165) %>%
  dplyr::pull(store.id_)

Price_fun("211")
Price_fun("24776")
Price_fun("21118")

Black_Box_df %>%
  dplyr::filter(store.id_ %in% Store_IN_165_WKS) %>%
  dplyr::group_by(store.id_) %>%
  summarize(n=n(),
            mean=mean(cy.gallo.unit.sales)) %>%
  dplyr::pull(mean) %>%
  mean()



Black_Box_df %>%
  dplyr::filter(store.id_ == 24744) %>%
  ggplot(aes(x=Week_No,y=cy.gallo.shelf.price,group=1))+
  geom_point()+geom_line()+
  labs(title = "Store 24744 (IN)")+
  geom_vline(xintercept=23,lty=2,color="red")+
  geom_vline(xintercept=114,lty=2,color="red")+
  scale_x_continuous(breaks = 1:165,labels=1:165)+
  theme(axis.text.x=element_text(angle=90,vjust=0.5))+
  annotate("text",x=24,y=17.5,label="WK 23",color="red",angle=90,size=2.5)+
  annotate("text",x=115,y=17.5,label="WK 114",color="red",angle=90,size=2.5)


### 3.2.2. simulation
Simulation_Grid = data.frame(cy.gallo.shelf.price=c(18,18,18,19,19,19),
                             cy.gallo.comp.shelf.gap=c(-1,0,1,0,1,2)) %>%
  dplyr::mutate(cy.comp.shelf.price = cy.gallo.shelf.price-cy.gallo.comp.shelf.gap) %>%
  as.data.frame()


Store_IN_Simulation_list = list()
for (i in 1:length(Store_IN_165_WKS)) {
  Store_IN_Simulation_list[[i]] = Simulation_Fun_Outputs(Store_IN_165_WKS[i],
                                                         c(23,113),
                                                         c(114,165),
                                                         Simulation_Grid,
                                                         52)
}

Store_IN_Simulation_df = do.call(rbind,Store_IN_Simulation_list)
Store_IN_Simulation_df
str(Store_IN_Simulation_df)

Store_IN_Simulation_df %>%
  dplyr::distinct(Store_ID,cy.gallo.shelf.price.coef) %>%
  dplyr::arrange(desc(Store_ID)) %>%
  ggplot(aes(x=as.character(Store_ID),y=cy.gallo.shelf.price.coef,group=1))+
  geom_point()+geom_line()+
  geom_hline(yintercept=0,lty=2,color="red")+
  theme(axis.text.x=element_text(angle=90,vjust=0.5))



Store_IN_Simulation_Final = 
  Store_IN_Simulation_df %>%
  dplyr::group_by(cy.gallo.shelf.price,cy.comp.shelf.price) %>%
  dplyr::summarize(n = n(),
                   `Post-Period Sales/WK.ave` = signif(mean(`Post-Period Sales/WK`),2),
                   `Post-Period Sales/WK.sd` = signif(sd(`Post-Period Sales/WK`)/sqrt(n),2),
                   `Post-Pre Diff/WK.ave` = signif(mean(`Post-Pre Diff/WK`),2),
                   `Post-Pre Diff/WK.sd` = signif(sd(`Post-Pre Diff/WK`)/sqrt(n),2),
                   mean.coef = signif(mean(cy.gallo.shelf.price.coef),2),
                   sd.coef = signif(sd(cy.gallo.shelf.price.coef)/sqrt(n),2)) %>%
  dplyr::mutate(`Revenue Change/WK` = `Post-Period Sales/WK.ave`*cy.gallo.shelf.price-
                  (`Post-Period Sales/WK.ave`-`Post-Pre Diff/WK.ave`)*17,
                `Revenue Change/WK_95_CI_Upper` = (`Post-Period Sales/WK.ave`+2*`Post-Pre Diff/WK.sd`)*cy.gallo.shelf.price-
                  (`Post-Period Sales/WK.ave`-`Post-Pre Diff/WK.ave`)*17,
                `Revenue Change/WK_95_CI_Lower` = (`Post-Period Sales/WK.ave`-2*`Post-Pre Diff/WK.sd`)*cy.gallo.shelf.price-
                  (`Post-Period Sales/WK.ave`-`Post-Pre Diff/WK.ave`)*17) %>%
  as.data.frame()

Store_IN_Simulation_Final


ggplot(Store_IN_Simulation_Final,
       aes(x=cy.gallo.shelf.price,y=`Post-Pre Diff/WK.ave`,
           color=as.character(cy.comp.shelf.price)))+
  geom_point() +
  geom_smooth(method="lm",se=F)+
  geom_hline(yintercept=0,lty=2,color="red")+
  scale_x_continuous(breaks=17:22,labels=17:22)+
  labs(title="State: IN (93 Stores)",
       x = "Target Gallo Shelf Price (from $17)",
       y = "Cases Increase (or Decrease)/wk")+
  guides(color=guide_legend("cy.comp.shelf.price"))


ggplot(Store_IN_Simulation_Final,
       aes(x=cy.gallo.shelf.price,y=`Post-Pre Diff/WK.ave`,
           fill=as.character(cy.comp.shelf.price)))+
  geom_bar(stat="identity",position=position_dodge(),width=0.5) +
  geom_errorbar(aes(ymin=`Post-Pre Diff/WK.ave`-2*`Post-Pre Diff/WK.sd`,
                    ymax=`Post-Pre Diff/WK.ave`+2*`Post-Pre Diff/WK.sd`),width=0.2,
                position=position_dodge(0.5))+
  labs(title="State: IN (93 Stores)",
       x = "Target Gallo Shelf Price (from $17)",
       y = "Post-Pre Diff in Case/WK (+/- 2 SE)")+
  guides(fill=guide_legend(title="cy.comp.shelf.price"))+
  scale_x_continuous(breaks=18:19,labels=18:19)


ggplot(Store_IN_Simulation_Final,
       aes(x=cy.gallo.shelf.price,y=`Revenue Change/WK`,
           fill=as.character(cy.comp.shelf.price)))+
  geom_bar(stat="identity",position=position_dodge(),width=0.5) +
  geom_errorbar(aes(ymin=`Revenue Change/WK_95_CI_Lower`,
                    ymax=`Revenue Change/WK_95_CI_Upper`),width=0.2,
                position=position_dodge(0.5))+
  labs(title="State: IN (93 Stores)",
       x = "Target Gallo Shelf Price (from $17)",
       y = "Revenue Change/wk (+/- 2 SE)")+
  guides(fill=guide_legend(title="cy.comp.shelf.price"))+
  scale_x_continuous(breaks=18:19,labels=18:19)



Store_IN_Simulation_df = Store_IN_Simulation_df %>%
  dplyr::mutate(Post_Pre_Diff_Per_WK = `Post-Pre Diff/WK`,
                `Pre-Period Sales/WK` = `Post-Period Sales/WK`- `Post-Pre Diff/WK`,
                Revenue_Diff_Per_WK = `Post-Period Sales/WK`*cy.gallo.shelf.price-
                  `Pre-Period Sales/WK`*17)

qqPlot(Store_IN_Simulation_df$Post_Pre_Diff_Per_WK)


hist(Store_IN_Simulation_df$Post_Pre_Diff_Per_WK,nclass=20)
hist(Store_IN_Simulation_df$Revenue_Diff_Per_WK,nclass=20)


### 3.2.2. statistical analysis
#### 3.2.2.1. cases/wk
Store_IN_Simulation_Case_Change_gls = gls(Post_Pre_Diff_Per_WK ~as.factor(cy.gallo.shelf.price)+
                                            as.factor(cy.comp.shelf.price),
                                          weights=varComb(varIdent(form=~1|cy.gallo.shelf.price),
                                                          varIdent(form=~1|cy.comp.shelf.price),
                                                          varIdent(form=~1|cy.gallo.comp.shelf.gap)),
                                          data=Store_IN_Simulation_df)

summary(Store_IN_Simulation_Case_Change_gls)
anova(Store_IN_Simulation_Case_Change_gls,type="sequential")
plot(Store_IN_Simulation_Case_Change_gls,residuals(.,type="normalized")~fitted(.),
     type=c("p","smooth"))
plot(Store_IN_Simulation_Case_Change_gls,residuals(.,type="normalized")~cy.gallo.shelf.price,
     type=c("p","smooth"))
plot(Store_IN_Simulation_Case_Change_gls,residuals(.,type="normalized")~cy.comp.shelf.price,
     type=c("p","smooth"))
qqPlot(residuals(Store_IN_Simulation_Case_Change_gls,type="normalized"))

pairs(emmeans(Store_IN_Simulation_Case_Change_gls,"cy.gallo.shelf.price"))
pairs(emmeans(Store_IN_Simulation_Case_Change_gls,"cy.comp.shelf.price"))
plot(predictorEffect("cy.gallo.shelf.price",Store_IN_Simulation_Case_Change_gls))


#### 3.2.2.2. Revenue per wk
Store_IN_Simulation_Revenue_gls = gls(Revenue_Diff_Per_WK ~as.factor(cy.gallo.shelf.price)+
                                            as.factor(cy.comp.shelf.price),
                                          weights=varComb(varIdent(form=~1|cy.gallo.shelf.price),
                                                          varIdent(form=~1|cy.comp.shelf.price),
                                                          varIdent(form=~1|cy.gallo.comp.shelf.gap)),
                                          data=Store_IN_Simulation_df)

summary(Store_IN_Simulation_Revenue_gls)
anova(Store_IN_Simulation_Revenue_gls,type="sequential")
plot(Store_IN_Simulation_Revenue_gls,residuals(.,type="normalized")~fitted(.),
     type=c("p","smooth"))
plot(Store_IN_Simulation_Revenue_gls,residuals(.,type="normalized")~cy.gallo.shelf.price,
     type=c("p","smooth"))
plot(Store_IN_Simulation_Revenue_gls,residuals(.,type="normalized")~cy.comp.shelf.price,
     type=c("p","smooth"))
qqPlot(residuals(Store_IN_Simulation_Revenue_gls,type="normalized"))

pairs(emmeans(Store_IN_Simulation_Revenue_gls,"cy.gallo.shelf.price"))
pairs(emmeans(Store_IN_Simulation_Revenue_gls,"cy.comp.shelf.price"))
plot(predictorEffect("cy.gallo.shelf.price",Store_IN_Simulation_Revenue_gls))


### 3.2.3. aggregate to state level
Store_IN_165_WKS_df = Black_Box_df %>%
  dplyr::filter(store.id_ %in% Store_IN_165_WKS) %>%
  dplyr::group_by(Week_No) %>%
  dplyr::summarize(cy.gallo.unit.sales = mean(cy.gallo.unit.sales),
                   cy.gallo.shelf.price = mean(cy.gallo.shelf.price),
                   cy.comp.shelf.price = mean(cy.comp.shelf.price)) %>%
  as.data.frame()

spectrum(ts(Store_IN_165_WKS_df$cy.gallo.unit.sales),log="no")
spectrum(ts(Store_IN_165_WKS_df$cy.gallo.unit.sales),log="no",plot=F)

ggplot(Store_IN_165_WKS_df,aes(x=Week_No,y=cy.gallo.unit.sales,group=1))+
  geom_point()+geom_line()

ggplot(Store_IN_165_WKS_df,aes(x=Week_No,y=cy.gallo.shelf.price,group=1))+
  geom_point()+geom_line()

ggplot(Store_IN_165_WKS_df,aes(x=Week_No,y=cy.comp.shelf.price,group=1))+
  geom_point()+geom_line()

ggplot(Store_IN_165_WKS_df,aes(x=Week_No,y=cy.gallo.unit.sales,group=1))+
  geom_point()+geom_line()+
  geom_point(aes(y=cy.gallo.shelf.price),color="blue")+
  geom_line(aes(y=cy.gallo.shelf.price),color="blue")+
  geom_point(aes(y=cy.comp.shelf.price),color="red")+
  geom_line(aes(y=cy.comp.shelf.price),color="red")+
  annotate("text",x=20,y=10,label="cy.gallo.unit.sales")+
  annotate("text",x=20,y=18.5,label="cy.gallo.shelf.price",color="blue")+
  annotate("text",x=20,y=17.5,label="cy.comp.shelf.price",color="red")+
  labs(title="IN stores (93)",y="unit sales (or shelf price)")+
  scale_x_continuous(breaks=1:165,labels=1:165)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  
#### 3.2.3.1. causal impact
names(Store_IN_165_WKS_df)
IN_93_Stores_bsts = CausalImpact(zoo(Store_IN_165_WKS_df[,2:4]),
                                 pre.period=c(24,113),
                                 post.period=c(114,165))

IN_93_Stores_bsts
plot(IN_93_Stores_bsts)
plot(IN_93_Stores_bsts$model$bsts.model,"coef")
IN_93_Stores_bsts$model$bsts.model$coefficients
hist(IN_93_Stores_bsts$model$bsts.model$coefficients[,2],nclass=50)
summary(IN_93_Stores_bsts$model$bsts.model$coefficients)
quantile(IN_93_Stores_bsts$model$bsts.model$coefficients[,2],prob=c(0.025,0.975))
Simulation_Grid


#### 3.2.3.2. ts
with(Store_IN_165_WKS_df[24:165,],cor(cy.gallo.shelf.price,cy.comp.shelf.price))

Store_IN_165_WKS_df$Pre_Post = factor(c(rep("Pre",113),rep("Post",52)),levels=c("Pre","Post"))

IN_93_Stores_gls_1 = gls(cy.gallo.unit.sales~Pre_Post,
                         correlation=corARMA(form=~Week_No|Pre_Post,p=1,q=1),
                         weights=varPower(),
                         data = Store_IN_165_WKS_df[24:165,])
summary(IN_93_Stores_gls_1)

IN_93_Stores_gls_2 = gls(cy.gallo.unit.sales~Pre_Post+cy.gallo.shelf.price,
                         correlation=corARMA(form=~Week_No|Pre_Post,p=1,q=1),
                         weights=varPower(),
                         data = Store_IN_165_WKS_df[24:165,])
summary(IN_93_Stores_gls_2)


IN_93_Stores_gls = gls(cy.gallo.unit.sales~Pre_Post+cy.gallo.shelf.price+
                         cy.comp.shelf.price,
                       correlation=corARMA(form=~Week_No|Pre_Post,p=1,q=1),
                       weights=varPower(),
                       data = Store_IN_165_WKS_df[24:165,])
summary(IN_93_Stores_gls)
Box.test(resid(IN_93_Stores_gls,type="normalized"),type="Ljung-Box")

acf2(resid(IN_93_Stores_gls,type="normalized"),max.lag=50)
plot(IN_93_Stores_gls,resid(.,type="normalized")~fitted(.),
     type=c("p","smooth"))
plot(IN_93_Stores_gls,resid(.,type="normalized")~cy.gallo.shelf.price,
     type=c("p","smooth"))
plot(IN_93_Stores_gls,resid(.,type="normalized")~cy.comp.shelf.price,
     type=c("p","smooth"))
qqPlot(resid(IN_93_Stores_gls,type="normalized"))


##### 
cy.gallo.shelf.price = c(17,18,19)
Model1_Pre = 10.32-0.21-0*cy.gallo.shelf.price
Model1_Post = 10.32-0*cy.gallo.shelf.price

Model2_Pre = 35.65-2.08-1.42*cy.gallo.shelf.price
Model2_Post = 35.65-1.42*cy.gallo.shelf.price





data = Store_IN_165_WKS_df %>%
  dplyr::mutate(Period = c(rep(2,23),rep(0,90),rep(1,52))) %>%
  as.data.frame()


sarima(ts(data[24:165,]$cy.gallo.unit.sales),
       p=1,d=0,q=1,
       P=1,D=0,Q=0,S=5,
       xreg=data[24:165,c(3:4,6)])




######################## August 8, 2024 #############################
# distributor level data
load("Distributor level data.RData")


list.files()
file = list.files(pattern="pie_data")
file
Distributor_df = read.csv(file)
str(Distributor_df)

Distributor_df = Distributor_df %>%
  dplyr::mutate(Date = as.Date(as.character(period_id),format="%Y%m%d"),
                Price = as.numeric(net_list),
                Sales = rab_depletions,
                Market = pricing_market_desc,
                Distributor = paste(pricing_market_desc,price_category_desc,sep="-"),
                pdelt = ifelse(is.na(pdelt_id),0,1),
                pre_pdelt = ifelse(is.na(pre_pdelt_id),0,1),
                post_pdelt = ifelse(is.na(post_pdelt_id),0,1)) %>%
  as.data.frame()

sum(is.na(Distributor_df$Price))


Distributor_df %>%
  dplyr::select(pricing_market_desc,price_category_desc) %>%
  dplyr::distinct()

# save(Distributor_df,file="Distributor level data.RData")

Distributors = sort(unique(Distributor_df$Distributor))
Distributors[1:10]

## Distributor 1
Distributor_1 = Distributor_df %>%
  dplyr::filter(Distributor == Distributors[1]) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate(Month_Order = 1:dplyr::n(),
                Phase = c(rep(0,33),rep(1,9))) %>%
  dplyr::select(Price,Sales,Date,pdelt,pre_pdelt,post_pdelt,Month_Order,
                Phase)
head(Distributor_1)

ggplot(Distributor_1,aes(x=Date,y=Price))+
  geom_point()+geom_line()+
  geom_vline(xintercept=as.Date("2023-09-01"),lty=2,color="red")+
  geom_smooth(method="lm",se=F)+
  geom_smooth(method="loess",se=F,color="red")+
  scale_x_continuous(breaks=Distributor_1$Date,
                     labels=Distributor_1$Date)+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  labs(title=Distributors[1])

ggplot(Distributor_1,aes(x=Date,y=Sales))+
  geom_point()+geom_line()+
  geom_vline(xintercept=as.Date("2023-09-01"),lty=2,color="red")+
  geom_smooth(method="lm",se=F)+
  geom_smooth(method="loess",se=F,color="red")+
  scale_x_continuous(breaks=Distributor_1$Date,
                     labels=Distributor_1$Date)+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  labs(title=Distributors[1])

### decompose
ts(Distributor_1$Sales)
ts(Distributor_1$Sales,start=c(2021,1), end=c(2024,6),deltat=1/12)
plot(ts(Distributor_1$Sales,start=c(2021,1), end=c(2024,6),deltat=1/12))

Distributor_1_ts = ts(Distributor_1$Sales,start=c(2021,1),frequency=12)

Distributor_1_decompose = decompose(Distributor_1_ts)

autoplot(Distributor_1_decompose)

Distributor_1_decompose$trend

Distributor_1_seas = Distributor_1_ts %>%
  seas() 



spectrum(ts(Distributor_1$Sales),plot=F,log="no")
spectrum(ts(Distributor_1$Sales),log="no")

spectrum(ts(Distributor_1$Sales),plot=F,log="no",method="ar")
spectrum(ts(Distributor_1$Sales),log="no",method="ar")


spectrum(ts(Distributor_1$Sales,start=c(2021,1),
            frequency=12),plot=F,log="no")
spectrum(ts(Distributor_1$Sales,start=c(2021,1),
            end=c(2024,6),deltat=1/12),log="no")




### sarima models
sarima(ts(Distributor_1$Sales),p=2,d=1,q=0,P=1,D=0,Q=0,S=12,
       xreg=Distributor_1[,c(1,8)])

fit = auto.arima(ts(Distributor_1$Sales),xreg=as.matrix(Distributor_1[,c(1,8)]))
plot(forecast(fit,h=20))



### gls
Distributor_1_gls = gls(Sales~Price*Phase,
                        data=Distributor_1)
summary(Distributor_1_gls)

spectrum(residuals(Distributor_1_gls,type="normalized"),log="no")
spectrum(residuals(Distributor_1_gls,type="normalized"),log="no",plot=F)



Distributor_1_gls = gls(Sales~Price+Phase+
                         sin(2*pi*1:42*4/45)+sin(2*pi*1:42*15/45),
                      #   weights=varPower(),
                        data=Distributor_1)
summary(Distributor_1_gls)

acf2(residuals(Distributor_1_gls,type="normalized"),max.lag=20)
plot(Distributor_1_gls,residuals(.,type="normalized")~fitted(.),
     type=c("p","smooth"))

qqPlot(residuals(Distributor_1_gls,type="normalized"),ylab="Residuals")

Box.test(residuals(Distributor_1_gls,type="normalized"))


### bsts/causalImpact (bad)
Distributor_1_CausalImpact = CausalImpact(zoo(Distributor_1[,c(2,1,8)]),
                                          model.args=list(niter=1000,nseasons=52,standardize.data=F),
                                          pre.period=c(1,33),
                                          post.period=c(34,42))

plot(Distributor_1_CausalImpact)
plot(Distributor_1_CausalImpact$model$bsts.model,"coef")
summary(Distributor_1_CausalImpact)
str(Distributor_1_CausalImpact$model$bsts.model)

## Distributor 2
Distributor_2 = Distributor_df %>%
  dplyr::filter(Distributor == Distributors[2]) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate(Month_Order = 1:dplyr::n(),
                Phase = c(rep(0,12),rep(1,30))) %>%
  dplyr::select(Price,Sales,Date,pdelt,pre_pdelt,post_pdelt,Month_Order,
                Phase)
head(Distributor_2)

ggplot(Distributor_2,aes(x=Date,y=Price))+
  geom_point()+geom_line()+
  geom_vline(xintercept=as.Date("2021-12-01"),lty=2,color="red")+
  geom_smooth(method="lm",se=F)+
  geom_smooth(method="loess",se=F,color="red")+
  scale_x_continuous(breaks=Distributor_2$Date,
                     labels=Distributor_2$Date)+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  labs(title=Distributors[2])

ggplot(Distributor_2,aes(x=Date,y=Sales))+
  geom_point()+geom_line()+
  geom_vline(xintercept=as.Date("2021-12-01"),lty=2,color="red")+
  geom_smooth(method="lm",se=F)+
  geom_smooth(method="loess",se=F,color="red")+
  scale_x_continuous(breaks=Distributor_2$Date,
                     labels=Distributor_2$Date)+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  labs(title=Distributors[2])



spectrum(ts(Distributor_2$Sales),log="no")
spectrum(ts(Distributor_2$Sales),log="no",plot=F)


Distributor_2_gls = gls(Sales~Price+Phase+
                        cos(2*pi*1:42*4/45)+
                        cos(2*pi*1:42*11/45),
                        weights=varPower(),
                        data=Distributor_2)
summary(Distributor_2_gls)

acf2(residuals(Distributor_2_gls,type="normalized"),max.lag=20)
plot(Distributor_2_gls,residuals(.,type="normalized")~fitted(.),
     type=c("p","smooth"))

qqPlot(residuals(Distributor_2_gls,type="normalized"),ylab="Residuals")

Box.test(residuals(Distributor_2_gls,type="normalized"))


spectrum(residuals(Distributor_2_gls,type="normalized"),log="no")
spectrum(residuals(Distributor_2_gls,type="normalized"),log="no",plot=F)





