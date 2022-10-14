setwd("~/Desktop/Dataset for R/Dataset for R")
df_summer=read.csv(file="df_hm_summer.csv",stringsAsFactors=FALSE)

#look into variables
unique(df_summer$club_member_status)
unique(df_summer$product_group_name)
unique(df_summer$section_name)
unique(df_summer$index_group_name)
unique(df_summer$index_name)
unique(df_summer$garment_group_name)
unique(df_summer$department_name)
unique(df_summer$product_type_name)
unique(df_summer$graphical_appearance_name)
unique(df_summer_subset)



df_summer$itembought_nbr<-seq(1,1)
df_summer$weekend<-ifelse(df_summer$weekend=="True",1,0)


#Remove outlier
Q <- quantile(df_summer$price, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(df_summer$price)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
df_summer<- subset(df_summer, df_summer$price > (Q[1] - 1.5*iqr) & df_summer$price < (Q[2]+1.5*iqr))


#Divide age into 4 groups
df_summer$age_16_25 <- ifelse(df_summer$age >= 16 & df_summer$age <= 25, 1, 0)
df_summer$age_25_30 <- ifelse(df_summer$age >= 25 & df_summer$age <= 30, 1, 0)
df_summer$age_30_46 <- ifelse(df_summer$age >= 30 & df_summer$age <= 46, 1, 0)
df_summer$age_46_99 <- ifelse(df_summer$age >= 46 & df_summer$age <= 99, 1, 0)

#get dummy product group name
df_summer$product_group_Nightwear<-ifelse(df_summer$product_group_name=="Nightwear",1,0)
df_summer$product_group_GLB<-ifelse(df_summer$product_group_name=="Garment Lower body",1,0)
df_summer$product_group_GUB<-ifelse(df_summer$product_group_name=="Garment Upper body",1,0)
df_summer$product_group_GFB<-ifelse(df_summer$product_group_name=="Garment Full body",1,0)
df_summer$product_group_Underwear<-ifelse(df_summer$product_group_name=="Underwear",1,0)
df_summer$product_group_Swimwear<-ifelse(df_summer$product_group_name=="Swimwear",1,0)
df_summer$product_group_Accessories<-ifelse(df_summer$product_group_name=="Accessories",1,0)
df_summer$product_group_Shoes<-ifelse(df_summer$product_group_name=="Shoes",1,0)
df_summer$product_group_SocksTights<-ifelse(df_summer$product_group_name=="Socks & Tights",1,0)
df_summer$product_group_Unknown<-ifelse(df_summer$product_group_name=="Unknown",1,0)


#get graphical appearance dummy
df_summer$graphical_solid<-ifelse(df_summer$graphical_appearance_name=="Solid",1,0)
df_summer$graphical_alloverpattern<-ifelse(df_summer$graphical_appearance_name=="All over pattern",1,0)
df_summer$graphical_stripe<-ifelse(df_summer$graphical_appearance_name=="Stripe",1,0)
df_summer$graphical_otherstructure<-ifelse(df_summer$graphical_appearance_name=="Other structure",1,0)
df_summer$graphical_denim<-ifelse(df_summer$graphical_appearance_name=="Denim",1,0)
df_summer$graphical_melange<-ifelse(df_summer$graphical_appearance_name=="Melange",1,0)

#get dummy perceived colour value name
df_summer$color_dustylight<-ifelse(df_summer$perceived_colour_value_name=="Dusty Light",1,0)
df_summer$color_dark<-ifelse(df_summer$perceived_colour_value_name=="Dark",1,0)
df_summer$color_light<-ifelse(df_summer$perceived_colour_value_name=="Light",1,0)
df_summer$color_medium<-ifelse(df_summer$perceived_colour_value_name=="Medium",1,0)
df_summer$color_bright<-ifelse(df_summer$perceived_colour_value_name=="Bright",1,0)
df_summer$color_mediumdusty<-ifelse(df_summer$perceived_colour_value_name=="Medium Dusty",1,0)



#get dummy perceived color master name
df_summer$color_black<-ifelse(df_summer$perceived_colour_master_name=="Black",1,0)
df_summer$color_white<-ifelse(df_summer$perceived_colour_master_name=="White",1,0)
df_summer$color_blue<-ifelse(df_summer$perceived_colour_master_name=="Blue",1,0)
df_summer$color_beige<-ifelse(df_summer$perceived_colour_master_name=="Beige",1,0)
df_summer$color_pink<-ifelse(df_summer$perceived_colour_master_name=="Pink",1,0)
df_summer$color_grey<-ifelse(df_summer$perceived_colour_master_name=="Grey",1,0)
df_summer$color_green<-ifelse(df_summer$perceived_colour_master_name=="Green",1,0)
df_summer$color_orange<-ifelse(df_summer$perceived_colour_master_name=="Orange",1,0)
df_summer$color_khakigreen<-ifelse(df_summer$perceived_colour_master_name=="Khaki green",1,0)

#index group name
df_summer$index_ladieswear <- ifelse(df_summer$index_group_name == "Ladieswear", 1, 0)
df_summer$index_divided <- ifelse(df_summer$index_group_name == "Divided", 1, 0)
df_summer$index_menswear <- ifelse(df_summer$index_group_name == "Menswear", 1, 0)
df_summer$index_sport <- ifelse(df_summer$index_group_name == "Sport", 1, 0)
df_summer$index_babychild <- ifelse(df_summer$index_group_name == "Baby/Children", 1, 0)

#garment group name
df_summer$garment_fancy <- ifelse(df_summer$garment_group_name == "Jersey Fancy", 1, 0)
df_summer$garment_basic <- ifelse(df_summer$garment_group_name == "Jersey Basic", 1, 0)
df_summer$garment_swimwear <- ifelse(df_summer$garment_group_name == "Swimwear", 1, 0)
df_summer$garment_nightwear <- ifelse(df_summer$garment_group_name == "Under-, Nightwear", 1, 0)
df_summer$garment_trousers <- ifelse(df_summer$garment_group_name == "Trousers", 1, 0)
df_summer$garment_blouses <- ifelse(df_summer$garment_group_name == "Blouses", 1, 0)
df_summer$garment_dress <- ifelse(df_summer$garment_group_name == "Dresses Ladies", 1, 0)
df_summer$garment_accessories <- ifelse(df_summer$garment_group_name == "Accessories", 1, 0)
df_summer$garment_knitwear <- ifelse(df_summer$garment_group_name == "Knitwear", 1, 0)

#get dummy section name
df_summer$section_women_collect <- ifelse(df_summer$section_name == "Womens Everyday Collection", 1, 0)
df_summer$section_divide_collect <- ifelse(df_summer$section_name == "Divided Collection", 1, 0)
df_summer$section_women_swimwear <- ifelse(df_summer$section_name == "Womens Swimwear, beachwear", 1, 0)
df_summer$section_women_lingerie <- ifelse(df_summer$section_name == "Womens Lingerie", 1, 0)
df_summer$section_women_tailor <- ifelse(df_summer$section_name == "Womens Tailoring", 1, 0)
df_summer$section_women_basic <- ifelse(df_summer$section_name == "Womens Everyday Basics", 1, 0)
df_summer$section_divide_basic <- ifelse(df_summer$section_name == "Divided Basics", 1, 0)
df_summer$section_lady_sport <- ifelse(df_summer$section_name == "Ladies H&M Sport", 1, 0)
df_summer$section_women_casual <- ifelse(df_summer$section_name == "Womens Casual", 1, 0)
df_summer$section_lady_denim <- ifelse(df_summer$section_name == "Ladies Denim", 1, 0)
df_summer$section_women_nightwear <- ifelse(df_summer$section_name == "Womens Nightwear, Socks & Tigh", 1, 0)
df_summer$section_women_s_accessories <- ifelse(df_summer$section_name == "Womens Small accessories", 1, 0)
df_summer$section_men_underwear <- ifelse(df_summer$section_name == "Men Underwear", 1, 0)
df_summer$section_women_shoes <- ifelse(df_summer$section_name == "Womens Shoes", 1, 0)
df_summer$section_women_trend <- ifelse(df_summer$section_name == "Womens Trend", 1, 0)
df_summer$section_hm_plus <- ifelse(df_summer$section_name == "H&M+", 1, 0)
df_summer$section_women_b_accessories <- ifelse(df_summer$section_name == "Womens Big accessories", 1, 0)




# Aggregate to customer level, create the subset and variables 

df_summer_subset<- subset(df_summer) %>% group_by(customer_id) %>% 
  summarise(firstpurch = min(as.Date(t_dat)),
            lastpurch08 = max(as.Date(t_dat[month<=8])), 
            purch06 = length(itembought_nbr[month==6]),
            purch07 = length(itembought_nbr[month==7]),
            purch08 = length(itembought_nbr[month==8]),
            purch09 = length(itembought_nbr[month==9]),
            price06 = sum(price[month==6]),
            price07 = sum(price[month==7]),
            price08 = sum(price[month==8]),
            FN = first(FN),
            Active = first(Active),
            club_member_status=first(club_member_status),
            postal_code = first(postal_code),
            sales_channel_id = first(sales_channel_id),
            weekend = sum(weekend),
            pd_garmentlowerbody = sum(product_group_GLB),
            pd_garmentupperbody = sum(product_group_GUB),
            pd_garmentfullbody = sum(product_group_GFB),
            pd_underwear = sum(product_group_Underwear),
            pd_swimwear = sum(product_group_Swimwear),
            graphical_solid = sum(graphical_solid),
            graphical_alloverpattern = sum(graphical_alloverpattern),
            colorvalue_dustylight= sum(color_dustylight),
            colorvalue_dark= sum(color_dark),
            colorvalue_light= sum(color_light),
            colorvalue_mediumdusty= sum(color_mediumdusty),
            colormaster_black= sum(color_black),
            colormaster_white= sum(color_white),
            colormaster_blue= sum(color_blue),
            colormaster_beige= sum(color_beige),
            colormaster_pink= sum(color_pink),
            indexgroup_ladieswear= sum(index_ladieswear),
            indexgroup_divided= sum(index_divided),
            garmentgroup_fancy= sum(garment_fancy),
            garmentgroup_basic= sum(garment_basic),
            garmentgroup_swimwear= sum(garment_swimwear),
            garmentgroup_nightwear= sum(garment_nightwear),
            sec_w_collect = sum(section_women_collect),
            sec_d_collect = sum(section_divide_collect),
            sec_w_swimwear = sum(section_women_swimwear),
            sec_w_lingerie = sum(section_women_lingerie),
            sec_w_tailor = sum(section_women_tailor),
            age_16_25 = sum(age_16_25),
            age_25_30 = sum(age_25_30),
            age_30_46 = sum(age_30_46),
            age_46_99 = sum(age_46_99),
            numpd = n_distinct(product_group_name[month>=6 & month<=8])
            )
            

#get rid of those customers that had their first purchase after August
df_summer_subset <- df_summer_subset[month(df_summer_subset$firstpurch)<=08,]

#retention on September
df_summer_subset$ret09 <- as.factor(ifelse(df_summer_subset$purch09>0,1,0))

# RFM measures

#RFM calc
df_summer_subset$numpurch <- df_summer_subset$purch06 + df_summer_subset$purch07 + df_summer_subset$purch08
df_summer_subset$dollpurch <- df_summer_subset$price06 + df_summer_subset$price07 + df_summer_subset$price08 
df_summer_subset$recent <- as.numeric(as.Date("2020-08-31")-as.Date(df_summer_subset$lastpurch08))

# Average purchase amount from first purchase until end of 2020:

df_summer_subset$monetary <- df_summer_subset$dollpurch/(as.numeric(as.Date("2020-08-31")- df_summer_subset$firstpurch))*30

#frequent
df_summer_subset$frequent <- df_summer_subset$numpurch/(as.numeric(as.Date("2020-08-31")- df_summer_subset$firstpurch))*30

# Now put into "quintiles" -  599002 - 108599-108599-108599-108599-108601

df_summer_subset <- as.data.frame(df_summer_subset[order(df_summer_subset$recent),])
df_summer_subset$rix <- 1:nrow(df_summer_subset) # recency index
df_summer_subset$R_Q<-ceiling(df_summer_subset$rix/108599)

df_summer_subset <- as.data.frame(df_summer_subset[order(df_summer_subset$frequent),])
df_summer_subset$fix <- 1:nrow(df_summer_subset) # frequency index
df_summer_subset$F_Q<-ceiling(df_summer_subset$fix/108599)

df_summer_subset <- as.data.frame(df_summer_subset[order(df_summer_subset$monetary),])
df_summer_subset$mix <- 1:nrow(df_summer_subset) # monetary index
df_summer_subset$M_Q<-ceiling(df_summer_subset$mix/108599)

# Create one RFM score

df_summer_subset$RFMscore<-df_summer_subset$R_Q*100+df_summer_subset$F_Q*10+df_summer_subset$M_Q

str(df_summer_subset)

#save as csv file to upload to python (check the correlation)
write.csv(df_summer_subset,file="h&msummersubset.csv",row.names = TRUE)

#after

#create sample
# Create sample
set.seed(13343)
train_ind <- createDataPartition(y = df_summer_subset$ret09,p=.8,list = FALSE)
train <- df_summer_subset[train_ind,]
test <- df_summer_subset[-train_ind,]

# Check balance
prop.table(table(df_summer_subset$ret09))
prop.table(table(train$ret09))
prop.table(table(test$ret09))

# Divide training sample to create validation sample 
set.seed(30000)
val_ind <- createDataPartition(y = train$ret09,p=.25,list = FALSE)
val <- train[val_ind,]
train <- train[-val_ind,]
table(train$ret09)

##################################
##  LOGISTIC REGRESSION ANAYSIS ##
##################################

model_logit <- glm(ret09 ~ graphical_solid + pd_garmentupperbody + colorvalue_dark + indexgroup_ladieswear + colormaster_black + colorvalue_dustylight +       
                     sec_w_collect + colorvalue_mediumdusty + pd_garmentlowerbody + garmentgroup_fancy + indexgroup_divided + colormaster_white + colormaster_beige + 
                     colorvalue_light +  weekend + numpd + numpurch + dollpurch + garmentgroup_basic + sec_d_collect + colormaster_blue +           
                     pd_garmentfullbody + graphical_alloverpattern + garmentgroup_nightwear + age_46_99 + sec_w_tailor + age_16_25 +                  
                     price08 + purch08 + pd_underwear + age_25_30 + age_30_46, 
                   data = train, family = "binomial"(link = "logit")) 
summary(model_logit)



summary(model_logit_step <- stepAIC(model_logit, direction = "both",trace = 1))

# The following is the "best" model

model_logit_step <-glm(formula = ret09 ~ graphical_solid + pd_garmentupperbody + 
                         colorvalue_dark + indexgroup_ladieswear + colormaster_black + 
                         colorvalue_dustylight + sec_w_collect + colorvalue_mediumdusty + 
                         pd_garmentlowerbody + garmentgroup_fancy + indexgroup_divided + 
                         colormaster_white + colormaster_beige + colorvalue_light + 
                         numpd + numpurch + dollpurch + garmentgroup_basic + sec_d_collect + 
                         colormaster_blue + pd_garmentfullbody + graphical_alloverpattern + 
                         garmentgroup_nightwear + age_46_99 + sec_w_tailor + age_16_25 + 
                         price08 + pd_underwear + age_25_30 + age_30_46, family = binomial(link = "logit"), 
                       data = train)

summary(model_logit_step)

#how well it predicts the validation sample
logit_probs <- predict(model_logit_step,newdata = val, type = "response")
logit_class <- rep("1",nrow(val))
logit_class[logit_probs <.50] <- "0" 

# Confusion Matrix
logit_class <- as.factor(logit_class)
confusionMatrix(as.factor(logit_class), positive = "1", as.factor(val$ret09))


# Other Assessment Tools

# ROC (Receiver Operating Characteristics) Curve
#Shows the tradeoff between sensitivity and specificity
val_class <- val$ret03

logistic_ROC_prediction <- ROCR::prediction(logit_probs,val_class)
logit_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") # true positive rate, false positive rate
plot(logit_ROC)

# AUC (Area under the curve)
# AUC of a perfect classifier is 100%, a random guess is 50%
AUC.tmp <- performance(logistic_ROC_prediction,"auc")
logit_AUC <- as.numeric(AUC.tmp@y.values)
logit_AUC

# Plot the "lift"
plotLift(logit_probs,val_class, cumulative = TRUE, n.buckets = 10)

#oversampling for logistic regression model
#create sample
# Create sample
set.seed(13343)
train_ind <- createDataPartition(y = df_summer_subset$ret09,p=.8,list = FALSE)
train <- df_summer_subset[train_ind,]
test <- df_summer_subset[-train_ind,]

# Check balance
prop.table(table(df_summer_subset$ret09))
prop.table(table(train$ret09))
prop.table(table(test$ret09))

# Deal with the imbalance by oversampling method
library("ROSE")
train<- ovun.sample(ret09 ~ graphical_solid + pd_garmentupperbody + colorvalue_dark + indexgroup_ladieswear + colormaster_black + colorvalue_dustylight +       
                      sec_w_collect + colorvalue_mediumdusty + pd_garmentlowerbody + garmentgroup_fancy + indexgroup_divided + colormaster_white + colormaster_beige + 
                      colorvalue_light +  weekend + numpd + numpurch + dollpurch + garmentgroup_basic + sec_d_collect + colormaster_blue +           
                      pd_garmentfullbody + graphical_alloverpattern + garmentgroup_nightwear + age_46_99 + sec_w_tailor + age_16_25 +                  
                      price08 + purch08 + pd_underwear + age_25_30 + age_30_46, 
                      data = df_summer_subset, method = "over",N =491214, seed=1)$data
table(train$ret09)

# Divide training sample to create validation sample 
set.seed(30000)
val_ind <- createDataPartition(y = train$ret09,p=.25,list = FALSE)
val <- train[val_ind,]
train <- train[-val_ind,]
table(train$ret09)

model_logit <- glm(ret09 ~ graphical_solid + pd_garmentupperbody + colorvalue_dark + indexgroup_ladieswear + colormaster_black + colorvalue_dustylight +       
                     sec_w_collect + colorvalue_mediumdusty + pd_garmentlowerbody + garmentgroup_fancy + indexgroup_divided + colormaster_white + colormaster_beige + 
                     colorvalue_light +  weekend + numpd + numpurch + dollpurch + garmentgroup_basic + sec_d_collect + colormaster_blue +           
                     pd_garmentfullbody + graphical_alloverpattern + garmentgroup_nightwear + age_46_99 + sec_w_tailor + age_16_25 +                  
                     price08 + purch08 + pd_underwear + age_25_30 + age_30_46,
                   data = train, family = "binomial"(link = "logit")) 
summary(model_logit)

#stepwise
summary(model_logit_step <- stepAIC(model_logit, direction = "both",trace = 1))

# The following is the "best" model

model_logit_step <- 

summary(model_logit_step)

#how well it predicts the validation sample
logit_probs <- predict(model_logit_step,newdata = val, type = "response")
logit_class <- rep("1",nrow(val))
logit_class[logit_probs <.50] <- "0" 

# Confusion Matrix
logit_class <- as.factor(logit_class)
confusionMatrix(logit_class, positive = "1", as.factor(val$ret09))


# Other Assessment Tools

# ROC (Receiver Operating Characteristics) Curve
#Shows the tradeoff between sensitivity and specificity
val_class <- val$ret03

logistic_ROC_prediction <- ROCR::prediction(logit_probs,val_class)
logit_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") # true positive rate, false positive rate
plot(logit_ROC)

# AUC (Area under the curve)
# AUC of a perfect classifier is 100%, a random guess is 50%
AUC.tmp <- performance(logistic_ROC_prediction,"auc")
logit_AUC <- as.numeric(AUC.tmp@y.values)
logit_AUC

# Plot the "lift"
plotLift(logit_probs,val_class, cumulative = TRUE, n.buckets = 10)

#################
##Random Forest##
#################
rf_model <- randomForest(ret09 ~ firstpurch + lastpurch08 + 
                           purch06 + purch07 + purch08 +
                           price06 + price07 + price08 +
                           age + FN + Active + club_member_status + sales_channel_id +
                           numpd + numgroup + numcolor + numgraphic +
                           weekend + numpurch +  dollpurch + RFMscore, data = train, ntree = 5, nodesize=10)
rf_class <- predict(rf_model,val,type="response")
rfconfmat <- confusionMatrix(rf_class, positive = "1", val$ret09)
rfconfmat

#ann
set.seed(13343)
train_ind <- createDataPartition(y = df_summer_subset$ret09,p=.8,list = FALSE)
train <- df_summer_subset[train_ind,]
test <- df_summer_subset[-train_ind,]

# Check balance
prop.table(table(df_summer_subset$ret09))
prop.table(table(train$ret09))
prop.table(table(test$ret09))

# Divide training sample to create validation sample 
set.seed(30000)
val_ind <- createDataPartition(y = train$ret09,p=.25,list = FALSE)
val <- train[val_ind,]
train <- train[-val_ind,]
table(train$ret09)

my.grid <- expand.grid(size = c(1,2,4), decay = c(0.25, 1, 2))

model_NN <- train(ret09~ firstpurch + lastpurch08 + 
                    purch06 + purch07 + purch08 +
                    price06 + price07 + price08 +
                    age + FN + Active + club_member_status + sales_channel_id +
                    numpd + numgroup + numcolor + numgraphic +
                    weekend + numpurch +  dollpurch + RFMscore,
                  data = train, method = "nnet",
                  tuneGrid = my.grid,trace = TRUE,
                  na.action = na.omit)

nn_class <- predict(model_NN,newdata = val)

plot(model_NN)

confusionMatrix(nn_class,val$ret09,positive = "1")
####################
#oversampling ann###
####################
pos_needed <- nrow(train[train$ret09==0,])  # at 50/50 we need as many pos as neg
pos_corpus <- train[train$ret09==1,]
neg_corpus <- train[train$ret09==0,]

set.seed(12345)
drawvec <- floor(runif(pos_needed, min=1, nrow(pos_corpus)))

# randomly draw from pos corpus and add to to negative to form new
#  training set

for (i in 1:pos_needed){
  neg_corpus <- rbind(neg_corpus,pos_corpus[drawvec[i],])
} 

# "os" = oversampling

os_training <- neg_corpus

##ann over
my.grid <- expand.grid(size = c(1,2,4), decay = c(0.25, 1, 2))
model_NN <- train(ret09~ firstpurch + lastpurch08 + 
                    purch06 + purch07 + purch08 +
                    price06 + price07 + price08 +
                    age + FN + Active + club_member_status + sales_channel_id +
                    numpd + numgroup + numcolor + numgraphic +
                    weekend + numpurch +  dollpurch + RFMscore,
                  data = os_training, method = "nnet",
                  tuneGrid = my.grid,trace = TRUE,
                  na.action = na.omit)

nn_class <- predict(model_NN,newdata = val)

plot(model_NN)

confusionMatrix(nn_class,val$ret09,positive = "1")


