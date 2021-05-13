library(dplyr)
library(tidyverse)
library(tidyr)
library(gamlr)
library(foreach)
library(ggplot2)
library(stargazer)
library(randomForest)
library(caret)
library(estimatr)
library(lubridate)
library(modelr)
library(rsample)
library(mosaic)
library(parallel)
library(foreach)
library(ggcorrplot)
library(utils)
library(fpc)

ppfi_trimmed <- read.csv('https://raw.githubusercontent.com/Joey-Herrera/Data-Mining-Project/main/NHES/ppfi_trimmed.csv')


#remove all -1
ppfi_recode <- ppfi_trimmed %>%
  filter(SEGRADES != -1)

ppfi_recode[ppfi_recode<0]<- 0
ppfi_recode<- lapply(ppfi_recode, as.numeric)
ppfi_recode<- data.frame(ppfi_recode)

#feature engineering

#hw involvement-- someone checks about or helps with homework
#creative opportunities if read to child, do crafts, go to zoo, plays, etc-- engaging with child in non-school creative pursuits
#family time talking about responsibilities, family history, having dinner together more than 3 times
#number of siblings combined, number of relatives in house, number non-relatives in house
ppfi_feateng<- ppfi_recode %>%
  mutate(hw_involve = ifelse(FHCHECKX > 3 | FHHELP > 1, 1,0), 
         create_op = ifelse(FOSTORY2X==1 | FOCRAFTS == 1 | FOGAMES == 1 | FOBUILDX == 1 |FOSPORT == 1, 1, 0),
         fam_time = ifelse(FORESPON==1 | FOHISTX == 1 | FODINNERX > 3, 1, 0),
         cultural_exp = ifelse(FOLIBRAYX==1|FOBOOKSTX==1|FOCONCRTX==1|FOMUSEUMX==1|FOZOOX==1|FOGROUPX==1|FOSPRTEVX==1, 1, 0),
         parent_involved_school = ifelse(FSSPORTX==1|FSVOL==1|FSMTNG==1|FSPTMTNG==1|FSATCNFN==1|FSFUNDRS==1|FSCOMMTE==1|FSCOUNSLR==1, 1, 0 ),
         gen_sch_comm = ifelse(FSNOTESX == 1|FSMEMO==1|FSPHONCHX==1, 1, 0),
         num_siblings = HHBROSX + HHSISSX,
         num_ext_fam = HHAUNTSX+HHUNCLSX+HHGMASX+HHGPASX+HHCSNSX+HHORELSX,
         num_nonrelatives = HHPRTNRSX+HHONRELSX,
         english_speakinghh = ifelse(HHENGLISH==1, 1, 0), 
         spanish_speakinghh = ifelse(HHSPANISH==1, 1,0),
         otherlang_speakinghh = ifelse(HHFRENCH == 1| HHCHINESE == 1 | HHOTHLANG==1,1,0),
         parents_together = ifelse(P1MRSTA == 1, 1, 0),
         parent_lang_english = ifelse(P1SPEAK == 1 | P1SPEAK == 3 | P1SPEAK == 5, 1,0), 
         P1HRSWK_bins = ifelse(P1HRSWK>0 & P1HRSWK<=10, 1, 
                               ifelse(P1HRSWK>10 & P1HRSWK<=20, 2,
                                      ifelse(P1HRSWK>20 & P1HRSWK<=30, 3,
                                             ifelse(P1HRSWK>30 & P1HRSWK<=40, 4,
                                                    ifelse(P1HRSWK>40 & P1HRSWK<=50, 5,
                                                           ifelse(P1HRSWK>50, 6, 0)))))),
         pub_ed = ifelse(EDCPUB == 1, 1, 0))
         


ppfi_feateng <- select(ppfi_feateng, -c(FHCHECKX, FHHELP, FOSTORY2X, FOCRAFTS, FOGAMES, FOBUILDX, FOSPORT, FORESPON, 
                                       FOHISTX, FODINNERX, FOLIBRAYX, FOBOOKSTX, FOCONCRTX, FOMUSEUMX, FOZOOX, FOGROUPX, 
                                       FOSPRTEVX, HHBROSX, HHSISSX, HHAUNTSX, HHUNCLSX, HHGMASX, HHGPASX, HHCSNSX, HHORELSX,
                                       HHPRTNRSX, HHONRELSX, HHENGLISH, HHSPANISH, HHFRENCH, HHCHINESE, HHOTHLANG, P1FRLNG, P1MRSTA,
                                       P1SPEAK, P1AGEMV, FSSPORTX, FSVOL,FSMTNG, FSPTMTNG, FSATCNFN,FSFUNDRS, FSCOMMTE,FSCOUNSLR,
                                       FSNOTESX, FSMEMO, FSPHONCHX, P1HRSWK, EDCPUB, EDCPRI, EDCHSFL, EDCCAT, EDCREL, EDCINTK12, EDCINTCOL, EDCCOL))


#intro graphs

#look at participation vs grades
grades_english <- ppfi_feateng %>%
  group_by(english_speakinghh)%>%
  summarize(meangrades = mean(SEGRADES))

involved_parentenglish <- ppfi_feateng %>%
  group_by(parent_lang_english)%>%
  summarize(meangrades = mean(SEGRADES))

ggplot(involved_parentenglish)+
  geom_col(aes(x = parent_lang_english, y = meangrades))

#based on child's race
ppfi_feateng %>%
  group_by(RACEETH) %>%
  summarize(avg_grades = mean(SEGRADES), avgparentinvolv = mean(parent_involved_school), avg_cultural_exp= mean(cultural_exp), meancreateop = mean(create_op), mean_enjoy = mean(SEENJOY))

#based on hours worked
ppfi_feateng %>%
  group_by(P1HRSWK_bins) %>%
  summarize(avg_grades = mean(SEGRADES),avgparentinvolv = mean(parent_involved_school), 
            avg_cultural_exp= mean(cultural_exp), avg_fam_time = mean(fam_time), meancreateop = mean(create_op),mean_enjoy = mean(SEENJOY))

#how difficult is it for parent to participate due to language diff 1-very, 2-somewhat, 3-not diff
ppfi_feateng%>%
  group_by(P1DIFFI) %>%
  filter(P1DIFFI!= -1)%>%
  summarize(avg_grades = mean(SEGRADES),avgparentinvolv = mean(parent_involved_school), 
            avg_cultural_exp= mean(cultural_exp), avg_fam_time = mean(fam_time), meancreateop = mean(create_op), mean_enjoy = mean(SEENJOY))

#speak spanish or other non-english at home
ppfi_feateng%>%
  group_by(english_speakinghh) %>%
  summarize(avg_grades = mean(SEGRADES),avgparentinvolv = mean(parent_involved_school), 
            avg_cultural_exp= mean(cultural_exp), avg_fam_time = mean(fam_time), meancreateop = mean(create_op), mean_enjoy = mean(SEENJOY))

#kids enjoy
ppfi_feateng%>%
  group_by(SEENJOY) %>%
  summarize(avg_grades = mean(SEGRADES),avgparentinvolv = mean(parent_involved_school), 
            avg_cultural_exp= mean(cultural_exp), avg_fam_time = mean(fam_time), meancreateop = mean(create_op))


#Building a model for success

ppfi_pca_data <- select(ppfi_feateng, -SEGRADES)
ppfi_pca_data = scale(ppfi_pca_data , center=TRUE, scale=TRUE)
mu = attr(ppfi_pca_data,"scaled:center") # mean
sigma = attr(ppfi_pca_data,"scaled:scale")

pca_ppfi = prcomp(ppfi_pca_data, scale=TRUE)

loadings_ppfi = pca_ppfi$rotation
scores_ppfi = pca_ppfi$x
summary(pca_ppfi)

ppfi_combined = data.frame(ppfi_feateng$SEGRADES, pca_ppfi$x[,1:55])

ppfi_split = initial_split(ppfi_combined)
n = nrow(ppfi_combined)
n_train = floor(0.8*n)
n_test = n - n_train
train_cases = sample.int(n, size=n_train, replace=FALSE)
ppfi_train = training(ppfi_split)
ppfi_test = testing(ppfi_split)


# run a random forest using PCA variables in train_author

ppfi_train$ppfi_feateng.SEGRADES = factor(ppfi_train$ppfi_feateng.SEGRADES) 

grades_forest = randomForest(ppfi_feateng.SEGRADES ~ .,
                             data = ppfi_train, importance = TRUE)

yhat = predict(grades_forest, ppfi_test)
comp_table<-as.data.frame(table(yhat,as.factor(ppfi_test$ppfi_feateng.SEGRADES)))
predicted<-yhat
actual<-as.factor(ppfi_test$ppfi_feateng.SEGRADES)
comp_table<-as.data.frame(cbind(actual,predicted))
comp_table$flag<-ifelse(comp_table$actual==comp_table$predicted,1,0)
sum(comp_table$flag)
sum(comp_table$flag)*100/nrow(comp_table)

#look at important PC
varImpPlot(grades_forest)

#examine top 5(ish) principle components to understand constituent parts

o1 = order(loadings_ppfi[,1], decreasing=TRUE)
colnames(ppfi_pca_data)[head(o1,5)]
colnames(ppfi_pca_data)[tail(o1,3)]

o2 = order(loadings_ppfi[,4], decreasing=TRUE)
colnames(ppfi_pca_data)[head(o2,5)]
colnames(ppfi_pca_data)[tail(o2,3)]

o3 = order(loadings_ppfi[,5], decreasing=TRUE)
colnames(ppfi_pca_data)[head(o3,5)]
colnames(ppfi_pca_data)[tail(o3,3)]


ggplot(train)+
  geom_violin(aes(x = PC1, y=SEGRADES))+ 
  scale_y_discrete(breaks = 1:4, labels=c("A's","B's","C's","D's"))

#try out some graphs
loadings_summary = pca_ppfi$rotation[,1:70] %>%
  as.data.frame() %>%
  rownames_to_column('SEGRADES')

loadings_summary %>%
  select(SEGRADES, PC19) %>%
  arrange(desc(PC19))

ggplot(shows) + 
  geom_col(aes(x=reorder(Show, PC1), y=PC1)) + 
  coord_flip()

#clustering
ppfi_clust_train = subset(ppfi_recode, select = -c(SEGRADES, X, BASMID))

ppfi_clust_train_scaled = scale(ppfi_clust_train, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(ppfi_clust_train_scaled,"scaled:center")
sigma = attr(ppfi_clust_train_scaled,"scaled:scale")

# Run k-means with 4 clusters and 25 starts
clust1 = kmeans(ppfi_clust_train_scaled, 4, nstart=25)

plotcluster(ppfi_recode, clust1$cluster)
  

##FOR SEGRADES-- higher number implies worse grade

##INTERESTING GRAPHS TO PROVE POINTS

#PARENT RACE VS INVOLVEMENT -- FEATURE ENGINEERING for involvement?





