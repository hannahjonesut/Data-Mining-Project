##see how well just feateng does
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
         P1HRSWK_bins = ifelse(P1HRSWK == 0, 0, 
                               ifelse(P1HRSWK>0 & P1HRSWK<=20, 1, 
                                      ifelse(P1HRSWK>20 & P1HRSWK<=40, 2,
                                             ifelse(P1HRSWK>40, 3,0)))),
         pub_ed = ifelse(EDCPUB == 1, 1, 0),
         sus_exp = ifelse(SESUSOUT==1|SESUSPIN==1|SEEXPEL==1,1,0),
         sing_parent = ifelse(HHPARN19_BRD==2|P2GUARD==2|FAMILY19_BRD>3|P1BFGF==2|FAMILY19X>3,1,0),
         internet_access = ifelse(INTACC<4|HVINTSPHO==1|HVINTCOM==1|CHLDNT<5|LRNCOMP==1|LRNTAB==1|LRNCELL==1,1,0),
         school_choice_pos = ifelse(SCCHOICE==1|SPUBCHOIX==1|S1STCHOI==1,1,0))



ppfi_feateng <- select(ppfi_feateng, -c(FHCHECKX, FHHELP, FOSTORY2X, FOCRAFTS, FOGAMES, FOBUILDX, FOSPORT, FORESPON, 
                                        FOHISTX, FODINNERX, FOLIBRAYX, FOBOOKSTX, FOCONCRTX, FOMUSEUMX, FOZOOX, FOGROUPX, 
                                        FOSPRTEVX, HHBROSX, HHSISSX, HHAUNTSX, HHUNCLSX, HHGMASX, HHGPASX, HHCSNSX, HHORELSX,
                                        HHPRTNRSX, HHONRELSX, HHENGLISH, HHSPANISH, HHFRENCH, HHCHINESE, HHOTHLANG, P1FRLNG, P1MRSTA,
                                        P1SPEAK, P1AGEMV, FSSPORTX, FSVOL,FSMTNG, FSPTMTNG, FSATCNFN,FSFUNDRS, FSCOMMTE,FSCOUNSLR,
                                        FSNOTESX, FSMEMO, FSPHONCHX, P1HRSWK, EDCPUB, EDCPRI, EDCHSFL, EDCCAT, EDCREL, EDCINTK12, EDCINTCOL, EDCCOL, SEREPTK, SEREPT1, SEREPT2, SEREPT3, SEREPT4, SEREPT5, SEREPT6, SEREPT7, SEREPT8, SEREPT9, SEREPT10, SEREPT11, SEREPT12, SESUSOUT, SESUSPIN, SEEXPEL, HHPARN19_BRD, P2GUARD, FAMILY19_BRD,P1BFGF,FAMILY19X, INTACC, HVINTSPHO, HVINTCOM, CHLDNT, LRNCOMP, LRNTAB, LRNCELL, SCCHOICE, SPUBCHOIX, S1STCHOI, LOCALE, SCHLSAFETY, SCHLSTFQUALITY, AVAILCOURSE, XTRACURRIC, STUDNTCHAR, STUDNTPERFORM, RELIGSOR, SPECALEDSERVS, SPECALFACILTS, CLSSIZE, SCHLCOST, SCONSIDR))


ppfi_cut <- select(ppfi_feateng, c(ALLGRADEX, SEGRADES,hw_involve,create_op, fam_time, cultural_exp, parent_involved_school, 
                                   gen_sch_comm, num_siblings, num_ext_fam, num_nonrelatives, english_speakinghh, spanish_speakinghh, otherlang_speakinghh, parents_together, parent_lang_english,
                                   P1HRSWK_bins, pub_ed, sus_exp, sing_parent, internet_access, school_choice_pos))

ppfi_pca_data <- select(ppfi_cut, -SEGRADES)

ppfi_pca_data = scale(ppfi_pca_data , center=TRUE, scale=TRUE)

mu = attr(ppfi_pca_data,"scaled:center") # mean
sigma = attr(ppfi_pca_data,"scaled:scale")

pca_ppfi = prcomp(ppfi_pca_data, scale=TRUE)

loadings_ppfi = pca_ppfi $rotation
scores_ppfi = pca_ppfi$x
summary(pca_ppfi)

ppfi_combined = data.frame(ppfi_cut, pca_ppfi$x)

ppfi_split = initial_split(ppfi_combined)
n = nrow(ppfi_combined)
n_train = floor(0.8*n)
n_test = n - n_train
train_cases = sample.int(n, size=n_train, replace=FALSE)
ppfi_train = training(ppfi_split)
ppfi_test = testing(ppfi_split)

grades_forest = randomForest(SEGRADES ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 +
                               PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 +,
                             data = ppfi_train, importance = TRUE)

yhat = predict(grades_forest, ppfi_test)
comp_table<-as.data.frame(table(yhat,as.factor(ppfi_test$SEGRADES)))
predicted<-round(yhat)
actual<-as.factor(ppfi_test$SEGRADES)
comp_table<-as.data.frame(cbind(actual,predicted))
comp_table$flag<-ifelse(comp_table$actual==comp_table$predicted,1,0)
sum(comp_table$flag)
sum(comp_table$flag)*100/nrow(comp_table)

#look at important PC
varImpPlot(grades_forest)

o1 = order(loadings[,1], decreasing=TRUE)
colnames(ppfi_cut)[head(o1,5)]
colnames(ppfi_cut)[tail(o1,3)]



train = data.frame(pca_ppfi$x[,1:55])
train['SEGRADES']= ppfi_train$SEGRADES
train_load = pca_ppfi$rotation[,1:55]

test_pre <- scale(ppfi_test) %*% train_load
test <- as.data.frame(test_pre)
test['SEGRADES']=ppfi_test$SEGRADES

ppfi_pc= merge(ppfi_feateng, pca_ppfi$x[,1:70], by="row.names")

- find lit/article about growing space for ML in education - The Importance of Machine Learning in the Education Sector, https://www.newgenapps.com/blog/the-importance-of-machine-learning-in-the-education-sector/#:~:text=Improving%20the%20Efficiency%20of%20Both,manage%20their%20content%20and%20curriculum.&text=Also%2C%20Machine%20Learning%20makes%20educators,classroom%20management%2C%20scheduling%2C%20etc.
  
  - discussion of how using creative ML/data mining can lead to more robust/actionable/empathetic understandings of students, beyond simply looking at grades and attendance-- contextual understanding of kids/kids success/family opportunities to elicit success

- find lit/article about what has been done to bolster parent engagement, and how lack of parent engagement affects children generally, not necesarily just through school ---> Visioning Parent Engagement in Urban Schools, https://journals.sagepub.com/doi/pdf/10.1177/105268460701700602
- Parents, in particular, parents of color support their children in ways that go undetected by schools.
- Parents—especially, low-income and minority parents—are more likely to be involved in education when schools invite their participation, provide multiple entry points for involvement, value their perspectives, and reach out in culturally appropriate ways.
- Researchers have noted the ways in which schools marginalize parents of color and low-income parents, and they have pointed to the limitations of traditional conceptions of parent involvement, geared to the school’s agenda.