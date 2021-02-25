## Many Researchers Project: team 84
# Anton Olsson Collentine & Erwin Gielens
# RQ1: is there an effect of religiosity on wellbeing?
# RQ2: is the effect of religiosity on wellbeing moderated by cultural norms?

#factors + indicators
# x: religiosity -- rel_1 to rel_9
# y: wellbeing -- generalized -- gen_1, gen_2
#                 physical -- phys_1 to phys_7
#                 psychological -- psych_1 to psych_6
#                 social -- soc_1, soc_2
# z: perceived cultural importance of relgion -- cnorm_1, cnorm_2
# controls: age, gender (3), education (7), ses, gdp,
#           sample_type (4), country (24)

##########################
#### Packages        ####
#########################

library(lavaan) #for running SEM models
library(simsem) #for simulating data to try out code for preregistration


#########################
#### Data Simulation ####
#########################
#Simulated data to test out code, not part of final analysis
#see e.g., https://github.com/simsem/simsem/wiki/Example-2:-Getting-Started-%28lavaan-input%29


m_sim <-  '
rel =~ 0.9*rel_1 + 0.7*rel_2 + 0.8*rel_3 + 0.6*rel_4 + 0.6*rel_5 + 1.2*rel_6 + 0.75*rel_7 + 0.55*rel_8 + 0.3*rel_9

wb_gen =~ 1.0*gen_1 + 0.9*gen_2
wb_phys =~ 0.6*phys_1 + 0.8*phys_2 + 0.4*phys_3 + 1.1*phys_4 + 1.2*phys_5 + 0.86*phys_6 + 0.99*phys_7
wb_psych =~ 0.6*psych_1 + 1.02*psych_2 + 0.9*psych_3 + 0.55*psych_4 + 0.83*psych_5 + 1.15*psych_6
wb_soc =~ 0.5*soc_1 + 0.6*soc_2
wb =~ 1.2*wb_gen + 0.6*wb_phys + 1.1*wb_psych + 0.5*wb_soc

cnorm =~ 1.1*cnorm_1 + 0.9*cnorm_2

wb ~ 0.6*rel + 
0.02*age + 0.15*gender_d1 + 0.3*gender_d2 + 
0.12*education + 
0.3*ses + 0.2*gdp +
0.02*sample_type_d1 + 0.01*sample_type_d2 + 0.001*sample_type_d3 

#fix factor variance for identification purposes
cnorm ~~ 1*cnorm
wb_soc ~~ 1*wb_soc
wb_gen ~~ 1*wb_gen
' 
n <- 24*400
dat_sim <- generate(m_sim, n)

#add country dummies
countries <- c('country_1','country_2','country_3','country_4','country_5','country_6','country_7','country_8',
               'country_9','country_10','country_11','country_12','country_13','country_14','country_15','country_16',
               'country_17','country_18','country_19','country_20','country_21','country_22','country_23', 'country_24')

country <- rep(countries, each = 400)
country <- factor(country)
country_dummies <- model.matrix(~country)
country_dummies <- country_dummies[,-1] #drop intercept column
colnames(country_dummies) <- countries[-24] #rename for clarity

dat_sim <- cbind(dat_sim, country_dummies) #add dummies to rest of dataframe


#########################
#### Model Estimation ###
#########################

#assuming MARP data placed in same folder as the r-script we do not need to specify setwd()
#Rstudio will automatically setwd() to script location when opening the file (IFF Rstudio was not already open)
df <- read.csv('MARP_data.csv', header=T)
names(df)

#Alternative way to create dummies: https://stackoverflow.com/questions/11952706/generate-a-dummy-variable

#dummies
table(df$gender)
df$gender_man <- ifelse(df$gender=='man', 1,0)
df$gender_other <- ifelse(df$gender=='other', 1,0)
table(df$sample_type)
df$sample_mixed <- ifelse(df$sample_type=='mixed', 1,0)
df$sample_online <- ifelse(df$sample_type=='online panel', 1,0)
df$sample_students <- ifelse(df$sample_type=='students', 1,0)
table(df$country)
df$country_belgium <- ifelse(df$country=='Belgium', 1,0)
df$country_brazil <- ifelse(df$country=='Brazil', 1,0)
df$country_canada <- ifelse(df$country=='Canada', 1,0)
df$country_chile <- ifelse(df$country=='Chile', 1,0)
df$country_china <- ifelse(df$country=='China', 1,0)
df$country_croatia <- ifelse(df$country=='Croatia', 1,0)
df$country_denmark <- ifelse(df$country=='Denmark', 1,0)
df$country_france <- ifelse(df$country=='France', 1,0)
df$country_germany <- ifelse(df$country=='Germany', 1,0)
df$country_india <- ifelse(df$country=='India', 1,0)
df$country_ireland <- ifelse(df$country=='Ireland', 1,0)
df$country_israel <- ifelse(df$country=='Israel', 1,0)
df$country_italy <- ifelse(df$country=='Italy', 1,0)
df$country_japan <- ifelse(df$country=='Japan', 1,0)
df$country_lithuania <- ifelse(df$country=='Lithuania', 1,0)
df$country_morocco <- ifelse(df$country=='Morocco', 1,0)
df$country_netherlands <- ifelse(df$country=='Netherlands', 1,0)
df$country_romania <- ifelse(df$country=='Romania', 1,0)
df$country_singapore <- ifelse(df$country=='Singapore', 1,0)
df$country_spain <- ifelse(df$country=='Spain', 1,0)
df$country_turkey <- ifelse(df$country=='Turkey', 1,0)
df$country_uk <- ifelse(df$country=='UK', 1,0)
df$country_us <- ifelse(df$country=='US', 1,0)


#*******************
#Test main effect
#*******************

#CFA
m_cfa <- '
rel =~ rel_1 + rel_2 + rel_3 + rel_4 + rel_5 + rel_6 + rel_7 + rel_8 + rel_9

wb_gen =~ 1*wb_gen_1 + wb_gen_2
wb_phys =~ wb_phys_1 + wb_phys_2 + wb_phys_3 + wb_phys_4 + wb_phys_5 + wb_phys_6 + wb_phys_7
wb_psych =~ wb_psych_1 + wb_psych_2 + wb_psych_3 + wb_psych_4 + wb_psych_5 + wb_psych_6
wb_soc =~ 1*wb_soc_1 + wb_soc_2
wb =~ wb_gen + wb_phys + wb_psych + wb_soc

cnorm =~ 1*cnorm_1 + cnorm_2

cnorm ~~ cnorm
wb_soc ~~ wb_soc
wb_gen ~~ wb_gen
' 

fit_cfa <- sem(m_cfa, data=df)
summary(fit_cfa, fit.measures=T, rsquare=T, standardized = T)
#fit looks ok, could be better. 
#cfa model used to test interaction later

#Direct effect
m_sem_direct <- '
rel =~ rel_1 + rel_2 + rel_3 + rel_4 + rel_5 + rel_6 + rel_7 + rel_8 + rel_9

wb_gen =~ 1*wb_gen_1 + wb_gen_2
wb_phys =~ wb_phys_1 + wb_phys_2 + wb_phys_3 + wb_phys_4 + wb_phys_5 + wb_phys_6 + wb_phys_7
wb_psych =~ wb_psych_1 + wb_psych_2 + wb_psych_3 + wb_psych_4 + wb_psych_5 + wb_psych_6
wb_soc =~ 1*wb_soc_1 + wb_soc_2
wb =~ wb_gen + wb_phys + wb_psych + wb_soc

wb_soc ~~ wb_soc
wb_gen ~~ wb_gen

wb ~ rel + 
age + gender_man + gender_other + 
education + 
ses + 
country_belgium + country_brazil + country_canada + country_chile + country_china +
country_croatia + country_denmark + country_france + country_germany + country_india +
country_ireland + country_israel + country_italy + country_japan + country_lithuania +
country_morocco + country_netherlands + country_romania + country_singapore + country_spain +
country_turkey + country_uk + country_us

' 


fit_sem_direct <- sem(m_sem_direct, data=df)
summary(fit_sem_direct, fit.measures=T, rsquare=T, standardized = T)
resid(fit_sem_direct, type = "cor")

standardizedsolution(fit_sem_direct) #get standardized parameter estimates, including CIs
#***********************
#Test interaction effect
#***********************
#using this approach for the interaction: https://stackoverflow.com/questions/24399353/r-lavaan-coding-latent-variable-interactions
#see also ?semTools::indProd for some references

#First compute the latent interaction variable
df2 <- data.frame(df, predict(fit_cfa))
df$rel_cnorm <- df2$rel * df2$cnorm #compute the latent interaction from the estimate latent variables

m_sem_int <- '
rel =~ rel_1 + rel_2 + rel_3 + rel_4 + rel_5 + rel_6 + rel_7 + rel_8 + rel_9

wb_gen =~ 1*wb_gen_1 + wb_gen_2
wb_phys =~ wb_phys_1 + wb_phys_2 + wb_phys_3 + wb_phys_4 + wb_phys_5 + wb_phys_6 + wb_phys_7
wb_psych =~ wb_psych_1 + wb_psych_2 + wb_psych_3 + wb_psych_4 + wb_psych_5 + wb_psych_6
wb_soc =~ 1*wb_soc_1 + wb_soc_2
wb =~ wb_gen + wb_phys + wb_psych + wb_soc

cnorm =~ 1*cnorm_1 + cnorm_2

wb_soc ~~ wb_soc
wb_gen ~~ wb_gen
cnorm ~~ cnorm

wb ~ rel + cnorm + rel_cnorm +
age + gender_man + gender_other + 
education + 
ses + 
country_belgium + country_brazil + country_canada + country_chile + country_china +
country_croatia + country_denmark + country_france + country_germany + country_india +
country_ireland + country_israel + country_italy + country_japan + country_lithuania +
country_morocco + country_netherlands + country_romania + country_singapore + country_spain +
country_turkey + country_uk + country_us


' 


fit_sem_int <- sem(m_sem_int, data=df)
summary(fit_sem_int, fit.measures=T, rsquare=T, standardized = T)

standardizedsolution(fit_sem_int) #get standardized parameter estimates, including CIs
