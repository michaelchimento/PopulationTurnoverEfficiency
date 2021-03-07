#The following R code reproduces the statistical analyses presented in the manuscript Population turnover facilitates selection for efficiency
#All analyses written by Michael Chimento
library(lme4)
library(lmerTest)
library(tidyverse)
library(stargazer)
library(scales)

options(scipen=5)
options(digits=5)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Results: Summary statistics ####
load("../data/df_solves.Rda")
#number of solutions produced OVERALL
df_solves %>% ungroup() %>% filter(solver==1) %>% summarise(n())

#number of solutions produced OVERALL with useable TTS
df_solves %>% ungroup() %>% filter(solver==1, !is.na(solve_speed)) %>% summarise(n())

#solution type by population
df_solves %>% ungroup() %>% group_by(condition,Event) %>% filter(solver==1) %>% summarise(n()) %>% print(n=Inf)

#average by ID OVERALL
df = df_solves %>% ungroup() %>% filter(solver==1) %>% group_by(ID) %>% summarise(total=n())
mean(df$total)
range(df$total)

#average by population OVERALL
df = df_solves %>% ungroup() %>% filter(solver==1) %>% group_by(population) %>% summarise(total=n())
mean(df$total)
range(df$total)

#daily average OVERALL
df = df_solves %>% ungroup() %>% filter(solver==1) %>% group_by(ID,exp_day_count) %>% summarise(total=n())
df = df %>% group_by(ID) %>% summarise(mean_solves_day = mean(total))
mean(df$mean_solves_day)

#number of solutions produced by TUTORS
df_solves %>% ungroup() %>% filter(solver==1,tutor==1) %>% summarise(n())
#daily average of tutors
df = df_solves %>% ungroup() %>% filter(solver==1,tutor==1) %>% group_by(ID,exp_day_count) %>% summarise(total=n())
df = df %>% group_by(ID) %>% summarise(mean_solves_day = mean(total))
mean(df$mean_solves_day)

#number of solutions produced by NAIVES
df_solves %>% ungroup() %>% filter(solver==1,tutor==0) %>% summarise(n())
#daily average of NAIVES
df = df_solves %>% ungroup() %>% filter(solver==1,tutor==0) %>% group_by(ID,exp_day_count) %>% summarise(total=n())
df = df %>% group_by(ID) %>% summarise(mean_solves_day = mean(total))
mean(df$mean_solves_day)




# Results A: LMM Individual improvements with experience (Table S1a) ####
load("../data/df_solves.Rda")
#individual level improvements over time
df_solves %>%
  ungroup() %>%
  filter(tutor==1) %>% summarise(n())

df = df_solves %>%
  ungroup() %>%
  filter(solver==1, tutor==0,!is.na(solve_speed),solve_speed<=60) %>% mutate(year=as.factor(year)) %>% droplevels()
df$Event = relevel(df$Event, "inefficient")
df = df %>% mutate(scaled_ind_solve_count_bytype = scale(ind_solve_count_bytype))

m0 = lmer(log(solve_speed+1) ~ age + sex + scaled_ind_solve_count_bytype*Event + (1 | year/population/ID),data=df)
m1 = lmer(log(solve_speed+1) ~ scaled_ind_solve_count_bytype*Event + (1 | year/population/ID),data=df)

#compare models
anova(m0,m1) #removal of age and sex doesn't significantly improve the model
class(m0) <- "lmerMod"


# LMM effect of experimental day on TTS between conditions (Table S1b) ####
load("../data/df_solves.Rda")
df = df_solves %>%
  ungroup() %>%
  filter(solver==1,tutor==0,!is.na(solve_speed),solve_speed<=60) %>% mutate(year=as.factor(year), exp_day_count = exp_day_count+5) %>% droplevels()
df = df %>% mutate(scaled_exp_day_count = rescale(exp_day_count,to = c(-1, 1)),scaled_ind_solve_count_bytype = scale(ind_solve_count_bytype),log_solve_speed=log(solve_speed+1))

m2 = lmer(log_solve_speed ~ age + sex + scaled_exp_day_count*condition + (1 | Event) + (1| year/population/ID), data=df)
class(m2) <- "lmerMod"


# Logistic GLMM quantifying selection for efficiency (Table S1c) ####
load("../data/df_solves.Rda")
df= df_solves %>%
  mutate(exp_day_count=exp_day_count-6) %>%  #subtract 6 days so that 0 is the first day the efficient soln. is unblocked
  dplyr::filter(solver==1,exp_day_count >=0, !is.na(Event), population!= 13)
df = df %>% mutate(efficient_solve = ifelse(Event=="efficient",1,0), turnover = ifelse(condition=="turnover",1,0)) %>% ungroup() %>% select(population,ID,age,sex,efficient_solve,condition, exp_day_count, year)
m3 = glmer(efficient_solve ~ age + sex + exp_day_count*condition + (1 | year / population), data=df, family=binomial(link = "logit"),verbose = 1)

# Table S1 ####
stargazer(m0,m2,m3, dep.var.labels = c("log(TTS+1)","log(TTS+1)","efficient solution"), covariate.labels = c("age (adult)","sex (male)","solution index (scaled)", "solution (efficient)", "solution index (scaled):solution (efficient)", "experimental day (scaled)","experimental day","condition (turnover)","experimental day (scaled):condition (turnover)","experimental day:condition (turnover)","intercept"), title="Individual improvement with experience; Selection for efficient solution between conditions; LMM: Improvement over course of experiment", font.size = "small",report="vcstp*",single.row=T)


# Logistic GLMM asking whether immigrants linearly sampled social information (Table S2) ####
load("../data/df_solves.Rda")
df_solves = df_solves %>% mutate(exp_day_count = exp_day_count+5) %>% filter(solver==1,tutor==0,exp_day_count>=12) %>% ungroup()

df_pops = df_solves %>% group_by(condition,population,exp_day_count,Event) %>% summarise(count = n()) %>% pivot_wider(names_from = Event, values_from=count, names_prefix="pop_", values_fill=0)

#create dataframe of individuals with count of solutions by type each day
df_ind = df_solves %>% group_by(condition,population,W1,ID,exp_day_count,Event) %>% summarise(count = n()) %>% pivot_wider(names_from = Event, values_from=count, names_prefix="ind_", values_fill=0) %>% mutate(ind_prop_efficient = ind_efficient/(ind_inefficient+ind_efficient),W1=as.factor(W1))

df_final = left_join(df_pops, df_ind)
summary(df_final)
#subtract 2 values to get count of socially observed solutions on first day of producing efficient solution
df_final = df_final %>% mutate(soc_efficient = pop_efficient-ind_efficient, soc_inefficient= pop_inefficient-ind_inefficient)

#create proportion
df_final = df_final %>% mutate(soc_prop_efficient = soc_efficient/(soc_inefficient+soc_efficient))
df_final = df_final %>% mutate(soc_prop_efficient = if_else(is.nan(soc_prop_efficient), 0, soc_prop_efficient))
df_final = df_final %>% mutate(resident=ifelse(W1==1,TRUE,FALSE))

m1 = lmer(ind_prop_efficient ~ soc_prop_efficient*resident + (1 | population / ID), data=df_final,verbose = 1)
class(m1) <- "lmerMod"

stargazer(m1,covariate.labels = c("efficient social information","resident","efficient social information : resident","intercept"),dep.var.labels = c("daily proportion of efficient solns."),title="LMM: Do immigrants amplify social information?",report="vcstp*",single.row=T)

# GLM comparison of innovation timing (Table S3) ####
load("../data/df_solves.Rda")
df_innov=df_solves %>% filter(innovation==1) %>% ungroup()%>% mutate(exp_day_count=exp_day_count+5)
summary(df_innov$solve_day_count)
df_innov$condition = relevel(df_innov$condition,"turnover")

#days solving
df_innov %>% group_by(condition) %>% summarise(mean(solve_day_count))
m1 = glm(solve_day_count ~ age + sex + condition, data=df_innov)
summary(m1)

#days solving with removed outliers
df_innov %>% group_by(condition) %>% summarise(mean(solve_day_count))
m1_1 = glm(solve_day_count ~ age + sex + condition, data=df_innov %>% filter(solve_day_count>4))
summary(m1_1)

#days exposure
df_innov %>% group_by(condition) %>% summarise(mean(ind_day_count))
m2 = glm(ind_day_count ~ age + sex + condition, data=df_innov)


#experimental day count
df_innov %>% group_by(condition) %>% summarise(mean(exp_day_count))
m3 = glm(exp_day_count ~ age + sex + condition, data=df_innov)
stargazer(m1,m2,m3,covariate.labels = c("age (adult)","sex (male)","condition (static)","intercept"),dep.var.labels = c("days solving","days exposure","experimental day"),title="GLM: Differences in innovation timing between conditions",report="vcstp*",single.row=T)



# GLM asking whether experience of conformity predicts behavioral conservatism (Table S4) ####
load("../data/df_solves.Rda")
#filter solvers which experienced both solutions
df_solves = df_solves %>% filter(total_count_efficient>0 & total_count_inefficient > 0, solver==1)
#select final 10% of solves from each solver
df_end = df_solves %>% ungroup() %>% arrange(full_timestamp) %>% group_by(population,ID) %>% slice_tail(prop = .1)
#determine if this 10% is majority efficient or inefficient
df_end_ID = df_end %>% group_by(year,population,age,sex,ID,Event) %>% summarise(count = n()) %>% pivot_wider(names_from = Event, values_from=count, values_fill=0) %>% mutate(switched=if_else(efficient>inefficient,1,0))

#create dataframe of when individuals first produced efficient solution
df_temp = df_solves %>% group_by(population,ID) %>% filter(Event=="efficient",solver==1) %>% slice_head(n=1) %>% select(solve_day_count,exp_day_count)
df_end_ID = left_join(df_end_ID,df_temp) %>% mutate(solve_day_count = if_else(is.na(solve_day_count),0,solve_day_count)) %>% select(-c(efficient,inefficient))

#create dataframe of populations with count of solutions by type each day
df_pops = df_solves %>% group_by(population,exp_day_count,Event) %>% summarise(count = n()) %>% pivot_wider(names_from = Event, values_from=count, names_prefix="pop_", values_fill=0)
df_end_ID = left_join(df_end_ID, df_pops)

#create dataframe of individuals with count of solutions by type each day
df_ind = df_solves %>% group_by(population,ID,exp_day_count,Event) %>% summarise(count = n()) %>% pivot_wider(names_from = Event, values_from=count, names_prefix="ind_", values_fill=0)
df_end_ID = left_join(df_end_ID, df_ind)

#subtract 2 values to get count of socially observed solutions on first day of producing efficient solution
df_end_ID = df_end_ID %>% mutate(soc_efficient = pop_efficient-ind_efficient, soc_inefficient= pop_inefficient-ind_inefficient)

#create proportion
df_end_ID = df_end_ID %>% mutate(soc_prop_inefficient = soc_inefficient/(soc_inefficient+soc_efficient))
df_end_ID = df_end_ID %>% mutate(soc_prop_inefficient = if_else(is.nan(soc_prop_inefficient), 0, soc_prop_inefficient),failed_switch = if_else(switched==1,0,1))
df_final = droplevels(df_end_ID)
#model what predicts failure to switch
m1 = glmer(failed_switch ~ solve_day_count + soc_prop_inefficient + (1|year/population), data=df_final, family=binomial(link = "logit"))
summary(m1)
stargazer(m1, covariate.labels = c("days experience","socially observed inefficient","intercept"),dep.var.labels = c("failure to adopt"),title="GLMM: Predictors of failure to adopt efficient solution",report="vcstp*",single.row=T)

#check how many innovators switched
load("../data/df_solves.Rda")
df_innovators = df_solves %>% group_by(population,ID) %>% summarise(innovator=sum(innovation)) %>% filter(innovator==1)
df_switched = df_end_ID %>% filter(switched==1)
df_innovators %>% filter(ID %in% df_switched$ID) #only 5 switched


# Does learning during the diffusion period inhibit sampling of both solutions (Table S5) ####
load("../data/df_solves.Rda")
#individual level improvements over time
df = df_solves %>%
  ungroup() %>% mutate(year=as.factor(year)) %>% droplevels()

df = df %>% filter(solver==1)

df1 = df %>% mutate(learned_diffusion=ifelse(day_first_solve<7,1,0),
                    sampler = ifelse((total_count_efficient>0 & total_count_inefficient > 0),1,0))

df1 = df1 %>% group_by(population,ID) %>% slice_head(n=1)

m1 = glmer(sampler ~ learned_diffusion + (1|population),data=df1, family="binomial")
stargazer(m1, covariate.labels = c("learned during diffusion","intercept"),dep.var.labels = c("sampler"),title="GLMM: Did learning during the diffusion period affect sampling?",report="vcstp*",single.row=T)
