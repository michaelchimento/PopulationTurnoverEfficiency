#The following R code reproduces the statistical analyses presented in the manuscript Population turnover facilitates selection for efficiency
#All analyses written by Michael Chimento
library(lme4)
library(lmerTest)
library(tidyverse)
library(rms)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Results: Individual improvements with experience (Table S1)
{
  load("../data/df_solves.Rda")
  #individual level improvements over time
  df = df_solves %>%
    ungroup() %>%
    filter(solver==1, tutor==0,!is.na(solve_speed),solve_speed<=60) %>% mutate(year=as.factor(year)) %>% droplevels()
  
  df = df %>% group_by(population,ID,Event) %>% mutate(scaled_ind_solve_count_bytype = scale(ind_solve_count_bytype))
  m1 = lmer(log(solve_speed+1) ~ age + sex + scaled_ind_solve_count_bytype + (1 | Event) + (1 | year/population/ID),data=df)
  summary(m1)
  exp(fixef(m1))
}

# Results A: logistic GLMM quantifying selection for efficiency (Table S2)
{
  load("../data/df_solves.Rda")
  df= df_solves %>% 
    mutate(exp_day_count=exp_day_count-6) %>%  #subtract 6 days so that 0 is the first day the efficient soln. is unblocked
    dplyr::filter(solver==1,exp_day_count >=0, !is.na(Event), population!= 13)
  df = df %>% mutate(efficient_solve = ifelse(Event=="efficient",1,0), turnover = ifelse(condition=="turnover",1,0)) %>% ungroup() %>% select(population,ID,age,sex,efficient_solve,turnover, exp_day_count, year)
  m1 = glmer(efficient_solve ~ age + sex + exp_day_count*turnover + (1 | year / population), data=df, family=binomial(link = "logit"),verbose = 1)
}

# Results B: LMM effect of experimental day on TTS between conditions (Table S3)
{
  load("../data/df_solves.Rda")
  df = df_solves %>%
    ungroup() %>%
    filter(solver==1,tutor==0,!is.na(solve_speed),solve_speed<=60) %>% mutate(week=as.factor(week),year=as.factor(year)) %>% droplevels()
  fit.lmer = lmer(log(solve_speed+1) ~ sex + age + exp_day_count*condition + (1| year/population), data=df)
  summary(fit.lmer)
  exp(fixef(fit.lmer))
  #Wilcox test of solve speeds in final week
  df$condition = relevel(df$condition,"turnover")
  x=wilcox.test(formula=solve_speed ~ condition, alternative="l", data=df %>% filter(week=="w5"))
  print(x)
}

# Results C: comparison of innovation timing
{
  load("../data/df_solves.Rda")
  df_innov=df_solves %>% filter(innovation==1) %>% ungroup()
  df_innov$condition = relevel(df_innov$condition,"turnover")
  
  #days exposure
  wilcox.test(df_innov$ind_day_count ~ df_innov$condition,alternative = "l")
  df_innov %>% group_by(condition) %>% summarize(mean(ind_day_count))
  wilcox.test(df_innov$solve_day_count ~ df_innov$condition,alternative = "l")
  df_innov %>% group_by(condition) %>% summarize(mean(solve_day_count))
  wilcox.test(df_innov$exp_day_count ~ df_innov$condition,alternative = "l")
  df_innov %>% group_by(condition) %>% summarize(mean(ind_day_count))
}

# Results D: GLM analyzing whether experience of conformity predicts behavioral conservatism (Table S4)
{
  load("../data/df_solves.Rda")
  #filter solvers which experienced both solutions
  df_solves = df_solves %>% filter(total_count_efficient>0 & total_count_inefficient > 0, solver==1)
  #select final 10% of solves from each solver
  df_end = df_solves %>% ungroup() %>% arrange(full_timestamp) %>% group_by(population,ID) %>% slice_tail(prop = .1)
  #determine if this 10% is majority efficient or inefficient
  df_end_ID = df_end %>% group_by(year,population,age,sex,ID,Event) %>% summarize(count = n()) %>% pivot_wider(names_from = Event, values_from=count, values_fill=0) %>% mutate(switched=if_else(efficient>inefficient,1,0))
  
  #create dataframe of when individuals first produced efficient solution
  df_temp = df_solves %>% group_by(population,ID) %>% filter(Event=="efficient",solver==1) %>% slice_head(n=1) %>% select(solve_day_count,exp_day_count)
  df_end_ID = left_join(df_end_ID,df_temp) %>% mutate(solve_day_count = if_else(is.na(solve_day_count),0,solve_day_count)) %>% select(-c(efficient,inefficient))
  
  #create dataframe of populations with count of solutions by type each day
  df_pops = df_solves %>% group_by(population,exp_day_count,Event) %>% summarize(count = n()) %>% pivot_wider(names_from = Event, values_from=count, names_prefix="pop_", values_fill=0)
  df_end_ID = left_join(df_end_ID, df_pops)
  
  #create dataframe of individuals with count of solutions by type each day
  df_ind = df_solves %>% group_by(population,ID,exp_day_count,Event) %>% summarize(count = n()) %>% pivot_wider(names_from = Event, values_from=count, names_prefix="ind_", values_fill=0)
  df_end_ID = left_join(df_end_ID, df_ind)
  
  #subtract 2 values to get count of socially observed solutions on first day of producing efficient solution
  df_end_ID = df_end_ID %>% mutate(soc_efficient = pop_efficient-ind_efficient, soc_inefficient= pop_inefficient-ind_inefficient)
  
  #create proportion
  df_end_ID = df_end_ID %>% mutate(soc_prop_inefficient = soc_inefficient/(soc_inefficient+soc_efficient))
  df_end_ID = df_end_ID %>% mutate(soc_prop_inefficient = if_else(is.nan(soc_prop_inefficient), 0, soc_prop_inefficient),failed_switch = if_else(switched==1,0,1))
  df_end_ID = droplevels(df_end_ID)
  summary(df_end_ID)
  
  #model what predicts failure to switch
  m1 = glmer(failed_switch ~ solve_day_count + soc_prop_inefficient + (1|year/population), data=df_end_ID, family=binomial(link = "logit"))
  summary(m1)
  stargazer(m1)
  m2 = glmer(failed_switch ~ age + sex + solve_day_count + soc_prop_inefficient + (1|year/population), data=df_end_ID, family=binomial(link = "logit"))
  summary(m2)
  
  anova(m1,m2) #select m1
  
  #check how many innovators switched
  load("../data/df_solves.Rda")
  df_innovators = df_solves %>% group_by(population,ID) %>% summarize(innovator=sum(innovation)) %>% filter(innovator==1)
  df_switched = df_end_ID %>% filter(switched==1)
  df_innovators %>% filter(ID %in% df_switched$ID) #only 5 switched
}

# Results E: estimates conditional probability of learning from experimental data
{
  #the following code estimates the condition probability of learning from latency to learn data from the experiment
  #this was then fed into the agent based model as is, and also reversed (Fig. S5)
  library(rms)
  
  load("../data/df_surv.Rda")
  summary(df)
  df = df_surv
  #cox-ph estimates hazard ratios without estimating a baseline hazard function, semiparametric. to get hazard function, we need full parametric
  dd = datadist(df)
  options(datadist='dd')
  fit.parametric = psm(Surv(latency_to_solve + 1, censor) ~ 1, data=df, dist="lognormal")
  fit.parametric2 = psm(Surv(latency_to_solve + 1, censor) ~ 1, data=df, dist="weibull")
  fit.parametric$loglik
  fit.parametric2$loglik
  #lognormal has higher loglik, go with that
  estimates = survest(fit.parametric, what="survival", times = c(1:40))
  learn_probs = cbind(estimates$time, estimates$surv)
  learn_probs = as.data.frame(learn_probs)
  #calculate conditional probability of failure
  learn_probs = learn_probs %>% mutate(cond_prob_fail = (V2 - lead(V2)) / V2)
  print(learn_probs)
}
