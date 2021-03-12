library(tidyverse)
library(ggpubr)
library(ggstance)
library(magick)
library(grid)
library(survival)
library(survminer)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Fig. S1, survival analysis of differences between learners ####
load(file="../data/df_surv.Rda")
df_surv = df_surv %>% mutate(age=ifelse(age==5,"juv.","ad"))

#check for differences between age, sex
fit = survfit(Surv(latency_to_solve, censor) ~ age + sex + 1, data=df_surv)
ggsurvplot(fit, data = df_surv, pval = TRUE, conf.int = T)
ggsave("../images/FigS3.png",width=10,height=10,units="cm",scale=2)

fit = survfit(Surv(latency_to_solve, censor) ~ condition + 1, data=df)
summary(fit)
ggsurvplot(fit, data = df, pval = TRUE, conf.int = T)
ggsave("../images/condition_survplot.png",width=10,height=10,units="cm",scale=1)

#Fig. S2, overview of all populations ####
load("../data/df_solves.Rda")

df = df_solves %>% filter(solver==1) %>% mutate(innovation=ifelse(is.na(innovation),0,innovation), exp_day_count=exp_day_count+5)
#number of solutions
nrow(df)
#number of birds
length(unique(df$ID))
df = df %>% ungroup() %>%
  group_by(condition,population,date_in_aviary,tutor,ID,Event,exp_day_count) %>%
  summarise(freq = n(),innov = as.factor(sum(innovation)))
df$Event = as.factor(df$Event)
df$Event = relevel(df$Event, "inefficient")
df$ID = as.factor(df$ID)
df = df %>% ungroup() %>% mutate(ID = fct_reorder(ID, desc(date_in_aviary)))
df1 = df %>% filter(Event %in% c("efficient","inefficient")) %>% mutate(population=as.factor(population))

solution_cols = c("#ff5959","#03039e")

#all plotted together
ggplot(df1,aes(x=exp_day_count,y=as.factor(ID)))+
  geom_rect(ymin=0, ymax=18, xmin=0, xmax=11.5, fill="gray", alpha=0.5) +
  facet_wrap(condition~population, scales="free",ncol=3)+
  scale_color_manual(values=rev(solution_cols),guide=F)+
  scale_size(name="Frequency",range=c(2,5))+
  geom_point(aes(color=Event,size=freq),position=position_dodgev(height=.8),alpha=0.9)+
  geom_point(data = df1 %>% subset(innov==1),shape="triangle",color="#e55050",size=5,alpha=1,position=position_nudge(x = 0, y = .2))+
  coord_cartesian(xlim=c(0,39))+
  #scale_y_discrete(limits = rev(levels(df1$population)))+
  labs(x="Experimental day",y="Bird ID")+
  geom_vline(data=subset(df1,condition=="turnover"), aes(xintercept = c(11.5)),linetype='dashed',size=1)+
  geom_vline(data=subset(df1,condition=="turnover"), aes(xintercept = c(18.5)),linetype='dashed',size=1)+
  geom_vline(data=subset(df1,condition=="turnover"), aes(xintercept = c(25.5)),linetype='dashed',size=1)+
  geom_vline(data=subset(df1,condition=="turnover"), aes(xintercept = c(32.5)),linetype='dashed',size=1)+
  theme_bw()+
  theme(text=element_text(size=16))
ggsave("../images/SI_pop_overview.png",width=17.4,height=20, units="cm",limitsize = FALSE, scale=2)

#Fig. S3, innovation timing between conditions ####
load("../data/df_solves.Rda")
df_innov=df_solves %>% filter(innovation==1) %>% ungroup() %>% mutate(exp_day_count=exp_day_count+5)
df_innov$condition = relevel(df_innov$condition,"turnover")
colors= c("#fca311","#14213d")

static = df_innov %>% filter(condition=="static") %>% select(ind_day_count)
turnover = df_innov %>% filter(condition=="turnover") %>% select(ind_day_count)

p1 = ggplot(df_innov,aes(y=ind_day_count,x=condition))+
  geom_jitter(height = 0,aes(color=condition), show.legend = F)+
  geom_boxplot(aes(color=condition,fill=condition),alpha=0.5, outlier.alpha = 0, show.legend = FALSE)+
  scale_color_manual(values=colors)+
  scale_fill_manual(values=colors)+
  labs(y="Days of exposure before innovation",x="Condition")+
  coord_cartesian(ylim=c(10,24))+
  theme_bw()

p2 = ggplot(df_innov,aes(y=solve_day_count,x=condition))+
  geom_jitter(height = 0, aes(color=condition), show.legend = F)+
  geom_boxplot(aes(color=condition,fill=condition),alpha=0.5, outlier.alpha = 0, show.legend = FALSE)+
  scale_color_manual(values=colors)+
  scale_fill_manual(values=colors)+
  labs(y="Days solving before innovation",x="Condition")+
  coord_cartesian(ylim=c(0,24))+
  theme_bw()

p3 = ggplot(df_innov,aes(y=exp_day_count,x=condition))+
  geom_jitter(height = 0, aes(color=condition), show.legend = F)+
  geom_boxplot(aes(color=condition,fill=condition),alpha=0.5, outlier.alpha = 0, show.legend = FALSE)+
  scale_color_manual(values=colors)+
  scale_fill_manual(values=colors)+
  labs(y="Experimental days",x="Condition")+
  coord_cartesian(ylim=c(10,23))+
  theme_bw()

ggarrange(p1,p2,p3,labels="AUTO",ncol=3)
ggsave("../images/SI_innovation_multipanel.png",width=17.8,height=6, units="cm",limitsize = FALSE, scale=1.5)
