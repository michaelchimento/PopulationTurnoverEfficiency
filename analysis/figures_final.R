library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Fig 1C
{
  load("../data/df_solves.Rda")
  
  #subset solutions produced by each solver into first quartile (inexperienced) and final 3 quartiles (experienced)
  df_inex = df_solves %>% filter(solver==1, tutor==0, solve_speed < 60) %>% 
    group_by(population,ID,Event) %>% arrange(full_timestamp) %>% 
    slice_head(prop=0.25) %>% mutate(level="inexperienced")
  df_ex = df_solves %>% filter(solver==1, tutor==0, solve_speed < 60) %>% 
    group_by(population,ID,Event) %>% arrange(full_timestamp) %>% 
    slice_tail(prop=0.75) %>% mutate(level="experienced")
  
  df = rbind(df_inex,df_ex)
  
  #summary
  df %>% group_by(Event) %>% summarize(length(unique(ID)))
  df %>% group_by(Event) %>% summarize(length(Event))
  df %>% group_by(Event) %>% summarize(mean(solve_speed))
  
  #wilcox tests within and between solutions
  wilcox.test(formula=solve_speed~level,data=df %>% filter(Event=="efficient"),alternative="l")
  wilcox.test(formula=solve_speed~level,data=df %>% filter(Event=="inefficient"),alternative="l")
  wilcox.test(formula=solve_speed~Event,data=df,alternative="l")
  
  #make plot
  solution_colors = c("#ff5959","#03039e")
  df$level = relevel(as.factor(df$level),"inexperienced")
  p_speed = ggplot(df, aes(x=Event, y=solve_speed))+
    stat_summary(fun.data="mean_cl_boot",aes(color=Event,shape=as.factor(level)),size=1)+
    stat_summary(fun.data = "mean_cl_boot",aes(color=Event,group=as.factor(level)), geom = "errorbar")+
    labs(y = "Time to solve (seconds)", x = "Solution type")+
    scale_shape_manual(values = c(16, 21),name="")+
    scale_color_manual(values=solution_colors,guide=F)+
    coord_cartesian(ylim=c(0,3))+
    scale_y_continuous(position = "right")+
    theme_bw()+
    theme(legend.position = "top",text = element_text(size = 15))
  p_speed
}

#Fig 2 A,B,C
{
  library(ggarrange)
  load("../data/df_solves.Rda")
  df = df_solves %>% filter(solver==1,solve_speed<60)
  solution_cols = c("#ff5959","#03039e")
  #population 6 for a good example for switching
  p1 = ggplot(subset(df,population==6), aes(x=exp_day_count, y = solve_speed))+
    stat_summary(fun.data="mean_cl_boot", aes(color=Event), show.legend = FALSE)+
    stat_summary(fun.data="mean_cl_boot", aes(color=Event), geom="line", show.legend = FALSE)+
    geom_vline(xintercept = c(6.5,13.5,20.5,27.5),linetype='dashed',size=1)+
    geom_text(check_overlap = TRUE, angle=90, vjust = 1.5, size=6, aes(x=6.5, y=.8, label="T1"))+
    geom_text(check_overlap = TRUE, angle=90, vjust = 1.5, size=6, aes(x=13.5, y=.8, label="T2"))+
    geom_text(check_overlap = TRUE, angle=90, vjust = 1.5, size=6, aes(x=20.5, y=.8, label="T3"))+
    geom_text(check_overlap = TRUE, angle=90, vjust = 1.5, size=6, aes(x=27.5, y=.8, label="T4"))+
    labs(y = "Time to solve (s)",x="",title = "Population 6")+
    scale_color_manual(values=solution_cols)+
    #coord_trans(y = "log2")+
    coord_cartesian(xlim=c(-5,34),ylim=c(0,8))+
    theme_bw()+
    theme(axis.title.y = element_text(size=16))
  
  #population 11 for refinement
  p2 = ggplot(df %>% filter(population==11), aes(x=exp_day_count))+
    stat_summary(fun.data="mean_cl_boot", aes(y = solve_speed,color=Event), show.legend = FALSE)+
    stat_summary(fun.data="mean_cl_boot", aes(y= solve_speed,color=Event), geom="line", show.legend = FALSE)+
    #geom_vline(xintercept = c(6.5,13.5,20.5,27.5),linetype='dashed',size=1)+
    labs(y = "", x = "", title = "Population 11")+
    scale_color_manual(values=rev(solution_cols))+
    coord_cartesian(xlim=c(-5,34))+
    #coord_trans(y = "log2")+
    theme_bw()
  
  #population 17 for extinction
  p3 = ggplot(df %>% filter(population==17),aes(x=exp_day_count, y = solve_speed))+
    stat_summary(fun.data="mean_cl_boot", aes(color=Event), show.legend = FALSE)+
    stat_summary(fun.data="mean_cl_boot", aes(color=Event), geom="line", show.legend = FALSE)+
    geom_vline(xintercept = c(6.5,13.5,20.5,27.5),linetype='dashed',size=1)+
    labs(y = "", x = "", title = "Population 17")+
    scale_color_manual(values=rev(solution_cols))+
    coord_cartesian(xlim=c(-5,34))+
    #coord_trans(y = "sqrt")+
    theme_bw()
  
  ggarrange(p1,p2,p3,labels=c("A","B","C"),nrow=1)
}

#Fig 2 D
{
  load("../data/df_solves.Rda")

  df = df_solves %>% mutate(innovation=ifelse(is.na(innovation),0,innovation))
  
  df = df %>% ungroup() %>% 
    group_by(condition,population,date_in_aviary,ID,Event,exp_day_count) %>%
    summarise(freq = n(),innov = as.factor(sum(innovation)))
  df$Event = as.factor(df$Event)
  df$Event = relevel(df$Event, "inefficient")
  df$ID = as.factor(df$ID)
  df = df %>% ungroup() %>% mutate(ID = fct_reorder(ID, desc(date_in_aviary)))
  df1 = df %>% filter(population %in% c(6,11,17), Event %in% c("efficient","inefficient")) %>% mutate(population=as.factor(population))
  
  solution_cols = c("#ff5959","#03039e")
  
  #summary
  df1 %>% summarize(length(unique(ID))) #20 birds
  df1 %>% summarize(sum(freq)) #29801 solutions
  
  
  p_4 = ggplot(df1,aes(x=exp_day_count,y=population))+
    scale_color_manual(values=rev(solution_cols),guide=F)+
    scale_size(name="Frequency",range=c(2,5))+
    geom_point(data = df1 %>% subset(innov==0),aes(color=Event,size=freq),position=position_jitter(w = 0, h = 0.3),alpha=0.5)+
    geom_point(data = df1 %>% subset(innov==1),shape="triangle",color="#ff5959",size=5,position=position_jitter(w = 0, h = 0.2),alpha=1)+
    coord_cartesian(xlim=c(-5,34))+
    scale_y_discrete(limits = rev(levels(df1$population)))+
    labs(x="Experimental day",y="Population")+
    geom_vline(xintercept = c(6.5,13.5,20.5,27.5),linetype='dashed',size=1)+
    theme_bw()+
    theme(text=element_text(size=16),legend.position = c(.9,.2))
  p_4
}

#Fig 3
{
  load("../data/df_solves.Rda")
  condition_colors= c("#14213d","#fca311")
  solution_cols = c("#ff5959","#03039e")
  
  df = df_solves %>% filter(exp_day_count <=34, solver==1, population!=13)
  
  #get number of birds included in each category
  df %>% group_by(condition) %>% distinct(ID,condition) %>% summarize(n())

  p1 = ggplot(data = df) +
    geom_bar(position="fill",aes(x=exp_day_count, y = (..count..), fill=Event),width=1,alpha=0.9,show.legend = T)+
    facet_wrap(~condition, ncol=2)+
    geom_vline(data = subset(df,condition=="turnover"), aes(xintercept = c(6.5)), linetype='dashed',size=1)+
    geom_vline(data = subset(df,condition=="turnover"), aes(xintercept = c(13.5)), linetype='dashed',size=1)+
    geom_vline(data = subset(df,condition=="turnover"), aes(xintercept = c(20.5)), linetype='dashed',size=1)+
    geom_vline(data = subset(df,condition=="turnover"), aes(xintercept = c(27.5)), linetype='dashed',size=1)+
    geom_text(data = subset(df,condition=="turnover"),check_overlap = TRUE, angle=90, vjust = 1.5, size=8,color="#f2de27", aes(x=6.5, y=.8, label="T1"))+
    geom_text(data = subset(df,condition=="turnover"),check_overlap = TRUE, angle=90, vjust = 1.5, size=8,color="#f2de27", aes(x=13.5, y=.8, label="T2"))+
    geom_text(data = subset(df,condition=="turnover"),check_overlap = TRUE, angle=90, vjust = 1.5, size=8,color="#f2de27", aes(x=20.5, y=.8, label="T3"))+
    geom_text(data = subset(df,condition=="turnover"),check_overlap = TRUE, angle=90, vjust = 1.5, size=8,color="#f2de27", aes(x=27.5, y=.8, label="T4"))+
    labs(y = "Proportion of solutions", x = "Experimental day")+
    scale_fill_manual(values = solution_cols, name="solution")+
    theme_bw()+
    theme(text = element_text(size = 16))
  #get num of solutions per condition
  summary(p1$data$condition)

  #mean time to solve over time
  df = df_solves %>%
    ungroup() %>%
    filter(solver==1,tutor==0,!is.na(solve_speed))
  
  #get number of birds included in each category
  df %>% group_by(condition) %>% distinct(ID,condition) %>% summarize(n())
  
  p2 = ggplot(data=df, aes(x=exp_day_count,y=solve_speed))+
    #facet_wrap(~condition)+
    #stat_summary_bin(aes(color=condition,group=ID), alpha= 0.4, fun.data="mean_cl_boot", geom="line")+
    #scale_color_manual(values=solution_colors)+
    stat_summary(aes(color=condition,group=condition),fun.data="mean_cl_boot", geom="line")+
    stat_summary(aes(color=condition,group=condition),fun.data="mean_cl_boot",geom="point")+
    stat_summary(aes(color=condition,group=condition),fun.data="mean_cl_boot",geom="errorbar")+
    scale_color_manual(values=c(condition_colors))+
    geom_vline(xintercept=c(6.5,13.5,20.5,27.5), linetype='dashed',size=1)+
    geom_text(check_overlap = TRUE, angle=90, vjust = 1.5, size=8, aes(x=6.5, y=8, label="T1"))+
    geom_text(check_overlap = TRUE, angle=90, vjust = 1.5, size=8, aes(x=13.5, y=8, label="T2"))+
    geom_text(check_overlap = TRUE, angle=90, vjust = 1.5, size=8, aes(x=20.5, y=8, label="T3"))+
    geom_text(check_overlap = TRUE, angle=90, vjust = 1.5, size=8, aes(x=27.5, y=8, label="T4"))+
    #new_scale_color() +
    #stat_summary_bin(aes(color=Event,shape=condition),geom="point",alpha=0.9)+
    #scale_color_manual(values=c(solution_colors),name="solution")+
    labs(y="Time-to-solve (seconds)", x="Experimental day")+
    #facet_wrap(condition~population)+
    coord_trans(y = "log2")+
    scale_y_continuous(breaks = round(seq(1, 10, by = 2),1))+
    theme_bw()+
    theme(text = element_text(size = 16))
  p2

  ggarrange(p1,p2,labels = c("A","B"),ncol=1)
}

#Fig 4
{
  load("../data/df_ABM.Rda")
  #exclude simulations in which behavior went completely extinct
  data = df_ABM[complete.cases(df_ABM),]
  condition_colors= c("#14213d","#fca311")
  
  p1 = ggplot(data, aes(x=s_i, y=prop_efficient))+
    #geom_point(aes(alpha=0.01), position = "jitter", show.legend = FALSE)+
    #stat_smooth(method="glm", formula = y ~ poly(x,3), aes(color=condition))+
    stat_summary(fun.data="mean_cl_boot",aes(color=condition))+
    stat_summary(fun.data="mean_cl_boot",aes(color=condition),geom="line")+
    labs(y = "prop. efficient solutions", x="social cue sensitivity")+
    theme_bw()+
    scale_color_manual(values=condition_colors)+
    ylim(0,1)+
    theme(text=element_text(size=16),axis.text.x = element_text(angle = 45, hjust = 1))
  
  p2 = ggplot(data, aes(x=g_i, y=prop_efficient))+
    #geom_point(aes(alpha=0.01), position = "jitter", )+
    #stat_smooth(method="glm", formula = y ~ poly(x,3), aes(color=condition),show.legend = FALSE)+
    stat_summary(fun.data="mean_cl_boot",aes(color=condition),show.legend = FALSE)+
    stat_summary(fun.data="mean_cl_boot",aes(color=condition),show.legend = FALSE,geom="line")+
    labs(y="",x="current payoff sensitivity")+
    scale_color_manual(values=condition_colors)+
    theme_bw()+
    ylim(0,1)+
    theme(text=element_text(size=16),axis.text.x = element_text(angle = 45, hjust = 1))
  
  p3 = ggplot(data, aes(x=inverse_temp, y=prop_efficient))+
    #geom_point(aes(alpha=0.01), position = "jitter", show.legend = FALSE)+
    #stat_smooth(method="glm", formula = y ~ poly(x,3), aes(color=condition), show.legend = FALSE)+
    stat_summary(fun.data="mean_cl_boot",aes(color=condition),show.legend = FALSE)+
    stat_summary(fun.data="mean_cl_boot",aes(color=condition),show.legend = FALSE,geom="line")+
    labs(y="",x="conservatism")+
    theme_bw()+
    scale_color_manual(values=condition_colors)+
    ylim(0,1)+
    theme(text=element_text(size=16),axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggarrange(p1,p2,p3,nrow = 1, ncol=3, legend = "bottom", common.legend = TRUE, labels="AUTO")
}
