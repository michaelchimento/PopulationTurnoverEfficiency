library(tidyverse)
library(ggpubr)
library(ggstance)
library(magick)
library(grid)
library(zoo)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Fig 1 ####
pinpoint = image_read("../images/pinpoint_example_puzzle.jpg")
pinpoint = image_convert(pinpoint,"png")
pinpoint = rasterGrob(pinpoint,height=1)
puzzle = image_read("../images/puzzle_img.png")
puzzle = rasterGrob(puzzle,height=1)

load("../data/df_solves.Rda")

df = df_solves %>% filter(solver==1, tutor==0, solve_speed < 60) %>% mutate(scaled_solveindex = scale(ind_solve_count_bytype)) %>% filter(!is.na(scaled_solveindex))
#summary
df %>% group_by(Event) %>% summarise(length(unique(ID)))
df %>% group_by(Event) %>% summarise(length(Event))
df %>% group_by(Event) %>% summarise(mean(solve_speed))

#make plot
solution_colors = c("#ff5959","#03039e")

p_speed = ggplot(df,aes(x=scaled_solveindex,y=solve_speed))+
  stat_summary_bin(aes(color=Event),bins=5,size=1,show.legend = F,breaks=c(-2.5,-1.5,-.5,.5,1.5,2.5))+
  stat_summary_bin(aes(color=Event),bins=5,size=1,show.legend = F,breaks=c(-2.5,-1.5,-.5,.5,1.5,2.5),geom="errorbar")+
  labs(y = "Time to solve (seconds)", x = "Scaled experience")+
  scale_color_manual(values=solution_colors,guide=F)+
  scale_y_continuous(position = "right")+
  coord_cartesian(ylim = c(0, 3))+
  theme_bw()+
  theme(legend.position = "top",text = element_text(size = 15))

ggarrange(puzzle,pinpoint,p_speed,labels = c("A","B","C"),
          font.label = list(color="black"),ncol=3,widths=c(1.2,1.4,.8),hjust=c(-.5,-.5,1.5))
ggsave("../images/fig1.tiff",width=17.4, height = 5,limitsize = TRUE, scale=2, units="cm")

# Fig 2 ####
load("../data/df_solves.Rda")
df = df_solves %>% mutate(innovation=ifelse(is.na(innovation),0,innovation),exp_day_count = exp_day_count+5)
df = df %>% ungroup() %>% filter(solver==1) %>%
  group_by(condition,population,date_in_aviary,ID,Event,exp_day_count) %>%
  summarise(freq = n(),innov = as.factor(sum(innovation)))
df$Event = as.factor(df$Event)
df$Event = relevel(df$Event, "inefficient")
df$ID = as.factor(df$ID)
df = df %>% ungroup() %>% mutate(ID = fct_reorder(ID, desc(date_in_aviary)))
df1 = df %>% filter(Event %in% c("efficient","inefficient")) #%>% mutate(population=as.factor(population))
solution_cols = c("#ff5959","#03039e")


df2 = df1 %>% filter(condition=="static")
#summary
df2 %>% summarise(length(unique(ID)))
df2 %>% summarise(sum(freq))
p1 = ggplot(df2 %>% filter(condition=="static"),aes(x=exp_day_count,y=as.factor(population)))+
  scale_color_manual(values=rev(solution_cols),guide=F)+
  scale_size(name="Daily solution frequency",range=c(2,5))+
  geom_rect(ymin=0, ymax=18, xmin=0, xmax=11.5,fill="grey", alpha=.1, show.legend=F) +
  geom_point(data = df2 %>% subset(innov==0),aes(color=Event,size=freq),position=position_jitter(w = 0, h = 0.3),alpha=0.4)+
  geom_point(data = df2 %>% subset(innov==1),shape="triangle",color="#e55050",size=5,alpha=1)+
  coord_cartesian(xlim=c(0,39))+
  scale_y_discrete(limits = levels(as.factor(df2$population)),labels=c(1,2,3,4,5,6,7,8,9))+
  labs(x="",y="Static population ID")+
  theme_bw()+
  theme(text=element_text(size=16))

df2 = df1 %>% filter(condition=="turnover")
#summary
df2 %>% summarise(length(unique(ID)))
df2 %>% summarise(sum(freq))
p2 = ggplot(df2 %>% filter(condition=="turnover"),aes(x=exp_day_count,y=as.factor(population)))+
  scale_color_manual(values=rev(solution_cols),guide=F)+
  scale_size(name="Daily solution frequency",range=c(2,5))+
  geom_rect(ymin=0, ymax=18, xmin=0, xmax=11.5, fill="gray", alpha=0.5) +
  geom_point(data = df2 %>% subset(innov==0),aes(color=Event,size=freq),position=position_jitter(w = 0, h = 0.3),alpha=0.4)+
  geom_point(data = df2 %>% subset(innov==1),shape="triangle",color="#e55050",size=5,alpha=1)+
  coord_cartesian(xlim=c(0,39))+
  scale_y_discrete(limits = levels(df2$population),labels=c(10,11,12,13,14,15,16,17,18))+
  labs(x="",y="Turnover population ID")+
  geom_vline(xintercept = c(11.5,18.5,25.5,32.5),linetype='dashed',size=1)+
  theme_bw()+
  theme(text=element_text(size=16))


load("../data/df_solves.Rda")

#reference dataset to see where grey lines go for chart
#df_datein = df_solves %>% group_by(ID) %>% select(ID,date_in_aviary,duration_of_stay,year) %>% slice(head=1)
#df_datein %>% filter(ID=="DA729")

df = df_solves %>% mutate(innovation=ifelse(is.na(innovation),0,innovation),exp_day_count = exp_day_count+5)
df = df %>% filter(solver==1) %>% ungroup() %>%
  group_by(condition,population,date_in_aviary,tutor,ID,Event,exp_day_count) %>%
  summarise(freq = n(),innov = as.factor(sum(innovation)))
df$Event = as.factor(df$Event)
df$Event = relevel(df$Event, "inefficient")
df$ID = as.factor(df$ID)
df = df %>% ungroup() %>% mutate(ID = fct_reorder(ID, desc(date_in_aviary)))
df1 = df %>% filter(Event %in% c("efficient","inefficient")) %>% mutate(population=as.factor(population))

solution_cols = c("#ff5959","#03039e")

#summary
df2 = df1 %>% filter(population==6)
df2 %>% summarise(length(unique(ID)))
df2 %>% summarise(sum(freq))

df_tutorcolor = df2 %>% group_by(ID) %>% slice(head=1)
color_vector <- ifelse(df_tutorcolor$tutor == 1, "gold", "black")
p3 = ggplot(df2,aes(x=exp_day_count,y=as.factor(ID)))+
  geom_rect(ymin=0, ymax=18, xmin=0, xmax=11.5, fill="gray", alpha=0.5) +
  geom_segment(y="E304C",yend="E304C",x=0,xend=11.5,color="black",size=1)+
  geom_segment(y="E2733",yend="E2733",x=0,xend=18.5,color="black",size=1)+
  geom_segment(y="E11CB",yend="E11CB",x=0,xend=25.5,color="black",size=1)+
  geom_segment(y="DDADC",yend="DDADC",x=0,xend=32.5,color="black",size=1)+
  geom_segment(y="DBAC1",yend="DBAC1",x=0,xend=18.5,color="black",size=1)+
  geom_segment(y="E02D1",yend="E02D1",x=11.5,xend=32.5,color="black",size=1)+
  geom_segment(y="DA729",yend="DA729",x=11.5,xend=25.5,color="black",size=1)+
  geom_segment(y="E2E79",yend="E2E79",x=18.5,xend=39,color="black",size=1)+
  geom_segment(y="D9E6A",yend="D9E6A",x=18.5,xend=39,color="black",size=1)+
  geom_segment(y="DE797",yend="DE797",x=25.5,xend=39,color="black",size=1)+
  geom_segment(y="DDB02",yend="DDB02",x=25.5,xend=39,color="black",size=1)+
  geom_segment(y="DA99E",yend="DA99E",x=32.5,xend=39,color="black",size=1)+
  scale_color_manual(values=rev(solution_cols),guide=F)+
  scale_size(name="Daily solution frequency",range=c(2,5),guide=F)+
  geom_point(aes(color=Event,size=freq),position=position_dodgev(height=1),alpha=0.9)+
  geom_point(data = df2 %>% subset(innov==1),shape="triangle",color="#e55050",size=5,alpha=1,position=position_nudge(x = 0, y = .275))+
  coord_cartesian(xlim=c(0,39))+
  scale_y_discrete(labels=c("m","l","k","j","i","h","g","f","e"))+
  labs(x="Experimental day",y="Bird ID")+
  geom_vline(xintercept = c(11.5,18.5,25.5,32.5),linetype='dashed',size=1)+
  theme_bw()+
  theme(text=element_text(size=16), axis.text.y=element_text(hjust = 1,colour = color_vector))

df2 = df1 %>% filter(population==8)
df2 %>% summarise(length(unique(ID))) #birds
df2 %>% summarise(sum(freq)) #29801 solutions
df_tutorcolor = df2 %>% group_by(ID) %>% slice(head=1)
color_vector <- ifelse(df_tutorcolor$tutor == 1, "gold", "black")
p4 = ggplot(df2,aes(x=exp_day_count,y=as.factor(ID)))+
  geom_rect(ymin=0, ymax=18, xmin=0, xmax=11.5, fill="gray", alpha=0.5) +
  geom_segment(y="E405A",yend="E405A",x=0,xend=39,color="black",size=1)+
  geom_segment(y="E3FBE",yend="E3FBE",x=0,xend=39,color="black",size=1)+
  geom_segment(y="DACDC",yend="DACDC",x=0,xend=39,color="black",size=1)+
  geom_segment(y="D9831",yend="D9831",x=0,xend=39,color="black",size=1)+
  scale_color_manual(values=rev(solution_cols),guide=F)+
  scale_size(name="Daily solution frequency",range=c(2,5),guide=F)+
  geom_point(aes(color=Event,size=freq),position=position_dodgev(height=.5),alpha=0.9)+
  geom_point(data = df2 %>% subset(innov==1),shape="triangle",color="#e55050",size=5,alpha=1,position=position_nudge(x = 0, y = .15))+
  coord_cartesian(xlim=c(0,39))+
  scale_y_discrete(labels=c("d","c","b","a"))+
  labs(x="Experimental day",y="Bird ID")+
  theme_bw()+
  theme(text=element_text(size=16), axis.text.y=element_text(hjust = 1,colour = color_vector))

ggarrange(p1,p2,p4,p3,labels=c("A","B","C","D"),common.legend = T,heights=c(1.75,1))
ggsave("../images/fig2.tiff",height=12,width=17.4,units="cm",scale=2)


# Fig 3 ####

load("../data/df_solves.Rda")
condition_colors= c("#14213d","#fca311")
solution_cols = c("#ff5959","#03039e")

df = df_solves %>% filter(exp_day_count <=34, solver==1, population!=13) %>% mutate(exp_day_count = exp_day_count+5)

#get number of birds included in each category
df %>% group_by(condition) %>% distinct(ID,condition) %>% summarise(n())

p1 = ggplot(data = df) +
  geom_bar(position="fill",aes(x=exp_day_count, y = (..count..), fill=Event),width=1,alpha=0.9,show.legend = T)+
  facet_wrap(~condition, ncol=2)+
  geom_vline(data = subset(df,condition=="turnover"), aes(xintercept = c(11.5)), linetype='dashed',size=1)+
  geom_vline(data = subset(df,condition=="turnover"), aes(xintercept = c(18.5)), linetype='dashed',size=1)+
  geom_vline(data = subset(df,condition=="turnover"), aes(xintercept = c(25.5)), linetype='dashed',size=1)+
  geom_vline(data = subset(df,condition=="turnover"), aes(xintercept = c(32.5)), linetype='dashed',size=1)+
  geom_text(data = subset(df,condition=="turnover"),check_overlap = TRUE, angle=90, vjust = 1.5, size=8,color="#f2de27", aes(x=11.5, y=.8, label="T1"))+
  geom_text(data = subset(df,condition=="turnover"),check_overlap = TRUE, angle=90, vjust = 1.5, size=8,color="#f2de27", aes(x=18.5, y=.8, label="T2"))+
  geom_text(data = subset(df,condition=="turnover"),check_overlap = TRUE, angle=90, vjust = 1.5, size=8,color="#f2de27", aes(x=25.5, y=.8, label="T3"))+
  geom_text(data = subset(df,condition=="turnover"),check_overlap = TRUE, angle=90, vjust = 1.5, size=8,color="#f2de27", aes(x=32.5, y=.8, label="T4"))+
  labs(y = "Proportion of solutions", x = "Experimental day")+
  scale_fill_manual(values = solution_cols, name="solution")+
  theme_bw()+
  theme(text = element_text(size = 16))
#get num of solutions per condition
summary(p1$data$condition)

#mean time to solve over time
df = df_solves %>%
  ungroup() %>%
  filter(solver==1,tutor==0,!is.na(solve_speed)) %>% mutate(exp_day_count = exp_day_count+5)

df_test = df %>% group_by(condition,exp_day_count) %>% summarise(mean = mean(solve_speed)) %>% group_by(condition) %>% mutate(rollmean = rollapply(mean, width = 7, FUN = mean, align = "right", partial = TRUE))

#get number of birds included in each category
df %>% group_by(condition) %>% distinct(ID,condition) %>% summarise(n())

p2 = ggplot(data=df, aes(x=exp_day_count,y=solve_speed))+
  #facet_wrap(~condition)+
  #stat_summary_bin(aes(color=condition,group=ID), alpha= 0.4, fun.data="mean_cl_boot", geom="line")+
  #scale_color_manual(values=solution_colors)+
  geom_line(data=df_test, aes(y=rollmean,color=condition),size=1.5) +
  stat_summary(aes(color=condition,group=condition), geom="line")+
  stat_summary(aes(color=condition,group=condition),geom="point")+
  stat_summary(aes(color=condition,group=condition),geom="errorbar")+
  scale_color_manual(values=c(condition_colors))+
  geom_vline(xintercept=c(11.5,18.5,25.5,32.5), linetype='dashed',size=1)+
  geom_text(check_overlap = TRUE, angle=90, vjust = 1.5, size=8, aes(x=11.5, y=7, label="T1"))+
  geom_text(check_overlap = TRUE, angle=90, vjust = 1.5, size=8, aes(x=18.5, y=7, label="T2"))+
  geom_text(check_overlap = TRUE, angle=90, vjust = 1.5, size=8, aes(x=25.5, y=7, label="T3"))+
  geom_text(check_overlap = TRUE, angle=90, vjust = 1.5, size=8, aes(x=32.5, y=7, label="T4"))+
  #new_scale_color() +
  #stat_summary_bin(aes(color=Event,shape=condition),geom="point",alpha=0.9)+
  #scale_color_manual(values=c(solution_colors),name="solution")+
  labs(y="Time-to-solve (seconds)", x="Experimental day")+
  #facet_wrap(condition~population)+
  coord_trans(y = "log2")+
  scale_y_continuous(breaks = round(seq(1, 10, by = 2),1))+
  theme_bw()+
  theme(text = element_text(size = 16))
ggarrange(p1,p2,labels = c("A","B"),ncol=1,heights = c(.4,.6))
ggsave("../images/fig3.tiff", width=11.4, height = 9,limitsize = FALSE, scale=2.5, units = "cm")


