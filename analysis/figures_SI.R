library(tidyverse)
library(ggpubr)
library(ggstance)
library(plotly)
library(magick)
library(grid)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#SI fig 1, overview of all populations ####
load("../data/df_solves.Rda")

df = df_solves %>% mutate(innovation=ifelse(is.na(innovation),0,innovation), exp_day_count=exp_day_count+5)
df = df %>% filter(solver==1) %>% ungroup() %>%
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
  facet_wrap(condition~population, scales="free",ncol=6)+
  scale_color_manual(values=rev(solution_cols),guide=F)+
  scale_size(name="Frequency",range=c(2,5))+
  geom_point(aes(color=Event,size=freq),position=position_dodgev(height=.8),alpha=0.9)+
  geom_point(data = df1 %>% subset(innov==1),shape="asterisk",color="#ff5959",size=5,alpha=1,position=position_nudge(x = 0, y = .2))+
  coord_cartesian(xlim=c(0,39))+
  #scale_y_discrete(limits = rev(levels(df1$population)))+
  labs(x="Experimental day",y="Bird ID")+
  geom_vline(data=subset(df1,condition=="turnover"), aes(xintercept = c(11.5)),linetype='dashed',size=1)+
  geom_vline(data=subset(df1,condition=="turnover"), aes(xintercept = c(18.5)),linetype='dashed',size=1)+
  geom_vline(data=subset(df1,condition=="turnover"), aes(xintercept = c(25.5)),linetype='dashed',size=1)+
  geom_vline(data=subset(df1,condition=="turnover"), aes(xintercept = c(32.5)),linetype='dashed',size=1)+
  theme_bw()+
  theme(text=element_text(size=16))
ggsave("../images/SI_pop_overview.png",width=17.8,height=12, units="cm",limitsize = FALSE, scale=2)

#SI fig 2, innovation timing ####
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

#SI Fig 3: ABM positive learning function ####
load("../data/df_ABM_positive.Rda")
data = df_ABM_positive %>% filter(conformity==5,timestep==34)
data %>% group_by(condition) %>% summarise(n())

#colors and color ramp for 3d plots
solution_cols = c("#03039e","white","#ff5959")
condition_colors= c("#14213d","#fca311")
f <- colorRamp(solution_cols)

data_summary = data %>%
  group_by(condition,conformity,s_i,g_i,inverse_temp) %>%
  summarise(mean_proportion_efficient=mean(prop_efficient)) %>%
  mutate(colors=rgb(f(mean_proportion_efficient)/255))

#create A,B 3d plots

p1=plot_ly(data_summary %>% filter(condition=="static",conformity==5), x = ~s_i, y = ~g_i, z = ~inverse_temp, marker=list(size=15), color = ~mean_proportion_efficient, colors = solution_cols) %>%
  add_markers(opacity=0.9) %>%
  #colorbar(title="Selection for \nefficiency")%>%
  layout(scene = list(xaxis = list(title = 'Social cue bias',titlefont=list(size=18)),
                      yaxis = list(title = 'Current payoff bias',titlefont=list(size=15),nticks=5),
                      zaxis = list(title = 'Conservatism',titlefont=list(size=18)),
                      aspectratio=list(x=.5,y=.5,z=.5),
                      camera = list(eye = list(x = .2, y = 1, z = 0)))) %>% hide_colorbar()
p2=plot_ly(data_summary %>% filter(condition=="turnover",conformity==5), x = ~s_i, y = ~g_i, z = ~inverse_temp, marker=list(size=15), color = ~mean_proportion_efficient, colors = solution_cols) %>%
  add_markers(opacity=0.9) %>%
  layout(scene = list(xaxis = list(title = 'Social cue bias',titlefont=list(size=18)),
                      yaxis = list(title = 'Current payoff bias',titlefont=list(size=15),nticks=5),
                      zaxis = list(title = 'Conservatism',titlefont=list(size=18)),
                      aspectratio=list(x=.5,y=.5,z=.5),
                      camera = list(eye = list(x = .2, y = 1, z = 0)))) %>% hide_colorbar()
#export images from interactive plotly graphs
orca(p1, file = "../images/static3d.png")
orca(p2, file = "../images/turnover3d.png")

#import and arrange them into top row
static = image_read("../images/static3d.png")
turnover = image_read("../images/turnover3d.png")
legend = image_read("../images/legend.png")

p1 = rasterGrob(static,height=1)
p2 = rasterGrob(turnover,height=1)
legend = rasterGrob(legend,height=1)
g1 = ggarrange(p1,p2,legend,labels = c("A","B",""),widths = c(.4,.4,.2),
               font.label = list(color="black"),align="hv",ncol=3)



#plot C,D,E marginalized averages

p3 = ggplot(data, aes(x=s_i, y=prop_efficient))+
  geom_point(aes(color=condition),alpha=0.1,position = position_jitter(width = 0.02,height = 0.01))+
  stat_summary(aes(color=condition),show.legend = T)+
  stat_summary(aes(color=condition),show.legend = T,geom="line")+
  stat_summary(aes(color=condition),show.legend = T,geom="errorbar")+
  labs(y = "Selection for efficiency", x="Social cue bias")+
  theme_bw()+
  scale_color_manual(values=condition_colors)+
  ylim(0,1)+
  theme(text=element_text(size=16),axis.text.x = element_text(angle = 45, hjust = 1))

p4 = ggplot(data, aes(x=g_i, y=prop_efficient))+
  geom_point(aes(color=condition),alpha=0.1,position = position_jitter(width = 0.02,height = 0.01))+
  stat_summary(aes(color=condition),show.legend = FALSE)+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="line")+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="errorbar")+
  labs(y="",x="Current payoff bias")+
  scale_color_manual(values=condition_colors)+
  theme_bw()+
  ylim(0,1)+
  theme(text=element_text(size=16),axis.text.x = element_text(angle = 45, hjust = 1))

p5 = ggplot(data, aes(x=inverse_temp, y=prop_efficient))+
  geom_point(aes(color=condition),alpha=0.1,position = position_jitter(width = 0.1,height = 0.01))+
  stat_summary(aes(color=condition),show.legend = FALSE)+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="line")+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="errorbar")+
  labs(y="",x="Conservatism")+
  theme_bw()+
  scale_color_manual(values=condition_colors)+
  ylim(0,1)+
  theme(text=element_text(size=16),axis.text.x = element_text(angle = 45, hjust = 1))
g2= ggarrange(p3,p4,p5,ncol=3,labels=c("C","D","E"),font.label = list(color="black"),align="hv",common.legend = T,legend = "bottom")


#arrange two rows together
gall = ggarrange(g1,g2,nrow=2,ncol=1)
ggsave("../images/SI_positive_learnfunc.png",width = 17.8, height = 10, units = "cm", scale=2)


#SI Fig 4:ABM no/high conformity ####
load("../data/df_ABM.Rda")
data = df_ABM %>% filter(conformity==1,timestep==34)
data %>% group_by(condition) %>% summarise(n())

#colors and color ramp for 3d plots
solution_cols = c("#03039e","white","#ff5959")
condition_colors= c("#14213d","#fca311")
f <- colorRamp(solution_cols)

data_summary = data %>%
group_by(condition,conformity,s_i,g_i,inverse_temp) %>%
summarise(mean_proportion_efficient=mean(prop_efficient)) %>%
mutate(colors=rgb(f(mean_proportion_efficient)/255))

#create A,B 3d plots
p1=plot_ly(data_summary %>% filter(condition=="static"), x = ~s_i, y = ~g_i, z = ~inverse_temp, marker=list(size=15), color = ~mean_proportion_efficient, colors = solution_cols) %>%
  add_markers(opacity=0.9) %>%
  #colorbar(title="Selection for \nefficiency")%>%
  layout(scene = list(xaxis = list(title = 'Social cue bias',titlefont=list(size=18)),
                      yaxis = list(title = 'Current payoff bias',titlefont=list(size=15),nticks=5),
                      zaxis = list(title = 'Conservatism',titlefont=list(size=18)),
                      aspectratio=list(x=.5,y=.5,z=.5),
                      camera = list(eye = list(x = .2, y = 1, z = 0)))) %>% hide_colorbar()
p1
p2=plot_ly(data_summary %>% filter(condition=="turnover"), x = ~s_i, y = ~g_i, z = ~inverse_temp, marker=list(size=15), color = ~mean_proportion_efficient, colors = solution_cols) %>%
  add_markers(opacity=0.9) %>%
  layout(scene = list(xaxis = list(title = 'Social cue bias',titlefont=list(size=18)),
                      yaxis = list(title = 'Current payoff bias',titlefont=list(size=15),nticks=5),
                      zaxis = list(title = 'Conservatism',titlefont=list(size=18)),
                      aspectratio=list(x=.5,y=.5,z=.5),
                      camera = list(eye = list(x = .2, y = 1, z = 0)))) %>% hide_colorbar()
p2
#export images from interactive plotly graphs
orca(p1, file = "../images/static3d.png")
orca(p2, file = "../images/turnover3d.png")

#import and arrange them into top row
static = image_read("../images/static3d.png")
turnover = image_read("../images/turnover3d.png")
legend = image_read("../images/legend.png")

p1 = rasterGrob(static,height=1)
p2 = rasterGrob(turnover,height=1)
legend = rasterGrob(legend,height=1)
g1 = ggarrange(p1,p2,legend,labels = c("A - No conformity","B",""),widths = c(.4,.4,.2),
               font.label = list(color="black"),align="hv",ncol=3)


#plot C,D,E marginalized averages
p3 = ggplot(data, aes(x=s_i, y=prop_efficient))+
  geom_point(aes(color=condition),alpha=0.1,position = position_jitter(width = 0.02,height = 0.01))+
  stat_summary(aes(color=condition),show.legend = T)+
  stat_summary(aes(color=condition),show.legend = T,geom="line")+
  stat_summary(aes(color=condition),show.legend = T,geom="errorbar")+
  labs(y = "Selection for efficiency", x="Social cue bias")+
  theme_bw()+
  scale_color_manual(values=condition_colors)+
  ylim(0,1)+
  theme(text=element_text(size=15),axis.text.x = element_text(angle = 45, hjust = 1))
p3
p4 = ggplot(data, aes(x=g_i, y=prop_efficient))+
  geom_point(aes(color=condition),alpha=0.1,position = position_jitter(width = 0.02,height = 0.01))+
  stat_summary(aes(color=condition),show.legend = FALSE)+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="line")+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="errorbar")+
  labs(y="",x="Current payoff bias")+
  scale_color_manual(values=condition_colors)+
  theme_bw()+
  ylim(0,1)+
  theme(text=element_text(size=15),axis.text.x = element_text(angle = 45, hjust = 1))

p5 = ggplot(data, aes(x=inverse_temp, y=prop_efficient))+
  geom_point(aes(color=condition),alpha=0.1,position = position_jitter(width = 0.1,height = 0.01))+
  stat_summary(aes(color=condition),show.legend = FALSE)+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="line")+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="errorbar")+
  labs(y="",x="Conservatism")+
  theme_bw()+
  scale_color_manual(values=condition_colors)+
  ylim(0,1)+
  theme(text=element_text(size=15),axis.text.x = element_text(angle = 45, hjust = 1))
g2= ggarrange(p3,p4,p5,ncol=3,labels=c("C","D","E"),font.label = list(color="black"),align="hv",common.legend = T,legend = "bottom")

load("../data/df_ABM.Rda")
data = df_ABM %>% filter(conformity==10,timestep==34)
data %>% group_by(condition) %>% summarise(n())

#colors and color ramp for 3d plots
solution_cols = c("#03039e","white","#ff5959")
condition_colors= c("#14213d","#fca311")
f <- colorRamp(solution_cols)

data_summary = data %>%
group_by(condition,conformity,s_i,g_i,inverse_temp) %>%
summarise(mean_proportion_efficient=mean(prop_efficient)) %>%
mutate(colors=rgb(f(mean_proportion_efficient)/255))

#create A,B 3d plots
p1=plot_ly(data_summary %>% filter(condition=="static"), x = ~s_i, y = ~g_i, z = ~inverse_temp, marker=list(size=15), color = ~mean_proportion_efficient, colors = solution_cols) %>%
  add_markers(opacity=0.9) %>%
  #colorbar(title="Selection for \nefficiency")%>%
  layout(scene = list(xaxis = list(title = 'Social cue bias',titlefont=list(size=18)),
                      yaxis = list(title = 'Current payoff bias',titlefont=list(size=15),nticks=5),
                      zaxis = list(title = 'Conservatism',titlefont=list(size=18)),
                      aspectratio=list(x=.5,y=.5,z=.5),
                      camera = list(eye = list(x = .2, y = 1, z = 0)))) %>% hide_colorbar()
p1
p2=plot_ly(data_summary %>% filter(condition=="turnover"), x = ~s_i, y = ~g_i, z = ~inverse_temp, marker=list(size=15), color = ~mean_proportion_efficient, colors = solution_cols) %>%
  add_markers(opacity=0.9) %>%
  layout(scene = list(xaxis = list(title = 'Social cue bias',titlefont=list(size=18)),
                      yaxis = list(title = 'Current payoff bias',titlefont=list(size=15),nticks=5),
                      zaxis = list(title = 'Conservatism',titlefont=list(size=18)),
                      aspectratio=list(x=.5,y=.5,z=.5),
                      camera = list(eye = list(x = .2, y = 1, z = 0)))) %>% hide_colorbar()
p2
#export images from interactive plotly graphs
orca(p1, file = "../images/static3d.png")
orca(p2, file = "../images/turnover3d.png")

#import and arrange them into top row
static = image_read("../images/static3d.png")
turnover = image_read("../images/turnover3d.png")
legend = image_read("../images/legend.png")

p1 = rasterGrob(static,height=1)
p2 = rasterGrob(turnover,height=1)
legend = rasterGrob(legend,height=1)
g3 = ggarrange(p1,p2,legend,labels = c("F - High conformity","G",""),widths = c(.4,.4,.2),
               font.label = list(color="black"),align="hv",ncol=3)


#plot C,D,E marginalized averages
p3 = ggplot(data, aes(x=s_i, y=prop_efficient))+
  geom_point(aes(color=condition),alpha=0.1,position = position_jitter(width = 0.02,height = 0.01))+
  stat_summary(aes(color=condition),show.legend = T)+
  stat_summary(aes(color=condition),show.legend = T,geom="line")+
  stat_summary(aes(color=condition),show.legend = T,geom="errorbar")+
  labs(y = "Selection for efficiency", x="Social cue bias")+
  theme_bw()+
  scale_color_manual(values=condition_colors)+
  ylim(0,1)+
  theme(text=element_text(size=15),axis.text.x = element_text(angle = 45, hjust = 1))
p4 = ggplot(data, aes(x=g_i, y=prop_efficient))+
  geom_point(aes(color=condition),alpha=0.1,position = position_jitter(width = 0.02,height = 0.01))+
  stat_summary(aes(color=condition),show.legend = FALSE)+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="line")+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="errorbar")+
  labs(y="",x="Current payoff bias")+
  scale_color_manual(values=condition_colors)+
  theme_bw()+
  ylim(0,1)+
  theme(text=element_text(size=15),axis.text.x = element_text(angle = 45, hjust = 1))

p5 = ggplot(data, aes(x=inverse_temp, y=prop_efficient))+
  geom_point(aes(color=condition),alpha=0.1,position = position_jitter(width = 0.1,height = 0.01))+
  stat_summary(aes(color=condition),show.legend = FALSE)+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="line")+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="errorbar")+
  labs(y="",x="Conservatism")+
  theme_bw()+
  scale_color_manual(values=condition_colors)+
  ylim(0,1)+
  theme(text=element_text(size=15),axis.text.x = element_text(angle = 45, hjust = 1))
g4= ggarrange(p3,p4,p5,ncol=3,labels=c("H","I","J"),font.label = list(color="black"),align="hv",common.legend = T,legend = "bottom")

gall = ggarrange(g1,g2,g3,g4,nrow=4,ncol=1)
ggsave(gall, file="../images/SI_highlow-conformity.png",width = 17.8, height = 18, units = "cm", scale=2)

#SI Fig 5: small/large payoffs ####
load("../data/df_ABM_payoff_small.Rda")
data = df_ABM_small %>% filter(conformity==5,timestep==34)
data %>% group_by(condition) %>% summarise(n())

#colors and color ramp for 3d plots
solution_cols = c("#03039e","white","#ff5959")
condition_colors= c("#14213d","#fca311")
f <- colorRamp(solution_cols)

data_summary = data %>%
group_by(condition,conformity,s_i,g_i,inverse_temp) %>%
summarise(mean_proportion_efficient=mean(prop_efficient)) %>%
mutate(colors=rgb(f(mean_proportion_efficient)/255))

#create A,B 3d plots
p1=plot_ly(data_summary %>% filter(condition=="static",conformity==5), x = ~s_i, y = ~g_i, z = ~inverse_temp, marker=list(size=15), color = ~mean_proportion_efficient, colors = solution_cols) %>%
  add_markers(opacity=0.9) %>%
  #colorbar(title="Selection for \nefficiency")%>%
  layout(scene = list(xaxis = list(title = 'Social cue bias',titlefont=list(size=18)),
                      yaxis = list(title = 'Current payoff bias',titlefont=list(size=15),nticks=5),
                      zaxis = list(title = 'Conservatism',titlefont=list(size=18)),
                      aspectratio=list(x=.5,y=.5,z=.5),
                      camera = list(eye = list(x = .2, y = 1, z = 0)))) %>% hide_colorbar()
p2=plot_ly(data_summary %>% filter(condition=="turnover",conformity==5), x = ~s_i, y = ~g_i, z = ~inverse_temp, marker=list(size=15), color = ~mean_proportion_efficient, colors = solution_cols) %>%
  add_markers(opacity=0.9) %>%
  layout(scene = list(xaxis = list(title = 'Social cue bias',titlefont=list(size=18)),
                      yaxis = list(title = 'Current payoff bias',titlefont=list(size=15),nticks=5),
                      zaxis = list(title = 'Conservatism',titlefont=list(size=18)),
                      aspectratio=list(x=.5,y=.5,z=.5),
                      camera = list(eye = list(x = .2, y = 1, z = 0)))) %>% hide_colorbar()
#export images from interactive plotly graphs
orca(p1, file = "../images/static3d.png")
orca(p2, file = "../images/turnover3d.png")

#import and arrange them into top row
static = image_read("../images/static3d.png")
turnover = image_read("../images/turnover3d.png")
legend = image_read("../images/legend.png")

p1 = rasterGrob(static,height=1)
p2 = rasterGrob(turnover,height=1)
legend = rasterGrob(legend,height=1)
g1 = ggarrange(p1,p2,legend,labels = c("A - Smaller payoffs","B",""),widths = c(.4,.4,.2),
               font.label = list(color="black"),align="hv",ncol=3)

p3 = ggplot(data, aes(x=s_i, y=prop_efficient))+
  geom_point(aes(color=condition),alpha=0.1,position = position_jitter(width = 0.02,height = 0.01))+
  stat_summary(aes(color=condition),show.legend = T)+
  stat_summary(aes(color=condition),show.legend = T,geom="line")+
  stat_summary(aes(color=condition),show.legend = T,geom="errorbar")+
  labs(y = "Selection for efficiency", x="Social cue bias")+
  theme_bw()+
  scale_color_manual(values=condition_colors)+
  ylim(0,1)+
  theme(text=element_text(size=15),axis.text.x = element_text(angle = 45, hjust = 1))

p4 = ggplot(data, aes(x=g_i, y=prop_efficient))+
  geom_point(aes(color=condition),alpha=0.1,position = position_jitter(width = 0.02,height = 0.01))+
  stat_summary(aes(color=condition),show.legend = FALSE)+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="line")+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="errorbar")+
  labs(y="",x="Current payoff bias")+
  scale_color_manual(values=condition_colors)+
  theme_bw()+
  ylim(0,1)+
  theme(text=element_text(size=15),axis.text.x = element_text(angle = 45, hjust = 1))

p5 = ggplot(data, aes(x=inverse_temp, y=prop_efficient))+
  geom_point(aes(color=condition),alpha=0.1,position = position_jitter(width = 0.1,height = 0.01))+
  stat_summary(aes(color=condition),show.legend = FALSE)+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="line")+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="errorbar")+
  labs(y="",x="Conservatism")+
  theme_bw()+
  scale_color_manual(values=condition_colors)+
  ylim(0,1)+
  theme(text=element_text(size=15),axis.text.x = element_text(angle = 45, hjust = 1))
g2= ggarrange(p3,p4,p5,ncol=3,labels=c("C","D","E"),font.label = list(color="black"),align="hv",common.legend = T,legend = "bottom")

load("../data/df_ABM_payoff_large.Rda")
data = df_ABM_payoff_large %>% filter(conformity==5,timestep==34)
data %>% group_by(condition) %>% summarise(n())

#colors and color ramp for 3d plots
solution_cols = c("#03039e","white","#ff5959")
condition_colors= c("#14213d","#fca311")
f <- colorRamp(solution_cols)

data_summary = data %>%
group_by(condition,conformity,s_i,g_i,inverse_temp) %>%
summarise(mean_proportion_efficient=mean(prop_efficient)) %>%
mutate(colors=rgb(f(mean_proportion_efficient)/255))

#create A,B 3d plots
p1=plot_ly(data_summary %>% filter(condition=="static",conformity==5), x = ~s_i, y = ~g_i, z = ~inverse_temp, marker=list(size=15), color = ~mean_proportion_efficient, colors = solution_cols) %>%
  add_markers(opacity=0.9) %>%
  #colorbar(title="Selection for \nefficiency")%>%
  layout(scene = list(xaxis = list(title = 'Social cue bias',titlefont=list(size=18)),
                      yaxis = list(title = 'Current payoff bias',titlefont=list(size=15),nticks=5),
                      zaxis = list(title = 'Conservatism',titlefont=list(size=18)),
                      aspectratio=list(x=.5,y=.5,z=.5),
                      camera = list(eye = list(x = .2, y = 1, z = 0)))) %>% hide_colorbar()
p2=plot_ly(data_summary %>% filter(condition=="turnover",conformity==5), x = ~s_i, y = ~g_i, z = ~inverse_temp, marker=list(size=15), color = ~mean_proportion_efficient, colors = solution_cols) %>%
  add_markers(opacity=0.9) %>%
  layout(scene = list(xaxis = list(title = 'Social cue bias',titlefont=list(size=18)),
                      yaxis = list(title = 'Current payoff bias',titlefont=list(size=15),nticks=5),
                      zaxis = list(title = 'Conservatism',titlefont=list(size=18)),
                      aspectratio=list(x=.5,y=.5,z=.5),
                      camera = list(eye = list(x = .2, y = 1, z = 0)))) %>% hide_colorbar()
#export images from interactive plotly graphs
orca(p1, file = "../images/static3d.png")
orca(p2, file = "../images/turnover3d.png")

#import and arrange them into top row
static = image_read("../images/static3d.png")
turnover = image_read("../images/turnover3d.png")
legend = image_read("../images/legend.png")

p1 = rasterGrob(static,height=1)
p2 = rasterGrob(turnover,height=1)
legend = rasterGrob(legend,height=1)
g3 = ggarrange(p1,p2,legend,labels = c("F - Larger payoffs","G",""),widths = c(.4,.4,.2),
               font.label = list(color="black"),align="hv",ncol=3)

#plot C,D,E marginalized averages
p3 = ggplot(data, aes(x=s_i, y=prop_efficient))+
  geom_point(aes(color=condition),alpha=0.1,position = position_jitter(width = 0.02,height = 0.01))+
  stat_summary(aes(color=condition),show.legend = T)+
  stat_summary(aes(color=condition),show.legend = T,geom="line")+
  stat_summary(aes(color=condition),show.legend = T,geom="errorbar")+
  labs(y = "Selection for efficiency", x="Social cue bias")+
  theme_bw()+
  scale_color_manual(values=condition_colors)+
  ylim(0,1)+
  theme(text=element_text(size=15),axis.text.x = element_text(angle = 45, hjust = 1))

p4 = ggplot(data, aes(x=g_i, y=prop_efficient))+
  geom_point(aes(color=condition),alpha=0.1,position = position_jitter(width = 0.02,height = 0.01))+
  stat_summary(aes(color=condition),show.legend = FALSE)+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="line")+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="errorbar")+
  labs(y="",x="Current payoff bias")+
  scale_color_manual(values=condition_colors)+
  theme_bw()+
  ylim(0,1)+
  theme(text=element_text(size=15),axis.text.x = element_text(angle = 45, hjust = 1))

p5 = ggplot(data, aes(x=inverse_temp, y=prop_efficient))+
  geom_point(aes(color=condition),alpha=0.1,position = position_jitter(width = 0.1,height = 0.01))+
  stat_summary(aes(color=condition),show.legend = FALSE)+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="line")+
  stat_summary(aes(color=condition),show.legend = FALSE,geom="errorbar")+
  labs(y="",x="Conservatism")+
  theme_bw()+
  scale_color_manual(values=condition_colors)+
  ylim(0,1)+
  theme(text=element_text(size=15),axis.text.x = element_text(angle = 45, hjust = 1))
g4= ggarrange(p3,p4,p5,ncol=3,labels=c("H","I","J"),font.label = list(color="black"),align="hv",common.legend = T,legend = "bottom")

gall = ggarrange(g1,g2,g3,g4,nrow=4,ncol=1)
ggsave("../images/SI_smalllarge_payoff.png",width = 17.8, height = 18, units = "cm", scale=2)
