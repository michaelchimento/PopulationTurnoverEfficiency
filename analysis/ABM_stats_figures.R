library(tidyverse)
library(rms)
library(ggpubr)
library(ggstance)
library(plotly)
library(magick)
library(grid)

#Estimates conditional probability of learning from experimental data ####
#the following code estimates the condition probability of learning from latency to learn data from the experiment
#this was then fed into the agent based model to inform the transition probability of agents from naive to knowledgable

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
#lognormal has higher loglik
estimates = survest(fit.parametric, what="survival", times = c(1:40))
learn_probs = cbind(estimates$time, estimates$surv)
learn_probs = as.data.frame(learn_probs)
#calculate conditional probability of failure
learn_probs = learn_probs %>% mutate(cond_prob_fail = (V2 - lead(V2)) / V2)
print(learn_probs)

#the following code was used to visualize model outputs across several conditions

# Fig 4 ####
load("../data/df_ABM.Rda")
data = df_ABM %>% filter(conformity==5,timestep==34)
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
{
  p1=plot_ly(data_summary %>% filter(condition=="static"), x = ~s_i, y = ~g_i, z = ~inverse_temp, marker=list(size=15), color = ~mean_proportion_efficient, colors = solution_cols) %>%
    add_markers(opacity=0.9) %>%
    #colorbar(title="Selection for \nefficiency")%>%
    layout(scene = list(xaxis = list(title = 'Social cue bias',titlefont=list(size=18)),
                        yaxis = list(title = 'Current payoff bias',titlefont=list(size=15),nticks=5),
                        zaxis = list(title = 'Conservatism',titlefont=list(size=18)),
                        aspectratio=list(x=.5,y=.5,z=.5),
                        camera = list(eye = list(x = .2, y = 1, z = 0)))) %>% hide_colorbar()

  p2=plot_ly(data_summary %>% filter(condition=="turnover"), x = ~s_i, y = ~g_i, z = ~inverse_temp, marker=list(size=15), color = ~mean_proportion_efficient, colors = solution_cols) %>%
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

  }


#plot C,D,E marginalized averages
{
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
}
#arrange two rows together
gall = ggarrange(g1,g2,nrow=2,ncol=1)
ggsave(gall,file="../images/fig4.png",width = 17.8, height = 10, units = "cm", scale=2)


#SI Fig 3:ABM no/high conformity ####
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

#SI Fig 4: small/large payoffs ####
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
