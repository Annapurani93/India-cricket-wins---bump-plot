library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(ggrepel)
library(ggbump)
tuesdata <- tidytuesdayR::tt_load('2021-11-30')
tuesdata$matches->matches
glimpse(matches)

matches%>%
  filter(winner=="India"|winner=="England"|winner=="South Africa"|winner=="New Zealand"|winner=="Australia")%>%
  separate(match_date,into = c("Date","Year"),sep = ", ")%>%
  select(Year, winner,match_id)%>%
  drop_na()%>%
  group_by(winner,Year)%>%
  count()->yearwise

yearwise%>%
  mutate(colour=ifelse(
    winner=="India","green","gray30"
  ))->yearwise

yearwise%>%
  mutate(label = ifelse(Year==2005,winner,""))%>%
  data.frame()->yearwise

ggplot(yearwise,aes(x=Year,y=n,group=winner,colour=colour,label=label))+
  geom_bump(size=1)+
  geom_point(aes(group=winner),size=3,show.legend = FALSE)+
  geom_label_repel(aes(label=label),fill="black",colour="white",size=3.6,hjust=1)+
  scale_color_manual(values=c("gray30","#0392cf"))+
  scale_y_continuous(lim=c(0,30),breaks=c(5,10,15,20,25,30))+
  theme(
    plot.background = element_rect(fill="black"),
    panel.background = element_rect(fill="black"),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(colour="white",face="bold",size=12),
    axis.text.x = element_text(colour="white",face="bold",size=12,margin=margin(t=-5)),
    axis.title.y = element_text(colour="white",face="bold",size=12, margin=margin(r=25)),
    legend.position = "none",
    plot.margin = unit(c(1,2,1,2),"cm"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title=element_markdown(size=16, face="bold",margin=margin(b=15)),
    plot.subtitle=element_markdown(size=14,margin=margin(b=30)),
    plot.caption=element_text(size=10,colour="white",hjust=0,margin=margin(t=30)))+
  labs( y="NUMBER OF WINS",
    title="<span style='color:#0392cf'>THE MEN IN BLUE</span>",
       subtitle="<span style='color:#0392cf'>Team India<span style= 'color:white'> stands fourth in the ICC Rankings for men. But how was its trajectory of wins<br> from 1996-2005 when compared to other countries in the Top 5 ICC Rankings?<br> The below visualization given a snapshot </span>",
       caption = "Data via Tidy Tuesday| Analysis and design: @annapurani93")->plot

ggsave("cricket1.png",plot,width=10,height=7.14)    



