setwd("C:/Users/memslie/OneDrive - Australian Institute of Marine Science/Desktop/working folder/change in coral assemblages/text/for submission/code and data for repo/Fig 6")

load(file='old.growth.weeds.RData')

acro <- old.growth.weeds |> 
  filter(ms_group=='Acropora') |> 
  group_by(A_SECTOR,SHELF,half_decade) |> 
  summarise(mean_cover=mean(cover, na.rm = TRUE),
            sd=sd(cover, na.rm = TRUE),
            n=n(),
            se=sd/sqrt(n),
            two_se=se*2,
            .groups="drop")

acro.plot<-ggplot(acro,aes(x=half_decade,y=mean_cover,colour=A_SECTOR))+
  geom_point(stat='identity',position=position_dodge(width=1),size=2,show.legend = FALSE)+
  geom_errorbar(stat='identity',aes(ymin=(mean_cover-two_se),ymax=mean_cover+two_se),width=0,position=position_dodge(width=1),show.legend = FALSE)+
  scale_y_continuous('Acropora cover (%)')+
  scale_colour_manual(values = RColorBrewer::brewer.pal(11, "Paired"))+
  ggtitle('b')+
  facet_grid(A_SECTOR~SHELF,scales='free_y',
             labeller = labeller(
               SHELF = c(
                 I = "Inner shelf",
                 M = "Mid-shelf",
                 O = "Outer shelf"
               )
             ))+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        panel.border = element_rect(
          colour = "black",
          fill = NA,
          linewidth = 0
        ),
        axis.line.x = element_line(colour = "black"))


acro.plot


porit <- old.growth.weeds |> 
  filter(ms_group=='massive Porites') |> 
  group_by(A_SECTOR,SHELF,half_decade) |> 
  summarise(mean_cover=mean(cover, na.rm = TRUE),
            sd=sd(cover, na.rm = TRUE),
            n=n(),
            se=sd/sqrt(n),
            two_se=se*2,
            .groups="drop")

por.plot<-ggplot(porit,aes(x=half_decade,y=mean_cover,colour=A_SECTOR))+
  geom_point(stat='identity',position=position_dodge(width=1),size=2,show.legend = FALSE)+
  geom_errorbar(stat='identity',aes(ymin=(mean_cover-two_se),ymax=mean_cover+two_se),width=0,
                position=position_dodge(width=1),show.legend = FALSE)+
  scale_y_continuous('Porites cover (%)')+
  scale_colour_manual(values = RColorBrewer::brewer.pal(11, "Paired"))+
  ggtitle('a')+
  facet_grid(A_SECTOR~SHELF,scales='free_y',
             labeller = labeller(
               SHELF = c(
                 I = "Inner shelf",
                 M = "Mid-shelf",
                 O = "Outer shelf"
               )
             ))+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        panel.border = element_rect(
          colour = "black",
          fill = NA,
          linewidth = 0
        ),
        axis.line.x = element_line(colour = "black"))

por.plot

old.growth.weeds.plot<-por.plot+acro.plot+
  plot_layout(guides = 'collect')

old.growth.weeds.plot

ggsave(old.growth.weeds.plot,file='old.growth.weeds.plot.pdf',height=9,width=12)
ggsave(old.growth.weeds.plot,file='old.growth.weeds.plot.png',height=9,width=12)


