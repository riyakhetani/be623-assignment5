setwd("~/be623_assignment5")
library(dplyr)
library(ggplot2)

#Question 1
circ_df<-data.frame(            
  Sample=paste0("S",1:12),
  Gene=c(
    rep("PER2",6),
    rep("BMAL1",6)
  ),
  ZT=rep(c(0,4,8,12,16,20), times = 2),
  Expression=c(
    1.8,3.2,6.5,9.4,7.1,3.6,
    8.6,5.9,2.7,1.5,3.4,6.8
  )
)
str(circ_df)


#Question 2
per2_rows<-circ_df[circ_df$Gene=="PER2",]
per2_rows
mean(per2_rows$Expression)
median(per2_rows$Expression)
sd(per2_rows$Expression)


#Question 3
write.csv(circ_df,"circadian_expression.csv",row.names = FALSE)
circ_reloaded<-read.csv("circadian_expression.csv")
identical(circ_df$Expression, circ_reloaded$Expression)



#Question 4
circ_df%>%
  filter(Gene=="PER2")%>%
  arrange(desc(Expression))

circ_df%>%
  filter(Gene=="BMAL1")%>%
  arrange(desc(Expression))



#Question 5
circ_df2<-circ_df%>%
  mutate(dark=ZT>=12)

#for dark samples
circ_df2%>%
  filter(dark)%>%
  group_by(Gene)%>%
  summarize(
    mean_expression_d=mean(Expression),
    dark_samples=n()
  )

#for light samples
circ_df2%>%
  filter(!dark)%>%
  group_by(Gene)%>%
  summarize(
    mean_expression_l=mean(Expression),
    light_samples=n()
  )


#Question 6
rhythm_plot<-ggplot(circ_df,aes(x=ZT, y=Expression, color=Gene))+
  geom_point(size=3)+
  scale_color_manual(values=c("PER2"="pink","BMAL1"="green"))+
  labs(
    title="Gene Expression of Different Genes",
    x="Zeitgeber time (h)",
    y="Expression"
  )+
  theme_minimal()
ggsave("rhythm_plot.png",plot=rhythm_plot,width=6,height=4,dpi=300)



#Question 7
expression_boxplot<-ggplot(circ_df,aes(x=Gene,y=Expression,fill=Gene))+
  geom_boxplot()+
  scale_fill_manual(values=c("PER2"="pink","BMAL1"="green"))+
  labs(title="Distribution of Gene Expression",
       x="Gene",
       y="Expression"
  )+
  theme_minimal()
ggsave("expression_boxplot.png",plot=expression_boxplot,width=6,height=4,dpi=300)

