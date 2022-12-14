library(ggplot2)
library(leaps)
library(mgcv)
library(readxl)                                      # Install reshape package
library(RColorBrewer)
library(foreach)
library(splines)
library(readxl)
library(reshape2)
library(Rmisc)


bj0<-bj_weekly2010_2021
bj1<-bj_weekly2010_2019
bj2<-bj_weekly2020_2021


#Figure 1 ???????أ?NPIs???????Է???

Fig_1a<-ggplot(bj0,aes(DATE,temp))+
  geom_line(lwd=1.0,col="#F9CE00")+ 
  theme_bw()+
  theme(panel.grid=element_blank(),axis.text.y = element_text(size=12),
        axis.text.x= element_text(size=12),axis.title.y=element_text(size=12,face="bold"))+
  scale_x_continuous(breaks = seq(2010,2022,1))+
  labs(title = paste('a'), x=' ', y = 'Mean temperature (??)', colour='') +
  coord_fixed(0.08)
Fig_1a 

Fig_1b<-ggplot(bj0,aes(DATE,RH))+
  geom_line(lwd=1.0,col="#79bd9a")+ 
  theme_bw()+
  theme(panel.grid=element_blank(),axis.text.y = element_text(size=12),
        axis.text.x= element_text(size=12),axis.title.y=element_text(size=12,face="bold"))+
  scale_x_continuous(breaks = seq(2010,2022,1))+
  labs(title = paste('b'), x=' ', y = 'Relative humidity', colour='')+ 
  coord_fixed(4.5)
Fig_1b 

Fig_1c<-ggplot(bj0,aes(DATE,AH))+
  geom_line(lwd=1.0,col="#d5a4cf")+ 
  theme_bw()+
  theme(panel.grid=element_blank(),axis.text.y = element_text(size=12),
        axis.text.x= element_text(size=12),axis.title.y=element_text(size=12,face="bold"))+
  scale_x_continuous(breaks = seq(2010,2022,1))+
  labs(title = paste('c'), x=' ', y = 'Absolute humidity (g/m3)', colour='')+ 
  coord_fixed(0.14)
Fig_1c

Fig_1d
  bj.melt <- melt(bj_weeklymatrix, id.vars = "DATE")
  ggplot(bj.melt, aes(x = variable, y = DATE, fill = value)) +
    geom_tile() + # 繪製熱圖
    scale_y_discrete(expand = c(0,0), limits = rev(bj_weeklymatrix$DATE)) +        # 移除多餘空白
    scale_x_discrete(expand = c(0,0), breaks = names(bj_weeklymatrix)[c(2,52,102)],
                     labels = c("2020","2021","2022")) +        # 移除多餘空白
    coord_fixed(ratio = 4) +                            # 設定 X ??? Y 軸等比例
    scale_fill_gradientn(colours = brewer.pal(6, "YlGnBu")[2:6]) + # 設定色盤
    theme(
      axis.title = element_blank(),
      legend.text = element_text(face="bold"), # 說明文字用粗???
      axis.ticks = element_blank(),     # 座標軸上的刻度寬???
      plot.background = element_blank(),       # 移除背景
      panel.border = element_blank(),          # 移除邊框
    )+
    labs(title = "d")

#???????ع????Է???
bj<-bj_weekly2010_2019
cor(bj[18:20],method="pearson",use = "complete")

#??????֤

fit=gam(log(ILI)~s(time)+s(temp)+s(RH)+pi+as.factor(holiday),data=bj_weekly2010_2019)
plot(fit,se=TRUE,col="blue")
preds2=predict(fit,newdata=bj_weekly2010_2019)
sqrt(mean((bj_weekly2010_2019$ILI-preds2)^2))
summary(fit)
AIC(fit)

RMSE <- c()
A <- c()
R<-c()
for (i in 1:105) {
  train <- bj_weekly2010_2019[(1:339)+i-1,]
  test <- bj_weekly2010_2019[(340:391)+i-1,]
  
  fit=gam(log(ILI)~s(time)+s(temp)+s(RH)+pi+as.factor(holiday),data=train)
  preds=predict(fit,newdata=test)
  
  RMSE[i] <- sqrt(mean((test$ILI-preds)^2))
  A[i] <- AIC(fit)
  R[i] <- summary(fit)$r.sq
}
RMSE
A
R
CI(RMSE,ci=0.95)
CI(A,ci=0.95)
CI(R,ci=0.95)

#????ģ??
fit=gam(log(ILI)~s(time)+s(temp)+s(RH)+pi+as.factor(holiday),data=bj_weekly2010_2019)
preds1=predict(fit,newdata=bj_weekly2010_2019)
sqrt(mean((bj_weekly2010_2019$ILI-preds1)^2))
summary(fit)
AIC(fit)


#Ԥ??2020-2021????ͼ2010-2021
q <- predict(fit, bj_weekly2010_2021,type="link", se.fit = TRUE)
upr <- q$fit + (1.96 * q$se.fit)
lwr <- q$fit - (1.96 * q$se.fit)

out1<-bj_weekly2010_2021
out1$pre<-exp(q$fit) 
out1$up<-exp(upr) 
out1$low<-exp(lwr) 
write.csv(out1, "C:/Users/123123/Desktop/thesis/2.NPIs/1114/out1.csv") 


Figure 3a<-
  ggplot(data=out1) +
  geom_rect(aes(xmin=DATE[496], xmax=DATE[597], ymin=-Inf, ymax=Inf,colour='COVID-19',fill='COVID-19'),alpha =0.05)+
  geom_line(aes(x = DATE, y = ILI,colour='observed'),size=0.8) + 
  theme_bw()+
  theme(panel.grid=element_blank(),axis.text.y = element_text(size=12),axis.text.x= element_text(size=12),axis.title.y=element_text(size=12,face="bold"))+
  geom_line(aes(x=DATE,y=pre,colour='predicted'),size=0.8)+
  geom_ribbon(aes(x=DATE,ymin=low,ymax=up,colour='CI',fill="CI"),alpha=0.2,linetype=0,show.legend = F)+
  scale_color_manual(name=paste(''), 
                     breaks = c( 'observed', 'COVID-19', 'predicted', 'CI'),
                     values = c(  '#79bd9a', '#dedcee', '#ea7070', '#ea7070')) + 
  scale_fill_manual(name='', 
                    breaks = c( 'observed', 'COVID-19', 'predicted', 'CI'),
                    values = c(  '#79bd9a', '#dedcee', '#ea7070', '#ea7070'))+
  scale_x_continuous(breaks = seq(2010,2022,1))+
  labs(title = paste('a'), x=' ', y = 'Weekly  ILI  counts', colour='') +
  ylim(0,600)+theme(legend.position = "none")+
  coord_fixed(0.0053)
Figure 3a

#?͹?ȥʮ???Ա???ͼ
Figure 4a<-
  ggplot(data=bj_mean) +
  geom_line(aes(x = week, y = mean,colour='mean'),size=0.8) + 
  theme_bw()+
  theme(panel.grid=element_blank(),axis.text.y = element_text(size=12),axis.text.x= element_text(size=12),axis.title.y=element_text(size=12,face="bold"))+
  geom_line(aes(x=week,y=first,colour='first'),size=0.8)+
  geom_line(aes(x=week,y=second,colour='second'),size=0.8)+
  geom_ribbon(aes(x=week,ymin=low,ymax=up,colour='CI',fill="CI"),alpha=0.2,linetype=0,show.legend = F)+
  scale_color_manual(name=paste(''), 
                     breaks = c( 'mean',  'first','second', 'CI'),
                     values = c(  '#ea7070', '#79bd9a','purple',  '#ea7070')) + 
  scale_fill_manual(name='', 
                    breaks = c(  'mean',  'first','second', 'CI'),
                    values = c(    '#ea7070', '#79bd9a','purple',  '#ea7070'))+
  scale_x_continuous(breaks = seq(1,52,3))+
  labs(title = paste('a'), x='Calender week ', y = 'Weekly ILI counts', colour='') +
  theme(legend.position = "none")+
  coord_fixed(0.12)
Figure 4a

#???????ع????Է???
cor(bj_weekly2020_2021_4_[29:31],method="pearson",use="complete")

#NPIs?????Է???
cor(bj_weekly2020_2021_4_[8:16],method="pearson", use="complete")

#NPIs??????Ӱ??
fit1=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+NPIs,data=bj_weekly2020_2021_4_)
preds1=predict(fit1,newdata=bj_weekly2020_2021_4_)
sqrt(mean((bj_weekly2020_2021_4_$dif-preds1)^2))
summary(fit1)
AIC(fit1)

#NPIs?????ط???

fit2=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C1,data=bj_weekly2020_2021_4_)
preds2=predict(fit2,newdata=bj_weekly2020_2021_4_)
sqrt(mean((bj_weekly2020_2021_4_$dif-preds2)^2))
summary(fit2)
AIC(fit2)

fit3=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C2,data=bj_weekly2020_2021_4_)
preds3=predict(fit3,newdata=bj_weekly2020_2021_4_)
sqrt(mean((bj_weekly2020_2021_4_$dif-preds3)^2))
summary(fit3)
AIC(fit3)

fit4=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C3,data=bj_weekly2020_2021_4_)
preds4=predict(fit4,newdata=bj_weekly2020_2021_4_)
sqrt(mean((bj_weekly2020_2021_4_$dif-preds4)^2))
summary(fit4)
AIC(fit4)

fit5=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C4,data=bj_weekly2020_2021_4_)
preds5=predict(fit5,newdata=bj_weekly2020_2021_4_)
sqrt(mean((bj_weekly2020_2021_4_$dif-preds5)^2))
summary(fit5)
AIC(fit5)

fit6=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C5,data=bj_weekly2020_2021_4_)
preds6=predict(fit6,newdata=bj_weekly2020_2021_4_)
sqrt(mean((bj_weekly2020_2021_4_$dif-preds6)^2))
summary(fit6)
AIC(fit6)

fit7=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C6,data=bj_weekly2020_2021_4_)
preds7=predict(fit7,newdata=bj_weekly2020_2021_4_)
sqrt(mean((bj_weekly2020_2021_4_$dif-preds7)^2))
summary(fit7)
AIC(fit7)

fit8=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C7,data=bj_weekly2020_2021_4_)
preds8=predict(fit8,newdata=bj_weekly2020_2021_4_)
sqrt(mean((bj_weekly2020_2021_4_$dif-preds8)^2))
summary(fit8)
AIC(fit8)

fit9=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C8,data=bj_weekly2020_2021_4_)
preds9=predict(fit9,newdata=bj_weekly2020_2021_4_)
sqrt(mean((bj_weekly2020_2021_4_$dif-preds9)^2))
summary(fit9)
AIC(fit9)

fit10=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+H6,data=bj_weekly2020_2021_4_)
preds10=predict(fit10,newdata=bj_weekly2020_2021_4_)
sqrt(mean((bj_weekly2020_2021_4_$dif-preds10)^2))
summary(fit10)
AIC(fit10)

#NPIs????Ч??????

fit11=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C12,data=bj_weekly2020_2021_4_)
preds11=predict(fit11,newdata=bj_weekly2020_2021_4_)
sqrt(mean((bj_weekly2020_2021_4_$dif-preds11)^2))
summary(fit11)
AIC(fit11)

fit12=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C34,data=bj_weekly2020_2021_4_)
preds12=predict(fit12,newdata=bj_weekly2020_2021_4_)
sqrt(mean((bj_weekly2020_2021_4_$dif-preds12)^2))
summary(fit12)
AIC(fit12)

fit13=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C567,data=bj_weekly2020_2021_4_)
preds13=predict(fit13,newdata=bj_weekly2020_2021_4_)
sqrt(mean((bj_weekly2020_2021_4_$dif-preds13)^2))
summary(fit13)
AIC(fit13)


#ȫ??NPIs??????ģ??
#single NPI
fit14=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C3+C4+C7+C8+H6,data=bj_weekly2020_2021_4_)
preds14=predict(fit15,newdata=bj_weekly2020_2021_4_)
sqrt(mean((bj_weekly2020_2021_4_$dif-preds14)^2))
summary(fit14)
AIC(fit14)

RMSE <- c()
A <- c()
R <-c()
for (i in 1:39) {
  train <- bj_weekly2020_2021_4_[(1:52)+i-1,]
  test <- bj_weekly2020_2021_4_[(53:60)+i-1,]
  
  fit14=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C3+C4+C7+C8+H6,data=train)
  pred14=predict(fit14,newdata=test)
  
  RMSE[i] <- sqrt(mean((test$dif-pred14)^2))
  R[i] <- summary(fit14)$r.sq
  A[i] <- AIC(fit14)
}
RMSE
A
R
CI(RMSE,ci=0.95)
CI(A,ci=0.95)
CI(R,ci=0.95)

#binding NPIs

fit15=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C34+C8+H6,data=bj_weekly2020_2021_4_)
preds15=predict(fit15,newdata=bj_weekly2020_2021_4_)
sqrt(mean((bj_weekly2020_2021_4_$dif-preds15)^2))
summary(fit15)
AIC(fit15)

RMSE <- c()
A <- c()
R <-c()
for (i in 1:39) {
  train <- bj_weekly2020_2021_4_[(1:52)+i-1,]
  test <- bj_weekly2020_2021_4_[(53:60)+i-1,]
  
  fit15=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C34+C8+H6,data=train)
  pred15=predict(fit15,newdata=test)
  
  RMSE[i] <- sqrt(mean((test$dif-pred15)^2))
  R[i] <- summary(fit15)$r.sq
  A[i] <- AIC(fit15)
}
RMSE
A
R
CI(RMSE,ci=0.95)
CI(A,ci=0.95)
CI(R,ci=0.95)

#????ģ?Ͳ?Ԥ??
fit14=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C3+C4+C7+C8+H6,data=bj_weekly2020_2021_4_)

p <- predict(fit14, bj_weekly2020_2021_4_, type = "link", se.fit = TRUE)
upr <- p$fit + (1.96 * p$se.fit)
lwr <- p$fit - (1.96 * p$se.fit)

out3=bj_weekly2020_2021_4_
out3$pre<-p$fit
out3$up<-upr
out3$low<-lwr
write.csv(out3, "C:/Users/123123/Desktop/thesis/2.NPIs/1022/1022(2)/out3.csv") 

#??ͼ
Figure s1a<-
  ggplot(data=out3) +
  geom_line(aes(x = DATE, y = dif,colour='observed'),size=0.8) + 
  theme_bw()+
  theme(panel.grid=element_blank(),axis.text.y = element_text(size=12),axis.text.x= element_text(size=12),axis.title.y=element_text(size=12,face="bold"))+
  geom_line(aes(x=DATE,y=pre,colour='predicted'),size=0.8)+
  geom_ribbon(aes(x=DATE,ymin=low,ymax=up,colour='CI',fill="CI"),alpha=0.2,linetype=0,show.legend = F)+
  scale_color_manual(name=paste(''), 
                     breaks = c( 'observed',  'predicted', 'CI'),
                     values = c(  '#79bd9a',  '#ea7070', '#ea7070')) + 
  scale_fill_manual(name='', 
                    breaks = c( 'observed',  'predicted', 'CI'),
                    values = c(  '#79bd9a', '#ea7070', '#ea7070'))+
  scale_x_continuous(breaks = seq(2010,2022,1))+
  labs(title = paste('a'), x=' ', y = 'Relative reduction of weekly ILI counts', colour='') +
  theme(legend.position = "none")+
  coord_fixed(0.26)
Figure s1a

#????ģ?Ͳ?Ԥ??
fit15=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C34+C8+H6,data=bj_weekly2020_2021_4_)

p <- predict(fit15, bj_weekly2020_2021_4_, type = "link", se.fit = TRUE)
upr <- p$fit + (1.96 * p$se.fit)
lwr <- p$fit - (1.96 * p$se.fit)

out5=bj_weekly2020_2021_4_
out5$pre<-p$fit
out5$up<-upr
out5$low<-lwr
write.csv(out5, "C:/Users/123123/Desktop/thesis/2.NPIs/1022/1022(2)/out5.csv") 

#??ͼ
Figure s2a<-
  ggplot(data=out5) +
  geom_line(aes(x = DATE, y = dif,colour='observed'),size=0.8) + 
  theme_bw()+
  theme(panel.grid=element_blank(),axis.text.y = element_text(size=12),axis.text.x= element_text(size=12),axis.title.y=element_text(size=12,face="bold"))+
  geom_line(aes(x=DATE,y=pre,colour='predicted'),size=0.8)+
  geom_ribbon(aes(x=DATE,ymin=low,ymax=up,colour='CI',fill="CI"),alpha=0.2,linetype=0,show.legend = F)+
  scale_color_manual(name=paste(''), 
                     breaks = c( 'observed',  'predicted', 'CI'),
                     values = c(  '#79bd9a',  '#ea7070', '#ea7070')) + 
  scale_fill_manual(name='', 
                    breaks = c( 'observed',  'predicted', 'CI'),
                    values = c(  '#79bd9a', '#ea7070', '#ea7070'))+
  scale_x_continuous(breaks = seq(2010,2022,1))+
  labs(title = paste('a'), x=' ', y = 'Relative reduction of weekly ILI counts', colour='') +
  theme(legend.position = "none")+
  coord_fixed(0.26)
Figure s2a



