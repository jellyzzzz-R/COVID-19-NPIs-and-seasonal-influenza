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
hk0<-hk_weekly2010_2021
hk1<-hk_weekly2010_2019
hk2<-hk_weekly2020_2021


#共线性分析
#Figure 2 气象因素，NPIs的描述性分析
Fig_2a<-ggplot(hk0)+
  geom_line(aes(DATE,temp),lwd=1.0,col="#F9CE00")+ 
  theme_bw()+
  theme(panel.grid=element_blank(),axis.text.y = element_text(size=12),
        axis.text.x= element_text(size=12),axis.title.y=element_text(size=12,face="bold"))+
  scale_x_continuous(breaks = seq(2010,2022,1))+
  labs(title = paste('a'), x=' ', y = 'Mean temperature (℃)', colour='') +
  coord_fixed(0.136)
Fig_2a 

Fig_2b<-ggplot(hk0,aes(DATE,RH))+
  geom_line(lwd=1.0,col="#79bd9a")+ 
  theme_bw()+
  theme(panel.grid=element_blank(),axis.text.y = element_text(size=12),
        axis.text.x= element_text(size=12),axis.title.y=element_text(size=12,face="bold"))+
  scale_x_continuous(breaks = seq(2010,2022,1))+
  labs(title = paste('b'), x=' ', y = 'Relative humidity', colour='')+ 
  coord_fixed(5)
Fig_2b 

Fig_2c<-ggplot(hk0,aes(DATE,AH))+
  geom_line(lwd=1.0,col="#d5a4cf")+ 
  theme_bw()+
  theme(panel.grid=element_blank(),axis.text.y = element_text(size=12),
        axis.text.x= element_text(size=12),axis.title.y=element_text(size=12,face="bold"))+
  scale_x_continuous(breaks = seq(2010,2022,1))+
  labs(title = paste('c'), x=' ', y = 'Absolute humidity (g/m3)', colour='')+ 
  coord_fixed(0.15)
Fig_2c

Fig_2d
hk.melt <- melt(hk_weeklymatrix, id.vars = "DATE")
ggplot(hk.melt, aes(x = variable, y = DATE, fill = value)) +
  geom_tile() + # 绻＝鐔卞湒
  scale_y_discrete(expand = c(0,0), limits = rev(hk_weeklymatrix$DATE)) +        # 绉婚櫎澶氶绌虹櫧
  scale_x_discrete(expand = c(0,0), breaks = names(hk_weeklymatrix)[c(2,52,102)],
                   labels = c("2020","2021","2022")) +        # 绉婚櫎澶氶绌虹櫧
  coord_fixed(ratio = 4) +                            # 瑷畾 X 鑸? Y 杌哥瓑姣斾緥
  scale_fill_gradientn(colours = brewer.pal(6, "YlGnBu")[2:6]) + # 瑷畾鑹茬洡
  theme(
    axis.title = element_blank(),
    legend.text = element_text(face="bold"), # 瑾槑鏂囧瓧鐢ㄧ矖楂?
    axis.ticks = element_blank(),     # 搴ф杌镐笂鐨勫埢搴﹀搴?
    plot.background = element_blank(),       # 绉婚櫎鑳屾櫙
    panel.border = element_blank(),          # 绉婚櫎閭婃
  )+
  labs(title = "d")

hk<-hk_weekly2010_2019
cor(hk[18:20],use = "complete")


#交叉验证

mod=gam(log(ILI)~s(time)+s(temp)+s(RH)+pi+as.factor(holiday),data=hk_weekly2010_2019)
plot(mod,se=TRUE,col="blue")
pre6=predict(mod,newdata=hk_weekly2010_2019)
sqrt(mean((hk_weekly2010_2019$ILI-pre6)^2))
summary(mod)
AIC(mod)

RMSE <- c()
A <- c()
R<-c()
for (i in 1:105) {
  train <- hk_weekly2010_2019[(1:339)+i-1,]
  test <- hk_weekly2010_2019[(340:391)+i-1,]
  
  mod=gam(log(ILI)~s(time)+s(temp)+s(RH)+pi+as.factor(holiday),data=train)
  pre=predict(mod,newdata=test)
  
  RMSE[i] <- sqrt(mean((test$ILI-pre)^2))
  A[i] <- AIC(mod)
  R[i] <- summary(mod)$r.sq
}
RMSE
A
R
CI(RMSE,ci=0.95)
CI(A,ci=0.95)
CI(R,ci=0.95)

#构建模型

mod=gam(log(ILI)~s(time)+s(temp)+s(RH)+pi+as.factor(holiday),data=hk_weekly2010_2019)
pre1=predict(mod,newdata=hk_weekly2010_2019)
sqrt(mean((hk_weekly2010_2019$ILI-pre1)^2))
summary(mod)
AIC(mod)


#预测2020-2021并做图
p <- predict(mod, hk_weekly2010_2021,type="link", se.mod = TRUE)
upr <- p$mod + (1.96 * p$se.mod)
lwr <- p$mod - (1.96 * p$se.mod)

out2<-hk_weekly2010_2021
out2$pre<-exp(p$mod)
out2$up<-exp(upr)
out2$low<-exp(lwr)
write.csv(out2,"C:/Users/123123/Desktop/thesis/2.NPIs/1114/out2.csv")

Figure 3b<-
  ggplot(data=out2) +
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
  labs(title = paste('b'), x=' ', y = 'Weekly  ILI  counts', colour='') +
  ylim(0,800)+theme(legend.position = "none")+
  coord_fixed(0.004)
Figure 3b

#和过去十年对比做图
Figure 4b<-
  ggplot(data=hk_mean) +
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
  labs(title = paste('b'), x='Calender week ', y = 'Weekly ILI counts', colour='') +
  theme(legend.position = "none")+
  coord_fixed(0.0608)
Figure 4b

#气象因素共线性分析
cor(hk_weekly2020_2021_4_[29:31],method="pearson",use="complete")

#NPIs共线性分析
cor(hk_weekly2020_2021_4_[8:16],method="pearson", use="complete")

#NPIs的总体影响
mod1=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+NPIs,data=hk_weekly2020_2021_4_)
preds1=predict(mod1,newdata=hk_weekly2020_2021_4_)
sqrt(mean((hk_weekly2020_2021_4_$dif-preds1)^2))
summary(mod1)
AIC(mod1)

#NPIs单因素分析

mod2=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C1,data=hk_weekly2020_2021_4_)
preds2=predict(mod2,newdata=hk_weekly2020_2021_4_)
sqrt(mean((hk_weekly2020_2021_4_$dif-preds2)^2))
summary(mod2)
AIC(mod2)

mod3=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C2,data=hk_weekly2020_2021_4_)
preds3=predict(mod3,newdata=hk_weekly2020_2021_4_)
sqrt(mean((hk_weekly2020_2021_4_$dif-preds3)^2))
summary(mod3)
AIC(mod3)

mod4=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C3,data=hk_weekly2020_2021_4_)
preds4=predict(mod4,newdata=hk_weekly2020_2021_4_)
sqrt(mean((hk_weekly2020_2021_4_$dif-preds4)^2))
summary(mod4)
AIC(mod4)

mod5=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C4,data=hk_weekly2020_2021_4_)
preds5=predict(mod5,newdata=hk_weekly2020_2021_4_)
sqrt(mean((hk_weekly2020_2021_4_$dif-preds5)^2))
summary(mod5)
AIC(mod5)

mod6=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C5,data=hk_weekly2020_2021_4_)
preds6=predict(mod6,newdata=hk_weekly2020_2021_4_)
sqrt(mean((hk_weekly2020_2021_4_$dif-preds6)^2))
summary(mod6)
AIC(mod6)

mod7=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C6,data=hk_weekly2020_2021_4_)
preds7=predict(mod7,newdata=hk_weekly2020_2021_4_)
sqrt(mean((hk_weekly2020_2021_4_$dif-preds7)^2))
summary(mod7)
AIC(mod7)

mod8=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C7,data=hk_weekly2020_2021_4_)
preds8=predict(mod8,newdata=hk_weekly2020_2021_4_)
sqrt(mean((hk_weekly2020_2021_4_$dif-preds8)^2))
summary(mod8)
AIC(mod8)

mod9=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C8,data=hk_weekly2020_2021_4_)
preds9=predict(mod9,newdata=hk_weekly2020_2021_4_)
sqrt(mean((hk_weekly2020_2021_4_$dif-preds9)^2))
summary(mod9)
AIC(mod9)

mod10=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+H6,data=hk_weekly2020_2021_4_)
preds10=predict(mod10,newdata=hk_weekly2020_2021_4_)
sqrt(mean((hk_weekly2020_2021_4_$dif-preds10)^2))
summary(mod10)
AIC(mod10)

#NPIs组合效果分析

mod11=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C12,data=hk_weekly2020_2021_4_)
preds11=predict(mod11,newdata=hk_weekly2020_2021_4_)
sqrt(mean((hk_weekly2020_2021_4_$dif-preds11)^2))
summary(mod11)
AIC(mod11)

mod12=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C34,data=hk_weekly2020_2021_4_)
preds12=predict(mod12,newdata=hk_weekly2020_2021_4_)
sqrt(mean((hk_weekly2020_2021_4_$dif-preds12)^2))
summary(mod12)
AIC(mod12)

mod13=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C567,data=hk_weekly2020_2021_4_)
preds13=predict(mod13,newdata=hk_weekly2020_2021_4_)
sqrt(mean((hk_weekly2020_2021_4_$dif-preds13)^2))
summary(mod13)
AIC(mod13)


#全部NPIs的最优模型
#single NPI
mod14=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C1+C2+C3+C4+C6+C7+C8+H6,data=hk_weekly2020_2021_4_)
preds14=predict(mod14,newdata=hk_weekly2020_2021_4_)
sqrt(mean((hk_weekly2020_2021_4_$dif-preds14)^2))
summary(mod14)
AIC(mod14)

RMSE <- c()
A <- c()
R <-c()
for (i in 1:39) {
  train <- hk_weekly2020_2021_4_[(1:52)+i-1,]
  test <- hk_weekly2020_2021_4_[(53:60)+i-1,]
  
  mod14=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C1+C2+C3+C4+C6+C7+C8+H6,data=train)
  pred14=predict(mod14,newdata=test)
  
  RMSE[i] <- sqrt(mean((test$dif-pred16)^2))
  R[i] <- summary(mod14)$r.sq
  A[i] <- AIC(mod14)
}
RMSE
A
R
CI(RMSE,ci=0.95)
CI(A,ci=0.95)
CI(R,ci=0.95)

#binding NPIs

mod15=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C12+C34+C567+C8,data=hk_weekly2020_2021_4_)
preds15=predict(mod15,newdata=hk_weekly2020_2021_4_)
sqrt(mean((hk_weekly2020_2021_4_$dif-preds15)^2))
summary(mod15)
AIC(mod15)

RMSE <- c()
A <- c()
R <-c()
for (i in 1:37) {
  train <- hk_weekly2020_2021_4_[(1:52)+i-1,]
  test <- hk_weekly2020_2021_4_[(53:60)+i-1,]
  
  fit15=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C12+C34+C567+C8,data=train)
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

#建立模型并预测
mod=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C1+C2+C3+C4+C6+C7+C8+H6,data=hk_weekly2020_2021_4_)

p <- predict(mod, hk_weekly2020_2021_4_, type = "link", se.fit = TRUE)
upr <- p$fit + (1.96 * p$se.fit)
lwr <- p$fit - (1.96 * p$se.fit)

out4=hk_weekly2020_2021_4_
out4$pre<-p$fit
out4$up<-upr
out4$low<-lwr
write.csv(out4, "C:/Users/123123/Desktop/thesis/2.NPIs/1022/1022(2)/out4.csv") 

#做图
Figure s1b<-
  ggplot(data=out4) +
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
  labs(title = paste('b'), x=' ', y = 'Relative reduction of weekly ILI counts', colour='') +
  theme(legend.position = "none")+
  coord_fixed(0.278)
Figure s1b

#建立模型并预测
mod=gam(dif~s(time)+s(temp0)+s(RH0)+pi0+C12+C34+C567+C8,data=hk_weekly2020_2021_4_)

p <- predict(mod, hk_weekly2020_2021_4_, type = "link", se.fit = TRUE)
upr <- p$fit + (1.96 * p$se.fit)
lwr <- p$fit - (1.96 * p$se.fit)

out6=hk_weekly2020_2021_4_
out6$pre<-p$fit
out6$up<-upr
out6$low<-lwr
write.csv(out6, "C:/Users/123123/Desktop/thesis/2.NPIs/1022/1022(2)/out6.csv") 

#做图
Figure s2b<-
  ggplot(data=out6) +
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
  labs(title = paste('b'), x=' ', y = 'Relative reduction of weekly ILI counts', colour='') +
  theme(legend.position = "none")+
  coord_fixed(0.265)
Figure s2b

