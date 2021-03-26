###   MATH97125 - Applied Statistics, Coursework final, question 1  ###

### code of Robin Mathelier ###


setwd("C:/Users/robin/Dropbox/Applications/Overleaf/Coursework Final Applied") # Ã  personnaliser
getwd()
.libPaths("C:/Users/robin/Documents/R/win-library/3.6")
rm(list=objects())

library(xtable)
library(latex2exp)
library(expm)
library(coda)
library(ggplot2)
library(MASS)
library('pracma')
library("lme4")
library(graphics)
library(faraway)
library(rcompanion)
library("RColorBrewer")


set.seed(42)

data = read.csv('CID1945214.csv')
head(data)
dim(data)

## Question 1 ##

summary(data)
xtable(summary(data,digits=3))

png(filename='pairplot_data_full.png')
pairs(data, upper.panel = NULL, pch = 16,cex=0.8,main='Pairplot of the variables')
dev.off()

par(mfrow=c(1,1))
png(filename='thresh_age.png')
plot(data$Thresh~data$Age,
     main='Thresh against Age',
     xlab='Age',
     ylab='Thresh')
dev.off()

png(filename='thresh_age_inv.png')
y = log(data$Age)
plot(data$Thresh~y,
     main='Thresh against log(Age)',
     xlab='log(Age)',
     ylab='Thresh')
dev.off()

par(mfrow=c(1,1))
png(filename = 'thresh_axis.png')
boxplot(data$Thresh~data$Axis,
        main='Thresh against Axis',
        xlab = 'Axis',
        ylab = 'Thresh')
dev.off()

png(filename = 'thresh_gender.png')
boxplot(data$Thresh~data$Gender,
        main='Thresh against Gender',
        xlab = 'Axis',
        ylab = 'Thresh')
dev.off()

png(filename='age_for_gender.png')
data_male = subset(data,data$Gender=='Male')
plot(data_male$Age,log(data_male$Thresh),
     main='Threshold against for different gender',
     xlab='Age',
     ylab='log(threshold))',pch=16,log='x',
     cex=0.7,col= 'cadetblue')
data_female = subset(data,data$Gender=='Female')
lines(data_female$Age,log(data_female$Thresh),
     main='Threshold against for different axis',
     xlab='Age',
     ylab='log(threshold))',pch=16,
     cex=0.7,col= 'brown2',type='p')
legend('bottomleft',
       c('Male','Female'),
       col=c('cadetblue','brown2'),
       pch=16,
       cex=0.9)
dev.off()

png(filename='Age_for_axis.png')
data_protan = subset(data,data$Axis=='Protan')
plot(data_protan$Age,log(data_protan$Thresh),
     main='Threshold against for different axis',
     xlab='Age',
     ylab='log(threshold))',pch=16,log='x',
     cex=0.7,col= 'cadetblue')
data_deutan = subset(data,data$Axis=='Deutan')
lines(data_deutan$Age,log(data_deutan$Thresh),
      main='Deutan',
      xlab='Age',
      ylab='log(threshold))',
      cex=0.7,pch=16,type='p',col='brown3')
data_tritan = subset(data,data$Axis=='Tritan')
lines(data_tritan$Age,log(data_tritan$Thresh),
      main='Tritan',
      xlab='Age',
      ylab='log(threshold))',
      cex=0.7,pch=16,type='p',col='chocolate1')
legend('bottomleft',
       c('Protan','Deutan','Tritan'),
       col=c('cadetblue','brown3','chocolate1'),
       pch=16,
       cex=0.8)
dev.off()

## Question 2 ##

# M1 Listwise deletion

sum(is.na(data))
list_lign_na = sapply(1:dim(data)[1],function(i) sum(is.na(data)[i,])>0)
sum(list_lign_na)
sum(list_lign_na)/dim(data)[1]

data_listwise_del = data[list_lign_na==F,]
summary(data_listwise_del)

# M2 Mean imputation

data_mean_imputation = data
col_cont = c('Age','Thresh','IQ')
for(col_name in col_cont){
  rep_val = mean(data_mean_imputation[,col_name],
                 na.rm=T)
  data_mean_imputation[,col_name][is.na(data_mean_imputation[,col_name])] = rep_val
}

data_mean_imputation$Gender[is.na(data_mean_imputation$Gender)] = 'Male'
data_mean_imputation$Axis[is.na(data_mean_imputation$Axis)] = 'Deutan'

summary(data_mean_imputation)

# comparison

summary(data)
summary(data_listwise_del)
summary(data_mean_imputation)

pairs(data, upper.panel = NULL, pch = 16,cex=0.8)
pairs(data_listwise_del,upper.panel = NULL, pch = 16,col='red',cex=0.8)
pairs(data_mean_imputation,upper.panel = NULL, pch = 16,col='green',cex=0.8)

png(filename = 'mean_imput.png')
par(mfrow=c(1,1))
plot(data$Age,data$Thresh,cex=1,
     main='Mean imputation added points',
     xlab='Age',
     ylab='Thresh'
     )
lines(data_mean_imputation$Age[is.na(data$Age)],
     data_mean_imputation$Thresh[is.na(data$Age)],col='red',
     type='p',cex=1)
legend('topright',
       c('original data points','points added with mean imputation'),
       cex=1,pch=1,col=c(1,2))
dev.off()

df = data_listwise_del
xtable(summary(df,digits=3))

## Question 3 ##

# df0

df0 = df

mylm_0 <- lm(Thresh~.,data=df0)
summary(mylm_0,digit=3)
contrasts(df0$Axis)
contrasts(df0$Gender)

# df1

df1 = df
df1$Age_log = log(df1$Age)

mylm_1 <- lm(Thresh~.,data=df1)
summary(mylm_1,digit=3)

png(filename = 'cook.png')
plot(mylm_1,which=5) # distance cook << 0.5, no outliers
dev.off()

max(df1$Thresh)/min(df1$Thresh) # big => boxplot useful

png(filename = 'boxcox.png')
boxcox(mylm_1, plotit = TRUE)
dev.off()

png(filename = 'boxcox_zoom.png')
boxcox(mylm_1, plotit = TRUE, lambda = seq(-0.1,0.1, by = 0.1))
dev.off()

df2 = df1
df2$Thresh_log = log(df2$Thresh)
mylm_2 = lm(Thresh_log~.,data=df2[,-2])
contrasts(df2$Gender)
summary(mylm_2)
xtable(summary(mylm_2))

df3 = df2[,c("Age",'Axis','Age_log','Thresh_log')]
mylm_3 = lm(Thresh_log~.,data=df3)
summary(mylm_3,digit=4)
xtable(summary(mylm_3,digit=4))
levels(df3$Axis) # Toutes les variables doivent être gardées sinon perte R^2

anova(mylm_2,mylm_3,test="F")
xtable(anova(mylm_2,mylm_3,test="F"),digit=4) # on garde mylm_3

png(filename = 'std_res_mylm_3.png')
par(mfrow=c(1,1))
x=predict(mylm_3)
y=mylm_3$res
plot(exp(x),y,main='Residuals against predicted threshold',
     xlab="Predicted threshold",ylab = 'Residuals',pch=16,cex=0.8)
dev.off()

png(filename = 'qqplots_mylm_3.png')
qqnorm(residuals(mylm_3))
qqline(residuals(mylm_3),col='red',lty=2)
dev.off()


png(filename = 'fitted_nlm.png',width = 17.5,height=10,units='cm',res=600)

par(mfrow=c(1,3))

df3_protan = subset(df3,df3$Axis=='Protan')
t_pred_protan = predict(mylm_3,df3_protan)
plot(df3_protan$Age,exp(df3_protan$Thresh),
     main='Protan',
     xlab='Age',
     ylab='threshold',
     cex=0.8,log=c('x','y'))
legend('topright',col=c(1,'chocolate1'),
       legend=c('observed response','predicted response'),
       ,cex=0.8,pch=c(1,16))
lines(df3_protan$Age,exp(t_pred_protan),col='chocolate1',type='p',pch=16,cex=0.8)

df3_deutan = subset(df3,df3$Axis=='Deutan')
t_pred_deutan = predict(mylm_3,df3_deutan)
plot(df3_deutan$Age,exp(df3_deutan$Thresh),
     main='Deutan',
     xlab='Age',
     ylab='threshold',
     cex=0.8,log=c('x','y'))
legend('topright',col=c(1,'chocolate1'),
       legend=c('observed response','predicted response'),
       ,cex=0.8,pch=c(1,16))
lines(df3_deutan$Age,exp(t_pred_deutan),col='chocolate1',type='p',pch=16,cex=0.8)


df3_tritan = subset(df3,df3$Axis=='Tritan')
t_pred_tritan = predict(mylm_3,df3_tritan)
plot(df3_tritan$Age,exp(df3_tritan$Thresh),
     main='Tritan',
     xlab='Age',
     ylab='log(threshold))',
     cex=0.8,log=c('x','y'))
legend('topright',col=c(1,'chocolate1'),
       legend=c('observed response','predicted response'),
       ,cex=0.8,pch=c(1,16))
lines(df3_tritan$Age,exp(t_pred_tritan),col='chocolate1',type='p',pch=16,cex=0.8)

dev.off()

## Question 4 ##

rep=data$Thresh
hist(rep)

# glm_1 

glm_1 = glm(Thresh~Axis+Age+I(Age^-1),
            family = Gamma(link ="identity"),
            data=df)

summary(glm_1)
glm_1$coefficients

glm_2 = glm(Thresh ~ Age:Axis + Axis:I(Age^-1),
            family = Gamma(link ="identity"),
            data=df)

summary(glm_2)
glm_2$coefficients

png(filename = 'fitted_glm_1.png',width = 17.5,height=10,units='cm',res=600)

par(mfrow=c(1,3))

df_protan = subset(df,df$Axis=='Protan')
t_pred_protan = predict(glm_1,df_protan)
plot(df_protan$Age,df_protan$Thresh,
     main='Protan',
     xlab='Age',
     ylab='Threshold',
     cex=0.8,log=c('x','y'))
legend('topright',col=c(1,2,'cadetblue'),
       legend=c('observed response',
                'glm_1 predicted response',
                'glm_2 predicted response'),
       ,cex=0.8,pch=c(1,16,16))
lines(df_protan$Age,t_pred_protan,col='red',type='p',pch=16,cex=0.8)
t_pred_protan = predict(glm_2,df_protan)
lines(df_protan$Age,t_pred_protan,col='cadetblue',type='p',pch=16,cex=0.8)

df_deutan = subset(df,df$Axis=='Deutan')
t_pred_deutan = predict(glm_1,df_deutan)
plot(df_deutan$Age,df_deutan$Thresh,
     main='Deutan',
     xlab='Age',
     ylab='Threshold',
     cex=0.8,log=c('x','y'))
lines(df_deutan$Age,t_pred_deutan,col='red',type='p',pch=16,cex=0.8)
legend('topright',col=c(1,2,'cadetblue'),
       legend=c('observed response',
                'glm_1 predicted response',
                'glm_2 predicted response'),
       ,cex=0.8,pch=c(1,16,16))
t_pred_deutan = predict(glm_2,df_deutan)
lines(df_deutan$Age,t_pred_deutan,col='cadetblue',type='p',pch=16,cex=0.8)


df_tritan = subset(df,df$Axis=='Tritan')
t_pred_tritan = predict(glm_1,df_tritan)
plot(df_tritan$Age,df_tritan$Thresh,
     main='Tritan',
     xlab='Age',
     ylab='Threshold',
     cex=0.8,log=c('x','y'))
lines(df_tritan$Age,t_pred_tritan,col='red',type='p',pch=16,cex=0.8)
t_pred_tritan = predict(glm_2,df_tritan)
lines(df_tritan$Age,t_pred_tritan,col='cadetblue',type='p',pch=16,cex=0.8)
legend('topright',col=c(1,2,'cadetblue'),
       legend=c('observed response',
       'glm_1 predicted response',
       'glm_2 predicted response'),
       ,cex=0.8,pch=c(1,16,16))

dev.off()

dev_res = rbind(sum(residuals(glm_1,type="deviance")^2),
            sum(residuals(glm_2,type="deviance")^2))
AICs = rbind(AIC(glm_1),
         AIC(glm_2))
tab = data.frame(cbind(dev_res,AICs))
xtable(tab)

compareGLM(glm_1,glm_2)
?compareGLM
glm_1$residuals^2

sum((log(predict(glm_1,df))-log(df$Thresh))^2)
sum((log(predict(glm_2,df))-log(df$Thresh))^2)

f = function(x) sd(data$Thresh[data$Thresh<x],na.rm=T)
g = Vectorize(f)
f_mean = function(x) mean(data$Thresh[data$Thresh<x],na.rm=T)
g_mean = Vectorize(g)
abs=seq(0.005,0.06,0.001)
abs_mean = g_mean((abs))
ord_sd=g(abs)

png(filename = 'var_vs_mean.png')
n=length(df$Thresh)
x=c()
y=c()
for(i in 1:10000){
abs_boot=sample(1:n,replace=T)
thresh_boot=df$Thresh[abs_boot]
x = c(x,mean(thresh_boot))
y = c(y,var(thresh_boot))}
par(mfrow=c(1,1))
plot(x,y,
     main = 'Variance against the mean',
     xlab='Mean',
     ylab='Variance')
dev.off()

cor(x,y)

png(filename = 'scatter_plot_thresh.png')
plot(data$Thresh,
     main='scatter plot of the thresholds',
     xlab='i',
     ylab=expression(Threshold[i]))
dev.off()

## Question 5 

png(filename = 'fitted_glm_2.png',width = 17.5,height=10,units='cm',res=600)

par(mfrow=c(1,3))

df_protan = subset(df,df$Axis=='Protan')
t_pred_protan = predict(glm_2,df_protan)
plot(df_protan$Age,df_protan$Thresh,
     main='Protan',
     xlab='Age',
     ylab='Threshold',
     cex=0.8,log=c('x','y'))
legend('topright',col=c(1,'cadetblue','chocolate1'),
       legend=c('observed response',
                'glm_2 predicted response',
                'mylm_3 predicted response'),
       ,cex=0.8,pch=c(1,16,16))
lines(df_protan$Age,t_pred_protan,col='cadetblue',type='p',pch=16,cex=0.8)
t_pred_protan = predict(mylm_3,df3_protan)
lines(df3_protan$Age,exp(t_pred_protan),col='chocolate1',type='p',pch=16,cex=0.8)

df_deutan = subset(df,df$Axis=='Deutan')
t_pred_deutan = predict(glm_2,df_deutan)
plot(df_deutan$Age,df_deutan$Thresh,
     main='Deutan',
     xlab='Age',
     ylab='Threshold',
     cex=0.8,log=c('x','y'))
legend('topright',col=c(1,'cadetblue','chocolate1'),
       legend=c('observed response',
                'glm_2 predicted response',
                'mylm_3 predicted response'),
       ,cex=0.8,pch=c(1,16,16))
lines(df_deutan$Age,t_pred_deutan,col='cadetblue',type='p',pch=16,cex=0.8)
t_pred_deutan = predict(mylm_3,df3_deutan)
lines(df3_deutan$Age,exp(t_pred_deutan),col='chocolate1',type='p',pch=16,cex=0.8)

df_tritan = subset(df,df$Axis=='Tritan')
t_pred_tritan = predict(glm_2,df_tritan)
plot(df_tritan$Age,df_tritan$Thresh,
     main='Tritan',
     xlab='Age',
     ylab='Threshold',
     cex=0.8,log=c('x','y'))
legend('topright',col=c(1,'cadetblue','chocolate1'),
       legend=c('observed response',
                'glm_2 predicted response',
                'mylm_3 predicted response'),
       ,cex=0.8,pch=c(1,16,16))
lines(df_tritan$Age,t_pred_tritan,col='cadetblue',type='p',pch=16,cex=0.8)
t_pred_tritan = predict(mylm_3,df3_tritan)
lines(df3_tritan$Age,exp(t_pred_tritan),col='chocolate1',type='p',pch=16,cex=0.8)


dev.off()


sum(mylm_3$res^2)
sum((log(predict(glm_1,df))-log(df$Thresh))^2)






png(filename = 'fitted_glm_1_group.png',width = 17.5,height=10,units='cm',res=600)

par(mfrow=c(1,3))

df_protan = subset(df,df$Axis=='Protan')
t_pred_protan = predict(glm_1,df_protan)
plot(df_protan$Age,df_protan$Thresh,
     main='Protan',
     xlab='Age',
     ylab='Threshold',
     cex=0.8,log=c('x','y'))
legend('topright',col=c(1,2,'cadetblue'),
       legend=c('observed response',
                'glm_1 predicted response')
       ,cex=0.8,pch=c(1,16))
lines(df_protan$Age,t_pred_protan,col='red',type='p',pch=16,cex=0.8)

df_deutan = subset(df,df$Axis=='Deutan')
t_pred_deutan = predict(glm_1,df_deutan)
plot(df_deutan$Age,df_deutan$Thresh,
     main='Deutan',
     xlab='Age',
     ylab='Threshold',
     cex=0.8,log=c('x','y'))
lines(df_deutan$Age,t_pred_deutan,col='red',type='p',pch=16,cex=0.8)
legend('topright',col=c(1,2,'cadetblue'),
       legend=c('observed response',
                'glm_1 predicted response'
                ),
       ,cex=0.8,pch=c(1,16))


df_tritan = subset(df,df$Axis=='Tritan')
t_pred_tritan = predict(glm_1,df_tritan)
plot(df_tritan$Age,df_tritan$Thresh,
     main='Tritan',
     xlab='Age',
     ylab='Threshold',
     cex=0.8,log=c('x','y'))
lines(df_tritan$Age,t_pred_tritan,col='red',type='p',pch=16,cex=0.8)
legend('topright',col=c(1,2),
       legend=c('observed response',
                'glm_1 predicted response'
                ),
       ,cex=0.8,pch=c(1,16))

dev.off()


