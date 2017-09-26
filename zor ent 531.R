setwd("~/Dropbox/Nomogram/")
d <- read.csv("zor ent 531.csv", head=T)

head(d)

d$ZE = as.factor(d$ZE)
d$BMI = as.numeric(d$BMI)
d$IIM = as.numeric(d$IIM)
d$TGL = as.numeric(d$TGL)
d$TMD = as.numeric(d$TMD)
d$MOV1 = as.factor(d$MOV1)

str(d)
library(rms)
ddist <- datadist(d); options(datadist='ddist')
str(d)


f <- lrm(ZE ~  IIM + TGL + MOV1, data=d, x=TRUE, y=TRUE)



nom <- nomogram(f, fun=function(x)1/(1+exp(-x)), fun.at=c(.001, .01, .05, seq(0.1, 0.9, by = .1), .95, .99, .999), funlabel="Zor Entübasyon Riski")
plot(nom)

newObs = data.frame(IIM = 70, TGL = 120, MOV1 = "Yes")

rms:::predict.lrm(f, newObs, type=c("fitted"))
# 
# #
# # val <- validate(f, method='boot', B=1000, bw=TRUE, rule="aic", type='residual', sls=0.05, aics=0, force=NULL, estimate=TRUE, pr=FALSE, emax.lim=c(0,1))
# # val
# #
# # cal <- calibrate(f, method='boot', B=1000, bw=TRUE, rule="aic", type='residual', sls=0.05, aics=0, force=NULL, estimate=TRUE, pr=FALSE, smoother='lowess')
# # plot(cal)
# #
# 
# 
# ####################################################### IIM (start)
# IIMDiff = nom$IIM$IIM-nom$IIM$IIM[1]
# coeffIIM = nom$IIM$points/(IIMDiff)
# coeffIIM[1] = 0
# coeffIIMLength = seq(1:45)
# repIIM = 5
# 
# coeffsIIM = c(rep(17.7777778 , 5), rep(7.7777778, 5), rep(4.4444444, 5), rep(2.7777778, 5), rep(1.7777778, 5), rep(1.1111111, 5), rep(0.6349206, 5), rep(0.2777778, 5), rep(0, 5))
# IIMPoints = coeffIIMLength*coeffsIIM
# IIMPoints2 = c(100, IIMPoints[c(5,10,15,20,25,30,35,40,45)])
# 
# repList = list(1:5, 6:10, 11:15, 16:20, 21:25, 26:30, 31:35, 36:40, 41:45)
# 
# IIMPoints3 = seq(1:45)
# 
# for(i in 1:9){
#   
#   # if(i != 9){
#     diff = abs((IIMPoints2[i]-IIMPoints2[i+1])/repIIM)
#     
#     mult = (diff*(1:repIIM))
#     mult[1] = 0
#     
#     res = rep(IIMPoints2[i][1], repIIM) - mult
#   # }else{
#   #   res = ((IIMPoints2[i])/repIIM)*(1:repIIM)
#   #   
#   # }
#   # 
#     IIMPoints3[repList[[i]]] = res
# }
# 
# 
# IIMPoints4 = seq(1:45)
# 
# repList = list(1:5, 6:10, 11:15, 16:20, 21:25, 26:30, 31:35, 36:40, 41:45)
# z = c(4,9,14,19,24,29,34,39,44)
# 
# 
# for(i in 1:9){
#   
#   # if(i != 9){
#   diff = abs((IIMPoints3[repList[[i]][1]]-IIMPoints3[z[i]])/repIIM)
#   
#   mult = (diff*(1:repIIM))
#   mult[1] = 0
#   
#   res = rep(IIMPoints3[repList[[i]][1]], repIIM) - mult
#   # }else{
#   #   res = ((IIMPoints2[i])/repIIM)*(1:repIIM)
#   #   
#   # }
#   # 
#   IIMPoints4[repList[[i]]] = res
# }
# 
# 
# IIMresult = data.frame(points = rev(seq(1:45)), points = IIMPoints4)
# 
# ####################################################### IIM (end)
# 
# 
# 
# 
# 
# ####################################################### TGL (start)
# TGLDiff = nom$TGL$TGL-nom$TGL$TGL[1]
# coeffTGL = nom$TGL$points/(TGLDiff)
# coeffTGL[1] = 0
# coeffTGLLength = seq(1:40)
# repTGL = 5
# 
# coeffsTGL = c(rep(3.44010742 , 5), rep(1.47433175, 5), rep(0.81907320, 5), rep(0.49144392, 5), rep(0.29486635, 5), rep(0.16381464, 5), rep(0.07020627, 5), rep(0, 5))
# TGLPoints = coeffTGLLength*coeffsTGL
# TGLPoints2 = c(19.657757 , TGLPoints[c(5,10,15,20,25,30,35,40)])
# 
# repList = list(1:5, 6:10, 11:15, 16:20, 21:25, 26:30, 31:35, 36:40)
# 
# TGLPoints3 = seq(1:40)
# 
# for(i in 1:8){
#   
#   # if(i != 9){
#   diff = abs((TGLPoints2[i]-TGLPoints2[i+1])/repTGL)
#   
#   mult = (diff*(1:repTGL))
#   mult[1] = 0
#   
#   res = rep(TGLPoints2[i][1], repTGL) - mult
#   # }else{
#   #   res = ((IIMPoints2[i])/repIIM)*(1:repIIM)
#   #   
#   # }
#   # 
#   TGLPoints3[repList[[i]]] = res
# }
# 
# 
# TGLPoints4 = seq(1:40)
# 
# repList = list(1:5, 6:10, 11:15, 16:20, 21:25, 26:30, 31:35, 36:40)
# z = c(4,9,14,19,24,29,34,39)
# 
# 
# for(i in 1:8){
#   
#   # if(i != 9){
#   diff = abs((TGLPoints3[repList[[i]][1]]-TGLPoints3[z[i]])/repIIM)
#   
#   mult = (diff*(1:repIIM))
#   mult[1] = 0
#   
#   res = rep(TGLPoints3[repList[[i]][1]], repIIM) - mult
#   # }else{
#   #   res = ((IIMPoints2[i])/repIIM)*(1:repIIM)
#   #   
#   # }
#   # 
#   TGLPoints4[repList[[i]]] = res
# }
# 
# 
# ####################################################### TGL (end)
# 
# 
# 
# 
# 
# 
# ####################################################### Probability (start)
#       probDiff = nom$`Zor Ent?basyon Riski`$x.real
#       coeffProb = nom$`Zor Ent?basyon Riski`$x/(probDiff)
#       # coeffProb[1] = 0
#       
#       probSeq = seq(0.001, 0.999, 0.001)
#       
#       
#       # first = coeffProb * probSeq
#       
#       coeffsProbList = list()
#       coeffProb2 = coeffProb
#       t = c(9, 40, 50, 100, 100, 100,100,100,100,100,100,50,40,9,1)
#       
#       
#       for(i in 1:(length(probDiff))){
#       
#         coeffsProbList[[i]] = rep(coeffProb2[i], t[i])
#       
#       }
#       
#       
#       coeffsProb = c(unlist(coeffsProbList))
#       
#       
#       df = data.frame(prob = probSeq, coeff = coeffsProb, point = probSeq*coeffsProb )
#       
#       plot(df[c(1,10,50,100,200,300,400,500,600,700,800,900,950,990,999),])
#       
#       nom$`Zor Ent?basyon Riski`$x
#       
#       
#       repList = list(1:9,10:49,50:99,100:199,200:299,300:399,400:499,500:599,600:699,700:799,800:899,900:949,950:989,990:998,999)
#       
#       z = c(10,50,100,200,300,400,500,600,700,800,900,950,990,999)
#       
#       
#       for(i in 1:15){
#         if(i != 15){
#           diff = (coeffsProb[repList[[i]]]-coeffsProb[z[i]])/(length(repList[[i]]))
#         
#         }else{
#         
#           diff = (coeffsProb[repList[[i]]])/length(repList[[i]])
#         
#         }
#       
#         diff2 = diff*1:(length(repList[[i]]))
#       
#         coeffsProb[repList[[i]]] = coeffsProb[repList[[i]]] - (diff2-diff)
#       }
#       
#       
#       
#       probPoints = probSeq * coeffsProb
#       plot(probPoints)
#       
#       
#       for(i in 1:15){
#         if(i != 15){
#           diff = abs((probPoints[repList[[i]][1]]-probPoints[z[i]-1])/(length(repList[[i]])))
#           
#           mult = (diff*(1:length(repList[[i]])))
#           mult[1] = 0
#           
#           res = rep(probPoints[repList[[i]][1]], length(repList[[i]])) + mult
#           
#         }else{
#           
#           res = (probPoints[repList[[i]]])/length(repList[[i]])
#           
#         }
#         
#         
#         
#         probPoints[repList[[i]]] = res
#       }
#       
#       head(probPoints)
#       plot(probPoints)
#       
# ####################################################### Probability (end)      
#       
#       
#       
#       
#       
#       
# 
#       