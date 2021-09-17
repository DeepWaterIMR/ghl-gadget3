
rm(list=ls())

library(Rstox)
library(plyr)
library(lattice)
library(data.table)
library(reshape)

inpath="C:/IMR/REDUS/BS_msp_model/Data/Survey/winter_survey/input/Nor-Rus_Barents_Winter_Trawl/"
outpath="C:/IMR/REDUS/BS_msp_model/Data/Survey/winter_survey/output/Nor-Rus_Barents_Winter_Trawl/"
code="C:/IMR/REDUS/BS_msp_model/Data/Survey/winter_survey/scripts/"
species="cod/"

# ## In first place obtain baseline for AbundancebyLength, superindvidual and catch databases for all years.
years=c(1993:2018)


for(i in 1:length(years))
{
  print(paste0("Year_", years[i]))
  pr <- openProject(paste0(inpath, species, years[i]))

  # pr <- reopenProject(paste0(inpath, species, years[i]))

  bs <- getBaseline(pr, exportCSV=TRUE)

  supind=bs$outputData$SuperIndAbundance
  supind$year=rep(years[i], length(supind$Row))
  if(i==1) {superindv=supind} else {superindv=rbind(superindv,supind)}

  tempstrata=ddply(supind, .(serialnumber), summarize, Stratum=unique(Stratum))
  catch=bs$outputData$FilterBiotic$FilterBiotic_BioticData_catchsample.txt
  catch$year=rep(years[i], dim(catch)[1])
  dim(catch)
  catch=merge(catch, tempstrata, by.x="serialnumber", by.y="serialnumber", all.x=TRUE)
  dim(catch)
  catch$Stratum[is.na(catch$Stratum)]="fishingsetnotinsuperindv"
  if(i==1) {catches=catch} else {catches=rbind(catches, catch)}

  bs <- getBaseline(pr, modelType = "report")

  fmd=bs$outputData$FillMissingData
  fmd$year=rep(years[i], length(fmd$Row))
  if(i==1) {filmisdat=fmd} else {filmisdat=rbind(fmd, filmisdat)}

  # To avoid errors due to memory isues with java, close the pr object after each loop
  closeProject(pr)
}
save.image(file=paste0(outpath, species, "baseline/baseline_all_years.RData"))

# load(paste0(outpath, species, "baseline/baseline_all_years.RData"))

# Exclude data from strata 24, 25 and 26.
superindv=superindv[superindv$Stratum<24,]
filmisdat=filmisdat[filmisdat$Stratum<24,]
capturas=catches[catches$Stratum!="fishingsetnotinsuperindv" & catches$Stratum<24,]


#######################################
############## Catch in Kg  ###########
#######################################

catchcodkg=ddply(capturas, .(year), summarize, catchkg=round(sum(na.omit(catchweight)), digits=0))

write.csv(catchcodkg, paste0(outpath, species, "baseline/catch_kg/catch_in_kg_allyears.csv"), row.names=FALSE)

### Figure catch in kg
png(filename=paste0(outpath, species, "baseline/catch_kg/catch_in_kg.png"), width=4000, height=3000, res=400, pointsize=12)
par(mar=c(2,4,3,2))
plot(catchcodkg$year, catchcodkg$catchkg, type="b", pch=19, cex=0.75, lwd=2, col="darkred",xlab="Year", ylab=("Catch in kg"), main=paste0("Catch in kg COD"), cex.main=2, cex.lab=1.5, bty="l")
dev.off()

### Para Gadget
catchcodkg$area=rep(1, length(catchcodkg$year))
catchcodkg$fleet=rep("cod.survey_trawl", length(catchcodkg$year))

gadget_catchkg=catchcodkg[,c("year", "area", "fleet", "catchkg")]

write.csv(gadget_catchkg, paste0(outpath, species, "baseline/xa_gadget/cod.catch.sur.trawl.win.bar.kg.csv"), row.names=F)


## Number of fishing sets by year
prueba=ddply(capturas, .(year, serialnumber), summarize, samples=length(serialnumber))
test=data.frame(tapply(prueba$serialnumber, prueba$year, length))
test$year=rownames(test)
colnames(test)=c("nsets", "year")
plot(test$year, test$nsets, type="b")



###################################################################################################################################################
### Next, estimate the needed data and parameters (length distributions, length-weight relation, growth and maturation parameters)
###################################################################################################################################################

###############################
### Age-Length Distribution
###############################

# Remove some cases with NA in age
filmisdat=filmisdat[complete.cases(filmisdat$age),]

len1=c(0:159)
len2=rep(seq(from=2.5, to=160, by=5), each=5)
datlen=data.frame(len1, len2)

dim(filmisdat)
filmisdat=merge(filmisdat, datlen, by.x="LenGrp", by.y="len1")

agelengthdistr=ddply(filmisdat, .(year, age,len2), summarize, abundance=sum(Abundance), .drop=FALSE)
colnames(agelengthdistr)=c("year", "age" ,"length", "abundance")

write.csv(agelengthdistr, paste0(outpath, species, "baseline/age-length_distr/age-length_distr_allyears.csv"), row.names=FALSE)

agelengthdistr=agelengthdistr[with(agelengthdistr,order(year, age, length)),]

# observed proportions instead of total numbers
sumnum=ddply(agelengthdistr, .(year, age), summarize, sumnum=sum(abundance), .drop=FALSE)
agelengthdistr=merge(sumnum, agelengthdistr, by.x=c("year", "age"), by.y=c("year", "age"), all.x=TRUE)

agelengthdistr$prop=agelengthdistr$abundance/agelengthdistr$sumnum

agelengthdistr$prop[is.na(agelengthdistr$prop)]=0
agelengthdistrsumyear=ddply(agelengthdistr, .(year), summarize, sum=sum(prop))
agelengthdistrsumyear

agelengthdistr=agelengthdistr[with(agelengthdistr,order(year, age, length)),]

years=sort(unique(agelengthdistr$year))
cont_years=length(years)
for(i in 1:cont_years)
{
  png(filename = paste0(outpath, species, "baseline/age-length_distr/trawl_lendistr_year", years[i], ".png"), width = 3000, height = 2000, units = "px", pointsize = 12, bg = "white", res = 300)
  seldat=agelengthdistr[agelengthdistr$year==years[i],]
  ages=sort(unique(seldat$age))
  cont_ages=length(ages)
  for(e in 1:cont_ages)
  {
    sel=seldat[seldat$age==ages[e],]
    if(e==1) {
      plot(sel$prop+ages[e], sel$length, type="l", col="black", ylim=c(0,150), xlim=c(0,15), main=years[i], lty=1, pch=19, cex=0.4)
    } else {
      lines(sel$prop+ages[e], sel$length, type="l", col="black", lty=1, pch=19, cex=0.4)
    }
  }
  legend("topleft", legend=c("area1"), col=c("black"), lty=c(1), bty="n")
  dev.off()
}


##### Para Gadget
years=c(1993:2018)
lengthes=agelengthdistr$length
lengr=paste0("len_", lengthes-2.5, "-", lengthes+1.5, ".9")
area=rep(1,length(lengr))
age=agelengthdistr$age
step=rep(1, length(lengr))
year=agelengthdistr$year
number=agelengthdistr$abundance

gadget=data.frame(year=year, step=step, area=area, age=age, lengr=lengr, length=lengthes, number=number)

gadget=gadget[with(gadget, order(year, age, length)),]

gadget=gadget[,c("year", "step", "area", "age", "lengr", "number")]

gadget$number[is.na(gadget$number)]=0

gadget$number=round(gadget$number, digits=0)

write.csv(gadget, paste0(outpath, species, "baseline/xa_gadget/cod.sur.trawl.win.bar.ageldist.csv"), row.names=F)


##########################
### Length distribution
##########################
lengthdistr=ddply(filmisdat, .(year, length=as.integer(LengthCentimeter)), summarize, abundance=sum(Abundance), .drop=FALSE)

write.csv(lengthdistr, paste0(outpath, species, "baseline/length_distr/abundance_by_length_allyears.csv"), row.names=FALSE)

# Figure with length distribution by year
png(filename=paste0(outpath, species, "baseline/length_distr/abundance_by_length_allyears.png"), width=4000, height=4000, res=300, pointsize=12)
par(mar=c(2,4,3,2))
layout(matrix(c(1:30), 6, 5, byrow = TRUE))

for(i in 1:length(years))
{
  
  seldat=lengthdistr[lengthdistr$year==years[i],]
  
  plot(seldat$length, seldat$abundance/1000000, type="b", pch=19, cex=0.75, lwd=2, col="darkred",xlab="Length", ylab=expression(paste(plain("Abundance x10") ^{6}, plain(" individuals"))), main=paste0("Year ", years[i]), cex.main=2, cex.lab=1.5, bty="l")
}
dev.off()


##### Para Gadget

lengthes=lengthdistr$length
lengr=paste0("len_", lengthes, "-", lengthes, ".9")
area=rep(1,length(lengr))
age=rep("allages", length(lengr))
step=rep(1, length(lengr))

gadget=data.frame(year=lengthdistr$year, step=step, area=area, age=age, lengr=lengr, length=lengthes, number=lengthdistr$abundance)

gadget=gadget[with(gadget, order(year, length)),]

gadget=gadget[,c("year", "step", "area", "age", "lengr", "number")]

gadget$number[is.na(gadget$number)]=0

gadget$number=round(gadget$number, digits=0)

write.csv(gadget, paste0(outpath, species, "baseline/xa_gadget/cod.sur.trawl.win.bar.ldist.csv"), row.names=F)


##########################################
### Weight-length, model parameters
##########################################
mubis=superindv[complete.cases(superindv$IndividualWeightGram),]

### Check visually if there are apparent differences in the length-weight relation

groups=c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5,5)
years=sort(unique(mubis$year))
datagroups=data.frame(year=years, group=groups)

mubis=merge(mubis, datagroups, by.x="year", by.y="year", all.x=TRUE)

mean=ddply(mubis, .(group, LengthCentimeter), summarize, weight=mean(IndividualWeightGram))

png(filename=paste0(outpath, species, "baseline/length-weight/Mean_weight_at_length_group_years.png"), width=4000, height=4000, res=300, pointsize=12)
xyplot(weight/1000~LengthCentimeter, mean, groups=group, type="l", auto.key=T)
dev.off()


## Mean estimate of a and b for all years together
Weight=tapply(mubis$IndividualWeightGram/1000,mubis$LengthCentimeter,mean)
Length=sort(unique(mubis$LengthCentimeter))
Data=data.frame(Length, Weight)
model=nls(formula= Weight~a*(Length^b), data=Data, start=list(a=0.000003, b=3),trace=TRUE)
a=summary(model)$coefficients[1]
pvaluea=summary(model)$coefficients[7]
b=summary(model)$coefficients[2]
pvalueb=summary(model)$coefficients[8]
Data$Predict=predict(model)

png(filename=paste0(outpath, species, "baseline/length-weight/Pars_length_weight_allyears_together.png"), width=4000, height=4000, res=300, pointsize=12)
plot(mubis$LengthCentimeter, mubis$IndividualWeightGram/1000, xlab="Length", ylab="Weight")
points(Data$Length, Data$Weight, pch=19, col="blue", cex=1.3)
lines(Data2$Length, Data2$Predict, col="red", lwd=2)
text(paste0("a=", round(a, digits=10)), x=20, y=20, cex=1.5)
text(paste0("b=", round(b, digits=3)), x=20, y=18, cex=1.5)
dev.off()


# Fit length-weight model for each year separately
years=sort(unique(mubis$year))
for(i in 1:length(years))
{
  dat=mubis[mubis$year==years[i],]
  seldata=data.frame(year=rep(years[i], length(dat$IndividualWeightGram)), length=dat$LengthCentimeter, weight=(dat$IndividualWeightGram/1000))
  
  if(i==1) {lenweigdat=seldata} else {lenweigdat=rbind(lenweigdat,seldata)}
  
  Weight=tapply(seldata$weight,seldata$length,mean)
  Length=sort(unique(seldata$length))
  Data=data.frame(Length, Weight)
  model=nls(formula= Weight~a*(Length^b), data=Data, start=list(a=0.000003, b=3),trace=TRUE)
  a=summary(model)$coefficients[1]
  pvaluea=summary(model)$coefficients[7]
  b=summary(model)$coefficients[2]
  pvalueb=summary(model)$coefficients[8]
  fit=data.frame(year=years[i], a=a, b=b, pvaluea=pvaluea, pvalueb=pvalueb)
  
  if(i==1) {fitlenweig=fit} else {fitlenweig=rbind(fitlenweig,fit)}
  
  Data$Predict=predict(model)
  Data$Year=rep(years[i], length(Data$Weight))
  
  if (i==1) {predictlenweig=Data} else {predictlenweig=rbind(predictlenweig, Data)}
}

write.csv(lenweigdat, paste0(outpath, species, "baseline/length-weight/lenweig_data_allyears.csv"), row.names=FALSE)
write.csv(fitlenweig, paste0(outpath, species, "baseline/length-weight/params_lenweig_model_allyears.csv"), row.names=FALSE)
write.csv(predictlenweig, paste0(outpath, species, "baseline/length-weight/predict_lenweig_allyears.csv"), row.names=FALSE)


## Figures weight-length per individual and year with fitted model
png(filename=paste0(outpath, species, "baseline/length-weight/length_weight_allyears.png"), width=4000, height=4000, res=300, pointsize=12)
par(mar=c(2,4,3,2))
layout(matrix(c(1:30), 6, 5, byrow = TRUE))

for(i in 1:length(years))
{
  dataindv=lenweigdat[lenweigdat$year==years[i],]
  datapredict=predictlenweig[predictlenweig$Year==years[i],]
  a_par=fitlenweig[fitlenweig$year==years[i],"a"]
  b_par=fitlenweig[fitlenweig$year==years[i],"b"]
  plot(dataindv$length, dataindv$weight, pch=19, cex=0.75, lwd=2, col="grey70",xlab="Length (cm)", ylab="Weight (kg)", main=paste0("Year ", years[i]), cex.main=2, cex.lab=1.5, bty="l")
  lines(datapredict$Length, datapredict$Predict, col="darkred", lwd=2)
  text(paste0("a=", round(a_par, digits=8)), y=max(datapredict$Predict)/2, x=max(datapredict$Length)/3)
  text(paste0("b=", round(b_par, digits=3)), y=max(datapredict$Predict)/2.8, x=max(datapredict$Length)/3.5)
}

dev.off()


png(filename=paste0(outpath, species, "baseline/length-weight/pars_length_weight_allyears.png"), width=4000, height=4000, res=300, pointsize=12)

par(mar = c(15,12,6,10))

plot(fitlenweig$year, fitlenweig$a, type="b", col="black", pch=19, xlab="Year", ylab="a pars", las=1, cex.axis=2.5)
mtext("a parameter",side=4,line=-78.5,cex=2.5,las=3)
par(new=TRUE,cex.axis=2.5,las=1)
plot(fitlenweig$year, fitlenweig$b, type="b", col="red", pch=19, xlab="", ylab="", xaxt="n", yaxt="n")
legend("topleft", legend=c("a","b"),col=c("black","red"),pch=c(19,19),bty="n",lwd=1,lty=c(1),cex=2.5)
axis(4,cex.axis=2.5,cex.lab=3.5,mgp=c(4,2,0))
mtext("b parameter",side=4,line=6,cex=2.5,las=3)
dev.off()



####################################################
### Length at age. Growth model parameters Males and Females
####################################################
mubis=superindv[complete.cases(superindv$age, superindv$sex),]

### Check visually if there are apparent differences in the length at age relation

groups=c(1,1,1,1,1,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5,6,6)
years=sort(unique(mubis$year))
datagroups=data.frame(year=years, group=groups)

mubis=merge(mubis, datagroups, by.x="year", by.y="year", all.x=TRUE)

mean=ddply(mubis, .(group, age), summarize, length=mean(LengthCentimeter))

png(filename=paste0(outpath, species, "baseline/growth_model_males_females/Mean_length_at_age_group_years.png"), width=4000, height=4000, res=300, pointsize=12)
xyplot(length~age, mean, groups=group, type="l", auto.key=T)
dev.off()

# Next, mean length at age over time
Length=data.frame(tapply(mubis$LengthCentimeter,list(mubis$year, mubis$age),mean))
colnames(Length)=sort(unique(mubis$age))
paramax=Length
paramax[is.na(paramax)]=0
maxlen=max(paramax)

Length$year=years


png(filename=paste0(outpath, species, "baseline/growth_model_males_females/Mean_length_at_age_over_the_years.png"), width=4000, height=3000, res=450, pointsize=12)
for(i in 1:(dim(Length)[2]-1))
{
  sequence=seq(from=20, to=length(colors()), by=10)[-(14:34)]
  colores=colors()[sequence]
  age=i
  length=Length[,i]
  year=Length$year
  if(i==1) {
    plot(year, length, type="b", pch=19, ylim=c(0,maxlen+20), col=colores[i], bty="l")} else {
      points(year, length, type="b", pch=19, col=colores[i])
    }
  legend("topleft", legend=paste0("age ", c(1:17)), col=colores[1:17], ncol=7, bty="n", pch=19, lty=1)
}
dev.off()


# Next, all age-length data for all years with a general growth model fitted to the mean length at age 
Length=tapply(mubis$LengthCentimeter,mubis$age,mean)
Age=sort(unique(mubis$age))
Data=data.frame(Age,Length)
model=nls(formula= Length~Linf*(1-(exp(-K*(Age-T0)))), data=Data, start=list(K=0.12, Linf=180, T0=0),trace=TRUE)
K=summary(model)$coefficients[1]
pvalueK=summary(model)$coefficients[10]
Linf=summary(model)$coefficients[2]
pvalueLinf=summary(model)$coefficients[11]
T0=summary(model)$coefficients[3]
pvalueT0=summary(model)$coefficients[12]
L0=Linf*(1-(exp(-K*(1-T0))))
Data$Predict=predict(model)

png(filename=paste0(outpath, species, "baseline/growth_model_males_females/growth_params_all_years.png"), width=4000, height=4000, res=300, pointsize=12)
plot(mubis$age, mubis$LengthCentimeter, xlab="Age", ylab="Length", cex=1.3, cex.axis=1.5, cex.lab=1.8, bty="l")
points(Data$Age, Data$Length, pch=19, col="blue", cex=1.3)
lines(Data$Age, Data$Predict, col="red", lwd=2)
text(paste0("Linf=", round(Linf, digits=2)), x=3, y=130, cex=1.5)
text(paste0("K=", round(K, digits=5)), x=3, y=120, cex=1.5)
text(paste0("T0=", round(T0, digits=5)), x=3, y=110, cex=1.5)
text(paste0("L0=", round(L0, digits=5)), x=3, y=100, cex=1.5)
dev.off()

# Finally, a growth model fit to each year separately
years=sort(unique(mubis$year))
for(i in 1:length(years))
{
  dat=mubis[mubis$year==years[i],]
  seldata=data.frame(year=dat$year, age=dat$age, length=dat$LengthCentimeter)
  
  if (i==1) {agedata=seldata} else {agedata=rbind(agedata, seldata)}
  
  if(years[i]==1986 | years[i]==1987 | years[i]==1988 | years[i]==1989 | years[i]==1999) {print("no possible fit")} else {
    Length=tapply(seldata$length,seldata$age,mean)
    Age=sort(unique(seldata$age))
    Data=data.frame(Age,Length)
    model=nls(formula= Length~Linf*(1-(exp(-K*Age))), data=Data, start=list(K=0.1, Linf=140),trace=TRUE, nls.control(maxiter = 1000, tol = 1e-05, minFactor = 1/1024,printEval = FALSE, warnOnly = FALSE))
    K=summary(model)$coefficients[1]
    pvalueK=summary(model)$coefficients[10]
    Linf=summary(model)$coefficients[2]
    pvalueLinf=summary(model)$coefficients[11]
    T0=summary(model)$coefficients[3]
    pvalueT0=summary(model)$coefficients[12]
    L0=Linf*(1-(exp(-K*(1-T0))))
    fit=data.frame(year=years[i], K=K, Linf=Linf, T0=T0, L0=L0, pvalueK=pvalueK, pvalueLinf=pvalueLinf, pvalueT0=pvalueT0)}
  
  if(i==1) {fitgrowth=fit} else {fitgrowth=rbind(fitgrowth,fit)}
  
  Data$Predict=predict(model)
  Data$Year=rep(years[i], length(Data$Age))
  
  if (i==1) {predictgrowth=Data} else {predictgrowth=rbind(predictgrowth, Data)}
}

write.csv(agedata, paste0(outpath, species, "baseline/growth_model_males_females/agedata_allyears.csv"), row.names=FALSE)
write.csv(fitgrowth, paste0(outpath, species, "baseline/growth_model_males_females/params_growth_model_allyears.csv"), row.names=FALSE)
write.csv(predictgrowth, paste0(outpath, species, "baseline/growth_model_males_females/predict_growth_allyears.csv"), row.names=FALSE)


### Figure mean length at age and fitted model
years=c(1993:2018)
png(filename=paste0(outpath, species, "baseline/growth_model_males_females/length_at_age_&_growthmodel_allyears.png"), width=4000, height=4000, res=300, pointsize=12)
par(mar=c(2,4,3,2))
layout(matrix(c(1:30), 6, 5, byrow = TRUE))

for(i in 1:length(years))
{
  if(years[i]==1986 | years[i]==1987 | years[i]==1988 | years[i]==1989 | years[i]==1999) {
    seldata1=agedata[agedata$year==years[i],]
    plot(seldata1$age,seldata1$length,col="grey50",main=years[i],pch=19, ylim=c(0,max(na.omit(agedata$length))), xlim=c(0, max(na.omit(agedata$age))))
    legend("topleft",legend=c("Observed"),col=c("grey50"),bty="n",lwd=1.5,lty=c(1),cex=1)} else {
      seldata1=agedata[agedata$year==years[i],]
      plot(seldata1$age,seldata1$length,col="grey50",main=years[i], ylim=c(0,max(na.omit(agedata$length))), xlim=c(0, max(na.omit(agedata$age))))
      seldata2=predictgrowth[predictgrowth$Year==years[i],]
      points(seldata2$Age, seldata2$Length, pch=19, col="black")
      lines(seldata2$Age,seldata2$Predict,type="l",col="red",pch=19)
      legend("topleft",legend=c("Observed","Predicted"),col=c("grey50","red"),bty="n",lwd=1.5,lty=c(1,1),cex=1)
      text(paste0("Linf=", round(fitgrowth[fitgrowth$year==years[i], "Linf"], digits=2)), x=12, y=50)
      text(paste0("K=", round(fitgrowth[fitgrowth$year==years[i], "K"], digits=4)), x=12, y=30)
      
    }
}
dev.off()


#######################################################################
#### Modelo simple von Bertalanffy tal y como se presenta en gadget
# Next, all age-length data for all years with a general growth model fitted to the mean length at age 
Length=tapply(mubis$LengthCentimeter,mubis$age,mean)
IncrL=Length[-c(1,18)]-Length[-c(17,18)]
Age=sort(unique(mubis$age))
Data=data.frame(Age=Age[-c(17,18)],Length=Length[-c(17,18)], IncrL)
model=nls(formula= IncrL~(Linf-Length)*(1-(exp(-K))), data=Data, start=list(K=0.12, Linf=180),trace=TRUE)
K=summary(model)$coefficients[1]
pvalueK=summary(model)$coefficients[7]
Linf=summary(model)$coefficients[2]
pvalueLinf=summary(model)$coefficients[8]
Data$Predict=predict(model)

png(filename=paste0(outpath, species, "baseline/growth_model_males_females/growth_params_all_years_modelo_von_Bertalanfy_gadget.png"), width=4000, height=4000, res=300, pointsize=12)
plot(Data$Age, Data$IncrL, xlab="Age", ylab="Length", cex=1.3, cex.axis=1.5, cex.lab=1.8, bty="l")
lines(Data$Age, Data$Predict, col="red", lwd=2)
text(paste0("Linf=", round(Linf, digits=2)), x=3, y=6, cex=1.5)
text(paste0("K=", round(K, digits=5)), x=3, y=5, cex=1.5)
dev.off()



#### Estimate standard deviation in lenght at age

stdev=ddply(mubis, .(age), summarize, stdev=round(sd(LengthCentimeter), digits=2))
write.csv(stdev, paste0(outpath, species, "baseline/growth_model_males_females/stdev_length_at_age.csv"), row.names=F)






####################################################
### Length at age. Growth model parameters Females
####################################################
mubis=superindv[complete.cases(superindv$age, superindv$sex),]
mubis=mubis[mubis$sex==1,]


### Check visually if there are apparent differences in the length at age relation

groups=c(1,1,1,1,1,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5,6,6)
years=sort(unique(mubis$year))
datagroups=data.frame(year=years, group=groups)

mubis=merge(mubis, datagroups, by.x="year", by.y="year", all.x=TRUE)

mean=ddply(mubis, .(group, age), summarize, length=mean(LengthCentimeter))

png(filename=paste0(outpath, species, "baseline/growth_model_females/Mean_length_at_age_group_years.png"), width=4000, height=4000, res=300, pointsize=12)
xyplot(length~age, mean, groups=group, type="l", auto.key=T)
dev.off()

# Next, mean length at age over time
Length=data.frame(tapply(mubis$LengthCentimeter,list(mubis$year, mubis$age),mean))
colnames(Length)=sort(unique(mubis$age))
paramax=Length
paramax[is.na(paramax)]=0
maxlen=max(paramax)

Length$year=years


png(filename=paste0(outpath, species, "baseline/growth_model_females/Mean_length_at_age_over_the_years.png"), width=4000, height=3000, res=450, pointsize=12)
for(i in 1:(dim(Length)[2]-1))
{
  sequence=seq(from=20, to=length(colors()), by=10)[-(14:34)]
  colores=colors()[sequence]
  age=i
  length=Length[,i]
  year=Length$year
  if(i==1) {
    plot(year, length, type="b", pch=19, ylim=c(0,maxlen+20), col=colores[i], bty="l")} else {
      points(year, length, type="b", pch=19, col=colores[i])
    }
  legend("topleft", legend=paste0("age ", c(1:17)), col=colores[1:17], ncol=7, bty="n", pch=19, lty=1)
}
dev.off()


# Next, all age-length data for all years with a general growth model fitted to the mean length at age 
Length=tapply(mubis$LengthCentimeter,mubis$age,mean)
Age=sort(unique(mubis$age))
Data=data.frame(Age,Length)
model=nls(formula= Length~Linf*(1-(exp(-K*(Age-T0)))), data=Data, start=list(K=0.12, Linf=180, T0=0),trace=TRUE)
K=summary(model)$coefficients[1]
pvalueK=summary(model)$coefficients[10]
Linf=summary(model)$coefficients[2]
pvalueLinf=summary(model)$coefficients[11]
T0=summary(model)$coefficients[3]
pvalueT0=summary(model)$coefficients[12]
L0=Linf*(1-(exp(-K*(1-T0))))
Data$Predict=predict(model)

png(filename=paste0(outpath, species, "baseline/growth_model_females/growth_params_all_years.png"), width=4000, height=4000, res=300, pointsize=12)
plot(mubis$age, mubis$LengthCentimeter, xlab="Age", ylab="Length", cex=1.3, cex.axis=1.5, cex.lab=1.8, bty="l")
points(Data$Age, Data$Length, pch=19, col="blue", cex=1.3)
lines(Data$Age, Data$Predict, col="red", lwd=2)
text(paste0("Linf=", round(Linf, digits=2)), x=3, y=130, cex=1.5)
text(paste0("K=", round(K, digits=5)), x=3, y=120, cex=1.5)
text(paste0("T0=", round(T0, digits=5)), x=3, y=110, cex=1.5)
text(paste0("L0=", round(L0, digits=5)), x=3, y=100, cex=1.5)
dev.off()

# Finally, a growth model fit to each year separately
years=sort(unique(mubis$year))
for(i in 1:length(years))
{
  dat=mubis[mubis$year==years[i],]
  seldata=data.frame(year=dat$year, age=dat$age, length=dat$LengthCentimeter)
  
  if (i==1) {agedata=seldata} else {agedata=rbind(agedata, seldata)}
  
  if(years[i]==1986 | years[i]==1987 | years[i]==1988 | years[i]==1989 | years[i]==1999) {print("no possible fit")} else {
    Length=tapply(seldata$length,seldata$age,mean)
    Age=sort(unique(seldata$age))
    Data=data.frame(Age,Length)
    model=nls(formula= Length~Linf*(1-(exp(-K*Age))), data=Data, start=list(K=0.1, Linf=140),trace=TRUE, nls.control(maxiter = 1000, tol = 1e-05, minFactor = 1/1024,printEval = FALSE, warnOnly = FALSE))
    K=summary(model)$coefficients[1]
    pvalueK=summary(model)$coefficients[10]
    Linf=summary(model)$coefficients[2]
    pvalueLinf=summary(model)$coefficients[11]
    T0=summary(model)$coefficients[3]
    pvalueT0=summary(model)$coefficients[12]
    L0=Linf*(1-(exp(-K*(1-T0))))
    fit=data.frame(year=years[i], K=K, Linf=Linf, T0=T0, L0=L0, pvalueK=pvalueK, pvalueLinf=pvalueLinf, pvalueT0=pvalueT0)}
  
  if(i==1) {fitgrowth=fit} else {fitgrowth=rbind(fitgrowth,fit)}
  
  Data$Predict=predict(model)
  Data$Year=rep(years[i], length(Data$Age))
  
  if (i==1) {predictgrowth=Data} else {predictgrowth=rbind(predictgrowth, Data)}
}

write.csv(agedata, paste0(outpath, species, "baseline/growth_model_females/agedata_allyears.csv"), row.names=FALSE)
write.csv(fitgrowth, paste0(outpath, species, "baseline/growth_model_females/params_growth_model_allyears.csv"), row.names=FALSE)
write.csv(predictgrowth, paste0(outpath, species, "baseline/growth_model_females/predict_growth_allyears.csv"), row.names=FALSE)


### Figure mean length at age and fitted model
years=c(1980:2018)
png(filename=paste0(outpath, species, "baseline/growth_model_females/length_at_age_&_growthmodel_allyears.png"), width=4000, height=4000, res=300, pointsize=12)
par(mar=c(2,4,3,2))
layout(matrix(c(1:30), 6, 5, byrow = TRUE))

for(i in 1:length(years))
{
  if(years[i]==1986 | years[i]==1987 | years[i]==1988 | years[i]==1989 | years[i]==1999) {
    seldata1=agedata[agedata$year==years[i],]
    plot(seldata1$age,seldata1$length,col="grey50",main=years[i],pch=19, ylim=c(0,max(na.omit(agedata$length))), xlim=c(0, max(na.omit(agedata$age))))
    legend("topleft",legend=c("Observed"),col=c("grey50"),bty="n",lwd=1.5,lty=c(1),cex=1)} else {
      seldata1=agedata[agedata$year==years[i],]
      plot(seldata1$age,seldata1$length,col="grey50",main=years[i], ylim=c(0,max(na.omit(agedata$length))), xlim=c(0, max(na.omit(agedata$age))))
      seldata2=predictgrowth[predictgrowth$Year==years[i],]
      points(seldata2$Age, seldata2$Length, pch=19, col="black")
      lines(seldata2$Age,seldata2$Predict,type="l",col="red",pch=19)
      legend("topleft",legend=c("Observed","Predicted"),col=c("grey50","red"),bty="n",lwd=1.5,lty=c(1,1),cex=1)
      text(paste0("Linf=", round(fitgrowth[fitgrowth$year==years[i], "Linf"], digits=2)), x=12, y=50)
      text(paste0("K=", round(fitgrowth[fitgrowth$year==years[i], "K"], digits=4)), x=12, y=30)
      
    }
}
dev.off()


#######################################################################
#### Modelo simple von Bertalanffy tal y como se presenta en gadget
# Next, all age-length data for all years with a general growth model fitted to the mean length at age 
Length=tapply(mubis$LengthCentimeter,mubis$age,mean)
IncrL=Length[-c(1,18)]-Length[-c(17,18)]
Age=sort(unique(mubis$age))
Data=data.frame(Age=Age[-c(17,18)],Length=Length[-c(17,18)], IncrL)
model=nls(formula= IncrL~(Linf-Length)*(1-(exp(-K))), data=Data, start=list(K=0.12, Linf=180),trace=TRUE)
K=summary(model)$coefficients[1]
pvalueK=summary(model)$coefficients[7]
Linf=summary(model)$coefficients[2]
pvalueLinf=summary(model)$coefficients[8]
Data$Predict=predict(model)

png(filename=paste0(outpath, species, "baseline/growth_model_females/growth_params_all_years_modelo_von_Bertalanfy_gadget.png"), width=4000, height=4000, res=300, pointsize=12)
plot(Data$Age, Data$IncrL, xlab="Age", ylab="Length", cex=1.3, cex.axis=1.5, cex.lab=1.8, bty="l")
lines(Data$Age, Data$Predict, col="red", lwd=2)
text(paste0("Linf=", round(Linf, digits=2)), x=3, y=6, cex=1.5)
text(paste0("K=", round(K, digits=5)), x=3, y=5, cex=1.5)
dev.off()



#### Estimate standard deviation in lenght at age

stdev=ddply(mubis, .(age), summarize, stdev=round(sd(LengthCentimeter), digits=2))
write.csv(stdev, paste0(outpath, species, "baseline/growth_model_females/stdev_length_at_age.csv"), row.names=F)





#################################################
### Length at age. Growth model parameters Males
#################################################
mubis=superindv[complete.cases(superindv$age, superindv$sex),]
mubis=mubis[mubis$sex==2,]

### Check visually if there are apparent differences in the length at age relation

groups=c(1,1,1,1,1,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5,6,6)
years=sort(unique(mubis$year))
datagroups=data.frame(year=years, group=groups)

mubis=merge(mubis, datagroups, by.x="year", by.y="year", all.x=TRUE)

mean=ddply(mubis, .(group, age), summarize, length=mean(LengthCentimeter))

png(filename=paste0(outpath, species, "baseline/growth_model_males/Mean_length_at_age_group_years.png"), width=4000, height=4000, res=300, pointsize=12)
xyplot(length~age, mean, groups=group, type="l", auto.key=T)
dev.off()

# Next, mean length at age over time
Length=data.frame(tapply(mubis$LengthCentimeter,list(mubis$year, mubis$age),mean))
colnames(Length)=sort(unique(mubis$age))
paramax=Length
paramax[is.na(paramax)]=0
maxlen=max(paramax)

Length$year=years


png(filename=paste0(outpath, species, "baseline/growth_model_males/Mean_length_at_age_over_the_years.png"), width=4000, height=3000, res=450, pointsize=12)
for(i in 1:(dim(Length)[2]-1))
{
  sequence=seq(from=20, to=length(colors()), by=10)[-(14:34)]
  colores=colors()[sequence]
  age=i
  length=Length[,i]
  year=Length$year
  if(i==1) {
    plot(year, length, type="b", pch=19, ylim=c(0,maxlen+20), col=colores[i], bty="l")} else {
      points(year, length, type="b", pch=19, col=colores[i])
    }
  legend("topleft", legend=paste0("age ", c(1:17)), col=colores[1:17], ncol=7, bty="n", pch=19, lty=1)
}
dev.off()


# Next, all age-length data for all years with a general growth model fitted to the mean length at age 
Length=tapply(mubis$LengthCentimeter,mubis$age,mean)
Age=sort(unique(mubis$age))
Data=data.frame(Age,Length)
model=nls(formula= Length~Linf*(1-(exp(-K*(Age-T0)))), data=Data, start=list(K=0.12, Linf=180, T0=0),trace=TRUE)
K=summary(model)$coefficients[1]
pvalueK=summary(model)$coefficients[10]
Linf=summary(model)$coefficients[2]
pvalueLinf=summary(model)$coefficients[11]
T0=summary(model)$coefficients[3]
pvalueT0=summary(model)$coefficients[12]
L0=Linf*(1-(exp(-K*(1-T0))))
Data$Predict=predict(model)

png(filename=paste0(outpath, species, "baseline/growth_model_males/growth_params_all_years.png"), width=4000, height=4000, res=300, pointsize=12)
plot(mubis$age, mubis$LengthCentimeter, xlab="Age", ylab="Length", cex=1.3, cex.axis=1.5, cex.lab=1.8, bty="l")
points(Data$Age, Data$Length, pch=19, col="blue", cex=1.3)
lines(Data$Age, Data$Predict, col="red", lwd=2)
text(paste0("Linf=", round(Linf, digits=2)), x=3, y=130, cex=1.5)
text(paste0("K=", round(K, digits=5)), x=3, y=120, cex=1.5)
text(paste0("T0=", round(T0, digits=5)), x=3, y=110, cex=1.5)
text(paste0("L0=", round(L0, digits=5)), x=3, y=100, cex=1.5)
dev.off()

# Finally, a growth model fit to each year separately
years=sort(unique(mubis$year))
for(i in 1:length(years))
{
  dat=mubis[mubis$year==years[i],]
  seldata=data.frame(year=dat$year, age=dat$age, length=dat$LengthCentimeter)
  
  if (i==1) {agedata=seldata} else {agedata=rbind(agedata, seldata)}
  
  if(years[i]==1986 | years[i]==1987 | years[i]==1988 | years[i]==1989 | years[i]==1999) {print("no possible fit")} else {
    Length=tapply(seldata$length,seldata$age,mean)
    Age=sort(unique(seldata$age))
    Data=data.frame(Age,Length)
    model=nls(formula= Length~Linf*(1-(exp(-K*Age))), data=Data, start=list(K=0.1, Linf=140),trace=TRUE, nls.control(maxiter = 1000, tol = 1e-05, minFactor = 1/1024,printEval = FALSE, warnOnly = FALSE))
    K=summary(model)$coefficients[1]
    pvalueK=summary(model)$coefficients[10]
    Linf=summary(model)$coefficients[2]
    pvalueLinf=summary(model)$coefficients[11]
    T0=summary(model)$coefficients[3]
    pvalueT0=summary(model)$coefficients[12]
    L0=Linf*(1-(exp(-K*(1-T0))))
    fit=data.frame(year=years[i], K=K, Linf=Linf, T0=T0, L0=L0, pvalueK=pvalueK, pvalueLinf=pvalueLinf, pvalueT0=pvalueT0)}
  
  if(i==1) {fitgrowth=fit} else {fitgrowth=rbind(fitgrowth,fit)}
  
  Data$Predict=predict(model)
  Data$Year=rep(years[i], length(Data$Age))
  
  if (i==1) {predictgrowth=Data} else {predictgrowth=rbind(predictgrowth, Data)}
}

write.csv(agedata, paste0(outpath, species, "baseline/growth_model_males/agedata_allyears.csv"), row.names=FALSE)
write.csv(fitgrowth, paste0(outpath, species, "baseline/growth_model_males/params_growth_model_allyears.csv"), row.names=FALSE)
write.csv(predictgrowth, paste0(outpath, species, "baseline/growth_model_males/predict_growth_allyears.csv"), row.names=FALSE)


### Figure mean length at age and fitted model
years=c(1980:2018)
png(filename=paste0(outpath, species, "baseline/growth_model_males/length_at_age_&_growthmodel_allyears.png"), width=4000, height=4000, res=300, pointsize=12)
par(mar=c(2,4,3,2))
layout(matrix(c(1:30), 6, 5, byrow = TRUE))

for(i in 1:length(years))
{
  if(years[i]==1986 | years[i]==1987 | years[i]==1988 | years[i]==1989 | years[i]==1999) {
    seldata1=agedata[agedata$year==years[i],]
    plot(seldata1$age,seldata1$length,col="grey50",main=years[i],pch=19, ylim=c(0,max(na.omit(agedata$length))), xlim=c(0, max(na.omit(agedata$age))))
    legend("topleft",legend=c("Observed"),col=c("grey50"),bty="n",lwd=1.5,lty=c(1),cex=1)} else {
      seldata1=agedata[agedata$year==years[i],]
      plot(seldata1$age,seldata1$length,col="grey50",main=years[i], ylim=c(0,max(na.omit(agedata$length))), xlim=c(0, max(na.omit(agedata$age))))
      seldata2=predictgrowth[predictgrowth$Year==years[i],]
      points(seldata2$Age, seldata2$Length, pch=19, col="black")
      lines(seldata2$Age,seldata2$Predict,type="l",col="red",pch=19)
      legend("topleft",legend=c("Observed","Predicted"),col=c("grey50","red"),bty="n",lwd=1.5,lty=c(1,1),cex=1)
      text(paste0("Linf=", round(fitgrowth[fitgrowth$year==years[i], "Linf"], digits=2)), x=12, y=50)
      text(paste0("K=", round(fitgrowth[fitgrowth$year==years[i], "K"], digits=4)), x=12, y=30)
      
    }
}
dev.off()


#######################################################################
#### Modelo simple von Bertalanffy tal y como se presenta en gadget
# Next, all age-length data for all years with a general growth model fitted to the mean length at age 
Length=tapply(mubis$LengthCentimeter,mubis$age,mean)
IncrL=Length[-c(1,18)]-Length[-c(17,18)]
Age=sort(unique(mubis$age))
Data=data.frame(Age=Age[-c(17,18)],Length=Length[-c(17,18)], IncrL)
model=nls(formula= IncrL~(Linf-Length)*(1-(exp(-K))), data=Data, start=list(K=0.12, Linf=180),trace=TRUE)
K=summary(model)$coefficients[1]
pvalueK=summary(model)$coefficients[7]
Linf=summary(model)$coefficients[2]
pvalueLinf=summary(model)$coefficients[8]
Data$Predict=predict(model)

png(filename=paste0(outpath, species, "baseline/growth_model_males/growth_params_all_years_modelo_von_Bertalanfy_gadget.png"), width=4000, height=4000, res=300, pointsize=12)
plot(Data$Age, Data$IncrL, xlab="Age", ylab="Length", cex=1.3, cex.axis=1.5, cex.lab=1.8, bty="l")
lines(Data$Age, Data$Predict, col="red", lwd=2)
text(paste0("Linf=", round(Linf, digits=2)), x=3, y=6, cex=1.5)
text(paste0("K=", round(K, digits=5)), x=3, y=5, cex=1.5)
dev.off()



#### Estimate standard deviation in lenght at age

stdev=ddply(mubis, .(age), summarize, stdev=round(sd(LengthCentimeter), digits=2))
write.csv(stdev, paste0(outpath, species, "baseline/growth_model_males/stdev_length_at_age.csv"), row.names=F)





# ##############################################################
# ### Maturity at length and age Females
# ##############################################################
# mubis=superindv[complete.cases(superindv$maturationstage),]
# mubis=mubis[mubis$sex==1,]
# mubis=subset(mubis, maturationstage<5)
# 
# years=sort(unique(mubis$year))
# for(i in 1:length(years))
# {
#   seldata=mubis[mubis$year==years[i],]
#   matdat=data.frame(age=seldata$age, length=seldata$LengthCentimeter, stage=ifelse(seldata$maturationstage==1, 0, 1))
#   matdat$year=rep(years[i], length(matdat$length))
# 
#   if(i==1) {matdata=matdat} else {matdata=rbind(matdata,matdat)}
# 
#   Logit<-glm(stage~length + age, family=binomial,data=matdata)
#   InterceptLength= Logit$coefficients[1]
#   Lengthres= Logit$coefficients[2]
#   Ageres= Logit$coefficients[3]
#   Year=years[i]
#   fitmat=data.frame(year=Year, intercept=InterceptLength, lengthpar=Lengthres, agepar=Ageres)
# 
#   if(i==1) {fitmatogive=fitmat} else {fitmatogive=rbind(fitmatogive,fitmat)}
# 
#   newdata=data.frame(expand.grid(length=c(0:160), age=c(1:20)))
#   newdata$predict=predict(Logit, newdata, type="response")
#   newdata$year=rep(years[i], length(newdata$length))
# 
#   if (i==1) {predictmat=newdata} else {predictmat=rbind(predictmat, newdata)}
# 
# }
# 
# write.csv(matdata, paste0(outpath, species, "baseline/maturity_ogive_length-age/maturity_ogive_females/data_maturity_allyears.csv"), row.names=FALSE)
# write.csv(fitmatogive, paste0(outpath, species, "baseline/maturity_ogive_length-age/maturity_ogive_females/params_maturity_ogive_allyears.csv"), row.names=FALSE)
# write.csv(predictmat, paste0(outpath, species, "baseline/maturity_ogive_length-age/maturity_ogive_females/predict_maturity_ogive_allyears.csv"), row.names=FALSE)
# 
# ### Figure maturation ogive by length and age
# years=sort(unique(predictmat$year))
# for(i in 1:length(years))
# {
#     selecdat=predictmat[predictmat$year==years[i],]
#     ages=sort(unique(selecdat$age))
#     png(filename=paste0(outpath, species, "baseline/maturity_ogive_length-age/maturity_ogive_females/maturity_ogive_by_length_and_age_", years[i], ".png"), width=4000, height=3000, res=400, pointsize=12)
#     for(e in 1:length(ages))
#     {
#     seldat=selecdat[selecdat$age==ages[e],]
#     seldat=seldat[with(seldat, order(length)),]
#     if(e==1) {plot(seldat$length, seldat$predict, type="l", col=paste0("gray", round((e*5),digits=0)), lwd=2.5, xlab="Length (cm)", ylab="Proportion mature", ylim=c(0,1))} else {
#         lines(seldat$length, seldat$predict, type="l", col=paste0("gray", round((e*5),digits=0)), lwd=2.5)}
#     }
#     dev.off()
# }
# 
# 
# ### Para Gadget
# 
# predictmat=predictmat[predictmat$age>2,]
# 
# # First the immature stock
# predictmat$step=rep(1,dim(predictmat)[1])
# predictmat$area=rep(1,dim(predictmat)[1])
# predictmat$stock=rep("cod.imm",dim(predictmat)[1])
# predictmat$age=paste0("age", predictmat$age)
# predictmat$lengr=paste0("len_", predictmat$length, "-", predictmat$length, ".9")
# predictmat$proportion=round(1-predictmat$predict, digits=5)
# 
# head(predictmat)
# 
# gadget_I=predictmat[,c("year", "step", "area", "stock","age", "lengr", "proportion")]
# 
# # Next the mature stock
# predictmat$stock=rep("cod.mat",dim(predictmat)[1])
# predictmat$proportion=round(predictmat$predict, digits=5)
# 
# head(predictmat)
# 
# gadget_II=predictmat[,c("year", "step", "area", "stock","age", "lengr", "proportion")]
# 
# 
# gadget=rbind(gadget_I, gadget_II)
# 
# write.csv(gadget, paste0(outpath, species, "baseline/xa_gadget/cod.sur.trawl.win.bar.female.ogive.csv"), row.names=F)
# 
# 
# 
# 
# ##############################################################
# ### Maturity at length and age Males
# ##############################################################
# mubis=superindv[complete.cases(superindv$maturationstage),]
# mubis=mubis[mubis$sex==2,]
# mubis=subset(mubis, maturationstage<5)
# 
# years=sort(unique(mubis$year))
# for(i in 1:length(years))
# {
#   seldata=mubis[mubis$year==years[i],]
#   matdat=data.frame(age=seldata$age, length=seldata$LengthCentimeter, stage=ifelse(seldata$maturationstage==1, 0, 1))
#   matdat$year=rep(years[i], length(matdat$length))
#   
#   if(i==1) {matdata=matdat} else {matdata=rbind(matdata,matdat)}
#   
#   Logit<-glm(stage~length + age, family=binomial,data=matdata)
#   InterceptLength= Logit$coefficients[1]
#   Lengthres= Logit$coefficients[2]
#   Ageres= Logit$coefficients[3]
#   Year=years[i]
#   fitmat=data.frame(year=Year, intercept=InterceptLength, lengthpar=Lengthres, agepar=Ageres)
#   
#   if(i==1) {fitmatogive=fitmat} else {fitmatogive=rbind(fitmatogive,fitmat)}
#   
#   newdata=data.frame(expand.grid(length=c(0:160), age=c(1:20)))
#   newdata$predict=predict(Logit, newdata, type="response")
#   newdata$year=rep(years[i], length(newdata$length))
#   
#   if (i==1) {predictmat=newdata} else {predictmat=rbind(predictmat, newdata)}
#   
# }
# 
# write.csv(matdata, paste0(outpath, species, "baseline/maturity_ogive_length-age/maturity_ogive_males/data_maturity_allyears.csv"), row.names=FALSE)
# write.csv(fitmatogive, paste0(outpath, species, "baseline/maturity_ogive_length-age/maturity_ogive_males/params_maturity_ogive_allyears.csv"), row.names=FALSE)
# write.csv(predictmat, paste0(outpath, species, "baseline/maturity_ogive_length-age/maturity_ogive_males/predict_maturity_ogive_allyears.csv"), row.names=FALSE)
# 
# ### Figure maturation ogive by length and age
# years=sort(unique(predictmat$year))
# for(i in 1:length(years))
# {
#   selecdat=predictmat[predictmat$year==years[i],]
#   ages=sort(unique(selecdat$age))
#   png(filename=paste0(outpath, species, "baseline/maturity_ogive_length-age/maturity_ogive_males/maturity_ogive_by_length_and_age_", years[i], ".png"), width=4000, height=3000, res=400, pointsize=12)
#   for(e in 1:length(ages))
#   {
#     seldat=selecdat[selecdat$age==ages[e],]
#     seldat=seldat[with(seldat, order(length)),]
#     if(e==1) {plot(seldat$length, seldat$predict, type="l", col=paste0("gray", round((e*5),digits=0)), lwd=2.5, xlab="Length (cm)", ylab="Proportion mature", ylim=c(0,1))} else {
#       lines(seldat$length, seldat$predict, type="l", col=paste0("gray", round((e*5),digits=0)), lwd=2.5)}
#   }
#   dev.off()
# }
# 
# 
# ### Para Gadget
# 
# predictmat=predictmat[predictmat$age>2,]
# 
# # First the immature stock
# predictmat$step=rep(1,dim(predictmat)[1])
# predictmat$area=rep(1,dim(predictmat)[1])
# predictmat$stock=rep("cod.imm",dim(predictmat)[1])
# predictmat$age=paste0("age", predictmat$age)
# predictmat$lengr=paste0("len_", predictmat$length, "-", predictmat$length, ".9")
# predictmat$proportion=round(1-predictmat$predict, digits=5)
# 
# head(predictmat)
# 
# gadget_I=predictmat[,c("year", "step", "area", "stock","age", "lengr", "proportion")]
# 
# # Next the mature stock
# predictmat$stock=rep("cod.mat",dim(predictmat)[1])
# predictmat$proportion=round(predictmat$predict, digits=5)
# 
# head(predictmat)
# 
# gadget_II=predictmat[,c("year", "step", "area", "stock","age", "lengr", "proportion")]
# 
# 
# gadget=rbind(gadget_I, gadget_II)
# 
# write.csv(gadget, paste0(outpath, species, "baseline/xa_gadget/cod.sur.trawl.win.bar.male.ogive.csv"), row.names=F)
# 
# 
# 
# 
# 
# ##############################################################
# ### Maturity at length and age Females and Males
# ##############################################################
# mubis=superindv[complete.cases(superindv$maturationstage),]
# mubis=subset(mubis, maturationstage<5)
# 
# years=sort(unique(mubis$year))
# for(i in 1:length(years))
# {
#   seldata=mubis[mubis$year==years[i],]
#   matdat=data.frame(age=seldata$age, length=seldata$LengthCentimeter, stage=ifelse(seldata$maturationstage==1, 0, 1))
#   matdat$year=rep(years[i], length(matdat$length))
#   
#   if(i==1) {matdata=matdat} else {matdata=rbind(matdata,matdat)}
#   
#   Logit<-glm(stage~length + age, family=binomial,data=matdata)
#   InterceptLength= Logit$coefficients[1]
#   Lengthres= Logit$coefficients[2]
#   Ageres= Logit$coefficients[3]
#   Year=years[i]
#   fitmat=data.frame(year=Year, intercept=InterceptLength, lengthpar=Lengthres, agepar=Ageres)
#   
#   if(i==1) {fitmatogive=fitmat} else {fitmatogive=rbind(fitmatogive,fitmat)}
#   
#   newdata=data.frame(expand.grid(length=c(0:160), age=c(1:20)))
#   newdata$predict=predict(Logit, newdata, type="response")
#   newdata$year=rep(years[i], length(newdata$length))
#   
#   if (i==1) {predictmat=newdata} else {predictmat=rbind(predictmat, newdata)}
#   
# }
# 
# write.csv(matdata, paste0(outpath, species, "baseline/maturity_ogive_length-age/maturity_ogive_males_females/data_maturity_allyears.csv"), row.names=FALSE)
# write.csv(fitmatogive, paste0(outpath, species, "baseline/maturity_ogive_length-age/maturity_ogive_males_females/params_maturity_ogive_allyears.csv"), row.names=FALSE)
# write.csv(predictmat, paste0(outpath, species, "baseline/maturity_ogive_length-age/maturity_ogive_males_females/predict_maturity_ogive_allyears.csv"), row.names=FALSE)
# 
# ### Figure maturation ogive by length and age
# years=sort(unique(predictmat$year))
# for(i in 1:length(years))
# {
#   selecdat=predictmat[predictmat$year==years[i],]
#   ages=sort(unique(selecdat$age))
#   png(filename=paste0(outpath, species, "baseline/maturity_ogive_length-age/maturity_ogive_males_females/maturity_ogive_by_length_and_age_", years[i], ".png"), width=4000, height=3000, res=400, pointsize=12)
#   for(e in 1:length(ages))
#   {
#     seldat=selecdat[selecdat$age==ages[e],]
#     seldat=seldat[with(seldat, order(length)),]
#     if(e==1) {plot(seldat$length, seldat$predict, type="l", col=paste0("gray", round((e*5),digits=0)), lwd=2.5, xlab="Length (cm)", ylab="Proportion mature", ylim=c(0,1))} else {
#       lines(seldat$length, seldat$predict, type="l", col=paste0("gray", round((e*5),digits=0)), lwd=2.5)}
#   }
#   dev.off()
# }
# 
# 
# ### Para Gadget
# 
# predictmat=predictmat[predictmat$age>2,]
# 
# # First the immature stock
# predictmat$step=rep(1,dim(predictmat)[1])
# predictmat$area=rep(1,dim(predictmat)[1])
# predictmat$stock=rep("cod.imm",dim(predictmat)[1])
# predictmat$age=paste0("age", predictmat$age)
# predictmat$lengr=paste0("len_", predictmat$length, "-", predictmat$length, ".9")
# predictmat$proportion=round(1-predictmat$predict, digits=5)
# 
# head(predictmat)
# 
# gadget_I=predictmat[,c("year", "step", "area", "stock","age", "lengr", "proportion")]
# 
# # Next the mature stock
# predictmat$stock=rep("cod.mat",dim(predictmat)[1])
# predictmat$proportion=round(predictmat$predict, digits=5)
# 
# head(predictmat)
# 
# gadget_II=predictmat[,c("year", "step", "area", "stock","age", "lengr", "proportion")]
# 
# 
# gadget=rbind(gadget_I, gadget_II)
# 
# write.csv(gadget, paste0(outpath, species, "baseline/xa_gadget/cod.sur.trawl.win.bar.male_females.ogive.csv"), row.names=F)
# 



###############################
### Maturity at length Females
###############################
mubis=superindv[complete.cases(superindv$maturationstage),]
mubis=mubis[mubis$sex==1,]
mubis=subset(mubis, maturationstage<5)

years=sort(unique(mubis$year))
for(i in 1:length(years))
{
  seldata=mubis[mubis$year==years[i],]
  matdat=data.frame(length=seldata$LengthCentimeter, stage=ifelse(seldata$maturationstage==1, 0, 1))
  matdat$year=rep(years[i], length(matdat$length))

  if(i==1) {matdata=matdat} else {matdata=rbind(matdata,matdat)}

  matdat=data.frame(age=seldata$age, length=seldata$LengthCentimeter, stage=ifelse(seldata$maturationstage==1, 0, 1))
  matdat$year=rep(years[i], length(matdat$length))
  model=step(glm(stage~age+length, data=matdat, family=binomial))

  Logit<-glm(stage~length, family=binomial,data=matdata)
  InterceptLength= Logit$coefficients[1]
  Lengthres= Logit$coefficients[2]
  L50=-(InterceptLength/Lengthres)
  Year=years[i]
  fitmat=data.frame(year=Year, L50=L50, Intercept=InterceptLength, Slope=Lengthres)

  if(i==1) {fitmatogive=fitmat} else {fitmatogive=rbind(fitmatogive,fitmat)}

  newlength=data.frame(length=c(0:160))
  newlength$predict=predict(Logit, newlength, type="response")
  newlength$year=rep(years[i], length(newlength$length))

  if (i==1) {predictmat=newlength} else {predictmat=rbind(predictmat, newlength)}

}

write.csv(matdata, paste0(outpath, species, "baseline/maturity_ogive_length/maturity_ogive_females/data_maturity_allyears.csv"), row.names=FALSE)
write.csv(fitmatogive, paste0(outpath, species, "baseline/maturity_ogive_length/maturity_ogive_females/params_maturity_ogive_allyears.csv"), row.names=FALSE)
write.csv(predictmat, paste0(outpath, species, "baseline/maturity_ogive_length/maturity_ogive_females/predict_maturity_ogive_allyears.csv"), row.names=FALSE)

### Figure maturation ogive
png(filename=paste0(outpath, species, "baseline/maturity_ogive_length/maturity_ogive_females/maturity_ogive_by_length_allyears.png"), width=4000, height=3000, res=400, pointsize=12)

for(i in 1:length(years))
{
  if(years[i]==1988) {"no biological data"} else {
    seldat=predictmat[predictmat$year==years[i],]
    seldat=seldat[with(seldat, order(length)),]
    L50sdat=fitmatogive[fitmatogive$year==years[i],]
    if(i==1) {plot(seldat$length, seldat$predict, type="l", col=paste0("gray", round((i*2.5),digits=0)), lwd=2.5, xlab="Length (cm)", ylab="Proportion mature")
      points(L50sdat$L50, 0.5, col="red", pch=19, cex=2)} else {
        lines(seldat$length, seldat$predict, type="l", col="grey30", lwd=2.5)
        points(L50sdat$L50, 0.5, col="red", pch=19, cex=2)}}
}
dev.off()


png(filename=paste0(outpath, species, "baseline/maturity_ogive_length/maturity_ogive_females/L50_allyears.png"), width=4000, height=3000, res=400, pointsize=12)
plot(fitmatogive$year, fitmatogive$L50, type="b", pch=19, col="grey60")
dev.off()


png(filename=paste0(outpath, species, "baseline/maturity_ogive_length/maturity_ogive_females/L50_sin_1984_1986_1987.png"), width=4000, height=3000, res=400, pointsize=12)
sel=fitmatogive[fitmatogive$year!=1984 & fitmatogive$year!=1986 & fitmatogive$year!=1987,]
plot(sel$year, sel$L50, type="b", pch=19, col="grey60")
dev.off()


### Para Gadget

# First the immature stock
predictmat$step=rep(1,dim(predictmat)[1])
predictmat$area=rep(1,dim(predictmat)[1])
predictmat$stock=rep("cod.imm",dim(predictmat)[1])
predictmat$age=rep("allages", dim(predictmat)[1])
predictmat$lengr=paste0("len_", predictmat$length, "-", predictmat$length, ".9")
predictmat$proportion=round(1-predictmat$predict, digits=5)

head(predictmat)

gadget_I=predictmat[,c("year", "step", "area", "stock","age", "lengr", "proportion")]

# Next the mature stock
predictmat$step=rep(1,dim(predictmat)[1])
predictmat$area=rep(1,dim(predictmat)[1])
predictmat$stock=rep("cod.mat",dim(predictmat)[1])
predictmat$age=rep("allages", dim(predictmat)[1])
predictmat$lengr=paste0("len_", predictmat$length, "-", predictmat$length, ".9")
predictmat$proportion=round(predictmat$predict, digits=5)

head(predictmat)

gadget_II=predictmat[,c("year", "step", "area", "stock","age", "lengr", "proportion")]


gadget=rbind(gadget_I, gadget_II)

write.csv(gadget, paste0(outpath, species, "baseline/xa_gadget/cod.sur.trawl.win.bar.female.ogive.csv"), row.names=F)





###############################
### Maturity at length Males
###############################
mubis=superindv[complete.cases(superindv$maturationstage),]
mubis=mubis[mubis$sex==2,]
mubis=subset(mubis, maturationstage<5)

years=sort(unique(mubis$year))
for(i in 1:length(years))
{
  seldata=mubis[mubis$year==years[i],]
  matdat=data.frame(length=seldata$LengthCentimeter, stage=ifelse(seldata$maturationstage==1, 0, 1))
  matdat$year=rep(years[i], length(matdat$length))

  if(i==1) {matdata=matdat} else {matdata=rbind(matdata,matdat)}

  Logit<-glm(stage~length, family=binomial,data=matdata)
  InterceptLength= Logit$coefficients[1]
  Lengthres= Logit$coefficients[2]
  L50=-(InterceptLength/Lengthres)
  Year=years[i]
  fitmat=data.frame(year=Year, L50=L50, Intercept=InterceptLength, Slope=Lengthres)

  if(i==1) {fitmatogive=fitmat} else {fitmatogive=rbind(fitmatogive,fitmat)}

  newlength=data.frame(length=c(0:160))
  newlength$predict=predict(Logit, newlength, type="response")
  newlength$year=rep(years[i], length(newlength$length))

  if (i==1) {predictmat=newlength} else {predictmat=rbind(predictmat, newlength)}

}

write.csv(matdata, paste0(outpath, species, "baseline/maturity_ogive_length/maturity_ogive_males/data_maturity_allyears.csv"), row.names=FALSE)
write.csv(fitmatogive, paste0(outpath, species, "baseline/maturity_ogive_length/maturity_ogive_males/params_maturity_ogive_allyears.csv"), row.names=FALSE)
write.csv(predictmat, paste0(outpath, species, "baseline/maturity_ogive_length/maturity_ogive_males/predict_maturity_ogive_allyears.csv"), row.names=FALSE)

### Figure maturation ogive
png(filename=paste0(outpath, species, "baseline/maturity_ogive_length/maturity_ogive_males/maturity_ogive_by_length_allyears.png"), width=4000, height=3000, res=400, pointsize=12)

for(i in 1:length(years))
{
  if(years[i]==1988) {"no biological data"} else {
    seldat=predictmat[predictmat$year==years[i],]
    seldat=seldat[with(seldat, order(length)),]
    L50sdat=fitmatogive[fitmatogive$year==years[i],]
    if(i==1) {plot(seldat$length, seldat$predict, type="l", col=paste0("gray", round((i*2.5),digits=0)), lwd=2.5, xlab="Length (cm)", ylab="Proportion mature")
      points(L50sdat$L50, 0.5, col="red", pch=19, cex=2)} else {
        lines(seldat$length, seldat$predict, type="l", col="grey30", lwd=2.5)
        points(L50sdat$L50, 0.5, col="red", pch=19, cex=2)}}
}
dev.off()


png(filename=paste0(outpath, species, "baseline/maturity_ogive_length/maturity_ogive_males/L50_allyears.png"), width=4000, height=3000, res=400, pointsize=12)
plot(fitmatogive$year, fitmatogive$L50, type="b", pch=19, col="grey60")
dev.off()


png(filename=paste0(outpath, species, "baseline/maturity_ogive_length/maturity_ogive_males/L50_sin_1984_1986_1987.png"), width=4000, height=3000, res=400, pointsize=12)
sel=fitmatogive[fitmatogive$year!=1984 & fitmatogive$year!=1986 & fitmatogive$year!=1987,]
plot(sel$year, sel$L50, type="b", pch=19, col="grey60")
dev.off()


### Para Gadget

# First the immature stock
predictmat$step=rep(1,dim(predictmat)[1])
predictmat$area=rep(1,dim(predictmat)[1])
predictmat$stock=rep("cod.imm",dim(predictmat)[1])
predictmat$age=rep("allages", dim(predictmat)[1])
predictmat$lengr=paste0("len_", predictmat$length, "-", predictmat$length, ".9")
predictmat$proportion=round(1-predictmat$predict, digits=5)

head(predictmat)

gadget_I=predictmat[,c("year", "step", "area", "stock","age", "lengr", "proportion")]

# Next the mature stock
predictmat$step=rep(1,dim(predictmat)[1])
predictmat$area=rep(1,dim(predictmat)[1])
predictmat$stock=rep("cod.mat",dim(predictmat)[1])
predictmat$age=rep("allages", dim(predictmat)[1])
predictmat$lengr=paste0("len_", predictmat$length, "-", predictmat$length, ".9")
predictmat$proportion=round(predictmat$predict, digits=5)

head(predictmat)

gadget_II=predictmat[,c("year", "step", "area", "stock","age", "lengr", "proportion")]


gadget=rbind(gadget_I, gadget_II)

write.csv(gadget, paste0(outpath, species, "baseline/xa_gadget/cod.sur.trawl.win.bar.male.ogive.csv"), row.names=F)





###############################################
### Maturity at length Males and Females
###############################################
mubis=superindv[complete.cases(superindv$maturationstage),]
mubis=subset(mubis, maturationstage<5)

years=sort(unique(mubis$year))
for(i in 1:length(years))
{
  seldata=mubis[mubis$year==years[i],]
  matdat=data.frame(length=seldata$LengthCentimeter, stage=ifelse(seldata$maturationstage==1, 0, 1))
  matdat$year=rep(years[i], length(matdat$length))
  
  if(i==1) {matdata=matdat} else {matdata=rbind(matdata,matdat)}
  
  Logit<-glm(stage~length, family=binomial,data=matdata)
  InterceptLength= Logit$coefficients[1]
  Lengthres= Logit$coefficients[2]
  L50=-(InterceptLength/Lengthres)
  Year=years[i]
  fitmat=data.frame(year=Year, L50=L50, Intercept=InterceptLength, Slope=Lengthres)
  
  if(i==1) {fitmatogive=fitmat} else {fitmatogive=rbind(fitmatogive,fitmat)}
  
  newlength=data.frame(length=c(0:160))
  newlength$predict=predict(Logit, newlength, type="response")
  newlength$year=rep(years[i], length(newlength$length))
  
  if (i==1) {predictmat=newlength} else {predictmat=rbind(predictmat, newlength)}
  
}

write.csv(matdata, paste0(outpath, species, "baseline/maturity_ogive_length/maturity_ogive_males_females/data_maturity_allyears.csv"), row.names=FALSE)
write.csv(fitmatogive, paste0(outpath, species, "baseline/maturity_ogive_length/maturity_ogive_males_females/params_maturity_ogive_allyears.csv"), row.names=FALSE)
write.csv(predictmat, paste0(outpath, species, "baseline/maturity_ogive_length/maturity_ogive_males_females/predict_maturity_ogive_allyears.csv"), row.names=FALSE)

### Figure maturation ogive
png(filename=paste0(outpath, species, "baseline/maturity_ogive_length/maturity_ogive_males_females/maturity_ogive_by_length_allyears.png"), width=4000, height=3000, res=400, pointsize=12)

for(i in 1:length(years))
{
  if(years[i]==1988) {"no biological data"} else {
    seldat=predictmat[predictmat$year==years[i],]
    seldat=seldat[with(seldat, order(length)),]
    L50sdat=fitmatogive[fitmatogive$year==years[i],]
    if(i==1) {plot(seldat$length, seldat$predict, type="l", col=paste0("gray", round((i*2.5),digits=0)), lwd=2.5, xlab="Length (cm)", ylab="Proportion mature")
      points(L50sdat$L50, 0.5, col="red", pch=19, cex=2)} else {
        lines(seldat$length, seldat$predict, type="l", col="grey30", lwd=2.5)
        points(L50sdat$L50, 0.5, col="red", pch=19, cex=2)}}
}
dev.off()


png(filename=paste0(outpath, species, "baseline/maturity_ogive_length/maturity_ogive_males_females/L50_allyears.png"), width=4000, height=3000, res=400, pointsize=12)
plot(fitmatogive$year, fitmatogive$L50, type="b", pch=19, col="grey60")
dev.off()


png(filename=paste0(outpath, species, "baseline/maturity_ogive_length/maturity_ogive_males_females/L50_sin_1984_1986_1987.png"), width=4000, height=3000, res=400, pointsize=12)
sel=fitmatogive[fitmatogive$year!=1984 & fitmatogive$year!=1986 & fitmatogive$year!=1987,]
plot(sel$year, sel$L50, type="b", pch=19, col="grey60")
dev.off()


### Para Gadget

# First the immature stock
predictmat$step=rep(1,dim(predictmat)[1])
predictmat$area=rep(1,dim(predictmat)[1])
predictmat$stock=rep("cod.imm",dim(predictmat)[1])
predictmat$age=rep("allages", dim(predictmat)[1])
predictmat$lengr=paste0("len_", predictmat$length, "-", predictmat$length, ".9")
predictmat$proportion=round(1-predictmat$predict, digits=5)

head(predictmat)

gadget_I=predictmat[,c("year", "step", "area", "stock","age", "lengr", "proportion")]

# Next the mature stock
predictmat$step=rep(1,dim(predictmat)[1])
predictmat$area=rep(1,dim(predictmat)[1])
predictmat$stock=rep("cod.mat",dim(predictmat)[1])
predictmat$age=rep("allages", dim(predictmat)[1])
predictmat$lengr=paste0("len_", predictmat$length, "-", predictmat$length, ".9")
predictmat$proportion=round(predictmat$predict, digits=5)

head(predictmat)

gadget_II=predictmat[,c("year", "step", "area", "stock","age", "lengr", "proportion")]


gadget=rbind(gadget_I, gadget_II)

write.csv(gadget, paste0(outpath, species, "baseline/xa_gadget/cod.sur.trawl.win.bar.male_females.ogive.csv"), row.names=F)




###########################################################################
######################### Biomass Survey indices  #########################
###########################################################################

# Total biomass survey index

dim(filmisdat)
data=filmisdat[complete.cases(filmisdat$IndividualWeightGram, filmisdat$Abundance),]
dim(data)

biom=ddply(data, .(year), summarize, biomass=round(sum(Abundance*IndividualWeightGram)/1000, digits=0))

write.csv(biom, paste0(outpath, species, "baseline/survey_index/total_biomass/cod.sur.trawl.biomass.indx.csv"), row.names = FALSE)

png(filename=paste0(outpath, species, "baseline/survey_index/total_biomass/cod.sur.trawl.biom.indx.png"), res=450, height=2000, width=2500, pointsize = 8)
plot(biom$year, biom$biomass/1000000, type="b", pch=19, ylab="Idx biom x10^6 kg", xlab="year", las=1)
dev.off()

# xa gadget
biom$step=rep(1, dim(biom)[1])  
biom$area=rep(1, dim(biom)[1])
biom$length=rep("alllen", dim(biom)[1])

biom=biom[,c("year", "step", "area", "length", "biomass")]

write.csv(biom, paste0(outpath, species, "baseline/xa_gadget/cod.sur.trawl.biom.indx.csv"), row.names = FALSE)



###########################################################################
######################### Abundance Survey indices  #######################
###########################################################################
dim(filmisdat)
data=filmisdat[complete.cases(filmisdat$Abundance),]
dim(data)

abund=ddply(data, .(year), summarize, abundance=sum(Abundance))

write.csv(abund, paste0(outpath, species, "baseline/survey_index/total_abundance/cod.sur.trawl.numbers.indx.csv"), row.names = FALSE)

png(filename=paste0(outpath, species, "baseline/survey_index/total_abundance/cod.sur.trawl.numbers.indx.png"), res=450, height=2000, width=2500, pointsize = 8)
plot(abund$year, abund$abundance/1000000000, type="b", pch=19, ylab="Idx abund x10^9 indv", xlab="year", las=1)
dev.off()


# xa gadget
abund$step=rep(1, dim(abund)[1])  
abund$area=rep(1, dim(abund)[1])
abund$length=rep("alllen", dim(abund)[1])

abund=abund[,c("year", "step", "area", "length", "abundance")]

write.csv(abund, paste0(outpath, species, "baseline/xa_gadget/cod.sur.trawl.numbers.indx.csv"), row.names = FALSE)




####################################################################################
######################### Abundance Survey indices at age 1, 2 and 3  #######################
####################################################################################
dim(filmisdat)
data=filmisdat[complete.cases(filmisdat$Abundance, filmisdat$age),]
dim(data)

abund=ddply(data, .(year, age), summarize, abundance=sum(Abundance))

abund=as.data.frame(cast(abund, year~age, value="abundance", fill=0))
colnames(abund)=c("year", paste0("age_", colnames(abund)[-1] ))

write.csv(abund, paste0(outpath, species, "baseline/survey_index/abundance_at_age/cod.sur.trawl.numbers_at_age.indx.csv"), row.names = FALSE)

# xa gadget

selec=abund[,colnames(abund) %in% c("year", "age_1")]
colnames(selec)=c("year", "abundance")
selec$step=rep(1, dim(abund)[1])  
selec$area=rep(1, dim(abund)[1])
selec$age=rep(paste0("age", 1), dim(abund)[1])
selec=selec[,c("year", "step", "area", "age", "abundance")]
write.csv(selec, paste0(outpath, species, "baseline/xa_gadget/cod.sur.trawl.numbers.age_1.indx.csv"), row.names = FALSE)

selec=abund[,colnames(abund) %in% c("year", "age_2")]
colnames(selec)=c("year", "abundance")
selec$step=rep(1, dim(abund)[1])  
selec$area=rep(1, dim(abund)[1])
selec$age=rep(paste0("age", 2), dim(abund)[1])
selec=selec[,c("year", "step", "area", "age", "abundance")]
write.csv(selec, paste0(outpath, species, "baseline/xa_gadget/cod.sur.trawl.numbers.age_2.indx.csv"), row.names = FALSE)

selec=abund[,colnames(abund) %in% c("year", "age_3")]
colnames(selec)=c("year", "abundance")
selec$step=rep(1, dim(abund)[1])  
selec$area=rep(1, dim(abund)[1])
selec$age=rep(paste0("age", 3), dim(abund)[1])
selec=selec[,c("year", "step", "area", "age", "abundance")]
write.csv(selec, paste0(outpath, species, "baseline/xa_gadget/cod.sur.trawl.numbers.age_3.indx.csv"), row.names = FALSE)

selec=abund[,colnames(abund) %in% c("year", "age_4")]
colnames(selec)=c("year", "abundance")
selec$step=rep(1, dim(abund)[1])  
selec$area=rep(1, dim(abund)[1])
selec$age=rep(paste0("age", 4), dim(abund)[1])
selec=selec[,c("year", "step", "area", "age", "abundance")]
write.csv(selec, paste0(outpath, species, "baseline/xa_gadget/cod.sur.trawl.numbers.age_4.indx.csv"), row.names = FALSE)


# Calculation correlation between consecutive ages with 1, 2 or 3 years of time lag
cont=dim(abund[,-c(1:2)])[2]-1
for(i in 1:cont)
{
  v1=abund[-dim(abund)[1],2+i]
  v2=abund[-1,3+i]
  correl=cor(v1, v2)
  combi=paste0(i,"-",1+i)
  if(i==1) {corr1=data.frame(combi, correl)} else {corr1=rbind(corr1, data.frame(combi, correl))}
}


cont=dim(abund[,-c(1:2)])[2]-2
for(i in 1:cont)
{
  v1=abund[-c((dim(abund)[1]-1):dim(abund)[1]),2+i]
  v2=abund[-c(1:2),4+i]
  correl=cor(v1, v2)
  combi=paste0(i,"-",2+i)
  if(i==1) {corr2=data.frame(combi, correl)} else {corr2=rbind(corr2, data.frame(combi, correl))}
}


cont=dim(abund[,-c(1:2)])[2]-3
for(i in 1:cont)
{
  v1=abund[-c((dim(abund)[1]-2):dim(abund)[1]),2+i]
  v2=abund[-c(1:3),5+i]
  correl=cor(v1, v2)
  combi=paste0(i,"-",3+i)
  if(i==1) {corr3=data.frame(combi, correl)} else {corr3=rbind(corr3, data.frame(combi, correl))}
}

cont=dim(abund[,-c(1:2)])[2]-4
for(i in 1:cont)
{
  v1=abund[-c((dim(abund)[1]-2):dim(abund)[1]),3+i]
  v2=abund[-c(1:3),6+i]
  correl=cor(v1, v2)
  combi=paste0(i,"-",3+i)
  if(i==1) {corr4=data.frame(combi, correl)} else {corr4=rbind(corr4, data.frame(combi, correl))}
}

corr=data.frame(corr1=corr1$correl[1:dim(corr4)[1]], corr2=corr2$correl[1:dim(corr4)[1]], corr3=corr3$correl[1:dim(corr4)[1]], corr4=corr4$correl[1:dim(corr4)[1]])
corr$age=c(1:dim(corr)[1])

png(filename=paste0(outpath, species, "baseline/survey_index/abundance_at_age/corr_idx_at_age_with_time_lag.png"), res=450, height=2000, width=2500, pointsize = 8)
layout(matrix(c(1:15), 3, 5, byrow=T))
corr=as.data.frame(t(corr))
colnames(corr)=paste0("age_", corr[dim(corr)[1],])
corr=corr[-dim(corr)[1],]
corr$lag=c(1:dim(corr)[1])
for(i in 1:(dim(corr)[2]-1))
{
  barplot(corr[,i]~corr$lag, xlab="time lag", ylab="correlation", main=paste0("age ", i))
}
dev.off()


















