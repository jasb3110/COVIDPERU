# Contents

- [Introdution](#introduction)
- [to clean dataset](#to-clean-dataset)
- [R code](#r-code)
  - [Exploratory plot](#exploratory-plot)
  - [SINADEF´s death on Peru](#sinadef´s-death-on-peru)
  - [SINADEF´s death by regions](#sinadef´s-death-by-regions)
  - [Positive-COVID´s people](#positive-covid´s-people)
  - [Molecular positivity of COVID´s in percent on Peru](##molecular-positivity-of-covid´s-in-percent-on-peru)
  - [OPENCOVIDPERU data set](#opencovidperu-data-set)
  - [Free bed of Intensive care unit](#free-bed-of-intensive-care-unit)
  - [Vaccinatation in progress](#vaccinatation-in-progress)
- [Final ideas](#final-ideas)
- [References](#references)
- [Support or Contact](#support-or-contact)

# Introdution
First of all, The SARS-COV-2 is well-know as Covid-19, spreading pandemic illness around world during 2019 to now. COVID-19 had come through a man who came to Italia on 21th febrary, 2020 [(1)](#references). In Peru, this sickness had come through Luis Felipe Zeballos who arrived to Lima on 26 February of 2020. After, He showed that first syntoms of COVID-19 [(2)](#references).

# to clean dataset
When Covid-19 had started to spread on Peru. The health government institutes were not systemic criteria to order COVID-19 reports. this issue had detrimental effect on reliable diffusion information to Peruvian people. So that,it was important to unificate different source in one way to present COVID-19 variables (positive rate of covid´s patients, numbers of death which have cause for COVID-19, excess of death which could have triggered for COVID-19, numbers of free UCI bed in hospitals and numbers of people were vaccinated).
Therefore, the first challenge was delete or omit different mistakes (Na, null values and outliers). whole of data were downloaded of Peruvian official sources,link: [datos abiertos](https://www.datosabiertos.gob.pe/). I would show you how to manage deep cleaning the data.

# R code
I´m R native programmer so that it too easily to proceed to clean of dataset with this program. It could possible to use anothers programs. I suggest that you will able to use Python. 

```markdown
################################################################################
#to start
setwd("~/covid19/")#directory
require("janitor")# package should have installed
require("dplyr")
require("plyr")
library("janitor")
library("dplyr")
library("plyr")
library("viridis")  
library("stringr")
library("gganimate")
library("transformr")
library("ggplot2")
library("gifski")
library("data.table")
library("pracma")
require("ggplot2")

#to read database
m=fread("fallecidos_sinadef.csv",sep="|",dec=".",header = TRUE,fill=TRUE)#fallecidos segun SINADEF/ total death in Peru per day
mcovid=fread("fallecidos_covid.csv",sep=";",dec=".",header = TRUE,fill=TRUE)#Personas reportadas como muertos por COVID / COVID´s patient which passed away per day

#to clean and sort of SINADEF´data
m=as.data.frame(m)
dead=m
rownames(dead)=NULL
dead$NÂº=NULL
dead$`TIPO SEGURO`=NULL

dead$`TIPO LUGAR`=NULL
dead$INSTITUCION=NULL
dead$NECROPSIA=NULL
dead=as.data.frame(dead[,1:15])

dead$dia=as.numeric(format(as.Date(dead$FECHA,format="%Y-%m-%d"), format = "%d"))
dead$meses=as.numeric(format(as.Date(dead$FECHA,format="%Y-%m-%d"), format = "%m"))
dead$años=as.numeric(format(as.Date(dead$FECHA,format="%Y-%m-%d"), format = "%Y"))

dead$EDAD=as.numeric(dead$EDAD)
dead$MES=as.numeric(dead$meses)
dead=as.data.frame(dead)
order=unique(sort(dead$FECHA))
dead$date=rep(NA,length(dead$FECHA))
for(i in 1:length(unique(sort(dead$FECHA)))){
  dead$date[which(dead$FECHA==order[i])]=i
}
dead=as.data.frame(dead)

#to find out mistakes in dates
#delete outliers or distort numbers for days
e1=sum(is.na(dead$dia))
e2=sum(which(dead$dia>31))
e3=sum(which(dead$dia<1))
e1
e2
e3

#delete outliers or distort numbers for moths
e4=sum(is.na(dead$meses))
e5=sum(which(dead$meses>12))
e6=sum(which(dead$meses<1))
e4
e5
e6

#delete outliers or distort numbers for years
#para dias e7|e8|e9 = 0
e7=sum(is.na(dead$años))
e8=sum(which(dead$años>2022))
e9=sum(which(dead$años<2017))
e7
e8
e9

MM=unique(dead$`MUERTE VIOLENTA`)# non-natural deaths
tt=dead[which(dead$`MUERTE VIOLENTA`==MM[1]|dead$`MUERTE VIOLENTA`==MM[2]|dead$`MUERTE VIOLENTA`==MM[3]|dead$`MUERTE VIOLENTA`==MM[9]),]

#to delete mistakes dates and replace for NAs
#my assumption is it had an error each column
TT=unique(tt$`TIEMPO EDAD`)
EC=unique(tt$`ESTADO CIVIL`)
NI=unique(tt$`NIVEL DE INSTRUCCIÃ“N`)

me=c(which(tt$`ESTADO CIVIL`==EC[2]&tt$EDAD>17),
     which(tt$`ESTADO CIVIL`==EC[6]&tt$EDAD>17),
     which(tt$`ESTADO CIVIL`==EC[7]&tt$EDAD>17),
     which(tt$`ESTADO CIVIL`==EC[8]&tt$EDAD>17),
     which(tt$`ESTADO CIVIL`==EC[9]&tt$EDAD>17))

mi=c(which(tt$`NIVEL DE INSTRUCCIÃ“N`==NI[2]&tt$EDAD>17),
     which(tt$`NIVEL DE INSTRUCCIÃ“N`==NI[3]&tt$EDAD>16),
     which(tt$`NIVEL DE INSTRUCCIÃ“N`==NI[5]&tt$EDAD>12),
     which(tt$`NIVEL DE INSTRUCCIÃ“N`==NI[6]&tt$EDAD>6),
     which(tt$`NIVEL DE INSTRUCCIÃ“N`==NI[7]&tt$EDAD>11),
     which(tt$`NIVEL DE INSTRUCCIÃ“N`==NI[8]&tt$EDAD>18),
     which(tt$`NIVEL DE INSTRUCCIÃ“N`==NI[9]&tt$EDAD>21),
     which(tt$`NIVEL DE INSTRUCCIÃ“N`==NI[11]&tt$EDAD>17))

#for secunds
m1=which(tt$EDAD[which(tt$`TIEMPO EDAD`==TT[7])]>60)
tt$TIEMPO.EDAD[intersect(m1,me)]=TT[1]
tt$TIEMPO.EDAD[intersect(m1,mi)]=TT[1]
ss1=which(tt$`TIEMPO EDAD`==TT[7])
tt$EDAD[ss1]=tt$EDAD[ss1]/(60*60*24*365)

#for minutes
m2=which(tt$EDAD[which(tt$`TIEMPO EDAD`==TT[2])]>60)
tt$`TIEMPO EDAD`[intersect(m2,me)]=TT[1]
tt$`TIEMPO EDAD`[intersect(m2,mi)]=TT[1]
ss2=which(tt$`TIEMPO EDAD`==TT[2])
tt$EDAD[ss2]=tt$EDAD[ss2]/(60*24*365)

#for hours
m3=which(tt$EDAD[which(tt$`TIEMPO EDAD`==TT[5])]>24)
tt$`TIEMPO EDAD`[intersect(m3,me)]=TT[1]
tt$`TIEMPO EDAD`[intersect(m3,mi)]=TT[1]
ss3=which(tt$`TIEMPO EDAD`==TT[5])
tt$EDAD[ss3]=tt$EDAD[ss3]/(24*365)  

#for days
m4=which(tt$EDAD[which(tt$`TIEMPO EDAD`==TT[4])]>31)
tt$`TIEMPO EDAD`[intersect(m4,me)]=TT[1]
tt$`TIEMPO EDAD`[intersect(m4,mi)]=TT[1]
ss4=which(tt$`TIEMPO EDAD`==TT[4])
tt$EDAD[ss4]=tt$EDAD[ss4]/(365) 

#for month 
m5=which(tt$EDAD[which(tt$`TIEMPO EDAD`==TT[3])]>12)
tt$`TIEMPO EDAD`[intersect(m5,me)]=TT[1]
tt$`TIEMPO EDAD`[intersect(m5,mi)]=TT[1]
ss4=which(tt$`TIEMPO EDAD`==TT[3])
tt$EDAD[ss4]=tt$EDAD[ss4]/(12) 
#for years
m6=which(tt$EDAD[which(tt$`TIEMPO EDAD`==TT[1])]>123)
tt$EDAD[m6]=NA

#for "ignorados" and "sin registro" (without specific names)
m7=c(which(tt$`TIEMPO EDAD`==TT[6]|tt$`TIEMPO EDAD`==TT[8]|tt$`TIEMPO EDAD`==TT[9]))
tt$`TIEMPO EDAD`[which(tt$EDAD[m7]>60)]=TT[1]
tt$EDAD[which(tt$EDAD[m7]<60)]=NA
tt$EDAD=as.numeric(tt$EDAD)
tt$EDAD[which(tt$EDAD>=123)]=NA

#sort of sexs and genre
sinsexo=tt
sinsexo$SEXO=NULL
sinsexo$EDAD=trunc(sinsexo$EDAD*100)/100
todo=as.data.frame(sinsexo%>%count(date,dia,meses,años,EDAD))
y1=tt[which(tt$SEXO=="FEMENINO"),]
y1$SEXO=NULL
y1$EDAD=trunc(y1$EDAD*100)/100
muj=as.data.frame(y1%>%count(date,dia,meses,años,EDAD))
y2=tt[which(tt$SEXO=="MASCULINO"),]
y2$SEXO=NULL
y2$EDAD=trunc(y2$EDAD*100)/100
hom=as.data.frame(y2%>%count(date,dia,meses,años,EDAD))
mujeres=as.data.frame(y1%>%count(date,dia,meses,años))
mujeres$fechas=paste(mujeres$dia,"-",mujeres$meses,"-",mujeres$años)
hombres=as.data.frame(y2%>%count(date,dia,meses,años))
hombres$fechas=paste(hombres$dia,"-",hombres$meses,"-",hombres$años)
todos=as.data.frame(sinsexo%>%count(date,dia,meses,años))
todos$fechas=as.Date(paste0(todos$dia,"-",todos$meses,"-",todos$años),format="%d-%m-%Y")

#to plot exploratory picture
png("exploratory.plot.png", width = 500, height = 318, units = 'mm', res =1200)

plot.new();par(mfrow = c(4, 2))
plot(mujeres$date,mujeres$n,type="l",ylab="Number of deaths",xlab=paste0("Days from ",mujeres$fechas[min(mujeres$date)]," to ",mujeres$fechas[max(mujeres$date)]),main = "Number of Women deaths")
plot(hombres$date,hombres$n,type="l",ylab="Number of deaths",xlab=paste0("Days from ",hombres$fechas[min(hombres$date)]," to ",hombres$fechas[max(hombres$date)]),main = "Number of men deaths")
plot(todos$date,todos$n,type="l",ylab="Number of deaths",xlab=paste0("Days from  ",todos$fechas[min(todos$date)]," to ",todos$fechas[max(todos$date)]),main = "Number of total deathss")
plot(todos$date,todos$n,type="l",col="gray50",ylab="Number of deathss",xlab=paste0("Days from ",todos$fechas[min(todos$date)]," to ",todos$fechas[max(todos$date)]),main = "Number of total deaths")
points(mujeres$date,mujeres$n,type="l",col="blue",ylab="Number of deaths",xlab=paste0("Days from ",mujeres$fechas[min(mujeres$date)]," to ",mujeres$fechas[max(mujeres$date)]),main = "Number of total deaths")
points(hombres$date,hombres$n,type="l",col="red",ylab="Number of deaths",xlab=paste0("Days from ",hombres$fechas[min(hombres$date)]," to ",hombres$fechas[max(hombres$date)]),main = "Number of total deaths")

interval=signif(log10(length(todo$EDAD))*3.3+1,)
k=seq(from=0,to=150,by=interval)

hist(na.contiguous(todo$EDAD),breaks = c(k),freq = FALSE,density =10,xlab=paste0("Age´s range each ",interval," years"),main = "Histogram of total deaths")
d1=density(x = na.contiguous(todo$EDAD))
points(d1,col=2,type="l",lwd=2)

interval=signif(log10(length(hom$EDAD))*3.3+1,)
k=seq(from=0,to=150,by=interval)
hist(na.contiguous(hom$EDAD),breaks = c(k),freq = FALSE,density =15,xlab=paste0("Age´s range each ",interval," years"),main = "Histogram of total men´s deaths")
d2=density(x = na.contiguous(hom$EDAD))
points(d2,col=2,type="l",lwd=2)

interval=signif(log10(length(muj$EDAD))*3.3+1,)
k=seq(from=0,to=150,by=interval)
hist(na.contiguous(muj$EDAD),breaks = c(k),freq = FALSE,density =20,xlab=paste0("Age´s range each ",interval," years"),main = "Histogram of total women´s deaths")
d3=density(x =na.contiguous(muj$EDAD))
points(d3,col=2,type="l",lwd=2)

plot(d1, col="blue",lwd=4,main="deaths density",ylim=c(0,max(cbind(d1$y,d2$y,d3$y))))
points(d2, col="red", type="l",lwd=4)
points(d3, col="gray70", type="l",lwd=4)
dev.off()
################################################################################
```
## Exploratory picture
It show that exploratory plot of SINADEF´s data set (death´s register). first view it seems to be clean and sort that you can looking down eight pictures. 

![alt text](https://github.com/jasb3110/COVIDPERU/blob/1fc1af589150d750db96aae4a4d21699846df300/exploratory.plot.png?raw=true)

Next, it is getting to clean and sort of SINADEF´s data. I try to delete non-natural death signal. The assumption is excess of death during 2020 to 2022, due to COVID-19 spreading. Also, I plotted numbers of men, women and people to evidence some patterns. Bellow I attached lines scripts.

```markdown 
################################################################################
# COVID-19´s death during to spread pandemic

"04 - 3 - 2020" #begining date
todos=todos[which(todos$años>=2019),]
ini=which(todos$dia==04&todos$meses==3&todos$años==2020)
fin=length(todos$fechas)

fin-ini# COVID-19´s death average 
media_covid=mean(todos$n[ini:fin])
std_covid=sd(todos$n[ini:fin])
100*std_covid/media_covid #Coefficient of Variation in percent

#total death before COVID-19 pandemic
iii=which(todos$dia==02&todos$meses==2&todos$años==2020)#02 - 2 - 2020 begin
fff=which(todos$dia==03&todos$meses==3&todos$años==2020)#03 - 3 - 2020 end 

fff-iii# amount of death before 30 days to come COVID-19 in Peru (Pre-covid period time)
media=mean(todos$n[iii:fff])
std=sd(todos$n[iii:fff])
100*std/media #Coefficient of Variation in percent
write.csv(cbind(media,std),"exceso.csv",sep=",",dec=".",col.names=TRUE)#save in .csv

#total of death during COVID is subtracting total death during pre-COVID
m_encovid_medio=(todos$n[ini:fin]-media)/std
m_encovid_medio=m_encovid_medio+min(m_encovid_medio)*-1
m_encovid_min=(todos$n[ini:fin]-media-std*1.96)/std
m_encovid_min=m_encovid_min++min(m_encovid_min)*-1
m_encovid_max=(todos$n[ini:fin]-media+std*1.96)/std
m_encovid_max=m_encovid_max+min(m_encovid_max)*-1

encovid=as.data.frame(cbind(as.Date(todos$fechas[ini:fin],format="%Y-%m-%d"),todos$dia[ini:fin],todos$meses[ini:fin],todos$años[ini:fin],m_encovid_medio,m_encovid_min,m_encovid_max))
colnames(encovid)=c("fechas","dia","mes","año","m_encovid_medio","m_encovid_min","m_encovid_max") 

# to plot with ggplot2

mes.abb=c("En","Fe","Ma","Ab","My","Jn","Jl","Au","Se","Oc","No","Di")
names.mes=paste0(c(rep(19,12),rep(20,12),rep(21,12),rep(22,12)),"-",mes.abb)# since 2019

#women
mujeres$nombre.mes=NULL
for(i in 1:length(mujeres$date)){
  mujeres$nombre.mes[i]=mes.abb[mujeres$meses[i]]
}  

mujeres$mes.año=paste0(as.numeric(mujeres$años)-2000,"-",mujeres$nombre.mes)
mujeres$mes.año=factor(mujeres$mes.año , levels=c(names.mes))

mujeres$monthYear = paste0((as.numeric(mujeres$años)-2000)+trunc((mujeres$meses-0.5)*100/12)/100)
mujeres=mujeres[mujeres$años>=2019,]
fall.muj=ggplot(data = mujeres, aes(x=mujeres$mes.año, y=mujeres$n)) + geom_boxplot()+labs(title="Women´s deaths time serie from 2019 to 2022",
                                                                                  x ="Months from 2019 to 2022", 
                                                                                  y = "Number of death´s women")+
  theme(axis.text.x=element_text(size=11,colour = "black",face="bold",angle=45, hjust=1),axis.text.y=element_text(size=11,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=14,face="bold"),title = element_text(size=16,colour = "black",face="bold"))

ggsave("fallecidos.mujeres.png", dpi = 600,   width = 250,
       height = 159,unit="mm",plot = fall.muj)

# Men
hombres$nombre.mes=NULL
for(i in 1:length(hombres$date)){
  hombres$nombre.mes[i]=mes.abb[hombres$meses[i]]
}  

hombres$mes.año=paste0(as.numeric(hombres$años)-2000,"-",hombres$nombre.mes)
hombres$mes.año=factor(hombres$mes.año , levels=c(names.mes))

hombres$monthYear = paste0((as.numeric(hombres$años)-2000)+trunc((hombres$meses-0.5)*100/12)/100)
hombres=hombres[hombres$años>=2019,]
fall.hom=ggplot(data = hombres, aes(x=hombres$mes.año, y=hombres$n)) + geom_boxplot()+labs(title="Death´s men in boxplot since 2019 to 2022",
                                                                                  x ="Dates since 2019 to 2022 per month", 
                                                                                  y = "Number of death´s men")+
  theme(axis.text.x=element_text(size=11,colour = "black",face="bold",angle=45, hjust=1),axis.text.y=element_text(size=11,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=14,face="bold"),title = element_text(size=16,colour = "black",face="bold"))

ggsave("fallecidos.hombres.png", dpi = 600,   width = 250,
       height = 159,unit="mm",plot = fall.hom)

#to merged (men + women)
todos$nombre.mes=NULL
for(i in 1:length(todos$date)){
  todos$nombre.mes[i]=mes.abb[todos$meses[i]]
}  

todos$mes.año=paste0(as.numeric(todos$años)-2000,"-",todos$nombre.mes)
todos$mes.año=factor(todos$mes.año , levels=c(names.mes))

todos$monthYear = paste0((as.numeric(todos$años)-2000)+trunc((todos$meses-0.5)*100/12)/100)
todos=todos[todos$años>=2019,]

fall.todos=ggplot(data = todos, aes(x=todos$mes.año, y=todos$n)) + geom_boxplot()+labs(title="Death´s people in boxplot since 2019 to 2022
22 per month", 
                                                                                  y = "Number of death´s people"",
                                                                                       x ="Dates since 2019 to 2022 per month", 
                                                                                       y = "Number of death´s people")+
  theme(axis.text.x=element_text(size=11,colour = "black",face="bold",angle=45, hjust=1),axis.text.y=element_text(size=11,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=14,face="bold"),title = element_text(size=16,colour = "black",face="bold"))

ggsave("fallecidos.todos.png", dpi = 600,   width = 250,
       height = 159,unit="mm",plot = fall.todos)
################################################################################
```
## SINADEF´s death on Peru
It show that numbers of death in timeseries per genre. here you can see men´s death which cause for COVID-19 since 2019 to now. X-axis is numbers of peeple who have pass away per day and Y-axis is date per day. 
![alt text](https://github.com/jasb3110/COVIDPERU/blob/1fc1af589150d750db96aae4a4d21699846df300/fallecidos.hombres.png?raw=true)
Women´s  death which cause for COVID-19 since 2019 to now
![alt text](https://github.com/jasb3110/COVIDPERU/blob/1fc1af589150d750db96aae4a4d21699846df300/fallecidos.mujeres.png?raw=true)

In first view, number of death men is major than number of death women. therefore, COVID-19 have detrimental effect on Men a lot. 
People´s death which cause for COVID-19 since 2019 to now

![alt text](https://github.com/jasb3110/COVIDPERU/blob/1fc1af589150d750db96aae4a4d21699846df300/fallecidos.todos.png?raw=true)

In three wave of COVID-19 on Peru, the most of number death people probably relation with COVID-19 are: May to August in 2020 (first wave), February to April in 2021 (Second wave) and January to February 2022 (Third wave). Second wave was more lethal than others waves around 1250 death.
Next, it is getting to clean and sort of SINADEF´s death data by regions. Bellow I attached lines scripts.

```markdown 
################################################################################
#SINADEF´s death data
library("tidyr")
sinadef=fread("SINADEF - Data.csv",sep=",",dec=".",header=TRUE,fill=TRUE)#fallecidos segun SINADEF
ub=fread("TB_UBIGEOS.csv",sep=",",dec=".",header=TRUE,fill=TRUE)#ubigeos reales
ubi=as.data.frame(ub)
ubi$dep=trunc(ubi$ubigeo_reniec/100)
sinadef=as.data.frame(sinadef[,1:29])
colnames(sinadef)[29]="Perú"
fechas1=sinadef$DATE[1:366]
fechas1.1=paste0(fechas1,"-","2020")
fechas1.1=as.Date(fechas1.1,format="%d-%m-%Y")
fechas2=sinadef$DATE[367:731]
fechas2.1=paste0(fechas2,"-","2021")
fechas3=sinadef$DATE[732:length(sinadef$DATE)]
fechas3.1=paste0(fechas3,"-","2022")
fechas2.1=as.Date(fechas2.1,format="%d-%m-%Y")
fechas3.1=as.Date(fechas3.1,format="%d-%m-%Y")
sinadef$fecha=as.Date(c(fechas1.1,fechas2.1,fechas3.1),format="%Y-%m-%d")
sinadef$DATE=sinadef$fecha
sinadef$fecha=NULL

n.pro=c("DATE","Perú","AMAZONAS","ANCASH","APURIMAC",     
        "AREQUIPA","AYACUCHO","CAJAMARCA",    
        "CALLAO","CUSCO","EXTRANJERO",   
        "HUANCAVELICA","HUANUCO","ICA",          
        "JUNIN","LA LIBERTAD","LAMBAYEQUE",   
        "LIMA","LORETO","MADRE DE DIOS",
        "MOQUEGUA","PASCO","PIURA",        
        "PUNO","SAN MARTIN","SIN REGISTRO", 
        "TACNA","TUMBES","UCAYALI")

ubigeo=unique(sinsexo$`COD# UBIGEO DOMICILIO`)
depp=unique(sinsexo$`DEPARTAMENTO DOMICILIO`)
vector=depp
for( i in 3:length(n.pro)){
vector[which(vector==n.pro[i])]=i
vector=na.omit(vector)  
}
vdepp=1:length(depp)
nn.ex=depp[vdepp[!vdepp %in% c(1:24,26,28:32)]]
sinsexo$Dep=sinsexo$`DEPARTAMENTO DOMICILIO`

sinsexo$`DEPARTAMENTO DOMICILIO`[which(sinsexo$`DEPARTAMENTO DOMICILIO`=="     ")]="SIN REGISTRO"
sinsexo$`DEPARTAMENTO DOMICILIO`[which(sinsexo$`DEPARTAMENTO DOMICILIO`=="")]="SIN REGISTRO"
sinsexo$`DEPARTAMENTO DOMICILIO`[which(sinsexo$`DEPARTAMENTO DOMICILIO`=="[NO DEFINIDO]")]="SIN REGISTRO"
sinsexo$`DEPARTAMENTO DOMICILIO`[which(sinsexo$`DEPARTAMENTO DOMICILIO`=="")]="SIN REGISTRO" 

for(i in 1:length(sinsexo$`DEPARTAMENTO DOMICILIO`)){
if (sum(nn.ex==sinsexo$`DEPARTAMENTO DOMICILIO`[i])==0){
  next()
}else{
  if (sum(which(nn.ex==sinsexo$`DEPARTAMENTO DOMICILIO`[i]))<4){  
    sinsexo$Dep[i]="SIN REGISTRO"
  }else{
    sinsexo$Dep[i]="EXTRANJERO"  
  }
 }
}

prov.sinadef=as.data.frame(sinsexo%>%count(FECHA,Dep))
unique(prov.sinadef$Dep)

m.plot2=prov.sinadef
colnames(m.plot2)=colnames(m.plot)
m.plot2$variable[which(m.plot2$variable=="[NO DEFINIDO]")]="SIN REGISTRO"
m.plot2$variable[which(m.plot2$variable=="")]="SIN REGISTRO"
m.plot2$variable[which(m.plot2$variable=="     ")]="SIN REGISTRO"
m.plot2$variable=factor(m.plot2$variable,levels=unique(prov.sinadef$Dep))
provincias2=as.data.frame(m.plot2[which(m.plot2$variable!="SIN REGISTRO"&m.plot2$variable!="EXTRANJERO"),])

provincias3=provincias2[min(which(provincias2$DATE=="2020-03-01")):length(provincias2$DATE),]

pro.plot2=provincias3%>%
  ggplot(aes(x=DATE,y=value,color=variable))+geom_line(lwd=0.2)+
  facet_wrap(~variable,scales="free_y",ncol=7)+ylab("Number of deaths")+xlab("Dates for each regions")+scale_x_date(date_breaks = "120 days",date_labels = "%d-%m-%Y")+
  guides(color=FALSE)+theme_bw()+
  scale_y_continuous(breaks =scales::pretty_breaks(n = 4))+
  theme(axis.text.x=element_text(size=10,colour = "black",face="bold",angle=45, hjust=1),axis.text.y=element_text(size=20,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=20,face="bold"),title = element_text(size=16,colour = "black",face="bold"))

ggsave("fallecidos.provincias2.png", dpi = 1200,   width = 500,
       height = 268,unit="mm",plot = pro.plot2)
################################################################################   
```
## SINADEF´s death by regions

It show that numbers of SINADEF´s death data in timeseries each regions. here you can see total death which cause for natural cause since 2019 to now. X-axis is numbers of peeple who have pass away per day and Y-axis is date per day.

In my view, whole regions shows that evidence three waves. but each region shows its particular patterns of trendency.  Lima region was recorded the most number of death. this aspect could be explained the Lima region is the densely populated of the country and where dayly reports were more reliable and accurate than others regions.

![alt text](https://github.com/jasb3110/COVIDPERU/blob/4645aecfc038e25f651fe9c67524e23a28572766/fallecidos.provincias2.png?raw=true)

Next, it is getting to clean and sort of positive COVID´s test dataset. Bellow I attached lines scripts.

```markdown
################################################################################
#To clean and sort of COVID test
covid=fread("positivos_covid.csv",sep=";",dec=".",header=TRUE,fill=TRUE)#people who have diagnosis positive COVID-19 test

covid$id_persona=NULL
covid$FECHA_RESULTADO=as.character(covid$FECHA_RESULTADO)
covid$FECHA_RESULTADO=as.Date(covid$FECHA_RESULTADO,format ="%Y%m%d")

covid$dia=as.numeric(format(as.Date(covid$FECHA_RESULTADO,format="%Y-%m-%d"), format = "%d"))
covid$meses=as.numeric(format(as.Date(covid$FECHA_RESULTADO,format="%Y-%m-%d"), format = "%m"))
covid$años=as.numeric(format(as.Date(covid$FECHA_RESULTADO,format="%Y-%m-%d"), format = "%Y"))

order1=sort(unique(covid$FECHA_RESULTADO))
covid$date=rep(NA,length(covid$FECHA_RESULTADO))

for(i in 1:length(order1)){
  covid$date[which(covid$FECHA_RESULTADO==order1[i])]=i
}

covid2=data.frame(covid$date,covid$dia,covid$meses,covid$años,covid$METODODX,covid$SEXO)

covid3=as.data.frame(na.omit(covid2))

colnames(covid3)=c("date","dia","meses","años","METODODX","SEXO")

covid3$METODODX=factor(covid3$METODODX,levels=unique(covid3$METODODX))
covid3$SEXO=factor(covid3$SEXO,levels=unique(covid3$SEXO))

covid3$fecha=as.Date(paste0(covid3$años,"-",covid3$meses,"-",covid3$dia),format="%Y-%m-%d")
covid19=as.data.frame(covid3%>%count(colnames(covid3)))

#delay days for each kind of tests
desfase_PCR=2#delay days for PCR test
desfase_PR=8#delay days for serological test
desfase_PA=5#delay days for antigen test

covid19$FECHA=rep(NA,length(covid19$fecha))
covid19$FECHA[which(covid19$METODODX=="PCR")]=as.character(covid19$fecha[which(covid19$METODODX=="PCR")]-desfase_PCR)
covid19$FECHA[which(covid19$METODODX=="PR")]=as.character(covid19$fecha[which(covid19$METODODX=="PR")]-desfase_PR)
covid19$FECHA[which(covid19$METODODX=="AG")]=as.character(covid19$fecha[which(covid19$METODODX=="AG")]-desfase_PA)
covid19$FECHA=as.Date(covid19$FECHA,format="%Y-%m-%d")

covid4=covid3
covid4$SEXO=NULL
covid19r=as.data.frame(covid4%>%count(colnames(covid4)))

covid19r$FECHA=rep(NA,length(covid19r$fecha))
covid19r$FECHA[which(covid19r$METODODX=="PCR")]=as.character(covid19r$fecha[which(covid19r$METODODX=="PCR")]-desfase_PCR)
covid19r$FECHA[which(covid19r$METODODX=="PR")]=as.character(covid19r$fecha[which(covid19r$METODODX=="PR")]-desfase_PR)
covid19r$FECHA[which(covid19r$METODODX=="AG")]=as.character(covid19r$fecha[which(covid19r$METODODX=="AG")]-desfase_PA)
covid19r$FECHA=as.Date(covid19r$FECHA,format="%Y-%m-%d")

sinsexocovid=covid19r
covidmuj=covid19[covid19$SEXO=="FEMENINO",]
covidmuj$SEXO=NULL
covidhom=covid19[covid19$SEXO=="MASCULINO",]
covidhom$SEXO=NULL

#Whole Peru

covidhom$FECHA=as.Date(covidhom$FECHA,format="%Y-%m-%d")
covidmuj$FECHA=as.Date(covidmuj$FECHA,format="%Y-%m-%d")
sinsexocovid$FECHA=as.Date(sinsexocovid$FECHA,format="%Y-%m-%d")

covidhom$METODODX=factor(covidhom$METODODX,levels=unique(covidhom$METODODX))
covidmuj$METODODX=factor(covidmuj$METODODX,levels=unique(covidmuj$METODODX))
sinsexocovid$METODODX=factor(sinsexocovid$METODODX,levels=unique(sinsexocovid$METODODX))

#for men
covid.hom=ggplot(data=covidhom, aes(x=covidhom$FECHA, y=covidhom$freq, group=covidhom$METODODX))+
  scale_x_date(date_breaks = "30 days",date_labels = "%d-%b")+
  geom_line(aes(colour=covidhom$METODODX))+
  theme(legend.position="top")+
  labs(colour="",title="Covid-19 positive´s men time serie",
       x ="Date", 
       y = "Number of Covid-19 positives")+
  theme(axis.text.x=element_text(size=11,colour = "black",face="bold",angle=45, hjust=1),axis.text.y=element_text(size=11,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=14,face="bold"),title = element_text(size=16,colour = "black",face="bold"))

ggsave("covid.hombres.png", dpi = 600,   width = 250,
       height = 159,unit="mm",plot = covid.hom)

#for women

covid.muj=ggplot(data=covidmuj, aes(x=covidmuj$FECHA, y=covidmuj$freq, group=covidmuj$METODODX))+
  scale_x_date(date_breaks = "30 days",date_labels = "%d-%b")+
  geom_line(aes(color=covidmuj$METODODX))+
  theme(legend.position="top")+
  labs(colour="",title="Covid-19 positive´s women time serie",
       x ="Dates", 
       y = "Number of Covid-19 positives")+  
  theme(axis.text.x=element_text(size=11,colour = "black",face="bold",angle=45, hjust=1),axis.text.y=element_text(size=11,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=14,face="bold"),title = element_text(size=16,colour = "black",face="bold"))

ggsave("covid.mujeres.png", dpi = 600,   width = 250,
       height = 159,unit="mm",plot = covid.muj)

#all people

covid.todo=ggplot(data=sinsexocovid, aes(x=sinsexocovid$FECHA, y=sinsexocovid$freq, group=sinsexocovid$METODODX))+
  scale_x_date(date_breaks = "30 days",date_labels = "%d-%b")+
  geom_line(aes(color=sinsexocovid$METODODX))+
  labs(colour="",title="Covid-19 positive´s people time serie",
       x ="Dates", 
       y = "Number of Covid-19 positives")+
  theme(legend.position="top",legend.text = element_text(color = "black", size = 14,face="bold"), axis.text.x=element_text(size=11,colour = "black",face="bold",angle=45, hjust=1),axis.text.y=element_text(size=11,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=14,face="bold"),title = element_text(size=16,colour = "black",face="bold"))

ggsave("covid.todo.png", dpi = 600,   width = 250,
       height = 159,unit="mm",plot = covid.todo)

###############################################################################
```
## Positive-COVID´s people

In this pictures are showed that number of people who had been had positive covid´s tests during 2020 to now. In the legend, "PM, PR and AG"  means molecular test, serological test and antigen test in spanish respectively.the most important to describe is to high magnitude of positive covid´s test in third wave. the dataset have not got number of negative covid´s tests. Because it wasn´t able to normalize. therefore, this dataset isn´t satisfied minimum requirement.

![alt text](https://github.com/jasb3110/COVIDPERU/blob/1fc1af589150d750db96aae4a4d21699846df300/covid.hombres.png?raw=true)

![alt text](https://github.com/jasb3110/COVIDPERU/blob/1fc1af589150d750db96aae4a4d21699846df300/covid.mujeres.png?raw=true)

![alt text](https://github.com/jasb3110/COVIDPERU/blob/1fc1af589150d750db96aae4a4d21699846df300/covid.todo.png?raw=true)

Many scientists and journalists said in Media that booming of mortality on Peru in SINADEF´s data which owned to COVID´s death growth. So that, I´m trying of finding the relationship between excess of death and COVID´s death. Therefore, SINADEF´s data was subtracted natural mortality effect in 2019. This step is to extract the possible effect of COVID´s death. Then I ought transform to normal distribution to the effect of COVID´s death and COVID´s death, reporting for MINSA. 


According to Spearman correlation test reported that excess of death and COVID´s death was 94% and coefficient of regression of Pearson was 0.74.   

![alt text](https://github.com/jasb3110/COVIDPERU/blob/1fc1af589150d750db96aae4a4d21699846df300/regresion.png?raw=true)

Alternately, I create a animation how to evolve COVID´s mortality since beginning of pandemic to now. this manner it could see famous three waves.

![alt text](https://github.com/jasb3110/COVIDPERU/blob/1fc1af589150d750db96aae4a4d21699846df300/regresion.gif?raw=true)

Next, it is getting to clean and sort  data set of COVID´s death acording to Peruvian health ministry. Bellow I attached lines scripts.

```markdown
################################################################################
#Covid mortality 
#to search NA values
mm1=sum(is.na(mcovid$FECHA_FALLECIMIENTO))
mm2=sum(is.na(mcovid$EDAD_DECLARADA))
mm3=sum(is.na(mcovid$SEXO))
mm4=sum(is.na(mcovid$FECHA_NAC))#falta datos de fechas nacimientos
mm5=sum(is.na(mcovid$DEPARTAMENTO))
mm6=sum(is.na(mcovid$PROVINCIA))
mm7=sum(is.na(mcovid$DISTRITO))
mm1
mm2
mm3
mm4
mm5
mm6
mm7

mm8=sum(mcovid$FECHA_FALLECIMIENTO=="")
mm9=sum(mcovid$EDAD_DECLARADA=="")
mm10=sum(mcovid$SEXO=="")
mm11=sum(mcovid$FECHA_NAC=="")#falta datos
mm12=sum(mcovid$DEPARTAMENTO=="")
mm13=sum(mcovid$PROVINCIA=="")#falta datos
mm14=sum(mcovid$DISTRITO=="")#falta datos
mm8
mm9
mm10
mm11
mm12
mm13#lack of data of provinces
mm14#lack of data of ditrits

mcovid$FECHA_FALLECIMIENTO=as.character(mcovid$FECHA_FALLECIMIENTO)
mcovid$dia=as.numeric(format(as.Date(mcovid$FECHA_FALLECIMIENTO,format="%Y%m%d"), format = "%d"))
mcovid$meses=as.numeric(format(as.Date(mcovid$FECHA_FALLECIMIENTO,format="%Y%m%d"), format = "%m"))
mcovid$años=as.numeric(format(as.Date(mcovid$FECHA_FALLECIMIENTO,format="%Y%m%d"), format = "%Y"))

mcovid$fechas=paste0(mcovid$años,"-",mcovid$meses,"-",mcovid$dia)
mcovid$fechas2=paste0(mcovid$dia,"-",mcovid$meses,"-",mcovid$años)
mcovid$EDAD_DECLARADA=as.numeric(mcovid$EDAD_DECLARADA)
covid=as.data.frame(mcovid)

#Delete Nas and  null values
muertos_covid_total=as.data.frame(mcovid%>%count(FECHA_FALLECIMIENTO
                                                 ,EDAD_DECLARADA
                                                 ,SEXO               
                                                 ,DEPARTAMENTO
                                                 ,dia
                                                 ,meses
                                                 ,años))


ord=unique(sort(mcovid$fechas))
mcovid$date=rep(NA,length(mcovid$FECHA_FALLECIMIENTO))
for(i in 1:length(ord)){
  mcovid$date[which(mcovid$fechas==ord[i])]=i
}
mcovid=as.data.frame(mcovid)

mcovid$fechas2=as.Date(mcovid$fechas2,format="%d-%m-%Y")
muertos_covid_total=as.data.frame(mcovid%>%count(fechas2,dia,meses,años))
encovid=as.data.frame(encovid)

plot(todos$fechas,todos$n,col="blue",ylim=c(0,1100))
points(muertos_covid_total$fechas2,muertos_covid_total$n,col="red")

write.csv(todos,"todos.csv",sep=",",dec=".",col.names=TRUE)

#to create data of COVID death and excess of death which likely due to COVID-19 illness.

deathsextra=as.data.frame(rbind(cbind(1:length(muertos_covid_total$n),encovid$m_encovid_medio[1:length(muertos_covid_total$n)],"Excess of normalized natural deaths"),
                               cbind(1:length(muertos_covid_total$n),muertos_covid_total$n,"COVID-19´s deaths")))

colnames(deathsextra)=c("Dias con el COVID","Número de muertos","Estimador")

deathsextra$`Dias con el COVID`=as.numeric(deathsextra$`Dias con el COVID`)
deathsextra$`Número de muertos`=as.numeric(deathsextra$`Número de muertos`)
deathsextra$Estimador=factor(deathsextra$Estimador,levels = unique(deathsextra$Estimador))
deathsextra$fecha=as.Date(muertos_covid_total$fechas2,format="%Y-%m-%d")

fall.serie=ggplot(data =deathsextra, aes(x = deathsextra$fecha, y =deathsextra$`Número de muertos`,group=deathsextra$Estimador))+
  geom_line(aes(color=deathsextra$Estimador))+
  scale_x_date(date_breaks = "60 days",date_labels = "%d-%m-%Y")+
  labs(colour="",title="Deaths time series since 16th march of 2020 to now",
       x ="Dates to begin Covid-19 epidemic", 
       y = "Number of deaths")+
  theme(legend.position="top",legend.text = element_text(color = "black", size = 14,face="bold"),axis.text.x=element_text(size=11,colour = "black",face="bold",angle=45, hjust=1),axis.text.y=element_text(size=11,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=14,face="bold"),title = element_text(size=16,colour = "black",face="bold"))

ggsave("serie.tiempo.fallecidosvsexcesodemuertos.png", dpi = 600,   width = 250,
       height = 159,unit="mm",plot =fall.serie)

#Relationship with covid death and excess of death

encovid$mes=as.numeric(encovid$mes)
encovid$año=as.numeric(encovid$año)
encovid$fecha=as.Date(paste0(encovid$año,"-",encovid$mes,"-",encovid$dia),format="%Y-%m-%d")
muertos_covid_total$fechas2=as.Date(muertos_covid_total$fechas2,format="%Y-%m-%d")

relation=as.data.frame(cbind(1:(length(muertos_covid_total$n)-1),
                             encovid$m_encovid_medio[which(encovid$fecha==muertos_covid_total$fechas2[2]):which(encovid$fecha==muertos_covid_total$fechas2[length(muertos_covid_total$fechas2)-7])],
                             muertos_covid_total$n[1:(length(muertos_covid_total$n)-1)]))
mes.abb=c("January","February","March","April","May","June","July","Agost","September","Octuber","November","December")

relation$mes=NULL
for(i in 1:(length(muertos_covid_total$n)-1)){
  relation$mes[i]=mes.abb[muertos_covid_total$meses[i]]
}  

colnames(relation)=c("Días","Exceso de muertos normalizada durante 2020 al 2022", "Muertos por COVID-19","mes")
relation$mes=factor(relation$mes,levels=mes.abb)
cor.test(relation$`Exceso de muertos normalizada durante 2020`,relation$`Muertos por COVID-19`,method = "spearman")#con desfase de un dia 

qqnorm(relation$`Muertos por COVID-19`,type="o")+qqline(relation$`Muertos por COVID-19`,col="red")
qqnorm(relation$`Exceso de muertos normalizada durante 2020`,type="o")+qqline(relation$`Exceso de muertos normalizada durante 2020`,col="red")

library(bestNormalize)
bestNormalize(relation$`Exceso de muertos normalizada durante 2020`)
relation$exceso.nor=predict(orderNorm(relation$`Exceso de muertos normalizada durante 2020`))

bestNormalize(relation$`Muertos por COVID-19`)
relation$muertos.nor=predict(orderNorm(relation$`Muertos por COVID-19`))
relation=as.data.frame(na.omit(relation))

write.csv(relation,"relation.csv",sep=",",dec=".",col.names=TRUE)
reg=lm(relation$exceso.nor~relation$muertos.nor,data = relation)

relation$fechas=as.Date("3-3-2020",format="%d-%m-%Y")+relation$Días-1

relation$fechas=as.Date(relation$fechas,format="%d-%m-%Y")

require(ggplot2)
ggplotRegression <- function (fit) {
  Label=paste("y =",round(fit$coef[[1]],3),"+",
              round(fit$coef[[2]], 3),"x",
              "  R2 = ", round(summary(fit)$r.squared, 2),
              "  n =",length(fit$fitted.values),
              "  p-value =",round(summary(fit)$coef[2,4],4))
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm",se=TRUE,col="black") +
    annotate(geom = 'text', x =-1, y =3, label = Label, parse=FALSE)
}
p=ggplotRegression(reg)+
  geom_point(aes(fill=relation$mes), shape = 21, alpha = 0.99,size=3)+
  scale_color_viridis(discrete = TRUE, option = "C",direction = -1)+
  scale_fill_viridis(discrete = TRUE,direction = -1)+
  theme_gray()+
  labs(colour="",title="Covid-19´s deaths vs Excess of deaths during pandemic",
       x ="Number of normalized Covid-19´s deaths", 
       y = "Excess of normalized deaths")+
  theme(axis.text=element_text(size=14,colour = "black",face="bold"),
        axis.title=element_text(size=16,face="bold"),
        title = element_text(size=18,colour = "black",face="bold"),
        legend.text = element_text(color = "black", size = 14,face="bold"),
        legend.title=element_blank()
  )
ggsave("regresion.png", dpi = 600,   width = 275,
       height = 175,unit="mm",plot =p)

#Animation of relationship between of excess of deaths and COVID´s death

p2=ggplot(relation,aes(x = relation$muertos.nor, y = relation$exceso.nor))+
  geom_point(aes(fill=relation$mes), shape = 21, alpha = 0.99,size=3)+
  scale_color_viridis(discrete = TRUE, option = "C",direction = -1)+
  scale_fill_viridis(discrete = TRUE,direction = -1)+
  theme_gray()+labs(colour="",
                    x ="Number of normalized Covid-19´s deaths", 
                    y = "Excess of normalized deaths")+
  theme(axis.text=element_text(size=14,colour = "black",face="bold"),
        axis.title=element_text(size=16,face="bold"),
        title = element_text(size=18,colour = "black",face="bold"),
        legend.text = element_text(color = "black", size = 14,face="bold"),
        legend.title=element_blank())+
  transition_time(relation$fechas)+
  shadow_mark(alpha = 0.3, size = 4)+
  labs(title = "Covid-19´s deaths vs Excess of deaths during pandemic on {frame_time}")+
  shadow_wake(wake_length = 0.1, alpha = 0.99)

animate(p2, fps = 15, duration =25,renderer = gifski_renderer("regresion.gif"),height=630,width=1000)

#Spearman correlation between of excess of deaths and COVID´s death
rho=cor.test(relation$muertos.nor,relation$exceso.nor,method = "spearman")$estimate
signif(rho,2)

#to calculate a underestime
muertosreportados=sum(muertos_covid_total$n[1:length(muertos_covid_total$fechas)])
muertostotales=sum(todos$n[(ini+3):fin])
muertostotales

estimado_muertos_reales=round(muertostotales*rho-muertosreportados,digits=0)
estimado_muertos_reales
subestimacion=(muertostotales*rho-muertosreportados)/(muertostotales*rho)#subestimacion subestimacion# underestimate in percent
################################################################################
```
## Molecular positivity of COVID´s in percent on Peru
The big deal of Peru was how to estimate positivity rate (%), when there isn´t enougth data (small tests and delay in reports). National of healthy institution of Peru(INS in spanish), generally update data per 15 days. However, I currently noticed that data was reported after two month. Well, I show that positive-COVID´s tests in percent (%) during 2020 to now. 

![alt text](https://github.com/jasb3110/COVIDPERU/blob/1fc1af589150d750db96aae4a4d21699846df300/relacion.positivas.positivas+negativas.molecular.png?raw=true)

In this picture, the most highlight is three sharp of curve of molecular positivity of COVID´s in percent (>20%). but, after third peak was recorded, the positivity rate had been decreased sharply. it could be due to synergy effect of vaccination on Peruvian people and wide spreading of COVID virus. bellow, attached a script.

```markdown
################################################################################
#INS dataset of molecular tests

#for 2020, data set by month

ma=read.delim("pm_mar_2020.csv",sep=",",dec=".",header=TRUE)#marzo
ab=read.delim("pm_apr_2020.csv",sep=",",dec=".",header=TRUE)#abril
my=read.delim("pm_may_2020.csv",sep=",",dec=".",header=TRUE)#mayo
jn=read.delim("pm_jun_2020.csv",sep=",",dec=".",header=TRUE)#junio
jl=read.delim("pm_jul_2020.csv",sep=",",dec=".",header=TRUE)#julio
ag=read.delim("pm_ago_2020.csv",sep=",",dec=".",header=TRUE)#agosto
st=read.delim("pm_set_2020.csv",sep=",",dec=".",header=TRUE)#setiembre
oc=read.delim("pm_oct_2020.csv",sep=",",dec=".",header=TRUE)#octubre
no=read.delim("pm_nov_2020.csv",sep=",",dec=".",header=TRUE)#noviembre
di=read.delim("pm_dic_2020.csv",sep=",",dec=".",header=TRUE)#diciembre

mar=fread("pm25Marzo2021.csv",sep="|",dec=".",header=TRUE,fill=TRUE)#marzo 2021

p.m0=rbind(ma,ab,my,jn,jl,ag,st,oc,no,di)#solo 2020
colnames(p.m0)[1]="FECHATOMAMUESTRA"
p.m0$FECHATOMAMUESTRA=as.Date(as.character(p.m0$FECHATOMAMUESTRA),format="%Y-%m-%d")
p.m0$dia=as.numeric(format(p.m0$FECHATOMAMUESTRA,format="%d"))
p.m0$mes=as.numeric(format(p.m0$FECHATOMAMUESTRA,format="%m"))
p.m0$año=as.numeric(format(p.m0$FECHATOMAMUESTRA,format="%Y"))
p.m0$año=2020

p.m1=mar
colnames(p.m1)[1]="FECHATOMAMUESTRA"

p.m1$FECHATOMAMUESTRA=as.Date(as.character(p.m1$FECHATOMAMUESTRA),format="%Y-%m-%d")
p.m1$dia=as.numeric(format(p.m1$FECHATOMAMUESTRA,format="%d"))
p.m1$mes=as.numeric(format(p.m1$FECHATOMAMUESTRA,format="%m"))
p.m1$año=as.numeric(format(p.m1$FECHATOMAMUESTRA,format="%Y"))
p.m1.2=p.m1
step=min(which(p.m1.2$FECHATOMAMUESTRA=="2021-1-1")):max(which(p.m1.2$FECHATOMAMUESTRA=="2021-2-28"))

p.m1.2=p.m1.2[step,]

p.m1.2=p.m1.2[which(p.m1.2$año==2021),]

p.m1.2$FECHATOMAMUESTRA=paste0(p.m1.2$año,"-",p.m1.2$mes,"-",p.m1.2$dia)

p.m.t1=as.data.frame(cbind(p.m1.2$FECHATOMAMUESTRA,p.m1.2$tipomuestra,p.m1.2$RESULTADO,
                           p.m1.2$edadpaciente_c,p.m1.2$sexopaciente,p.m1.2$Institucion,
                           p.m1.2$DepOrigen,p.m1.2$ProvOrigen,p.m1.2$DepOrigen,p.m1.2$ProvOrigen,
                           p.m1.2$dia,p.m1.2$mes,p.m1.2$año))


setie=fread("pm21Septiembre2021.csv",sep="|",dec=".",header=TRUE,fill=TRUE)

p.m=setie
colnames(p.m)[3]="FECHATOMAMUESTRA"
p.m$UUID=NULL
p.m$FECHATOMAMUESTRA=as.Date(as.character(p.m$FECHATOMAMUESTRA),format="%Y%m%d")
p.m$dia=as.numeric(format(p.m$FECHATOMAMUESTRA,format="%d"))
p.m$mes=as.numeric(format(p.m$FECHATOMAMUESTRA,format="%m"))
p.m$año=as.numeric(format(p.m$FECHATOMAMUESTRA,format="%Y"))
p.m2=p.m
p.m2$año[which(p.m2$año==2121)]=2021
p.m2$FECHATOMAMUESTRA=paste0(p.m2$año,"-",p.m2$mes,"-",p.m2$dia)
step2=min(which(p.m2$FECHATOMAMUESTRA=="2021-3-1")):max(which(p.m2$FECHATOMAMUESTRA=="2021-7-31"))
p.m2=p.m2[step2,]# marzo a julio
step2.1=which(p.m2$mes==3|p.m2$mes==4|p.m2$mes==5|p.m2$mes==6|p.m2$mes==7)
p.m2=p.m2[step2.1,]

p.m2$año[which(p.m2$año==2001)]=2021
p.m2$año[which(p.m2$año==2011)]=2021
p.m2$año[which(p.m2$año==2012)]=2021
p.m2$año[which(p.m2$año==2019)]=2021
p.m2$año[which(p.m2$año==2121)]=2021

step2.2=which(p.m2$año==2021)
p.m2=p.m2[step2.2,]

p.m.t2=as.data.frame(cbind(p.m2$FECHATOMAMUESTRA,p.m2$TIPO_MUESTRA,p.m2$RESULTADO,
                          p.m2$Edad,p.m2$Sexo,p.m2$Institucion,p.m2$DEPARTAMENTO_PACIENTE,
                          p.m2$PROVINCIA_PACIENTE,p.m2$DEPARTAMENTO_MUESTRA,p.m2$PROVINCIA_MUESTRA,
                          p.m2$dia,p.m2$mes,p.m2$año))

novie=fread("pm28Noviembre2021.csv",sep="|",dec=".",header=TRUE,fill=TRUE)

p.m=novie
colnames(p.m)[3]="FECHATOMAMUESTRA"
p.m$UUID=NULL
p.m$FECHATOMAMUESTRA=as.Date(as.character(p.m$FECHATOMAMUESTRA),format="%Y%m%d")
p.m$dia=as.numeric(format(p.m$FECHATOMAMUESTRA,format="%d"))
p.m$mes=as.numeric(format(p.m$FECHATOMAMUESTRA,format="%m"))
p.m$año=as.numeric(format(p.m$FECHATOMAMUESTRA,format="%Y"))
p.m3=p.m
p.m3$año[which(p.m3$año==2121)]=2021
p.m3$FECHATOMAMUESTRA=paste0(p.m3$año,"-",p.m3$mes,"-",p.m3$dia)
step3=min(which(p.m3$FECHATOMAMUESTRA=="2021-8-1")):max(which(p.m3$FECHATOMAMUESTRA=="2021-10-31"))
p.m3=p.m3[step3,]# agosto a 31/10/2021
step3.1=which(p.m3$mes==8|p.m3$mes==9|p.m3$mes==10)
p.m3=p.m3[step3.1,]

p.m3$año[which(p.m3$año==2001)]=2021
p.m3$año[which(p.m3$año==2011)]=2021
p.m3$año[which(p.m3$año==2012)]=2021
p.m3$año[which(p.m3$año==2019)]=2021
p.m3$año[which(p.m3$año==2121)]=2021
p.m3$año[which(p.m3$año==2020&p.m3$mes<=11)]=2021
step3.2=which(p.m3$año==2021)
p.m3=p.m3[step3.2,]

p.m.t3=as.data.frame(cbind(p.m3$FECHATOMAMUESTRA,p.m3$TIPO_MUESTRA,p.m3$RESULTADO,
                           p.m3$Edad,p.m3$Sexo,p.m3$Institucion,p.m3$DEPARTAMENTO_PACIENTE,
                           p.m3$PROVINCIA_PACIENTE,p.m3$DEPARTAMENTO_MUESTRA,p.m3$PROVINCIA_MUESTRA,
                           p.m3$dia,p.m3$mes,p.m3$año))

#
diciem=fread("pm14Enero2022.csv",sep="|",dec=".",header=TRUE,fill=TRUE)

p.m=diciem
colnames(p.m)[3]="FECHATOMAMUESTRA"
p.m$UUID=NULL
p.m$FECHATOMAMUESTRA=as.Date(as.character(p.m$FECHATOMAMUESTRA),format="%Y%m%d")
p.m$dia=as.numeric(format(p.m$FECHATOMAMUESTRA,format="%d"))
p.m$mes=as.numeric(format(p.m$FECHATOMAMUESTRA,format="%m"))
p.m$año=as.numeric(format(p.m$FECHATOMAMUESTRA,format="%Y"))
p.m4=p.m
p.m4$año[which(p.m4$año==2121)]=2021
p.m4$FECHATOMAMUESTRA=paste0(p.m4$año,"-",p.m4$mes,"-",p.m4$dia)
step4=min(which(p.m4$FECHATOMAMUESTRA=="2021-11-1")):max(which(p.m4$FECHATOMAMUESTRA=="2021-12-31"))
p.m4=p.m4[step4,]#01/11/2021 a 31/12/2021
step4.1=which(p.m4$mes==11|p.m4$mes==12)
p.m4=p.m4[step4.1,]

p.m4$año[which(p.m4$año==2001)]=2021
p.m4$año[which(p.m4$año==2011)]=2021
p.m4$año[which(p.m4$año==2012)]=2021
p.m4$año[which(p.m4$año==2019)]=2021
p.m4$año[which(p.m4$año==2121)]=2021
p.m4$año[which(p.m4$año==2020&p.m4$mes<=12)]=2021
step4.2=which(p.m4$año==2021)
p.m4=p.m4[step4.2,]

p.m.t4=as.data.frame(cbind(p.m4$FECHATOMAMUESTRA,p.m4$TIPO_MUESTRA,p.m4$RESULTADO,
                           p.m4$Edad,p.m4$Sexo,p.m4$Institucion,p.m4$DEPARTAMENTO_PACIENTE,
                           p.m4$PROVINCIA_PACIENTE,p.m4$DEPARTAMENTO_MUESTRA,p.m4$PROVINCIA_MUESTRA,
                           p.m4$dia,p.m4$mes,p.m4$año))

#
enero=fread("pm19Abr2022.csv",sep="|",dec=".",header=TRUE,fill=TRUE)

p.m=enero
colnames(p.m)[3]="FECHATOMAMUESTRA"
p.m$UUID=NULL
p.m$FECHATOMAMUESTRA=as.Date(as.character(p.m$FECHATOMAMUESTRA),format="%Y%m%d")
p.m$dia=as.numeric(format(p.m$FECHATOMAMUESTRA,format="%d"))
p.m$mes=as.numeric(format(p.m$FECHATOMAMUESTRA,format="%m"))
p.m$año=as.numeric(format(p.m$FECHATOMAMUESTRA,format="%Y"))
p.m5=p.m
p.m5$FECHATOMAMUESTRA=paste0(p.m5$año,"-",p.m5$mes,"-",p.m5$dia)
step5=min(which(p.m5$FECHATOMAMUESTRA=="2022-01-1")):max(which(p.m5$FECHATOMAMUESTRA=="2022-04-19"))
p.m5=p.m5[step5,]#01/01/2022 a 19/04/2022
step5.1=which(p.m5$mes==01|p.m5$mes==02|p.m5$mes==03|p.m5$mes==04)
p.m5=p.m5[step5.1,]

p.m5$año[which(p.m5$año==2002)]=2022
p.m5$año[which(p.m5$año==2012)]=2022
p.m5$año[which(p.m5$año==2022&p.m5$mes<=04)]=2022
step5.2=which(p.m5$año==2022)
p.m5=p.m5[step5.2,]

p.m.t5=as.data.frame(cbind(p.m5$FECHATOMAMUESTRA,p.m5$TIPO_MUESTRA,p.m5$RESULTADO,
                           p.m5$Edad,p.m5$Sexo,p.m5$Institucion,p.m5$DEPARTAMENTO_PACIENTE,
                           p.m5$PROVINCIA_PACIENTE,p.m5$DEPARTAMENTO_MUESTRA,p.m5$PROVINCIA_MUESTRA,
                           p.m5$dia,p.m5$mes,p.m5$año))
#
names.pm=c("fecha","tipomuestra",
           "RESULTADO","edad",   
           "sexo","Institucion",      
           "Departamento","Provincia",       
           "Disa_DiresaOrigen","Red",        
           "dia","mes",              
           "año")

colnames(p.m0)=names.pm
colnames(p.m.t1)=names.pm
colnames(p.m.t2)=names.pm
colnames(p.m.t3)=names.pm
colnames(p.m.t4)=names.pm
colnames(p.m.t5)=names.pm

pm=rbind(p.m0,p.m.t1,p.m.t2,p.m.t3,p.m.t4,p.m.t5)#grupo de datos
pm=as.data.frame(pm)

#to clean of dataset

pm$edad=gsub("años","",pm$edad)
pm$edad=gsub("Años","",pm$edad)
pm$edad=gsub("AÃ±os","",pm$edad)
pm$edad=gsub("aÃ±os","",pm$edad)
pm$edad=gsub(" ","",pm$edad)

grupo_etarios=unique(pm$edad)
pm$edad=factor(pm$edad,levels=grupo_etarios)

pm2=pm

pm2$RESULTADO[which(is.na(pm2$RESULTADO))]="NEGATIVO" 
pm2$RESULTADO[which(pm2$RESULTADO=="INVALIDO")]="NEGATIVO"                                                                        
pm2$RESULTADO[which(pm2$RESULTADO=="NEGTIVO")]="NEGATIVO"                                                                                                                                                              
pm2$RESULTADO[which(pm2$RESULTADO=="Identificación del Panel Virus Respiratorio   Identificación del Panel Virus Respiratorio : Influenza A   NEGATIVO   Identificación del Panel Virus Respiratorio : Influenza B   NEGATIVO   Identificación del Panel Virus Respiratorio : SARSCoV2   NEGATIVO")]="NEGATIVO"
pm2$RESULTADO[which(pm2$RESULTADO=="Identificación del Panel Virus Respiratorio   Identificación del Panel Virus Respiratorio : Influenza A   NEGATIVO   Identificación del Panel Virus Respiratorio : Influenza B   NEGATIVO   Identificación del Panel Virus Respiratorio : SARSCoV2   POSITIVO")]="POSITIVO"
pm2$RESULTADO[which(pm2$RESULTADO=="Identificación del Panel Virus Respiratorio   Identificación del Panel Virus Respiratorio : SARSCoV2   NEGATIVO")]="NEGATIVO"                                                                                                                                                
pm2$RESULTADO[which(pm2$RESULTADO=="Identificación del Panel Virus Respiratorio   Identificación del Panel Virus Respiratorio : Influenza A   POSITIVO   Identificación del Panel Virus Respiratorio : Influenza B   NEGATIVO   Identificación del Panel Virus Respiratorio : SARSCoV2   NEGATIVO")]="NEGATIVO"
pm2$RESULTADO[which(pm2$RESULTADO=="Identificación del Panel Virus Respiratorio   Identificación del Panel Virus Respiratorio : Influenza A   POSITIVO   Identificación del Panel Virus Respiratorio : Influenza B   POSITIVO   Identificación del Panel Virus Respiratorio : SARSCoV2   NEGATIVO")]="NEGATIVO"
pm2$RESULTADO[which(pm2$RESULTADO=="")]="NEGATIVO"                                                                                                                                                                                                                                                                 
pm2$RESULTADO[which(pm2$RESULTADO=="- POSITIVO")]="POSITIVO"                                                                                                                                                                                                                                                       
pm2$RESULTADO[which(pm2$RESULTADO=="Identificación del Panel Virus Respiratorio -  Identificación del Panel Virus Respiratorio : VSR -  POSITIVO -")]="POSITIVO" 
pm2$RESULTADO[which(pm2$RESULTADO=="NEGATI")]="NEGATIVO" 


pm2$fecha=as.Date(as.character(paste0(pm2$año,"-",pm2$mes,"-",pm2$dia)),format="%Y-%m-%d")

data.pm=pm2%>%count(fecha
                  ,RESULTADO
                  ,dia
                  ,mes
                  ,año)
data.pm$fecha=as.Date(data.pm$fecha,format="%Y-%m-%d")
data.pm$n=as.numeric(data.pm$n)
data.pm$RESULTADO=factor(data.pm$RESULTADO,levels=unique(data.pm$RESULTADO))

data.pm=na.omit(data.pm)

resul.mol=ggplot(data=data.pm, aes(x=data.pm$fecha, y=data.pm$n, group=data.pm$RESULTADO))+
  scale_x_date(date_breaks = "30 days",date_labels = "%d-%m-%Y")+
  geom_line(aes(color=data.pm$RESULTADO))+
  geom_point(aes(color=data.pm$RESULTADO))+
  labs(colour="",title="Covid-19´s molecular test time serie",
       x ="Dates", 
       y = "Number of Molecular tests")+
  theme(legend.position="top",legend.text = element_text(color = "black", size = 14,face="bold"),
        axis.text.x=element_text(size=11,colour = "black",face="bold",angle=45, hjust=1),
        axis.text.y=element_text(size=11,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size=16,colour = "black",face="bold"))

ggsave("serie.tiempo.resultados.moleculares.png", dpi = 600,   width = 250,
       height = 159,unit="mm",plot =resul.mol)

razon=as.data.frame(cbind(as.Date(data.pm$fecha[2*(1:(length(data.pm$fecha)*0.5))],format="%Y-%m-%d"),rep(NA,length(data.pm$fecha)*0.5)))
colnames(razon)=c("fecha","positividad")

razon$fecha=as.Date(data.pm$fecha[2*(1:(length(data.pm$fecha)*0.5))],format="%Y-%m-%d")
razon$dia=as.numeric(format(as.Date(data.pm$fecha[2*(1:(length(data.pm$fecha)*0.5))],format="%Y-%m-%d"), format = "%d"))
razon$mes=as.numeric(format(as.Date(data.pm$fecha[2*(1:(length(data.pm$fecha)*0.5))],format="%Y-%m-%d"), format = "%m"))
razon$año=as.numeric(format(as.Date(data.pm$fecha[2*(1:(length(data.pm$fecha)*0.5))],format="%Y-%m-%d"), format = "%Y"))

mes.abb=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")
razon$Mes=NULL
for(i in 1:length(razon$fecha)){
  razon$Mes[i]=mes.abb[razon$mes[i]]
}  

for(i in 1:(length(data.pm$fecha)*0.5)){
  razon$positividad[i]=round(data.pm$n[2*i]*100/(data.pm$n[2*i]+data.pm$n[2*i-1]),2)
}

razon$Mes=factor(razon$Mes,levels=mes.abb)

pos.mol=ggplot(data=razon, aes(x=razon$fecha, y=razon$positividad))+
  scale_x_date(date_breaks = "15 days",date_labels = "%d-%b")+
  geom_line()+
  geom_point(aes(fill=razon$Mes), shape = 21, alpha = 0.99,size=2)+scale_color_viridis(discrete = TRUE, option = "C",direction = -1)+
  scale_fill_viridis(discrete = TRUE,direction = -1)+
  labs(colour="",title="Covid-19´s molecular positivity rate",
       x ="Dates", 
       y = "Percent %")+
  theme(legend.position="top",legend.title=element_blank(),legend.text = element_text(color = "black", size = 14,face="bold"), axis.text.x=element_text(size=11,colour = "black",face="bold",angle=45, hjust=1),axis.text.y=element_text(size=11,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=14,face="bold"),title = element_text(size=16,colour = "black",face="bold"))

ggsave("relacion.positivas.positivas+negativas.molecular.png", dpi = 600,   width = 250,
       height = 159,unit="mm",plot =pos.mol)

```
## OPENCOVIDPERU data set
I really appreciate a supporting of OPENCOVIDPERU for sharing your data source with me. In this step, I tried to show principal results [(3)](#references).










```markdown
#OPENCOVIDPERU data set
defase=read.delim("DESFASE CIFRAS - MAIN.csv",sep=",",dec=".")#fallecidos segun SINADEF
pruebas=read.delim("PRUEBAS - Data Peru.csv",sep=",",dec=".")
uci=read.delim("UCI HOSP SUSALUD - UCI Beds.csv",sep=",",dec=".")

mes.abb=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")

defase=as.data.frame(defase)
pruebas=as.data.frame(pruebas)
uci=as.data.frame(uci)
UCI.MINSA=as.data.frame(UCI.MINSA)

camitass=as.data.frame(UCI.MINSA[,c(1,12,21,22,31,32,41,42,51,52,61,62,70,71,79,80,88,89,97,98,106,107,116,117)])

for(i in 3:ncol(camitass)){
camitass[which(is.na(camitass[,i])),i]=0
}

camitas=camitass[,1:5]

camitas[,4]=rowSums(camitass[,seq(3,ncol(camitass),2)])
camitas[,5]=rowSums(camitass[,seq(4,ncol(camitass),2)])
camitas[,3]=camitas[,4]+camitas[,5]

camitas$fecha2=as.character(camitas$FECHACORTE)
camitas$fecha2=as.Date(camitas$fecha2,format="%Y%m%d")
camitas$FECHACORTE=NULL

colnames(camitas)=c("provincia","total","disponible","ocupada","fecha")

ccama=as.data.frame(camitas%>%count(fecha,provincia,total,disponible,ocupada))

ccama$dis=ccama$disponible*ccama$n
ccama$ocu=ccama$ocupada*ccama$n

ccam=ccama
ccam[which(ccam$ocu==0&ccam$dis==0),]=NA
ccam=na.omit(ccam)

ccam$disponible=NULL
ccam$ocupada=NULL
ccam$n=NULL
ccam$total=NULL
prov=unique(ccam$provincia)
fechasss=unique(ccam$fecha)
ccam$provincia=factor(ccam$provincia,levels=prov)

#errror

#prov.perd=ccam$fecha[which(ccam$provincia=="")]
#prov.p=ccam$provincia[which(ccam$fecha==prov.perd-1)]
#prov.p2=ccam$provincia[which(ccam$fecha==prov.perd-1)]
#prov.p3=ccam$provincia[which(ccam$fecha==prov.perd+1)]

#count(prov.p)[2]-rbind(count(prov.p2)[2])#dep 3 o 6
#count(prov.p)[2]-rbind(count(prov.p3)[2])#del 2 o 6
#se asume dep 6 
#ccam$provincia[which(ccam$provincia=="")]=count(prov.p)[[1]][6]

depa=c("Amazonas",
       "Ancash",
       "Apurimac",
       "Arequipa",
       "Ayacucho",
       "Cajamarca",
       "Callao",
       "Cuzco",
       "Huancavelica",
       "Huanuco",
       "Ica",
       "Junín",
       "La Libertad",
       "Lambayeque",
       "Lima provincia",
       "Loreto",
       "Madre de Dios",
       "Moquegua",
       "Pasco",
       "Piura",
       "Puno",
       "San Martín",
       "Tacna",
       "Tumbes",
       "Ucayali")


mdepa=c("AMAZONAS",
       "ANCASH",
       "APURIMAC",
       "AREQUIPA",
       "AYACUCHO",
       "CAJAMARCA",
       "CALLAO",
       "CUZCO",
       "HUANCAVELICA",
       "HUANUCO",
       "ICA",
       "JUNIN",
       "LA LIBERTAD",
       "LAMBAYAQUE",
       "LIMA",
       "LORETO",
       "MADRE DE DIOS",
       "MOQUEGUA",
       "PASCO",
       "PIURA",
       "PUNO",
       "SAN MARTIN",
       "TACNA",
       "TUMBES",
       "UCAYALI")

prov=mdepa
ccam$provincia=factor(ccam$provincia,levels=mdepa)

depylima=c("Amazonas",
           "Ancash",
           "Apurimac",
           "Arequipa",
           "Ayacucho",
           "Cajamarca",
           "Callao",
           "Cuzco",
           "Huancavelica",
           "Huanuco",
           "Ica",
           "Junín",
           "La Libertad",
           "Lambayeque",
           "Lima provincia",
           "Loreto",
           "Madre de Dios",
           "Moquegua",
           "Pasco",
           "Piura",
           "Puno",
           "San Martín",
           "Tacna",
           "Tumbes",
           "Ucayali",
           "Lima metropolitana")

fecha=paste0(defase$Fecha[1:191],"-",2020,"")
fecha[192:length(defase$Fecha)]=paste0(defase$Fecha[192:length(defase$Fecha)],"-",2021,"")
defase$fecha=as.Date(fecha,format="%d-%m-%Y")

defase$dia=  as.numeric(format(as.Date(fecha,format="%d-%m-%Y"), format = "%d"))
defase$meses=as.numeric(format(as.Date(fecha,format="%d-%m-%Y"), format = "%m"))
defase$años= as.numeric(format(as.Date(fecha,format="%d-%m-%Y"), format = "%Y"))
defase$fecha=as.Date(paste0(defase$años,"-",defase$meses,"-",defase$dia),format="%Y-%m-%d")  

desfase=as.data.frame(cbind(rep(defase$fecha,(length(depa)+length(depylima)))
                            ,rep(NA,length(defase$fecha)*(length(depa)+length(depylima)))
                            ,rep(NA,length(defase$fecha)*(length(depa)+length(depylima)))
                            ,rep(NA,length(defase$fecha)*(length(depa)+length(depylima)))
                            ,rep(NA,length(defase$fecha)*(length(depa)+length(depylima)))
))
colnames(desfase)=c("Fecha","Zona","Fuente","Casos","Fallecidos")
desfase$Fecha=as.Date(rep(defase$fecha,length(depa)+length(depylima)),format="%Y-%m-%d")

#Para la casos covid diresa
library("tidyr")
diresa=defase[,27:51]
for(i in 1:25){
  diresa[,i]=as.numeric(gsub(",","",defase[,26+i]))
}
colnames(diresa)=depa
diresa=gather(diresa)
colnames(diresa)=c("Departamento","Casos")
write.csv(diresa,"diresa.csv",sep="")
Diresa=read.delim("diresa.csv",sep=",",header = TRUE)
Diresa$fuente="diresa"

#Para la casos covid minsa
minsa=defase[,53:78]
for(i in 1:26){
  minsa[,i]=as.numeric(gsub(",","",defase[,52+i]))
}
colnames(minsa)=depylima
minsa=gather(minsa)
colnames(minsa)=c("Departamento","Casos")
write.csv(minsa,"minsa.csv",sep="")
Minsa=read.delim("minsa.csv",sep=",",header = TRUE)
Minsa$fuente="minsa"

#Para la fallecidos covid diresa
diresa.f=defase[,131:155]
for(i in 1:25){
  diresa.f[,i]=as.numeric(gsub(",","",defase[,130+i]))
}
colnames(diresa.f)=depa
diresa.f=gather(diresa.f)
colnames(diresa.f)=c("Departamento","Casos")
write.csv(diresa.f,"diresa.f.csv",sep="")
Diresa.f=read.csv("diresa.f.csv",sep=",",header = TRUE)
Diresa.f$fuente="diresa"

#Para la fallecidos covid minsa
minsa.f=defase[,157:182]
for(i in 1:26){
  minsa.f[,i]=as.numeric(gsub(",","",defase[,156+i]))
}
colnames(minsa.f)=depylima
minsa.f=gather(minsa.f)
colnames(minsa.f)=c("Departamento","Casos")
write.csv(minsa.f,"minsa.f.csv",sep="")
Minsa.f=read.delim("minsa.f.csv",sep=",",header = TRUE)
Minsa.f$fuente="minsa"

#Para la casos covid diris
DIRIS=c("CENTRO","NORTE","SUR","ESTE")
diris=defase[,85:88]
for(i in 1:4){
  diris[,i]=as.numeric(gsub(",","",defase[,84+i]))
}
colnames(diris)=DIRIS
diris=gather(diris)
colnames(diris)=c("Zona","Casos")
write.csv(diris,"diris.csv",sep="")
Diris=read.delim("diris.csv",sep=",",header = TRUE)
Diris$fuente="diris"
Diris=as.data.frame(Diris)
#Diris[is.na(Diris)]=0
colnames(Diris)=c("Fecha","Zona","Casos", "fuente")
Diris$Fecha=rep(fecha,4)

#Para la casos covid MINSA zonas
MINSA=c("CENTRO","NORTE","SUR","ESTE")
minsaz=defase[,90:93]
for(i in 1:4){
  minsaz[,i]=as.numeric(gsub(",","",defase[,89+i]))
}
colnames(minsaz)=MINSA
minsaz=gather(minsaz)
colnames(minsaz)=c("Zona","Casos")
write.csv(minsaz,"minsaz.csv",sep="")
Minsaz=read.delim("minsaz.csv",sep=",",header = TRUE)
Minsaz$fuente="minsa"
Minsaz=as.data.frame(Minsaz)
#Minsaz[is.na(Minsaz)]=0
colnames(Minsaz)=c("Fecha","Zona","Casos", "fuente")
Minsaz$Fecha=rep(fecha,4)

#uniendo la  data diresa, diris y minsa 
desfase$Zona=c(Diresa$Departamento,
               Minsa$Departamento)
desfase$Fuente=c(Diresa$fuente,
                 Minsa$fuente)
desfase$Casos=c(Diresa$Casos,
                Minsa$Casos)
desfase$Fallecidos=c(Diresa.f$Casos,
                     Minsa.f$Casos)
df=as.data.frame(desfase)
#df[is.na(df)]<-0

Diris.casos=as.data.frame(cbind(Diris$Fecha,Diris$Zona,Diris$fuente,Diris$Casos,rep(NA,length(Diris$Casos))))
colnames(Diris.casos)=c("Fecha","Zona","Fuente","Casos","Fallecidos")
Diris.casos$Fecha=as.Date(Diris$Fecha,format="%Y-%m-%d")
data=rbind(df,Diris.casos)
colnames(data)=c("Fecha","Zona","Fuente","Casos","Fallecidos")
data$Fecha=as.Date(data$Fecha,format="%Y-%m-%d")

data$dia=  as.numeric(format(as.Date(data$Fecha,format="%d-%m-%Y"), format = "%d"))
data$meses=as.numeric(format(as.Date(data$Fecha,format="%d-%m-%Y"), format = "%m"))
data$años= as.numeric(format(as.Date(data$Fecha,format="%d-%m-%Y"), format = "%Y"))
defase$años=as.numeric(format(as.Date(defase$fecha,format="%d-%m-%Y"), format = "%Y"))
data$fecha2=as.Date(paste0(defase$años,"-",defase$meses,"-",defase$dia),format="%Y-%m-%d") 

write.csv(defase,"defase.csv",sep=",",dec=".",col.names=TRUE)

data$Fallecidos[is.na(data$Fallecidos)]=0
data$Casos[is.na(data$Casos)]=0
todo.casos=as.data.frame(data%>%count(fecha2
                                      ,Fuente
                                      ,Casos))

diresa.diris=todo.casos[which(todo.casos$Fuente!="minsa"&todo.casos$Casos!=0),]

sum(diresa.diris$n>1)# error no se sabe por que
diresa.diris[which(diresa.diris$n>1),]#datos duplicados 
diresa.diris$n=NULL
infectados=as.data.frame(diresa.diris%>%count(fecha2
                                              ,Casos))
sum(infectados$n>1)# error no se sabe por que
infectados[which(infectados$n>1),]#datos duplicados
infectados$n=NULL
infectados$Casos=as.numeric(infectados$Casos)
infectados$fecha=as.Date(infectados$fecha2,format="%Y-%m-%d")

infe=aggregate(infectados$Casos, by=list(fechas=infectados$fecha), FUN=sum)
colnames(infe)=c("fecha","infec")
infe$mes=as.numeric(format(as.Date(infe$fecha,format="%Y-%m-%d"),format="%m"))

infe$meses=NULL
for(i in 1:length(infe$fecha)){
  infe$meses[i]=mes.abb[infe$mes[i]]
} 

infe$fecha=as.Date(infe$fecha,format="%Y-%m-%d")
infectados.acumulados=infe$infec
infe$infec=round(as.numeric(infe$infec)/1000000,6)
infe$meses=factor(infe$meses,levels=mes.abb)
infe=as.data.frame(infe,row.names =NULL)

infect=ggplot(data=infe, aes(x=infe$fecha, y=infe$infec))+
  scale_x_date(date_breaks="30 days",date_labels = "%d-%b")+
  geom_line()+
  geom_point(aes(fill=infe$meses), shape =21, alpha = 0.99,size=3)+
  scale_color_viridis(discrete = TRUE, option = "C",direction = -1)+
  scale_fill_viridis(discrete = TRUE,direction = -1)+
  labs(colour="",title="Covid-19´s patient according to Diresa & Diris",
       x ="Dates", 
       y = "Patients (Millons)")+
  scale_y_continuous(limits = c(0,1.5), breaks = round(seq(0,1.5,length.out = 4),2))+
  theme(legend.position="top",legend.title=element_blank(),legend.text = element_text(color = "black", size = 14,face="bold"),
        axis.text.x=element_text(size=11,colour = "black",face="bold",angle=45, hjust=1),axis.text.y=element_text(size=11,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=14,face="bold"),title = element_text(size=16,colour = "black",face="bold"))

ggsave("infectados.diresa.diris.png", dpi = 600,   width = 250,
       height = 159,unit="mm",plot =infect)

####Solo Minsa

solo.minsa=todo.casos[which(todo.casos$Fuente=="minsa"&todo.casos$Casos!=0),]

sum(solo.minsa$n>1)# error no se sabe por que
solo.minsa[which(solo.minsa$n>1),]#datos duplicados 

solo.minsa$n=NULL
solo.minsa$Casos=as.numeric(solo.minsa$Casos)
solo.minsa$fecha=as.Date(solo.minsa$fecha2,format="%Y-%m-%d")

infm=aggregate(solo.minsa$Casos, by=list(fechas=solo.minsa$fecha), FUN=sum)
colnames(infm)=c("fecha","infec")
infm$mes=as.numeric(format(as.Date(infm$fecha,format="%Y-%m-%d"),format="%m"))

infm$meses=NULL
for(i in 1:length(infm$fecha)){
  infm$meses[i]=mes.abb[infm$mes[i]]
} 

infm$fecha=as.Date(infm$fecha,format="%Y-%m-%d")
infm$infec=round(as.numeric(infm$infec)/1000000,6)
infm$meses=factor(infm$meses,levels=mes.abb)
infm=as.data.frame(infm,row.names =NULL)

infecm=ggplot(data=infm, aes(x=infm$fecha, y=infm$infec))+
  scale_x_date(date_breaks="30 days",date_labels = "%d-%b")+
  geom_line()+
  geom_point(aes(fill=infm$meses), shape = 21, alpha = 0.99,size=3)+
  scale_color_viridis(discrete = TRUE, option = "C",direction = -1)+
  scale_fill_viridis(discrete = TRUE,direction = -1)+
  labs(colour="",title="Infectados acumulados con Covid-19 Minsa",
       x ="Fechas", 
       y = "Millones de Infectados")+
  scale_y_continuous(limits = c(0,1.2), breaks = round(seq(0,1.2,length.out = 4),2))+
  theme(legend.position="top",legend.title=element_blank(),legend.text = element_text(color = "black", size = 14,face="bold"),
        axis.text.x=element_text(size=11,colour = "black",face="bold",angle=45, hjust=1),axis.text.y=element_text(size=11,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=14,face="bold"),title = element_text(size=16,colour = "black",face="bold"))

ggsave("infectados.minsa.acumulado.png", dpi = 600,   width = 250,
       height = 159,unit="mm",plot =infecm)

############vuebas unidas moleculares y rapidas

prueba=pruebas[,c(2,3,4,5,7,8,9)]
colnames(prueba)=c("fechas","M+","R+","A+","TM","TR","TA")

prueba$fechas[1:303]=paste0(prueba$fechas[1:303],"-2020")
prueba$fechas[304:length(prueba$fechas)]=paste0(prueba$fechas[304:length(prueba$fechas)],"-2021")
prueba$fechas=as.Date(prueba$fechas,format="%d-%m-%Y")
prueba$mes=as.numeric(format(as.Date(prueba$fecha,format="%d-%m-%Y"),format="%m"))

prueba$meses=NULL
for(i in 1:length(prueba$fechas)){
  prueba$meses[i]=mes.abb[prueba$mes[i]]
}
prueba$meses=factor(prueba$meses,levels=mes.abb)

datos=prueba[,2:7]
for(i in 1:length(colnames(datos))){
  datos[,i]=as.numeric(gsub(",","",datos[,i]))
}
datos[is.na(datos)]=0
prueba[,2:7]=datos

mol=prueba[,c(1,2,5,8,9)]
mol$tipo="Prueba Molecular"
colnames(mol)=c("fechas","positivas","total de pruebas","mes","meses","tipo")
ser=prueba[,c(1,3,6,8,9)]
ser$tipo="Prueba Serológica"
colnames(ser)=c("fechas","positivas","total de pruebas","mes","meses","tipo")
ant=prueba[,c(1,4,7,8,9)]
ant$tipo="Prueba Antigénica"
colnames(ant)=c("fechas","positivas","total de pruebas","mes","meses","tipo")

pos=rbind(mol,ser,ant)

pos$p=round(pos$positivas*100/pos$`total de pruebas`,2)

pos$`total de pruebas`[8:10]=c(round(pos$`total de pruebas`[7]+
                                       (pos$`total de pruebas`[11]-pos$`total de pruebas`[7])/4), #Asumiendo correcciones
                               round(pos$`total de pruebas`[7]+2*(pos$`total de pruebas`[11]-
                                                                    pos$`total de pruebas`[7])/4),round(pos$`total de pruebas`[7]+
                                                                                                          3*(pos$`total de pruebas`[11]-pos$`total de pruebas`[7])/4))

pos$tipo=factor(pos$tipo, levels=unique(pos$tipo))

pos$p.dia=c(round((pos$positivas[2:round(length(pos$positivas))]-
                     pos$positivas[1:round(length(pos$positivas)-1)])*100/
                    (pos$`total de pruebas`[2:round(length(pos$`total de pruebas`))]-
                       pos$`total de pruebas`[1:round(length(pos$`total de pruebas`)-1)]),2),NA)

pos$esfuerzo=c((pos$`total de pruebas`[2:round(length(pos$`total de pruebas`))]-
                  pos$`total de pruebas`[1:round(length(pos$`total de pruebas`)-1)]),NA)


posi=ggplot(data=pos, aes(x = pos$fechas, y =pos$p,group=pos$tipo))+
  geom_line(aes(color=pos$tipo))+
  scale_x_date(date_breaks = "30 days",date_labels = "%d-%b%Y")+
  labs(colour="",title="Series de Tiempo de Seropositividad Acumulada",
       x ="Fecha", 
       y = "Positividad(%)")+
  theme(legend.position="top",legend.text = element_text(color = "black", size = 14,face="bold"),axis.text.x=element_text(size=11,colour = "black",face="bold",angle=45, hjust=1),axis.text.y=element_text(size=11,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=14,face="bold"),title = element_text(size=16,colour = "black",face="bold"))

ggsave("serie.tiempo.positividad.png", dpi = 600,   width = 250,
       height = 159,unit="mm",plot =posi)


posi.r=ggplot(data=pos, aes(x = pos$fechas, y =pos$p.dia,group=pos$tipo))+
  geom_line(aes(color=pos$tipo))+
  #geom_point(aes(color=pos$tipo))+
  scale_x_date(date_breaks = "15 days",date_labels = "%d-%b-%Y")+
  labs(colour="",title="Covid-19´s positivity rate per day",
       x ="Dates", 
       y = "Positivity(%)")+
  theme(legend.position="top",legend.text = element_text(color = "black", size = 14,face="bold"),axis.text.x=element_text(size=11,colour = "black",face="bold",angle=45, hjust=1),axis.text.y=element_text(size=11,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=14,face="bold"),title = element_text(size=16,colour = "black",face="bold"))

ggsave("serie.tiempo.positividad.dia.png", dpi = 600,   width = 250,
       height = 159,unit="mm",plot =posi.r)

#traslape de infectados inferidos prueba molecular: rojo (positivos/(negativos + indefinidos)) y negro(positivos/negativos)
x11();plot(razon$positividad[1:(length(razon$fecha)-1)],type="l")
points(pos$p.dia[5:(length(pos$p.dia)/3)],type="l",col="red")

#length(razon$fecha)+inicio.pandemia
sum(pos$`total de pruebas`[3:(length(data.esfuerzo$fecha)+2)]>data.esfuerzo$n)#data open.covid es mas real que la INS

write.csv(pos,"pos.csv",sep=",",dec=".",col.names = TRUE)

```

## Free bed of Intensive care unit 
ABCDE


```markdown
#Camas UCI
UCI=as.data.frame(row_to_names(uci,row_number = 1, remove_row = TRUE, remove_rows_above = TRUE))
UCI[,81]=NULL
UCI[,80]=NULL
row.names(UCI)=NULL
depa.per=c(depa,"Perú")
nombre=colnames(UCI)

UCI=as.data.frame(UCI)

for (i in 1:length(depa.per)){
  colnames(UCI)[(3*i-1):(3*i+1)]=paste0(depa.per[i],"-",nombre[(3*i-1):(3*i+1)])
}

fecha.error=as.Date(paste0(UCI$DATE[1],"."),"%d-%b")
fecha.inicio=as.Date(fecha.error-730,format="%Y-%m-%d")
#fecha.inicio=as.Date(gsub("2021","2020","2022",fecha.inicio),format="%Y-%m-%d")
fecha.final=as.Date(fecha.inicio+length(UCI$DATE)-1,format="%Y-%m-%d")
UCI$DATE=seq(as.Date(fecha.inicio), as.Date(fecha.final), by="days")

begin=UCI$DATE[length(UCI$DATE)]+1
bbb=which(fechasss==begin)
last=fechasss[length(fechasss)]
lll=which(fechasss==last)

UCI.update=as.data.frame(rbind(UCI,UCI[1:(lll-bbb+1),]))
colnames(UCI.update)=colnames(UCI)
UCI.update[(length(UCI$DATE)+1):length(UCI.update[,1]),1]=fechasss[bbb:lll]
datesss=UCI.update$DATE

UCI.u=as.data.frame(matrix(NA,ncol=ncol(UCI),nrow= length(fechasss[bbb:lll])))

for(i in 1: length(fechasss[bbb:lll])){
for(j in 1:length(prov)){
UCI.u[i,3*j]=sum(as.numeric(ccam$dis[which(ccam$provincia==prov[j]&ccam$fecha==fechasss[55+i])]))
UCI.u[i,(3*j-1)]=sum(as.numeric(ccam$ocu[which(ccam$provincia==prov[j]&ccam$fecha==fechasss[55+i])]))
UCI.u[i,(3*j+1)]=sum(c(ccam$ocu[which(ccam$provincia==prov[j]&ccam$fecha==fechasss[55+i])],ccam$dis[which(ccam$provincia==prov[j]&ccam$fecha==fechasss[55+i])]))
}
}

for(i in 1: length(fechasss[bbb:lll])){

  UCI.u[i,77]=sum(as.numeric(UCI.u[i,c(seq(3,76,3)-1)]))
  UCI.u[i,78]=sum(as.numeric(UCI.u[i,c(seq(3,76,3))]))
  UCI.u[i,79]=sum(as.numeric(UCI.u[i,c(seq(3,76,3)+1)]))
}

colnames(UCI.u)=colnames(UCI.update)
UCI.update[(length(UCI$DATE)+1):length(UCI.update[,1]),]=UCI.u
UCI.update$DATE=datesss

UCI=UCI.update
write.csv(UCI,"UCI.csv",sep=",",dec=".",col.names=TRUE)

camas.ocupadas=as.data.frame(cbind(UCI$DATE,UCI$`Perú-Total Sum of UCI OCU`))
colnames(camas.ocupadas)=c("fecha","Números de Camas")
camas.ocupadas$Estado=rep("Ocupadas",length(UCI$DATE))
camas.libres=as.data.frame(cbind(UCI$DATE,UCI$`Perú-Total Sum of UCI DISP`))
colnames(camas.libres)=c("fecha","Números de Camas")
camas.libres$Estado=rep("Disponibles",length(UCI$DATE))

#error de data

camas.libres$`Números de Camas`=as.numeric(camas.libres$`Números de Camas`)
camas.ocupadas$`Números de Camas`=as.numeric(camas.ocupadas$`Números de Camas`)

#OJO
k1=camas.libres$`Números de Camas`[582]/camas.libres$`Números de Camas`[581]
k2=camas.ocupadas$`Números de Camas`[582]/camas.ocupadas$`Números de Camas`[581]
camas.libres$`Números de Camas`[582:length(camas.libres$`Números de Camas`)]=round(camas.libres$`Números de Camas`[582:length(camas.libres$`Números de Camas`)]/k1)
camas.ocupadas$`Números de Camas`[582:length(camas.ocupadas$`Números de Camas`)]=round(camas.ocupadas$`Números de Camas`[582:length(camas.ocupadas$`Números de Camas`)]/k2)
#
camas=as.data.frame(rbind(camas.ocupadas,camas.libres))
camas$fecha=c(as.Date(UCI$DATE,format="%Y-%m-%d"),
              as.Date(UCI$DATE,format="%Y-%m-%d"))

camas$`Números de Camas`=as.numeric(camas$`Números de Camas`)
camas$Estado=factor(camas$Estado,levels=unique(camas$Estado))

write.csv(camas,"camas.csv",sep=",",dec=".",col.names=TRUE)

cama=ggplot(data=camas, aes(x = camas$fecha, y =camas$`Números de Camas`,group=camas$Estado))+
  geom_line(aes(color=camas$Estado))+
  scale_x_date(date_breaks = "30 days",date_labels = "%d-%b%Y")+
  labs(colour="",title="UCIs bed timeseries",
       x ="Dates", 
       y = "number of bed")+
  scale_y_continuous(limits = c(0,1.05*max(camas$`Números de Camas`,na.rm = TRUE)), breaks = round(seq(0,1.05*max(camas$`Números de Camas`,na.rm = TRUE),length.out = 4),2))+
  theme(legend.position="top",legend.text = element_text(color = "black", size = 14,face="bold"),
        axis.text.x=element_text(size=11,colour = "black",face="bold",angle=45, hjust=1),
        axis.text.y=element_text(size=11,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=14,face="bold"),title = element_text(size=16,colour = "black",face="bold"))

ggsave("serie.tiempo.UCI.png", dpi = 600,   width = 250,
       height = 159,unit="mm",plot =cama)

```
## Vaccinatation in progress
ABCDE


```markdown
#Vaccinatation in progress

vac=fread("vacunas_covid.csv",sep=",",dec=".",header = TRUE,fill=TRUE)
vac=as.data.frame(cbind(vac$FECHA_VACUNACION,vac$EDAD,vac$DOSIS,vac$DEPARTAMENTO))
colnames(vac)=c("fecha","Edad","Dosis","Departamento")
vac$fecha=as.character(vac$fecha)
vac$fecha=as.Date(vac$fecha,format="%Y%m%d")

#vvv=as.data.frame(count(vac,c("fecha","Edad","Dosis","Departamento")))
#v.prov=as.data.frame(count(vac,c("fecha","Edad","Dosis","Departamento")))
v.peru=as.data.frame(vac%>%count(fecha,Dosis))
dosis=unique(v.peru$Dosis)
v.peru$Dosis=factor(v.peru$Dosis,levels =dosis)

vacuna=ggplot(data=v.peru, aes(x = v.peru$fecha, y =v.peru$n,group=v.peru$Dosis))+
  geom_line(aes(color=v.peru$Dosis),size=.5)+
  scale_x_date(date_breaks = "30 days",date_labels = "%d-%m-%Y")+
  labs(colour="",title="Vacunation in progress by doses",
       x ="Dates", 
       y = "Number of vaccinated people")+
  scale_y_continuous(limits = c(0,1.01*max(v.peru$n,na.rm = TRUE)), breaks = round(seq(0,1.05*max(v.peru$n,na.rm = TRUE),length.out = 4)))+
  theme(legend.position="top",legend.text = element_text(color = "black", size = 14,face="bold"),
        axis.text.x=element_text(size=11,colour = "black",face="bold",angle=45, hjust=1),
        axis.text.y=element_text(size=11,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=14,face="bold"),title = element_text(size=16,colour = "black",face="bold"))

ggsave("serie.tiempo.vacunados.png", dpi = 600,   width = 250,
       height = 159,unit="mm",plot =vacuna)

inicio.pandemia=as.Date("2020-03-04", format="%Y-%m-%d")#inicio arbitrario el 4 de marzo del 2020
today <- Sys.Date()
hoy=format(today, format="%Y-%m-%d")
Dates=seq(as.Date(inicio.pandemia,format="%Y-%m-%d"), as.Date(hoy, format="%Y-%m-%d"), by="days")
Dates=as.Date(Dates,format="%Y-%m-%d")

vacunados=as.data.frame(matrix(0,ncol=7,nrow=length(Dates)))
colnames(vacunados)=c("Fecha","1ra","2da","3ra","4ta","5ta","6ta")
vacunados$Fecha=Dates

for( i in 1:length(v.peru$fecha)){
if(v.peru$Dosis[i]=="1"){
  vacunados$`1ra`[which(vacunados$Fecha==v.peru$fecha[i])]=v.peru$n[i]
  }
if(v.peru$Dosis[i]=="2"){
  vacunados$`2da`[which(vacunados$Fecha==v.peru$fecha[i])]=v.peru$n[i]
  }
if(v.peru$Dosis[i]=="3"){
    vacunados$`3ra`[which(vacunados$Fecha==v.peru$fecha[i])]=v.peru$n[i]
  }
  if(v.peru$Dosis[i]=="4"){
    vacunados$`4ta`[which(vacunados$Fecha==v.peru$fecha[i])]=v.peru$n[i]
  }
  if(v.peru$Dosis[i]=="5"){
    vacunados$`5ta`[which(vacunados$Fecha==v.peru$fecha[i])]=v.peru$n[i]
  }
  if(v.peru$Dosis[i]=="6"){
    vacunados$`6ta`[which(vacunados$Fecha==v.peru$fecha[i])]=v.peru$n[i]
  }
}

write.csv(vacunados,"vacunados.csv",sep=",",dec=".",col.names=TRUE)

```
# Final ideas



# References
1. Rodriguez-Morales, A. J., Gallego, V., Escalera-Antezana, J. P., Mendez, C. A., Zambrano, L. I., Franco-Paredes, C., … Cimerman, S. (2020). COVID-19 in Latin America: The implications of the first confirmed case in Brazil

2. [La Republica-Coronavirus en Perú: Testimonio del paciente cero tras su recuperación](https://larepublica.pe/sociedad/2020/04/13/coronavirus-en-peru-testimonio-del-paciente-cero-tras-su-recuperacion-mdga/)

3.[OPENCOVIDPERU data set](https://www.tagacat.com/covid/links)

# Support or Contact

You can use the [editor on GitHub](https://github.com/jasb3110/COVIDPERU/edit/gh-pages/index.md) to maintain and preview the content for your website in Markdown files.

Whenever you commit to this repository, GitHub Pages will run [Jekyll](https://jekyllrb.com/) to rebuild the pages in your site, from the content in your Markdown files.

For more details see [Basic writing and formatting syntax](https://docs.github.com/en/github/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax).

Having trouble with Pages? Check out our [documentation](https://docs.github.com/categories/github-pages-basics/) or [contact support](https://support.github.com/contact) and we’ll help you sort it out.
