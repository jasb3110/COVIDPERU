## Introdution
First of all, The SARS-COV-2 is well-know as Covid-19, spreading pandemic illness around world during 2019 to now. COVID-19 had come through a man who came to Italia on 21th febrary, 2020. In Peru, this sickness had come through Luis Felipe Zeballos who arrived to Lima on 26 febrary of 2020. After, He showed that first syntoms of COVID-19 (2).

## to clean dataset
When Covid-19 had started to spread on Peru. The health government institutes were not systemic criteria to order COVID-19 reports. this issue had detrimental effect on reliable diffusion information to Peruvian people. So that,it was important to unificate different source in one way to present COVID-19 variables (positive rate of covid´s patients, numbers of death which have cause for COVID-19, excess of death which could have triggered for COVID-19, numbers of free UCI bed in hospitals and numbers of people were vaccinated).
Therefore, the first challenge was delete or omit different mistakes (Na, null values and outliers). whole of data were downloaded of Peruvian official sources (https://www.datosabiertos.gob.pe/). I would show you how to manage deep cleaning the data.

#### R code
I´m R native programmer so that it too easily to proceed to clean of dataset with this program. It could possible to use anothers programs. I suggest that you will able to use Python. Watch out, many columns have spanish names 

```markdown
############################################################################################################################################################
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
#dead=row_to_names(m,row_number = 2, remove_row = TRUE, remove_rows_above = TRUE)
#
dead=m
rownames(dead)=NULL
dead$NÂº=NULL
#dead$TIEMPO.EDAD=NULL
#dead$`COD# UBIGEO DOMICILIO`=NULL
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
#para dias e1|e2|e3 = 0
e1=sum(is.na(dead$dia))
e2=sum(which(dead$dia>31))
e3=sum(which(dead$dia<1))
e1
e2
e3
#for month
#para dias e4|e5|e6 = 0
e4=sum(is.na(dead$meses))
e5=sum(which(dead$meses>12))
e6=sum(which(dead$meses<1))
e4
e5
e6
#for years
#para dias e7|e8|e9 = 0
e7=sum(is.na(dead$años))
e8=sum(which(dead$años>2022))
e9=sum(which(dead$años<2017))
e7
e8
e9

MM=unique(dead$`MUERTE VIOLENTA`)# not natural death
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

#for moth 
m5=which(tt$EDAD[which(tt$`TIEMPO EDAD`==TT[3])]>12)
tt$`TIEMPO EDAD`[intersect(m5,me)]=TT[1]
tt$`TIEMPO EDAD`[intersect(m5,mi)]=TT[1]
ss4=which(tt$`TIEMPO EDAD`==TT[3])
tt$EDAD[ss4]=tt$EDAD[ss4]/(12) 
#for years
m6=which(tt$EDAD[which(tt$`TIEMPO EDAD`==TT[1])]>123)
tt$EDAD[m6]=NA
#for "ignorados" and "sin registro" ( without specific names)
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

#plot exploratory
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
############################################################################################################################################################
```
It show that exploratory plot of SINADEF´s dataset. first view it seems to be clean and sort that you can looking down eigth pictures. 

![alt text](https://github.com/jasb3110/COVIDPERU/blob/439ae71be1a16eefe1662a2ae45b18a5916d3de8/exploratory.plot.png?raw=true)

Next, it is getting to clean and sort of SINADEF´s data. I try to delete non-natural death signal. My assumption is excess of death during 2020 to 2022, due to COVID-19 spreading. Also, I ploted numbers of men, women and people to evidence some patterns. Bellow I attached lines scripts.

```markdown 
############################################################################################################################################################
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

### women
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

### Men
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

### merged ( men + women)
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
#############################################################################################################################################################
```
It show that numbers of death in timeseries per genre. here you can see men´s death which cause for COVID-19 since 2019 to now. X-axis is numbers of peeple who have pass away per day and Y-axis is date per day. 
![alt text](https://github.com/jasb3110/COVIDPERU/blob/439ae71be1a16eefe1662a2ae45b18a5916d3de8/fallecidos.hombres.png?raw=true)
Women´s  death which cause for COVID-19 since 2019 to now
![alt text](https://github.com/jasb3110/COVIDPERU/blob/439ae71be1a16eefe1662a2ae45b18a5916d3de8/fallecidos.mujeres.png?raw=true)

In first view, number of death men is major than number of death women. therefore, COVID-19 have detrimental effect on Men a lot. 
People´s death which cause for COVID-19 since 2019 to now

![alt text](https://github.com/jasb3110/COVIDPERU/blob/439ae71be1a16eefe1662a2ae45b18a5916d3de8/fallecidos.todos.png?raw=true)

In three wave of COVID-19 on Peru, the most of number death people probably relation with COVID-19 are: May to August in 2020 (first wave), February to April in 2021 (Second wave) and January to February 2022 (Third wave). Second wave was more lethal than others waves around 1250 death.
Next, it is getting to clean and sort of SINADEF´s death data by regions. Bellow I attached lines scripts.

```markdown 
###########################################################################################################################################################
#Sinadef´s death data
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
#provincias2=as.data.frame(m.plot2)
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
############################################################################################################################################################     
```
It show that numbers of SINADEF´s death data in timeseries each regions. here you can see total death which cause for natural cause since 2019 to now. X-axis is numbers of peeple who have pass away per day and Y-axis is date per day.

In my view, whole regions shows that evidence three waves. but each region shows its particular patterns of trendency.  Lima region was recorded the most number of death. this aspect could be explained the Lima region is the densely populated of the country and where dayly reports were more reliable and accurate than others regions.

![alt text](https://github.com/jasb3110/COVIDPERU/blob/4645aecfc038e25f651fe9c67524e23a28572766/fallecidos.provincias2.png?raw=true)

Next, it is getting to clean and sort of SINADEF´s death data by regions. Bellow I attached lines scripts.

```markdown
#To clean and sort of COVID test
covid=fread("positivos_covid.csv",sep=";",dec=".",header=TRUE,fill=TRUE)#Personas con diagnostico COVID
#cprueba=fread("TB_F100_SICOVID.csv",sep=",",dec=".",header=TRUE,fill=TRUE)#Personas con diagnostico COVID

covid$id_persona=NULL
covid$FECHA_RESULTADO=as.character(covid$FECHA_RESULTADO)
covid$FECHA_RESULTADO=as.Date(covid$FECHA_RESULTADO,format ="%Y%m%d")

covid19=as.data.frame(covid%>%count(FECHA_CORTE,DEPARTAMENTO,PROVINCIA,DISTRITO,METODODX,EDAD,SEXO,FECHA_RESULTADO,UBIGEO))

#dealy days each method
desfase_PCR=5#dias que hay que que restar PCR
desfase_PR=8#dias que hay que restar PR
covid19$fecha=rep(NA,length(covid19$FECHA_RESULTADO))
covid19$fecha[which(covid19$METODODX=="PCR")]=as.Date(covid19$FECHA_RESULTADO[which(covid19$METODODX=="PCR")]-desfase_PCR)
covid19$fecha[which(covid19$METODODX=="PR")]=as.Date(covid19$FECHA_RESULTADO[which(covid19$METODODX=="PR")]-desfase_PCR)
covid19$fecha[which(covid19$METODODX=="AG")]=as.Date(covid19$FECHA_RESULTADO[which(covid19$METODODX=="AG")]-desfase_PCR)

sinsexocovid=covid19[,c(2:6,7,8,10,11)]
covidmuj=covid19[covid19$SEXO=="FEMENINO",]
covidmuj$SEXO=NULL
covidhom=covid19[covid19$SEXO=="MASCULINO",]
covidhom$SEXO=NULL

#whole Peru
#fechas del eje del X 
unique(covid19$FECHA_RESULTADO[which(covid19$fecha==min(covid19$fecha,na.rm = TRUE))])
unique(covid19$FECHA_RESULTADO[which(covid19$fecha==max(covid19$fecha,na.rm = TRUE))])

covidhom$FECHA_RESULTADO=as.Date(covidhom$FECHA_RESULTADO,format="%Y-%m-%d")
covidmuj$FECHA_RESULTADO=as.Date(covidmuj$FECHA_RESULTADO,format="%Y-%m-%d")
sinsexocovid$FECHA_RESULTADO=as.Date(sinsexocovid$FECHA_RESULTADO,format="%Y-%m-%d")

#Men
chom=as.data.frame(covidhom%>%count(METODODX,EDAD,fecha,FECHA_RESULTADO))
ch=as.data.frame(chom%>%count(fecha,METODODX,FECHA_RESULTADO))
ch$METODODX[which(ch$METODODX=="CONFIRMADOS COVID-19 28.07.2021.xlsx/CONFIRMADO_ANTIGENO")]="ANT"
ch$METODODX[which(ch$METODODX=="CONFIRMADOS COVID-19 28.07.2021.xlsx/CONFIRMADOS PR")]="PR"
ch$METODODX=factor(ch$METODODX,levels=unique(ch$METODODX))

covid.hom=ggplot(data=ch, aes(x=ch$FECHA_RESULTADO, y=ch$n, group=ch$METODODX))+
  scale_x_date(date_breaks = "30 days",date_labels = "%d-%b")+
  geom_line(aes(color=ch$METODODX))+
  theme(legend.position="top")+
  labs(colour="",title="Covid-19 positive´s men time serie",
       x ="Date", 
       y = "Number of Covid-19 positives")+
  theme(axis.text.x=element_text(size=11,colour = "black",face="bold",angle=45, hjust=1),axis.text.y=element_text(size=11,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=14,face="bold"),title = element_text(size=16,colour = "black",face="bold"))

ggsave("covid.hombres.png", dpi = 600,   width = 250,
       height = 159,unit="mm",plot = covid.hom)

#Women
cmuj=as.data.frame(covidmuj%>%count(METODODX,EDAD,fecha,FECHA_RESULTADO))
cm=as.data.frame(cmuj%>%count(fecha,METODODX,FECHA_RESULTADO))
cm$METODODX[which(cm$METODODX=="CONFIRMADOS COVID-19 28.07.2021.xlsx/CONFIRMADO_ANTIGENO")]="ANT"
cm$METODODX[which(cm$METODODX=="CONFIRMADOS COVID-19 28.07.2021.xlsx/CONFIRMADOS PR")]="PR"
cm$METODODX=factor(cm$METODODX,levels=unique(cm$METODODX))

covid.muj=ggplot(data=cm, aes(x=cm$FECHA_RESULTADO, y=cm$n, group=cm$METODODX))+
  scale_x_date(date_breaks = "30 days",date_labels = "%d-%b")+
  geom_line(aes(color=cm$METODODX))+
  theme(legend.position="top")+
  labs(colour="",title="Covid-19 positive´s women time serie",
       x ="Dates", 
       y = "Number of Covid-19 positives")+  
  theme(axis.text.x=element_text(size=11,colour = "black",face="bold",angle=45, hjust=1),axis.text.y=element_text(size=11,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=14,face="bold"),title = element_text(size=16,colour = "black",face="bold"))

ggsave("covid.mujeres.png", dpi = 600,   width = 250,
       height = 159,unit="mm",plot = covid.muj)

# all people
ctod=as.data.frame(sinsexocovid%>%count(METODODX,EDAD,FECHA_RESULTADO))
tod=as.data.frame(ctod%>%count(METODODX,FECHA_RESULTADO))

tod$METODODX[which(tod$METODODX=="CONFIRMADOS COVID-19 28.07.2021.xlsx/CONFIRMADO_ANTIGENO")]="ANT"
tod$METODODX[which(tod$METODODX=="CONFIRMADOS COVID-19 28.07.2021.xlsx/CONFIRMADOS PR")]="PR"
tod$METODODX=factor(tod$METODODX,levels=unique(tod$METODODX))

covid.todo=ggplot(data=tod, aes(x=tod$FECHA_RESULTADO, y=tod$n, group=tod$METODODX))+
  scale_x_date(date_breaks = "30 days",date_labels = "%d-%b")+
  geom_line(aes(color=tod$METODODX))+
  labs(colour="",title="Covid-19 positive´s people time serie",
       x ="Dates", 
       y = "Number of Covid-19 positives")+
  theme(legend.position="top",legend.text = element_text(color = "black", size = 14,face="bold"), axis.text.x=element_text(size=11,colour = "black",face="bold",angle=45, hjust=1),axis.text.y=element_text(size=11,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=14,face="bold"),title = element_text(size=16,colour = "black",face="bold"))

ggsave("covid.todo.png", dpi = 600,   width = 250,
       height = 159,unit="mm",plot = covid.todo)

```
I show that...


![alt text](https://github.com/jasb3110/COVIDPERU/blob/ff93b0325576aa837548696788debc9a169613db/covid.hombres.png?raw=true)


![alt text](https://github.com/jasb3110/COVIDPERU/blob/ff93b0325576aa837548696788debc9a169613db/covid.mujeres.png?raw=true)


![alt text](https://github.com/jasb3110/COVIDPERU/blob/ff93b0325576aa837548696788debc9a169613db/covid.todo.png?raw=true)





```markdown

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)








```






## References
1. Rodriguez-Morales, A. J., Gallego, V., Escalera-Antezana, J. P., Mendez, C. A., Zambrano, L. I., Franco-Paredes, C., … Cimerman, S. (2020). COVID-19 in Latin America: The implications of the first confirmed case in Brazil

2. https://larepublica.pe/sociedad/2020/04/13/coronavirus-en-peru-testimonio-del-paciente-cero-tras-su-recuperacion-mdga/

### Support or Contact

You can use the [editor on GitHub](https://github.com/jasb3110/COVIDPERU/edit/gh-pages/index.md) to maintain and preview the content for your website in Markdown files.

Whenever you commit to this repository, GitHub Pages will run [Jekyll](https://jekyllrb.com/) to rebuild the pages in your site, from the content in your Markdown files.

For more details see [Basic writing and formatting syntax](https://docs.github.com/en/github/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax).

Having trouble with Pages? Check out our [documentation](https://docs.github.com/categories/github-pages-basics/) or [contact support](https://support.github.com/contact) and we’ll help you sort it out.
