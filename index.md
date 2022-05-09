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
It show that exploratory plot of dataset. first view it seems to be clean and sort that you can looking down eigth pictures. Sorry, there are many spanish labels in plot.

![alt text](https://github.com/jasb3110/COVIDPERU/blob/COVID-19-dataset/exploratory%20plot.png?raw=true)

Next, it is getting to clean and sort of COVID´s death data. Bellow I attached lines scripts.

```markdown 
############################################################################################################################################################
# COVID-19´s death during to spread pandemic

"04 - 3 - 2020" #begining date
todos=todos[which(todos$años>=2019),]
ini=which(todos$dia==04&todos$meses==3&todos$años==2020)
fin=length(todos$fechas)

fin-ini# COVID-19´s death average 
media_covid=mean(todos$freq[ini:fin])
std_covid=sd(todos$freq[ini:fin])
100*std_covid/media_covid #Coefficient of Variation in percent

#total death before COVID-19 pandemic
iii=which(todos$dia==02&todos$meses==2&todos$años==2020)#02 - 2 - 2020 begin
fff=which(todos$dia==03&todos$meses==3&todos$años==2020)#03 - 3 - 2020 end 

fff-iii# amount of death before 30 days to come COVID-19 in Peru (Pre-covid period time)
media=mean(todos$freq[iii:fff])
std=sd(todos$freq[iii:fff])
100*std/media #Coefficient of Variation in percent
write.csv(cbind(media,std),"exceso.csv",sep=",",dec=".",col.names=TRUE)#save in .csv

#total of death during COVID is subtracting total death during pre-COVID
m_encovid_medio=(todos$freq[ini:fin]-media)/std
m_encovid_medio=m_encovid_medio+min(m_encovid_medio)*-1
m_encovid_min=(todos$freq[ini:fin]-media-std*1.96)/std
m_encovid_min=m_encovid_min++min(m_encovid_min)*-1
m_encovid_max=(todos$freq[ini:fin]-media+std*1.96)/std
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
fall.muj=ggplot(data = mujeres, aes(x=mujeres$mes.año, y=mujeres$freq)) + geom_boxplot()+labs(title="Diagrama de cajas mensuales de Mujeres fallecidas desde 2019 al 2022",
                                                                                  x ="Meses desde 2019 al 2022", 
                                                                                  y = "Número de mujeres muertas")+
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
fall.hom=ggplot(data = hombres, aes(x=hombres$mes.año, y=hombres$freq)) + geom_boxplot()+labs(title="Diagrama de cajas mensuales de Varones fallecidos desde 2019 al 2022",
                                                                                  x ="Meses desde 2019 al 2022", 
                                                                                  y = "Número de Varones muertos")+
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

fall.todos=ggplot(data = todos, aes(x=todos$mes.año, y=todos$freq)) + geom_boxplot()+labs(title="Diagrama de cajas mensuales de los fallecidos desde 2019 al 2022",
                                                                                       x ="Meses desde 2019 al 2022", 
                                                                                       y = "Número de Fallecidos")+
  theme(axis.text.x=element_text(size=11,colour = "black",face="bold",angle=45, hjust=1),axis.text.y=element_text(size=11,colour = "black",face="bold",hjust=1),
        axis.title=element_text(size=14,face="bold"),title = element_text(size=16,colour = "black",face="bold"))

ggsave("fallecidos.todos.png", dpi = 600,   width = 250,
       height = 159,unit="mm",plot = fall.todos)
#############################################################################################################################################################
```
It show that numbers of death in timeseries per genre. here you can see men´s death which cause for COVID-19 since 2019 to now. X-axis is numbers of peeple who have pass away per day and Y-axis is date per day. 
![alt text](https://github.com/jasb3110/COVIDPERU/blob/COVID-19-dataset/covid.hombres.png?raw=true)
Women´s  death which cause for COVID-19 since 2019 to now
![alt text](https://github.com/jasb3110/COVIDPERU/blob/COVID-19-dataset/covid.mujeres.png?raw=true)
People´s death which cause for COVID-19 since 2019 to now
![alt text](https://github.com/jasb3110/COVIDPERU/blob/COVID-19-dataset/covid.todo.png?raw=true)

Next, it is getting to clean and sort of COVID´s death data. Bellow I attached lines scripts.



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
