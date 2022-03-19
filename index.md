## Introdution
First of all, The SARS-COV-2 is well-know as Covid-19, spreading pandemic illness around world during 2019 to now. COVID-19 had come through a man who came to Italia on 21th febrary, 2020. In Peru, this sickness had come through Luis Felipe Zeballos who arrived to Lima on 26 febrary of 2020. After, He showed that first syntoms of COVID-19 (2).

## to clean dataset
When Covid-19 had started to spread on Peru. The health government institutes were not systemic criteria to order COVID-19 reports. this issue had detrimental effect on reliable diffusion information to Peruvian people. So that,it was important to unificate different source in one way to present COVID-19 variables (positive rate of covid´s patients, numbers of death which have cause for COVID-19, excess of death which could have triggered for COVID-19, numbers of free UCI bed in hospitals and numbers of people were vaccinated).
Therefore, the first challenge was delete or omit different mistakes (Na, null values and outliers). whole of data were downloaded of Peruvian official sources (https://www.datosabiertos.gob.pe/). I would show you how to manage deep cleaning the data.

#### R code
I´m R native programmer so that it too easily to proceed to clean of dataset with this program. It could possible to use anothers programs. I suggest that you will able to use Python. 

```markdown
#to start
`setwd("~/covid19/")#directorio donde se guarda los archivos .csv
require("janitor")#paquetes que se deben instalar
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
library("pracma")´

#Leyendo la data
m=fread("fallecidos_sinadef.csv",sep="|",dec=".",header = TRUE,fill=TRUE)#fallecidos segun SINADEF
mcovid=fread("fallecidos_covid.csv",sep=";",dec=".",header = TRUE,fill=TRUE)#Personas reportadas como muertos por COVID

#limpiando y ordenando la data de muertos sinadef
m=as.data.frame(m)
#dead=row_to_names(m,row_number = 2, remove_row = TRUE, remove_rows_above = TRUE)
dead=m
rownames(dead)=NULL
dead$NÂº=NULL
#dead$TIEMPO.EDAD=NULL
#dead$`COD# UBIGEO DOMICILIO`=NULL
dead$`TIPO SEGURO`=NULL

dead$`TIPO LUGAR`=NULL
dead$INSTITUCION=NULL
dead$NECROPSIA=NULL
dead=as.data.frame(dead[,1:14])
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
#errores en la data de fechas
#para dias e1|e2|e3 = 0
e1=sum(is.na(dead$dia))
e2=sum(which(dead$dia>31))
e3=sum(which(dead$dia<1))
e1
e2
e3
#para meses
#para dias e4|e5|e6 = 0
e4=sum(is.na(dead$meses))
e5=sum(which(dead$meses>12))
e6=sum(which(dead$meses<1))
e4
e5
e6
#para años
#para dias e7|e8|e9 = 0
e7=sum(is.na(dead$años))
e8=sum(which(dead$años>2022))
e9=sum(which(dead$años<2017))
e7
e8
e9

MM=unique(dead$`MUERTE VIOLENTA`)
tt=dead[which(dead$`MUERTE VIOLENTA`==MM[1]|dead$`MUERTE VIOLENTA`==MM[2]|dead$`MUERTE VIOLENTA`==MM[3]|dead$`MUERTE VIOLENTA`==MM[9]),]

#Limpiando errores
#se asume que hay un solo error por columna de datos

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

#Segundos
m1=which(tt$EDAD[which(tt$`TIEMPO EDAD`==TT[7])]>60)
tt$TIEMPO.EDAD[intersect(m1,me)]=TT[1]
tt$TIEMPO.EDAD[intersect(m1,mi)]=TT[1]
ss1=which(tt$`TIEMPO EDAD`==TT[7])
tt$EDAD[ss1]=tt$EDAD[ss1]/(60*60*24*365)
#minutos
m2=which(tt$EDAD[which(tt$`TIEMPO EDAD`==TT[2])]>60)
tt$`TIEMPO EDAD`[intersect(m2,me)]=TT[1]
tt$`TIEMPO EDAD`[intersect(m2,mi)]=TT[1]
ss2=which(tt$`TIEMPO EDAD`==TT[2])
tt$EDAD[ss2]=tt$EDAD[ss2]/(60*24*365)
#HORAS
m3=which(tt$EDAD[which(tt$`TIEMPO EDAD`==TT[5])]>24)
tt$`TIEMPO EDAD`[intersect(m3,me)]=TT[1]
tt$`TIEMPO EDAD`[intersect(m3,mi)]=TT[1]
ss3=which(tt$`TIEMPO EDAD`==TT[5])
tt$EDAD[ss3]=tt$EDAD[ss3]/(24*365)  
#dias
m4=which(tt$EDAD[which(tt$`TIEMPO EDAD`==TT[4])]>31)
tt$`TIEMPO EDAD`[intersect(m4,me)]=TT[1]
tt$`TIEMPO EDAD`[intersect(m4,mi)]=TT[1]
ss4=which(tt$`TIEMPO EDAD`==TT[4])
tt$EDAD[ss4]=tt$EDAD[ss4]/(365) 
#MESES
m5=which(tt$EDAD[which(tt$`TIEMPO EDAD`==TT[3])]>12)
tt$`TIEMPO EDAD`[intersect(m5,me)]=TT[1]
tt$`TIEMPO EDAD`[intersect(m5,mi)]=TT[1]
ss4=which(tt$`TIEMPO EDAD`==TT[3])
tt$EDAD[ss4]=tt$EDAD[ss4]/(12) 
#A?OS
m6=which(tt$EDAD[which(tt$`TIEMPO EDAD`==TT[1])]>123)
tt$EDAD[m6]=NA

#ignorados y sin registro
m7=c(which(tt$`TIEMPO EDAD`==TT[6]|tt$`TIEMPO EDAD`==TT[8]|tt$`TIEMPO EDAD`==TT[9]))
tt$`TIEMPO EDAD`[which(tt$EDAD[m7]>60)]=TT[1]
tt$EDAD[which(tt$EDAD[m7]<60)]=NA

tt$EDAD=as.numeric(tt$EDAD)
tt$EDAD[which(tt$EDAD>=123)]=NA

#ordenando por sexos y edades
sinsexo=tt
sinsexo$SEXO=NULL

sinsexo$EDAD=trunc(sinsexo$EDAD*100)/100

todo=as.data.frame(count(sinsexo,c("date","dia","meses","años","EDAD")))


y1=tt[which(tt$SEXO=="FEMENINO"),]
y1$SEXO=NULL
y1$EDAD=trunc(y1$EDAD*100)/100

muj=as.data.frame(count(y1,c("date","dia","meses","años","EDAD")))

y2=tt[which(tt$SEXO=="MASCULINO"),]
y2$SEXO=NULL
y2$EDAD=trunc(y2$EDAD*100)/100
hom=as.data.frame(count(y2,c("date","dia","meses","años","EDAD")))

mujeres=as.data.frame(count(y1,c("date","dia","meses","años")))
mujeres$fechas=paste(mujeres$dia,"-",mujeres$meses,"-",mujeres$años)
hombres=as.data.frame(count(y2,c("date","dia","meses","años")))
hombres$fechas=paste(hombres$dia,"-",hombres$meses,"-",hombres$años)
todos=as.data.frame(count(sinsexo,c("date","dia","meses","años")))
todos$fechas=as.Date(paste0(todos$dia,"-",todos$meses,"-",todos$años),format="%d-%m-%Y")`




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
