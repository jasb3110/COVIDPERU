import os
import pip
os.getcwd()
os.chdir('D:\\Pepe\\2020\\covid19')
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import tensorflow as tf
import array
from tensorflow.keras.layers import Embedding, Input, Flatten
from tensorflow.keras.models import Model
from tensorflow.keras import Input  
from tensorflow.keras.layers import *
from tensorflow.keras.utils import plot_model
from tensorflow import keras
import random
import graphviz
import datetime as dt
import sklearn.preprocessing as sk
import math as mt
import statistics as st
import winsound
import seaborn as sns
import scipy.stats
from scipy.stats import poisson
from scipy.stats import norm
from statsmodels.tsa.ar_model import AR
import graphviz
from tensorflow.keras.models import save_model
from numpy import loadtxt
from tensorflow.keras.models import load_model
import pydot
import visualkeras
from PIL import ImageFont

duration = 100  # milliseconds
freq = 480  # Hz

ff = pd.read_csv('d.csv', encoding='latin-1',sep=",")

fff=ff.replace(np.nan, 0)

ini2=1162#all datas
day_lag=15

ini1=954-day_lag#all datas with desfase

fff=fff[0:ini1]

#View(fff)

for i in range((len(fff.columns)-1)):
  fff[fff.columns[i]]=np.array(fff.iloc[:,i],dtype=float)
return()

#View(fff)
#View(ff)
################################################################################
#ML
output1=pd.concat([fff['DIRESA & DIRIS death'],fff['SINADEF excess of death']],axis=1)

plt.figure()
sns.distplot(output1["DIRESA & DIRIS death"])
plt.show()

plt.figure()
sns.distplot(output1["SINADEF excess of death"])
plt.show()

plt.figure()
plt.ylabel("SINADEF excess of death")
plt.xlabel("dates") 
plt.plot(fff["dates"],fff["SINADEF excess of death"])
plt.show()


fff["vaccination"]=0
vaccination=np.array([(fff["Vaccinated-1st"]-fff["Vaccinated-2nd"])*.6+
                   (fff["Vaccinated-2nd"]-fff["Vaccinated-3rd"])*.7+
                   (fff["Vaccinated-3rd"]-fff["Vaccinated-4th"])*.8+
                   (fff["Vaccinated-4th"]-fff["Vaccinated-5th"])*.9+
                   (fff["Vaccinated-5th"]-fff["Vaccinated-6th"])*.95+
                   (fff["Vaccinated-6th"]+fff["Vaccinated-7th"]+fff["Vaccinated-8th"]+fff["Vaccinated-9th"]+fff["Vaccinated-10th"]+fff["Vaccinated-11th"])*.99],dtype=float)  

fff["vaccination"]=vaccination.flatten()

plt.figure()
plt.ylabel("COVID vaccination")
plt.xlabel("dates") 
plt.plot(fff["dates"],fff["vaccination"])
plt.show()

################################################################################
np.where(fff.columns=="DIRESA & DIRIS death")#8
np.where(fff.columns=="SINADEF excess of death")#6

#lag=pd.concat([fff['DIRESA & DIRIS death'],fff['SINADEF excess of death'],fff['Molecular positivity'],fff['free ICUÂ´s bed %'],fff[fff.columns[11:22]]], axis=1)
lag=pd.concat([fff['DIRESA & DIRIS death'],fff['SINADEF excess of death'],fff['Molecular positivity'],fff['free ICUÂ´s bed %'],fff["vaccination"]], axis=1)

ini3=ini1-day_lag#all datas with desfase
lag1=lag[0:ini3]
#ceros=pd.DataFrame(np.zeros((day_lag,15))).
ceros=pd.DataFrame(np.zeros((day_lag,5)))
ceros.columns=lag1.columns
lag2 = pd.concat([ceros,lag1]).reset_index(drop=True)

input1= pd.concat([fff['Unnamed: 0'],fff['Molecular positivity']], axis=1)#molecular positivity
input2 = pd.concat([fff['Unnamed: 0'],fff['free ICUÂ´s bed %']], axis=1)#%free bed
#input3 = pd.concat([fff['Unnamed: 0'],fff[fff.columns[11:22]]], axis=1)#vacination
input3 = pd.concat([fff['Unnamed: 0'],fff["vaccination"]], axis=1)#vacination
input4 = pd.concat([fff['Unnamed: 0']-day_lag,lag2],axis=1)
input4["Unnamed: 0"][0:(day_lag-1)]=0

View(input4)
################################################################################
#inputs
i1= keras.layers.Input(shape=(2,))
i2= keras.layers.Input(shape=(2,))
#i3= keras.layers.Input(shape=(12,))
i3= keras.layers.Input(shape=(2,))
#i4= keras.layers.Input(shape=(16,))
i4= keras.layers.Input(shape=(6,))

dp=.3#dropout
n0=100#number of neronal cells on flower level
n1=90#number of neronal cells on first level
n2=80#number of neronal cells on second level
n3=70#number of neronal cells on third level
n4=60#number of neronal cells on second level
n5=50#number of neronal cells on third level
n6=40#number of neronal cells on second level
n7=30#number of neronal cells on third level
n8=20#number of neronal cells on second level

#Numerical inputs
#branch
#molecular rate%
num1 = keras.layers.Dense(n0,input_dim=2, activation=keras.activations.relu,use_bias=True)(i1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n1,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n2,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n3,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n4,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n5,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n6,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1= keras.models.Model(inputs=i1,outputs=num1)

#free bed %
num2 = keras.layers.Dense(n0,input_dim=2, activation=keras.activations.relu,use_bias=True)(i2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n1,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n2,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n3,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n4,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n5,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n6,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2= keras.models.Model(inputs=i2,outputs=num2)

#vaccination effect
num3 = keras.layers.Dense(n0,input_dim=2, activation=keras.activations.relu,use_bias=True)(i3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n1,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n2,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n3,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n4,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n5,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n6,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3= keras.models.Model(inputs=i3,outputs=num3)

#lag of days
num4 = keras.layers.Dense(n0,input_dim=6, activation=keras.activations.relu,use_bias=True)(i4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n1,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n2,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n3,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n4,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n5,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n6,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.models.Model(inputs=i4,outputs=num4)

#conbined all branchs
mergedpre = concatenate([num1.output,num2.output,num3.output,num4.output])

out1 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre)
keras.layers.Dropout(rate=dp)
out1= keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(out1)
keras.layers.Dropout(rate=dp)

out2 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre)
keras.layers.Dropout(rate=dp)
out2 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(out2)
keras.layers.Dropout(rate=dp)

out3 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre)
keras.layers.Dropout(rate=dp)
out3 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(out3)
keras.layers.Dropout(rate=dp)

out4 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre)
keras.layers.Dropout(rate=dp)
out4 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(out4)
keras.layers.Dropout(rate=dp)

out5 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre)
keras.layers.Dropout(rate=dp)
out5 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(out5)
keras.layers.Dropout(rate=dp)

out6 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre)
keras.layers.Dropout(rate=dp)
out6 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(out6)
keras.layers.Dropout(rate=dp)

mergedpre2 = concatenate([num1.output,num2.output,num3.output,num4.output,out1,out2,out3,out4,out5,out6])

out7 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre2)
keras.layers.Dropout(rate=dp)
out7 = keras.layers.Dense(n8,activation=keras.activations.linear,use_bias=True)(out7)
keras.layers.Dropout(rate=dp)
 
out8 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre2)
keras.layers.Dropout(rate=dp)
out8 = keras.layers.Dense(n8,activation=keras.activations.linear,use_bias=True)(out8)
keras.layers.Dropout(rate=dp)

out9 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre2)
keras.layers.Dropout(rate=dp)
out9 = keras.layers.Dense(n8,activation=keras.activations.linear,use_bias=True)(out9)
keras.layers.Dropout(rate=dp)

out = concatenate([num1.output,num2.output,num3.output,num4.output,out7,out8,out9])

out= keras.layers.Dense(n5,activation=keras.activations.linear,use_bias=True)(out)
keras.layers.Dropout(rate=dp)
out= keras.layers.Dense(n6,activation=keras.activations.linear,use_bias=True)(out)
keras.layers.Dropout(rate=dp)
out = keras.layers.Dense(n7,activation=keras.activations.linear,use_bias=True)(out)
keras.layers.Dropout(rate=dp)
out = keras.layers.Dense(n8,activation=keras.activations.linear,use_bias=True)(out)
keras.layers.Dropout(rate=dp)
out = keras.layers.Dense(2)(out)

model = keras.models.Model(inputs=[i1,i2,i3,i4],outputs=out)

model.compile(loss='mse', 
              optimizer=tf.keras.optimizers.Adamax(0.005), 
              metrics=['mse','mae','acc'])
################################################################################
#To train
ep=25600# number of epochs
es =tf.keras.callbacks.EarlyStopping(monitor='val_loss',
                                     min_delta=0.001,
                                     patience=.4*ep,
                                     mode="min", 
                                     verbose=0)

print("to start")
obj_now = dt.datetime.now()
print("Current date & time: ", str(obj_now))
historical=model.fit([input1,input2,input3,input4],output1,epochs=ep, 
                     batch_size=64 ,
                     validation_split=.05,
                     shuffle=False,
                     callbacks=[es],
                     verbose=False)
print("to end")
obj_now = dt.datetime.now()
print("Current date & time: ", str(obj_now))
winsound.Beep(freq, duration)
winsound.Beep(freq, duration)
winsound.Beep(freq, duration)
winsound.Beep(freq, duration)
winsound.Beep(freq, duration)
winsound.Beep(freq, duration)
################################################################################
#sesgo
plt.figure()
plt.xlabel("iterations")
plt.ylabel("error for an instance of a neural network") 
plt.plot(historical.history["mse"])
plt.plot(historical.history["val_mse"])
plt.title('model mse')
plt.legend(['trained', 'validation'], loc='upper left')
plt.minorticks_on()
plt.show()
plt.savefig("two.variables.model-mse.png",facecolor='white', bbox_inches="tight",
            pad_inches=0.1, transparent=True,dpi=900)

#Validation
plt.figure()
plt.xlabel("iterations")
plt.ylabel("validation accuracy of a neural network")  
plt.plot(historical.history['acc'])
plt.plot(historical.history['val_acc'])
plt.title('model accuracy')
plt.legend(['train', 'validation'], loc='upper left')
plt.minorticks_on()
plt.show()
plt.savefig("two.variables.model-accuracy.png",facecolor='white', bbox_inches="tight",
            pad_inches=0.1, transparent=True,dpi=900)
################################################################################
#prediction with validation data 
resultado=model.predict([input1,input2,input3,input4])
resu=np.array(resultado,dtype=float)

#sesgo
k=2#number of signifance 
plt.figure()
plt.xlabel("Real")
plt.ylabel("Virtual")
plt.minorticks_on()
plt.plot(output1["DIRESA & DIRIS death"]/max(output1["DIRESA & DIRIS death"]),resu.round(k)[:, [0]]/max(resu.round(k)[:, [0]]), 'o', color='black')
plt.plot(output1["SINADEF excess of death"]/max(output1["SINADEF excess of death"]),resu.round(k)[:, [1]]/max(resu.round(k)[:, [1]]), 'o', color='green')

m, b = np.polyfit(output1["DIRESA & DIRIS death"]/max(output1["DIRESA & DIRIS death"]),resu.round(k)[:, [0]]/max(resu.round(k)[:, [0]]),1)
m1, b1 = np.polyfit(output1["SINADEF excess of death"]/max(output1["SINADEF excess of death"]),resu.round(k)[:, [1]]/max(resu.round(k)[:, [1]]),1)

plt.plot(output1["DIRESA & DIRIS death"]/max(output1["DIRESA & DIRIS death"]),m*output1["DIRESA & DIRIS death"]/max(output1["DIRESA & DIRIS death"])+ b)
plt.plot(output1["SINADEF excess of death"]/max(output1["SINADEF excess of death"]),m1*output1["SINADEF excess of death"]/max(output1["SINADEF excess of death"])+ b1)

plt.title('Real vs Virtual')
plt.legend(["DIRESA & DIRIS death", "SINADEF excess of death"], loc='upper left')
plt.show()
plt.savefig("two.variables.Normalized.Real.vs.Virtual.png",facecolor='white', bbox_inches="tight",
            pad_inches=0.1, transparent=True,dpi=900)

#Relation ship between real vs virtual without transform
plt.figure()
df = pd.DataFrame({'real': output1["DIRESA & DIRIS death"],
                   'virtual':output1["DIRESA & DIRIS death"]})
df["virtual"]=resu.round(k)[:, [0]]
#create regplot
p = sns.regplot(x=df.real, y=df.virtual,data=df)
plt.title('Real vs Virtual '+"DIRESA & DIRIS death")
#calculate slope and intercept of regression equation
slope, intercept, r, p, sterr = scipy.stats.linregress(df)

#add regression equation to plot
plt.text(500, 2500, 'y = ' + str(round(intercept,5)) + ' + ' + str(round(slope,5)) + 'x'+", p-value="+str(round(p,5))+", r-squared="+str(round(r,5)))
plt.minorticks_on()
plt.show()
plt.savefig("DIRESA.&.DIRIS.death.Real.vs.Virtual.png",facecolor='white', bbox_inches="tight",
            pad_inches=0.1, transparent=True,dpi=900)

#Relation ship between real vs virtual without transform
plt.figure()
df2 = pd.DataFrame({'real': output1["SINADEF excess of death"],
                   'virtual':output1["SINADEF excess of death"]})
df2["virtual"]=resu.round(k)[:, [1]]
#create regplot
p2 = sns.regplot(x=df2.real, y=df2.virtual,data=df2)
plt.title('Real vs Virtual '+"SINADEF excess of death")
#calculate slope and intercept of regression equation
slope2, intercept2, r2, p2, sterr2 = scipy.stats.linregress(df2)
#add regression equation to plot
plt.text(15,50, 'y = ' + str(round(intercept2,5)) + ' + ' + str(round(slope2,5)) + 'x'+", p-value="+str(round(p2,5))+", r-squared="+str(round(r2,5)))
plt.minorticks_on()
plt.show()
plt.savefig("SINADEF.excess.of.death.Real.vs.Virtual.png",facecolor='white', bbox_inches="tight",
            pad_inches=0.1, transparent=True,dpi=900)

#times series DIRESA & DIRIS death
plt.figure()
plt.xlabel("days")
plt.ylabel("DIRESA & DIRIS death") 
plt.plot(fff['Unnamed: 0'],output1["DIRESA & DIRIS death"],'o-',markersize=1, color="black")
plt.plot(fff['Unnamed: 0'],resu.round(k)[:, [0]],'-', markersize=.1, color='red')
plt.title('Time serie')
plt.legend(['Real', 'Virtual'], loc='upper left')
plt.minorticks_on()
plt.show()
plt.savefig("DIRESA.&.DIRIS.death.time.series.png",facecolor='white', bbox_inches="tight",
            pad_inches=0.1, transparent=True,dpi=900)

#times series SINADEF excess of death
plt.figure()
plt.xlabel("days")
plt.ylabel("SINADEF excess of death time series") 
plt.plot(fff['Unnamed: 0'],output1["SINADEF excess of death"],'o-',markersize=1, color='black')
plt.plot(fff['Unnamed: 0'],resu.round(k)[:, [1]],'-',markersize=0.1, color='red')
plt.title('Time serie')
plt.legend(['Real', 'Virtual'], loc='upper left')
plt.minorticks_on()
plt.show()
plt.savefig("SINADEF.excess.of.eath.time.series.png",facecolor='white', bbox_inches="tight",
            pad_inches=0.1, transparent=True,dpi=900)
################################################################################
plot_model(model, to_file='model.png')
visualkeras.layered_view(model, to_file='2var.output.png').show() # write and show

#save model
save_model(model, "2var.model.h5")
# load model
model = load_model('2var.model.h5')
# summarize model.
model.summary()

# split into input (X) and output (Y) variables
X = [input1,input2,input3,input4]
Y = output1
# evaluate the model
score = model.evaluate(X, Y, verbose=-1)
print("%s: %.2f%%" % (model.metrics_names[3], score[3]*100))
        
################################################################################
#just diresa and diris death
#ML
output1_1=pd.concat([fff['DIRESA & DIRIS death']],axis=1)
################################################################################
#Numerical inputs
#branch
#molecular rate%
num1 = keras.layers.Dense(n0,input_dim=2, activation=keras.activations.relu,use_bias=True)(i1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n1,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n2,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n3,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n4,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n5,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n6,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1= keras.models.Model(inputs=i1,outputs=num1)

#free bed %
num2 = keras.layers.Dense(n0,input_dim=2, activation=keras.activations.relu,use_bias=True)(i2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n1,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n2,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n3,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n4,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n5,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n6,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2= keras.models.Model(inputs=i2,outputs=num2)

#vaccination effect
num3 = keras.layers.Dense(n0,input_dim=2, activation=keras.activations.relu,use_bias=True)(i3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n1,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n2,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n3,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n4,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n5,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n6,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3= keras.models.Model(inputs=i3,outputs=num3)

#lag of days
num4 = keras.layers.Dense(n0,input_dim=6, activation=keras.activations.relu,use_bias=True)(i4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n1,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n2,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n3,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n4,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n5,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n6,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.models.Model(inputs=i4,outputs=num4)

#conbined all branchs
mergedpre = concatenate([num1.output,num2.output,num3.output,num4.output])

out1 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre)
keras.layers.Dropout(rate=dp)
out1= keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(out1)
keras.layers.Dropout(rate=dp)

out2 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre)
keras.layers.Dropout(rate=dp)
out2 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(out2)
keras.layers.Dropout(rate=dp)

out3 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre)
keras.layers.Dropout(rate=dp)
out3 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(out3)
keras.layers.Dropout(rate=dp)

out4 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre)
keras.layers.Dropout(rate=dp)
out4 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(out4)
keras.layers.Dropout(rate=dp)

out5 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre)
keras.layers.Dropout(rate=dp)
out5 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(out5)
keras.layers.Dropout(rate=dp)

out6 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre)
keras.layers.Dropout(rate=dp)
out6 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(out6)
keras.layers.Dropout(rate=dp)

mergedpre2 = concatenate([num1.output,num2.output,num3.output,num4.output,out1,out2,out3,out4,out5,out6])

out7 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre2)
keras.layers.Dropout(rate=dp)
out7 = keras.layers.Dense(n8,activation=keras.activations.linear,use_bias=True)(out7)
keras.layers.Dropout(rate=dp)
 
out8 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre2)
keras.layers.Dropout(rate=dp)
out8 = keras.layers.Dense(n8,activation=keras.activations.linear,use_bias=True)(out8)
keras.layers.Dropout(rate=dp)

out9 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre2)
keras.layers.Dropout(rate=dp)
out9 = keras.layers.Dense(n8,activation=keras.activations.linear,use_bias=True)(out9)
keras.layers.Dropout(rate=dp)

out = concatenate([num1.output,num2.output,num3.output,num4.output,out7,out8,out9])

out= keras.layers.Dense(n5,activation=keras.activations.linear,use_bias=True)(out)
keras.layers.Dropout(rate=dp)
out= keras.layers.Dense(n6,activation=keras.activations.linear,use_bias=True)(out)
keras.layers.Dropout(rate=dp)
out = keras.layers.Dense(n7,activation=keras.activations.linear,use_bias=True)(out)
keras.layers.Dropout(rate=dp)
out = keras.layers.Dense(n8,activation=keras.activations.linear,use_bias=True)(out)
keras.layers.Dropout(rate=dp)
out = keras.layers.Dense(1)(out)

model1 = keras.models.Model(inputs=[i1,i2,i3,i4],outputs=out)

model1.compile(loss='mse', 
              optimizer=tf.keras.optimizers.Adamax(0.005), 
              metrics=['mse','mae','acc'])
################################################################################
#To train
print("to start")
obj_now = dt.datetime.now()
print("Current date & time: ", str(obj_now))
historical1=model1.fit([input1,input2,input3,input4],output1_1,epochs=ep, 
                     batch_size=64 ,
                     validation_split=.05,
                     shuffle=False,
                     callbacks=[es],
                     verbose=False)
print("to end")
obj_now = dt.datetime.now()
print("Current date & time: ", str(obj_now))
winsound.Beep(freq, duration)
winsound.Beep(freq, duration)
winsound.Beep(freq, duration)
winsound.Beep(freq, duration)
winsound.Beep(freq, duration)
winsound.Beep(freq, duration)
################################################################################
#sesgo
plt.figure()
plt.xlabel("iterations")
plt.ylabel("error for an instance of a neural network") 
plt.plot(historical1.history["mse"])
plt.plot(historical1.history["val_mse"])
plt.title('model mse')
plt.legend(['trained', 'validation'], loc='upper left')
plt.minorticks_on()
plt.show()
plt.savefig("DIRESA.&.DIRIS.death.model-mse.png",facecolor='white', bbox_inches="tight",
            pad_inches=0.1, transparent=True,dpi=900)

#Validation
plt.figure()
plt.xlabel("iterations")
plt.ylabel("validation accuracy of a neural network")  
plt.plot(historical1.history['acc'])
plt.plot(historical1.history['val_acc'])
plt.title('model accuracy')
plt.legend(['train', 'validation'], loc='upper left')
plt.minorticks_on()
plt.show()
plt.savefig("DIRESA.&.DIRIS.death.model-accuracy.png",facecolor='white', bbox_inches="tight",
            pad_inches=0.1, transparent=True,dpi=900)

##############################
#prediction with validation data 
resultado1=model1.predict([input1,input2,input3,input4])
resu1=np.array(resultado1,dtype=float)

#sesgo
k=2#number of signifance 

#Relation ship between real vs virtual without transform
plt.figure()
df1 = pd.DataFrame({'real': output1_1["DIRESA & DIRIS death"],
                   'virtual':output1_1["DIRESA & DIRIS death"]})
df1["virtual"]=resu1.round(k)[:, [0]]
#create regplot
p0 = sns.regplot(x=df1.real, y=df1.virtual,data=df1)
plt.title('Real vs Virtual '+"DIRESA & DIRIS death")
#calculate slope and intercept of regression equation
slope3, intercept3, r3, p3, sterr3 = scipy.stats.linregress(df1)

#add regression equation to plot
plt.text(500, 2500, 'y = ' + str(round(intercept3,5)) + ' + ' + str(round(slope3,5)) + 'x'+", p-value="+str(round(p3,5))+", r-squared="+str(round(r3,5)))
plt.minorticks_on()
plt.show()
plt.savefig("DIRESA.&.DIRIS.death.Real.vs.Virtual2.png",facecolor='white', bbox_inches="tight",
            pad_inches=0.1, transparent=True,dpi=900)
            
#times series DIRESA & DIRIS death
plt.figure()
plt.xlabel("days")
plt.ylabel("DIRESA & DIRIS death") 
plt.plot(fff['Unnamed: 0'],output1_1["DIRESA & DIRIS death"],'o-',markersize=1, color='black')
plt.plot(fff['Unnamed: 0'],resu1.round(k)[:, [0]],'-',markersize=.1, color='red')
plt.title('Time serie')
plt.legend(['Real', 'Virtual'], loc='upper left')
plt.minorticks_on()
plt.show()
plt.savefig("DIRESA.&.DIRIS.death.time.series2.png",facecolor='white', bbox_inches="tight",
            pad_inches=0.1, transparent=True,dpi=900)
################################################################################            
plot_model(model1, to_file='model1.png')
visualkeras.layered_view(model1, to_file='diresa.diris.output.png').show() # write and show

#save model
save_model(model1, "diresa_diris_model.h5")
# load model
model_diresa_diris = load_model('diresa_diris_model.h5')
# summarize model.
model_diresa_diris.summary()

# split into input (X) and output (Y) variables
X1 = [input1,input2,input3,input4]
Y1 = output1_1
# evaluate the model
score1 = model_diresa_diris.evaluate(X1, Y1, verbose=0)
print("%s: %.2f%%" % (model_diresa_diris.metrics_names[3], score1[3]*100))
################################################################################
#just SINADEF DEATH
#ML
output1_2=pd.concat([fff['SINADEF excess of death']],axis=1)
################################################################################
#Numerical inputs
#branch
#molecular rate%
num1 = keras.layers.Dense(n0,input_dim=2, activation=keras.activations.relu,use_bias=True)(i1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n1,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n2,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n3,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n4,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n5,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n6,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(num1)
keras.layers.Dropout(rate=dp)
num1= keras.models.Model(inputs=i1,outputs=num1)

#free bed %
num2 = keras.layers.Dense(n0,input_dim=2, activation=keras.activations.relu,use_bias=True)(i2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n1,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n2,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n3,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n4,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n5,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n6,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(num2)
keras.layers.Dropout(rate=dp)
num2= keras.models.Model(inputs=i2,outputs=num2)

#vaccination effect
num3 = keras.layers.Dense(n0,input_dim=2, activation=keras.activations.relu,use_bias=True)(i3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n1,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n2,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n3,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n4,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n5,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n6,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(num3)
keras.layers.Dropout(rate=dp)
num3= keras.models.Model(inputs=i3,outputs=num3)

#lag of days
num4 = keras.layers.Dense(n0,input_dim=6, activation=keras.activations.relu,use_bias=True)(i4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n1,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n2,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n3,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n4,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n5,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n6,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(num4)
keras.layers.Dropout(rate=dp)
num4 = keras.models.Model(inputs=i4,outputs=num4)

#conbined all branchs
mergedpre = concatenate([num1.output,num2.output,num3.output,num4.output])

out1 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre)
keras.layers.Dropout(rate=dp)
out1= keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(out1)
keras.layers.Dropout(rate=dp)

out2 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre)
keras.layers.Dropout(rate=dp)
out2 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(out2)
keras.layers.Dropout(rate=dp)

out3 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre)
keras.layers.Dropout(rate=dp)
out3 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(out3)
keras.layers.Dropout(rate=dp)

out4 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre)
keras.layers.Dropout(rate=dp)
out4 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(out4)
keras.layers.Dropout(rate=dp)

out5 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre)
keras.layers.Dropout(rate=dp)
out5 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(out5)
keras.layers.Dropout(rate=dp)

out6 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre)
keras.layers.Dropout(rate=dp)
out6 = keras.layers.Dense(n8,activation=keras.activations.relu,use_bias=True)(out6)
keras.layers.Dropout(rate=dp)

mergedpre2 = concatenate([num1.output,num2.output,num3.output,num4.output,out1,out2,out3,out4,out5,out6])

out7 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre2)
keras.layers.Dropout(rate=dp)
out7 = keras.layers.Dense(n8,activation=keras.activations.linear,use_bias=True)(out7)
keras.layers.Dropout(rate=dp)
 
out8 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre2)
keras.layers.Dropout(rate=dp)
out8 = keras.layers.Dense(n8,activation=keras.activations.linear,use_bias=True)(out8)
keras.layers.Dropout(rate=dp)

out9 = keras.layers.Dense(n7,activation=keras.activations.relu,use_bias=True)(mergedpre2)
keras.layers.Dropout(rate=dp)
out9 = keras.layers.Dense(n8,activation=keras.activations.linear,use_bias=True)(out9)
keras.layers.Dropout(rate=dp)

out = concatenate([num1.output,num2.output,num3.output,num4.output,out7,out8,out9])

out= keras.layers.Dense(n5,activation=keras.activations.linear,use_bias=True)(out)
keras.layers.Dropout(rate=dp)
out= keras.layers.Dense(n6,activation=keras.activations.linear,use_bias=True)(out)
keras.layers.Dropout(rate=dp)
out = keras.layers.Dense(n7,activation=keras.activations.linear,use_bias=True)(out)
keras.layers.Dropout(rate=dp)
out = keras.layers.Dense(n8,activation=keras.activations.linear,use_bias=True)(out)
keras.layers.Dropout(rate=dp)
out = keras.layers.Dense(1)(out)

model2 = keras.models.Model(inputs=[i1,i2,i3,i4],outputs=out)

model2.compile(loss='mse', 
              optimizer=tf.keras.optimizers.Adamax(0.005), 
              metrics=['mse','mae','acc'])
################################################################################
#To train
print("to start")
obj_now = dt.datetime.now()
print("Current date & time: ", str(obj_now))
historical2=model2.fit([input1,input2,input3,input4],output1_2,epochs=ep, 
                     batch_size=64 ,
                     validation_split=.05,
                     shuffle=False,
                     callbacks=[es],
                     verbose=False)
print("to end")
obj_now = dt.datetime.now()
print("Current date & time: ", str(obj_now))
winsound.Beep(freq, duration)
winsound.Beep(freq, duration)
winsound.Beep(freq, duration)
winsound.Beep(freq, duration)
winsound.Beep(freq, duration)
winsound.Beep(freq, duration)
################################################################################
#sesgo
plt.figure()
plt.xlabel("iterations")
plt.ylabel("error for an instance of a neural network") 
plt.plot(historical2.history["mse"])
plt.plot(historical2.history["val_mse"])
plt.title('model mse')
plt.legend(['trained', 'validation'], loc='upper left')
plt.minorticks_on()
plt.show()
plt.savefig("SINADEF.excess.of.death.model-mse.png",facecolor='white', bbox_inches="tight",
            pad_inches=0.1, transparent=True,dpi=900)

#Validation
plt.figure()
plt.xlabel("iterations")
plt.ylabel("validation accuracy of a neural network")  
plt.plot(historical2.history['acc'])
plt.plot(historical2.history['val_acc'])
plt.title('model accuracy')
plt.legend(['train', 'validation'], loc='upper left')
plt.minorticks_on()
plt.show()
plt.savefig("SINADEF.excess.of.death.model-accuracy.png",facecolor='white', bbox_inches="tight",
            pad_inches=0.1, transparent=True,dpi=900)
            
##############################
#prediction with validation data 
resultado2=model2.predict([input1,input2,input3,input4])
resu2=np.array(resultado2,dtype=float)

#Relation ship between real vs virtual without transform
plt.figure()
df2 = pd.DataFrame({'real': output1_2["SINADEF excess of death"],
                   'virtual':output1_2["SINADEF excess of death"]})
df2["virtual"]=resu2.round(k)[:, [0]]
#create regplot
p2 = sns.regplot(x=df2.real, y=df2.virtual,data=df2)
plt.title('Real vs Virtual '+"SINADEF excess of death")
#calculate slope and intercept of regression equation
slope4, intercept4, r4, p4, sterr4 = scipy.stats.linregress(df2)

#add regression equation to plot
plt.text(20, 50, 'y = ' + str(round(intercept4,5)) + ' + ' + str(round(slope4,5)) + 'x'+", p-value="+str(round(p4,5))+", r-squared="+str(round(r4,5)))
plt.minorticks_on()
plt.show()
plt.savefig("SINADEF.excess.of.death.Real.vs.Virtual2.png",facecolor='white', bbox_inches="tight",
            pad_inches=0.1, transparent=True,dpi=900)

#times series DIRESA & DIRIS death
plt.figure()
plt.xlabel("days")
plt.ylabel("DIRESA & DIRIS death") 
plt.plot(fff['Unnamed: 0'],output1_2["SINADEF excess of death"],'o-',markersize=1, color='black')
plt.plot(fff['Unnamed: 0'],resu2.round(k)[:, [0]],'-',markersize=.1, color='red')
plt.title('Time serie')
plt.legend(['Real', 'Virtual'], loc='upper left')
plt.minorticks_on()
plt.show()
plt.savefig("SINADEF.excess.of.death.time.series2.png",facecolor='white', bbox_inches="tight",
            pad_inches=0.1, transparent=True,dpi=900)
            
################################################################################            
plot_model(model2, to_file='model2.png')
visualkeras.layered_view(model2, to_file='sinadef.output.png').show() # write and show

#save model
save_model(model2, "sinadef.h5")
# load model
model_sinadef = load_model('sinadef.h5')
# summarize model.
model_sinadef.summary()

# split into input (X) and output (Y) variables
X2 = [input1,input2,input3,input4]
Y2 = output1_2
# evaluate the model
score2 = model_sinadef.evaluate(X2, Y2, verbose=0)
print("%s: %.2f%%" % (model_sinadef.metrics_names[3], score2[3]*100))
