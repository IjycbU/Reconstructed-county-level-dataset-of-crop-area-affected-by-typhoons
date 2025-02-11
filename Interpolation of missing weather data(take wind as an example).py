import numpy as np
import pandas as pd
filename0="D:/Wwj/data/disaster data2/530sta.txt"
df1=pd.read_csv(filename0,header=None,sep='\s+')
df1.columns=['station','lat','lon']
station = df1['station']
lat = df1['lat']
lon = df1['lon']

path1 = "D:/Wwj/data/disaster data2/weather data/wind/"    #wind data for the stations
path2= "D:/Wwj/data/disaster data2/dis2-530/"  #Distance from other staions 
path3="D:/Wwj/data/disaster data2/weather data/wind/"  
path0= "D:/Wwj/data/disaster data2/weather data/wind插值/"  #output
for s in range(len(station)):                                 
    filename1=path1+('%5d.dat'%(station[s]))
    stid1 = []
    lat1 = []
    lon1 = []
    year1 = []
    month1 = []
    day1 = []
    fs1 = []
    fx1=[]
    with open(filename1, 'r', encoding='UTF-8') as f1:
        lines = f1.readlines()
        for line in lines:
            mm = line.split()
            stid1.append(int(mm[0]))
            lat1.append(float(mm[1]))
            lon1.append(float(mm[2]))
            year1.append(int(mm[3]))
            month1.append(int(mm[4]))
            day1.append(int(mm[5]))
            fs1.append(int(mm[6]))
            fx1.append(int(mm[7]))
    fs22=np.zeros(15706)    
    fs222 = np.zeros(15706)
    dis2= np.zeros(15706)
    for i in range(len(day1)):               
        if fs1[i] == 32766:
            #print(year1[i])
            filename2 = path2 + ('%5d.dat' % (stid1[i]))
            sta10=[]
            Distance10=[]
            with open(filename2, 'r', encoding='UTF-8') as f2:
                lines = f2.readlines()
                for line in lines:
                    mm = line.split()
                    sta10.append(int(mm[0]))
                    Distance10.append(float(mm[1]))
            #print(sta10)
            for j in range(len(sta10)):        
                filename3 = path3 + ('%5d.dat' % (sta10[j]))
                #print(filename1)
                fs2=[]
                day2=[]
                year2=[]
                month2=[]
                fx2=[]
                with open(filename3, 'r', encoding='UTF-8') as f3:
                    lines = f3.readlines()
                    for line in lines:
                        mm = line.split()
                        year2.append(int(mm[3]))
                        month2.append(int(mm[4]))
                        day2.append(int(mm[5]))
                        fs2.append(int(mm[6]))
                        fx2.append(int(mm[7]))
                    for k in range(len(day2)):
                        if  year1[i]==year2[k] and month1[i]==month2[k] and day1[i]==day2[k] and fs2[k]!=32766 :
                            #print(fs2[k])
                            fs22[i] = fs22[i] + fs2[k] / Distance10[j] / Distance10[j] 
                            dis2[i]= dis2[i] + 1 / Distance10[j] / Distance10[j]
                            break
            if dis2[i] != 0.:
                fs222[i] =fs22[i] / dis2[i]
                fs1[i]=fs222[i]
                fx1[i] = 0.
##################################
            else:
                fs1[i]=32766
                fx1[i]=32766
            #print(fs1[i],fx1[i])
    outfile0 = path0 + ('%5d.dat' % (station[s]))
    print(outfile0)
    with open(outfile0, 'a') as fw:
        for kk in range(len(day1)):  
            fw.write("%5d %4.2f %5.2f %4d %2d %2d %5.2f %5d\n" % (stid1[kk], lat1[kk], lon1[kk], year1[kk], month1[kk], day1[kk], fs1[kk], fx1[kk]))
