import numpy as np
import pandas as pd
from sklearn.cross_decomposition import CCA
from sklearn import preprocessing
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import r2_score
import statsmodels.api as sm
from scipy.optimize import leastsq

def mean_fc(u1):   
    u1_m=np.mean(u1)
    sigma=np.std(u1)
    return(u1_m,sigma)

def max_min(u2):
    u2_max=max(u2)
    u2_min=min(u2)
    return(u2_min,u2_max)

def CCAmodel(u,v):
    cca = CCA(n_components=1) 
    cca.fit(u, v)
    u_train_r, v_train_r = cca.transform(u, v)
    return(cca.coef_)

filename='D:/Wwj/data/disaster data4/530sta.txt'
path0='D:/Wwj/data/disaster data4/Reconstruction2/Singlestation25.2/'
station = []
lat0 = []
lon0 = []
with open(filename, 'r', encoding='UTF-8') as f:
    lines = f.readlines()
    for line in lines:
        mm = line.split()
        station.append(str(mm[0]))
        lat0.append(float(mm[1]))
        lon0.append(float(mm[2]))
for i in range(len(station)):
    datafile = path0 +('%s.dat' % (station[i]))
    df = pd.read_csv(datafile,header=None,sep='\s+',encoding='gbk')

    df.columns=['sheng','shi','xian','xbm','Tcbh','Tcxh','sta','lat','lon','fx','max_rain','pro_rain', 'fs','szmj'] 
    Y1=df['szmj']
    ###########################################################################################
    yuan_yumean=mean_fc(df['pro_rain'])[0]
    yuan_yusigema=mean_fc(df['pro_rain'])[1]

    yuan_fsmean=mean_fc(df['fs'])[0]
    yuan_fssigema=mean_fc(df['fs'])[1]

    yuan_yumaxmean = mean_fc(df['max_rain'])[0]
    yuan_yumaxsigema = mean_fc(df['max_rain'])[1]
    
    yuan_zqmean=mean_fc(df['szmj'])[0] 
    yuan_zqsigema=mean_fc(df['szmj'])[1]
#################  z-score  #######################################################
    #data = preprocessing.scale(values) 
    X1data = preprocessing.scale(df['pro_rain'])
    X2data = preprocessing.scale(df['fs'])
    X3data = preprocessing.scale(df['max_rain'])
    Y0data = preprocessing.scale(df['szmj'])
    ############################################################################
    X1=X1data 
    X2=X2data
    X3=X3data
    X0=np.transpose([X1,X2,X3])
    X=X0.tolist()
   
    Y0=Y0data 
    Y3=np.transpose(Y0)
    Y=Y3.tolist()
    ###############################################################################
    X00=CCAmodel(X,Y)
    rain_weight=X00[0,0]
    wind_weight=X00[0,1]
    maxrain_weight=X00[0,2]
    zzyz=rain_weight*X1+wind_weight*X2+maxrain_weight*X3
    ###############################################################################
    zzyz_min = max_min(zzyz)[0]
    zzyz_max = max_min(zzyz)[1]
    ######################################################################
    x =zzyz.flatten()
    zzyz2=x.tolist()
    df['xxxx'] =zzyz2
    x=df['xxxx']
    y=Y
    coef = np.polyfit(x, y, 2)
    getmodel=np.poly1d(np.polyfit(x, y,2))
    y_fit = np.polyval(coef, x)
    R_square=r2_score(y,getmodel(x))
    #############################################################################
    nsample=len(x)
    xx= np.column_stack((x, x**2))
    xx= sm.add_constant(xx)
    beta =(coef[2],coef[1],coef[0])    
    model = sm.OLS(y,xx)
    results = model.fit()
    jianyan=results.f_pvalue
    outfile0 ='D:/Wwj/data/disaster data4/Reconstruction/County2-25.2.txt'
    with open(outfile0, 'a') as fw:
        fw.write("%s %7.4f %7.4f %7.4f %2.4f %5.4f %5.4f %5.4f %5.4f %5.4f %5.4f %5.4f %5.4f  %2.3f %2.3f %2.3f %5.4f %5.4f %2.3f\n" %
                 (station[i],coef[0],coef[1],coef[2], R_square,yuan_yumean,yuan_yusigema,yuan_fsmean,yuan_fssigema,yuan_yumaxmean,yuan_yumaxsigema,yuan_zqmean,yuan_zqsigema,
                  rain_weight,wind_weight,maxrain_weight,zzyz_max,zzyz_min,jianyan))
