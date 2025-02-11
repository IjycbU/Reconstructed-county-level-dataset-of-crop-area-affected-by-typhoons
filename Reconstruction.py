import pandas as pd
import numpy as np
datafile1='D:/Wwj/data/disaster data4/80-22pre.txt'
datafile2='D:/Wwj/data/disaster data4/Reconstruction2/County25.2.txt'

df1=pd.read_csv(datafile1,header=None,sep='\s+')
df1.columns=['name','sta','lat','lon','pro_rain','max_rain','rainday','fs','fx','zjs0']

disa1=np.zeros(156251)
disa=np.zeros(156251)

df2=pd.read_csv(datafile2,header=None,sep='\s+')
df2.columns=['station','x2','x1','x0','R^2','o_prorainmean','o_prorainstandarddeviation' ,'o_windmean', 'o_windstandarddeviation',
              'o_maxrainmean','o_maxrainstandarddeviation','o_disa_mean', 'o_disastandarddeviation','prorainweight', 'windweight','maxrainweight', 'zzyzmax' ,'zzyzmin','jianyan']

for i in range(len(df1['sta'])):
    for j in range(len(df2['station'])):
        if df1['sta'][i]==df2['station'][j]:
         
            rain00 = (df1['pro_rain'][i] - df2['o_prorainmean'][j]) / (df2['o_prorainstandarddeviation'][j])                 #Z-score
            wind00 = (df1['fs'][i] - df2['o_windmean'][j]) / (df2['o_windstandarddeviation'][j])                       #Z-score
            rain00_max = (df1['max_rain'][i] - df2['o_maxrainmean'][j]) / (df2['o_maxrainstandarddeviation'][j])         #Z-score
            zzyz00 = rain00 * df2['prorainweight'][j] + wind00 * df2['windweight'][j]+rain00_max* df2['maxrainweight'][j]       

            disa1[i] = zzyz00 * zzyz00 * df2['x2'][j] + zzyz00 * df2['x1'][j] + df2['x0'][j]
            disa[i] = disa1[i] * df2['o_disa_mean'][j]+df2['o_disastandarddeviation'][j]
            break

    outfile0 = 'D:/Wwj/data/disaster data4/Reconstruction2/Results25.2.txt'
    with open(outfile0, 'a') as fw:
        fw.write("%s %5d %3.2f %4.2f %4.2f %4.2f %2d %3.2f %4.2f %12.5f %s\n" %
                 (df1['name'][i], df1['sta'][i], df1['lat'][i], df1['lon'][i], df1['pro_rain'][i], df1['max_rain'][i],
                  df1['rainday'][i], df1['fs'][i], df1['fx'][i], disa[i],df1['name'][i][4:10]))
