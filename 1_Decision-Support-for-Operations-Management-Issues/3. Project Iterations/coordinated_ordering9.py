# To add a new cell, type '# %%'
# To add a new markdown cell, type '# %% [markdown]'

# %% [markdown]
# Read Data 

 
# %%
import numpy as np
import pandas as pd
import math


df= pd.read_excel(r'C:\Users\aduzo\Desktop\Scientific Project R\Scientific Project Main\Decision-Support-for-Operations-Management-Issues\product_data.xlsx') 

print(df)
period_mat= np.zeros(shape=(3,262))


# %%
item= 2#this is item 30 since counting starts from zero
item =item -1

rack_length=6000

b_not_sorting =df[['b_not_sorting']]
b_not_sorting =b_not_sorting.to_numpy()

l_start2 = df[['number_of_lanes']]
l_start2 =l_start2.to_numpy()
#type(l_start2)
dem = df[['deman_per_day_boxes']]
dem = dem.to_numpy()

quant= df[['Lot_size_q']]
quant = quant.to_numpy()

stock_level = (l_start2*rack_length)/b_not_sorting
stock_level[item]

#stock_level

period_mat= np.zeros(shape=(3,263))
period_mat.shape
m = df[['cycle_time_in_days']]
m = m.to_numpy()


# %%

t=0
for t in range(0,263):
    if item == 11:
        if t==0:
            period_mat[0,t]=dem[item]
            period_mat[2,t] = stock_level[item]

        else:
            period_mat[0,t]=dem[item]
            if  t % math.floor(m[item]) == 51 :
                period_mat[1,t] = stock_level[item] - period_mat[2,t-1]

                #fill the capacity
                period_mat[2,t] = stock_level[item]
            else:
                period_mat[1,t] = 0
                period_mat[2,t] = period_mat[2,t-1]-dem[item]
    
    elif item == 29:
        if t==0:
            period_mat[0,t]=dem[item]
            period_mat[2,t] = stock_level[item]

        else:
            period_mat[0,t]=dem[item]
            if  t % math.floor(m[item]) == 37 :
                period_mat[1,t] = stock_level[item] - period_mat[2,t-1]

                #fill the capacity
                period_mat[2,t] = stock_level[item]
            else:
                period_mat[1,t] = 0
                period_mat[2,t] = period_mat[2,t-1]-dem[item]


    else:

        if t==0:
            period_mat[0,t]=dem[item]
            period_mat[2,t] = stock_level[item]
        
        elif t==1:
            period_mat[0,t]=dem[item]
            period_mat[1,t]= 0
            period_mat[2,t] = stock_level[item]

        else:
            period_mat[0,t]=dem[item]
            if  t % math.floor(m[item]) == 1 :
                
            
                period_mat[1,t] = stock_level[item] - period_mat[2,t-1]

                #fill the capacity
                period_mat[2,t] = stock_level[item]
            else:
                period_mat[1,t] = 0
                period_mat[2,t] = period_mat[2,t-1]-dem[item]


print(period_mat)


# %%
period_mat[0,t]=dem[item]
pd.DataFrame(data= period_mat, index=["demand","quantity","Inventory Level"],columns=range(0,263))


