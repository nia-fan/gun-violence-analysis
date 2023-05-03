#!/usr/bin/env python
# coding: utf-8

# In[2]:


import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt 
import numpy as np 
import plotly.express as px
import plotly.graph_objects as go
from plotly.subplots import make_subplots


# In[1]:


gundf = pd.read_csv('C:/Users/niafa/SDSU/Spring2023/Capstone/cleaned_data.csv')
gundf.head()


# In[2]:


rows_count = len(gundf.index)
print(rows_count)


# In[3]:


column_names = list(gundf.columns.values)
print(column_names)


# In[4]:


nan_count_lat = gundf['latitude'].isna().sum()
print(nan_count_lat)

nan_count_lon = gundf['longitude'].isna().sum()
print(nan_count_lon)


# In[5]:


# Drop rows with None/NaN values in latitude
guns = gundf[gundf.latitude.notnull()]
print(guns)


# In[6]:


rows_count = len(guns.index)
print(rows_count)


# In[7]:


# drop rows in year 2013
guns = guns[guns["date"].str.contains("2013")==False]


# In[8]:


rows_count = len(guns.index)
print(rows_count)


# In[24]:


# new csv which only includes rows with latitude/longitude
guns.to_csv('C:/Users/niafa/SDSU/Spring2023/Capstone/heatmap.csv')


# In[3]:


# participants table
participants = pd.read_csv('C:/Users/niafa/SDSU/Spring2023/Capstone/participants.csv')
participants.head()


# In[3]:


# see types of participants 
participants.type.unique()


# In[5]:


# filter based on suspect or victim type 
victims = participants[participants.type == 'Victim']
suspects = participants[participants.type == 'Subject-Suspect']

# drop age values 0 
victims = victims[victims.age != 0]
suspects = suspects[suspects.age != 0]

# drop max victim age 209
victims = victims[victims.age != 209]

# drop max suspect age 311
suspects = suspects[suspects.age != 311]

print('victim count: ') 
print(len(victims))
print('suspect count: ')
print(len(suspects))


# In[5]:


victims.hist(column='age', bins = [0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80])


# In[6]:


suspects.hist(column='age', bins = [0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80])


# In[8]:


# plotly histogram for victim age
victim_age = px.histogram(victims, x="age", title = "Victim Age")
victim_age.update_traces(nbinsx=50, selector=dict(start=0, end=100, size=5,type='histogram'))
victim_age.show()
victim_age.write_html("C:/Users/niafa/SDSU/Spring2023/Capstone/Website/project/project/assets/images/victim-age.html")


# In[9]:


# plotly histogram for suspect age
suspect_age = px.histogram(suspects, x="age", title = "Suspect Age")
suspect_age.update_traces(nbinsx=50, selector=dict(start=0, end=100, size=5,type='histogram'))
suspect_age.show()
suspect_age.write_html("C:/Users/niafa/SDSU/Spring2023/Capstone/Website/project/project/assets/images/suspect-age.html")


# In[10]:


# filter victims/suspects based on gender
# drop gender values 0 
victims = victims[victims.gender != '0']
victims = victims[victims.gender != 'Male,-female']

suspects = suspects[suspects.gender != '0']


# In[11]:


# see genders
victims.gender.unique()


# In[12]:


# plotly histogram victim gender
victim_gender = px.histogram(victims, x="gender", title = 'Victims by Gender', category_orders=dict(gender=["Female", "Male"]))
victim_gender.show()
victim_gender.write_html("C:/Users/niafa/SDSU/Spring2023/Capstone/Website/project/project/assets/images/victim-gender.html")


# In[13]:


# plotly histogram suspect gender
suspect_gender = px.histogram(suspects, x="gender", title = 'Suspects by Gender', category_orders=dict(gender=["Female", "Male"]))
suspect_gender.show()
suspect_gender.write_html("C:/Users/niafa/SDSU/Spring2023/Capstone/Website/project/project/assets/images/suspect-gender.html")


# In[14]:


# see relationship types 
participants.relationship.unique()


# In[15]:


# drop relationship values of 0 and print frequency counts of each value
participants = participants[participants.relationship != '0']
print('Participant Relationships')
print(participants.relationship.value_counts())


# In[16]:


# plotly histogram participant relationships
relationships = px.histogram(participants, x="relationship", title = "Relationships between Suspects and Victims", 
                             category_orders=dict(relationship=["Family", "Armed-Robbery", "Significant-others---current-or-former", 
                                                               "Friends", "Aquaintance", "Neighbor", "Gang-vs-Gang",
                                                                "Home-Invasion---Perp-Does-Not-Know-Victim", 
                                                                "Home-Invasion---Perp-Knows-Victim", "Co-worker", "Drive-by---Random-victims",
                                                               "Mass-shooting---Random-victims", "Mass-shooting---Perp-Knows-Victims"]))
relationships.show()
relationships.write_html("C:/Users/niafa/SDSU/Spring2023/Capstone/Website/project/project/assets/images/relationships.html")


# In[17]:


# see status types
participants.status.unique()


# In[7]:


# drop status type values of 0 and print frequency counts of each value for suspects
suspects = suspects[suspects.status != '0']
# drop status types that don't make sense
suspects = suspects[suspects.status != 'Killed,-Arrested']
suspects = suspects[suspects.status != 'Injured,-Unharmed,-Arrested']
suspects = suspects[suspects.status != 'Killed,-Unharmed']
suspects = suspects[suspects.status != 'Killed,-Unharmed,-Arrested']
suspects = suspects[suspects.status != 'Injured,-Unharmed']
suspects = suspects[suspects.status != 'Killed,-Injured']

print('Suspect Status')
print(suspects.status.value_counts())


# In[8]:


# histogram suspect status
sus_status = px.histogram(suspects, x="status", title = "Suspect Status", 
                             category_orders=dict(status=["Unharmed,-Arrested", "Unharmed", "Arrested", 
                                                               "Killed", "Injured", "Injured,-Arrested", "Killed,-Arrested",
                                                                "Injured,-Unharmed,-Arrested", 
                                                                "Killed,-Unharmed", "Killed,-Unharmed,-Arrested", "Injured,-Unharmed",
                                                               "Killed,-Injured"]))
sus_status.show()
sus_status.write_html("C:/Users/niafa/SDSU/Spring2023/Capstone/Website/project/project/assets/images/sus_status.html")


# In[3]:


# visualizations
guns = pd.read_csv('C:/Users/niafa/SDSU/Spring2023/Capstone/guns.csv')
incidents = pd.read_csv('C:/Users/niafa/SDSU/Spring2023/Capstone/incidents.csv')
participants = pd.read_csv('C:/Users/niafa/SDSU/Spring2023/Capstone/participants.csv')


# In[20]:


# gun ID histogram
gun_hist = px.histogram(guns['gun_id'], title = "Gun ID Distribution")
gun_hist.show()
gun_hist.write_html("C:/Users/niafa/SDSU/Spring2023/Capstone/Website/v4/proj/proj/assets/images/gun-id-hist.html")


# In[33]:


# gun stolen status histogram
guns = guns[guns.stolen != '0']
guns = guns[guns.stolen != 'Unknown']
gun_stolen = px.histogram(guns['stolen'], title = "Gun Stolen Status")
gun_stolen.show()
gun_stolen.write_html("C:/Users/niafa/SDSU/Spring2023/Capstone/Website/v4/proj/proj/assets/images/gun-stolen.html")


# In[35]:


# gun type histogram
guns = pd.read_csv('C:/Users/niafa/SDSU/Spring2023/Capstone/guns.csv')
guns = guns[guns.type != 'Unknown']
guntype = px.histogram(guns['type'], title = "Gun Type")
guntype.show()
guntype.write_html("C:/Users/niafa/SDSU/Spring2023/Capstone/Website/v4/proj/proj/assets/images/gun-type.html")


# In[24]:


# victim age group distribution
p_vic = participants.loc[(participants['type'] == 'Victim')]
p_vic = p_vic[p_vic.age_group != '0']
vicagegroup = px.histogram(p_vic['age_group'], title="Victim Age Group Distribution")
vicagegroup.show()
vicagegroup.write_html("C:/Users/niafa/SDSU/Spring2023/Capstone/Website/v4/proj/proj/assets/images/victim-age-dist.html")


# In[25]:


# suspect age group distribution
p_su = participants.loc[(participants['type'] == 'Subject-Suspect')]
p_su = p_su[p_su.age_group != '0']
susagegroup = px.histogram(p_su['age_group'], title="Suspect Age Group Distribution")
susagegroup.show()
susagegroup.write_html("C:/Users/niafa/SDSU/Spring2023/Capstone/Website/v4/proj/proj/assets/images/suspect-age-dist.html")


# In[26]:


# suspect and victim counts
parttype = px.histogram(participants["type"])
parttype.show()
parttype.write_html("C:/Users/niafa/SDSU/Spring2023/Capstone/Website/v4/proj/proj/assets/images/suspect-victim-count.html")


# In[27]:


participants["age_num"] = pd.to_numeric(participants["age"])
participants["age_num"].describe()


# In[28]:


np.where(participants["age_num"]>100)


# In[29]:


p_age_range = participants.loc[(participants["age_num"] >= 1) & (participants["age_num"] <= 150)]

fig = px.histogram(x=p_age_range['age_num'])
fig.update_traces(marker_line_width=1,marker_line_color="white")
fig.show()


# In[32]:


p_vic_1 = p_age_range.loc[(p_age_range['type'] == 'Victim')]
p_sus_1 = p_age_range.loc[(p_age_range['type'] == 'Subject-Suspect')]

import plotly.offline as py
import plotly.graph_objs as go

x0 = p_vic_1['age_num']
x1 = p_sus_1['age_num']
victim = go.Histogram(
    x=x0,
    opacity=0.6
)
suspect = go.Histogram(
    x=x1,
    opacity=0.4
)
data = [victim, suspect]
layout = go.Layout(barmode='overlay')
fig = go.Figure(data=data, layout=layout)
fig.update_traces(marker_line_width=1,marker_line_color="white")
fig.show()
fig.write_html("C:/Users/niafa/SDSU/Spring2023/Capstone/Website/v4/proj/proj/assets/images/victim-suspect-ages.html")


# In[ ]:




