#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Sep 19 11:03:17 2023

@author: iair
"""

import arviz as az
import pymc as pm
import preliz as pz
import numpy as np
import scipy.stats as stats
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from tabulate import tabulate

az.style.use('arviz-doc')

# read df
d = pd.read_csv("/home/iair/Documents/Investigación/perceptual_stability_autism/Big_pilot_4(dot_reference_first)/df_exp.csv")
df_NotExperimentData = pd.read_csv("/home/iair/Documents/Investigación/perceptual_stability_autism/Big_pilot_4(dot_reference_first)/df_NotExperimentData.csv")


# Filter participants with estimation values above 100
participants_to_exclude = d[d['estimation']> 100]['participants'].unique()
df_filtered_1 = d[~d['participants'].isin(participants_to_exclude)]

# Calculate summary statistics by 'type'
df_summary = df_filtered_1.groupby('type').agg(
    avg_estimation = ('estimation', 'mean'),
    std_estimation = ('estimation','std'))

# Filter participants with estimations aboive 2 std of the mean by type
participants_to_exclude_2 = df_filtered_1.merge(df_summary, on='type')
participants_to_exclude_2 = participants_to_exclude_2[participants_to_exclude_2['estimation'] >= participants_to_exclude_2['avg_estimation'] + 2* participants_to_exclude_2['std_estimation']]['participants'].unique()
df_filtered_2 = df_filtered_1[~df_filtered_1['participants'].isin(participants_to_exclude_2)]

# Create the bias
# Crear un DataFrame pivot con 'participants' como índice y 'type' como columnas, y calcular el bias
df_pivot = df_filtered_2.pivot(index='participants', columns='type', values='estimation')
df_pivot['bias'] = df_pivot['few'] - df_pivot['morph']
df_pivot.reset_index(inplace=True)

# Fusionar solo la columna 'bias' con df_filtered_2
df_filtered_2 = df_filtered_2.merge(df_pivot[['participants', 'bias']], on='participants')


## Combine both df
df_NotExperimentData_filtered = df_NotExperimentData[df_NotExperimentData['participants'].isin(df_filtered_2['participants'])]
df_filtered_2_selectedColumns = df_filtered_2[['participants', 'bias']]
df_filtered_2_unique = df_filtered_2_selectedColumns.drop_duplicates(subset=['participants'])
df_merged = df_NotExperimentData_filtered.merge(df_filtered_2_unique, on = 'participants')

## exclude some participants that doesnt complete the AQ very well.
participants_to_exclude_3 = list(range(50,59))
df_merged_filtered = df_merged[~df_merged['participants'].isin(participants_to_exclude_3)]


# Model: yi = α + βaq x AQ_scaledi + βage x Age_scaledi + βsex x sex_codei + errori

#Where:  y = bias = many - morph 
#        AQ_scaled = AQ - mean(AQ) 
#        Age_scaled = age - mean(age) 
#        sex = {0 = Male ; 1 = Female}

## morph: 0 ; many: 1
#d_without_outliers['order_code'] = d_without_outliers['order'].replace(['morphFirst'], 0)
#d_without_outliers['order_code'] = d_without_outliers['order_code'].replace(['manyFirst'], 1)
df_merged_filtered['age_scaled'] = df_merged_filtered['age'] - df_merged_filtered['age'].mean()
df_merged_filtered['sex_code'] = df_merged_filtered['sex'].replace(['Male'], 0)
df_merged_filtered['sex_code'] = df_merged_filtered['sex_code'].replace(['Female'], 1)


df_merged_filtered['AQ_scaled'] = df_merged_filtered['AQ'] - df_merged_filtered['AQ'].mean()
df_merged_filtered['AQ_social_scaled'] = df_merged_filtered['AQ_social'] - df_merged_filtered['AQ_social'].mean()
df_merged_filtered['AQ_attentional_switches_scaled'] = df_merged_filtered['AQ_attentional_switches'] - df_merged_filtered['AQ_attentional_switches'].mean()
df_merged_filtered['AQ_attencion_detail_scaled'] = df_merged_filtered['AQ_attencion_detail'] - df_merged_filtered['AQ_attencion_detail'].mean()
df_merged_filtered['AQ_communication_scaled'] = df_merged_filtered['AQ_communication'] - df_merged_filtered['AQ_communication'].mean()
df_merged_filtered['AQ_imagination_scaled'] = df_merged_filtered['AQ_imagination'] - df_merged_filtered['AQ_imagination'].mean()

AQ_rates = df_merged_filtered['AQ_imagination_scaled']

with pm.Model() as model_dot_AQ:
    α = pm.Normal("α", mu=0, sigma=10)
    β1 = pm.Normal("β1", mu=0, sigma=10)
    β2 = pm.Normal("β2", mu=0, sigma=10)
    β3 = pm.Normal("β3", mu=0, sigma=10)
    σ = pm.HalfNormal("σ", 15)
    μ = pm.Deterministic("μ", α +
                         β1 * AQ_rates +
                         β2 * df_merged_filtered.age_scaled +
                         β3 * df_merged_filtered.sex_code)
    _ = pm.Normal('y_pred', mu=μ, sigma=σ, observed=df_merged_filtered.bias)

    idata_dot_AQ = pm.sample(random_seed=123, chains=4)


pm.model_to_graphviz(model_dot_AQ)

# Diagnostics
az.summary(idata_dot_AQ, kind="diagnostics")
az.plot_trace(idata_dot_AQ, combined=True);


# posterior 94% hdi
az.plot_posterior(idata_dot_AQ, var_names=['~μ'], textsize=25);

az.summary(idata_dot_AQ, var_names=['~μ']) 

az.plot_forest(idata_dot_AQ, combined = True, var_names=['~μ']);
plt.axvline(x=0, color='red', linestyle='--')
plt.show()

# posterior predictive
pm.sample_posterior_predictive(idata_dot_AQ, model=model_dot_AQ, random_seed=2, extend_inferencedata=True)
ax = az.plot_ppc(idata_dot_AQ, num_pp_samples=200)


# Plot of both regression lines, one per order
# df_summary = az.summary(idata_dot_AQ, var_names=['~μ']) 
# print(tabulate(df_summary, headers='keys', tablefmt='fancy_grid'))

# for index, row in df_summary.iterrows():
#     globals()[index] = row['mean']

# intercept_order_code_0 = α
# intercept_order_code_1 = α + β0 
# slope = β1

# plt.figure(figsize=(10, 8))
# sns.scatterplot(data=d_without_outliers, x='AQ_scaled', y='bias', hue='order_code', palette={0: 'darkred', 1: 'darkblue'})
# plt.plot(d_without_outliers['AQ_scaled'], intercept_order_code_0 + slope * d_without_outliers['AQ_scaled'], color='darkred', linewidth=1.5, label='α + βaq x AQ_scaled')
# plt.plot(d_without_outliers['AQ_scaled'], intercept_order_code_1 + slope * d_without_outliers['AQ_scaled'], color='darkblue', linewidth=1.5, label='α + βor x order_code + βaq x AQ_scaled')
# plt.axhline(y=0, linestyle='--', color='black')
# plt.xlabel('AQ_scaled', fontsize=14)
# plt.ylabel('bias', fontsize=14)
# plt.legend(fontsize=12, title='order code', labels=['α + βaq x AQ_scaled', 'α + βor x order_code + βaq x AQ_scaled'])
# plt.grid(False)
# plt.show()


