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
d = pd.read_csv("/home/iair/Documents/Investigación/perceptual_stability_autism/pilot_3/df_exp_long_DotsTotal.csv")


### Filter participant by bias/response above X sd of the mean

def outliers(d, by="bias", sd_out=2):
    if by == "bias":
        sd_bias = np.std(d['bias'])
        mean_bias = np.mean(d['bias'])

        # Filter participant by bias above X sd of the mean
        participants_to_remove = d[
            np.abs(d['bias'] - mean_bias) >= sd_out * sd_bias
        ]['participant'].unique()

        d_without_outliers = d[~d['participant'].isin(participants_to_remove)]

    if by == "response":
        # Calculate the median and sd by type of stim
        stimulus_stats = d.groupby('type').agg(
            mean_response=pd.NamedAgg(column='response', aggfunc='mean'),
            sd_response=pd.NamedAgg(column='response', aggfunc='std')
        ).reset_index()

        # merge with df
        df_exp_with_stats = d.merge(stimulus_stats, on='type', how='left')

        # Filter participant by response above X sd of the mean
        participants_to_remove = df_exp_with_stats[
            np.abs(df_exp_with_stats['response'] - df_exp_with_stats['mean_response']) >= sd_out * df_exp_with_stats['sd_response']
        ]['participant'].unique()

        d_without_outliers = df_exp_with_stats[~df_exp_with_stats['participant'].isin(participants_to_remove)]

    return d_without_outliers

# filter
d_without_outliers = outliers(d, by="response", sd_out=2)

# calculate bias_yuval
df_with_bias_yuval = d_without_outliers.copy()
pivoted_df = df_with_bias_yuval.pivot(index='participant', columns='type', values='response')
pivoted_df['bias_yuval'] = pivoted_df['many'] / pivoted_df['morph']
d_without_outliers = df_with_bias_yuval.merge(pivoted_df['bias_yuval'], on='participant', how='left')

# Model: yi = α + βor x order_codei + βaq x AQ_scaledi + βage x Age_scaledi + βsex x sex_codei + errori

#Where:  y = bias_yuval = many - morph 
#        order = {0 = morph First ; 1 = many First} 
#        AQ_scaled = AQ - mean(AQ) 
#        Age_scaled = age - mean(age) 
#        sex = {0 = Male ; 1 = Female}

## morph: 0 ; many: 1
d_without_outliers['order_code'] = d_without_outliers['order'].replace(['morphFirst'], 0)
d_without_outliers['order_code'] = d_without_outliers['order_code'].replace(['manyFirst'], 1)
d_without_outliers['AQ_scaled'] = d_without_outliers['AQ'] - d_without_outliers['AQ'].mean()
d_without_outliers['age_scaled'] = d_without_outliers['age'] - d_without_outliers['age'].mean()
d_without_outliers['sex_code'] = d_without_outliers['sex'].replace(['Male'], 0)
d_without_outliers['sex_code'] = d_without_outliers['sex_code'].replace(['Female'], 1)

print(d_without_outliers[['order_code', 'AQ_scaled','age_scaled','sex_code']])

with pm.Model() as model_dot_AQ:
    α = pm.Normal("α", mu=1, sigma=10)
    β0 = pm.Normal("β0", mu=0, sigma=10)
    β1 = pm.Normal("β1", mu=0, sigma=10)
    β2 = pm.Normal("β2", mu=0, sigma=10)
    β3 = pm.Normal("β3", mu=0, sigma=10)
    σ = pm.HalfNormal("σ", 15)
    μ = pm.Deterministic("μ", α +
                         β0 * d_without_outliers.order_code +
                         β1 * d_without_outliers.AQ_scaled +
                         β2 * d_without_outliers.age_scaled +
                         β3 * d_without_outliers.sex_code)
    _ = pm.Normal('y_pred', mu=μ, sigma=σ, observed=d_without_outliers.bias_yuval)

    idata_dot_AQ = pm.sample(random_seed=123, chains=4)


pm.model_to_graphviz(model_dot_AQ)

# Diagnostics
az.summary(idata_dot_AQ, kind="diagnostics")
az.plot_trace(idata_dot_AQ, combined=True);


# posterior 94% hdi
az.plot_posterior(idata_dot_AQ, var_names=['~μ']);

az.summary(idata_dot_AQ, var_names=['~μ']) 

az.plot_forest(idata_dot_AQ, combined = True, var_names=['~μ']);
plt.axvline(x=0, color='red', linestyle='--')
plt.show()

# posterior predictive
pm.sample_posterior_predictive(idata_dot_AQ, model=model_dot_AQ, random_seed=2, extend_inferencedata=True)
ax = az.plot_ppc(idata_dot_AQ, num_pp_samples=200)


# Plot of both regression lines, one per order
df_summary = az.summary(idata_dot_AQ, var_names=['~μ']) 
print(tabulate(df_summary, headers='keys', tablefmt='fancy_grid'))


for index, row in df_summary.iterrows():
    globals()[index] = row['mean']

intercept_order_code_0 = α
intercept_order_code_1 = α + β0 
slope = β1

plt.figure(figsize=(10, 8))
sns.scatterplot(data=d_without_outliers, x='AQ_scaled', y='bias_yuval', hue='order_code', palette={0: 'darkred', 1: 'darkblue'})
plt.plot(d_without_outliers['AQ_scaled'], intercept_order_code_0 + slope * d_without_outliers['AQ_scaled'], color='darkred', linewidth=1.5, label='α + βaq x AQ_scaled')
plt.plot(d_without_outliers['AQ_scaled'], intercept_order_code_1 + slope * d_without_outliers['AQ_scaled'], color='darkblue', linewidth=1.5, label='α + βor x order_code + βaq x AQ_scaled')
plt.axhline(y=1, linestyle='--', color='black')
plt.xlabel('AQ_scaled', fontsize=14)
plt.ylabel('bias_yuval', fontsize=14)
plt.legend(fontsize=12, title='order code', labels=['α + βaq x AQ_scaled', 'α + βor x order_code + βaq x AQ_scaled'])
plt.grid(False)
plt.show()


