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

az.style.use('arviz-doc')

# read df
d = pd.read_csv("/home/iair/Documents/Investigación/perceptual_stability_autism/Big_pilot_4(dot_reference_first)/df_exp.csv")


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


## normal-normal model

# likelihood (filter by morph to have only one estimate of bias per participant)
bias = df_filtered_2["bias"].to_numpy()
print(bias)

# run the model
with pm.Model() as model_bias:
    # prior
    μ = pm.Normal('μ', 0, 10)
    σ = pm.HalfNormal("σ", sigma=1)

    # el likelihood
    y = pm.Normal("y", mu=μ, sigma=σ, observed=bias)
    idata_bias = pm.sample(chains=4)

# model figure
pm.model_to_graphviz(model_bias)

# Chain diagnostics
az.summary(idata_bias, kind="diagnostics")
az.plot_trace(idata_bias)
az.plot_rank(idata_bias)

# Plot 94% hdi
az.plot_posterior(idata_bias);

# posterior predictive
ppc_bias = pm.sample_posterior_predictive(idata_bias, model=model_bias)
az.plot_ppc(ppc_bias, num_pp_samples=200);




