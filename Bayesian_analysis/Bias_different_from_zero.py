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


### Model when Many was first

filtro = d_without_outliers["order"].isin(["manyFirst"])
d_without_outliers_ManyFirst = d_without_outliers[filtro]
d_without_outliers_ManyFirst.shape

## normal-normal model for dot-manyFirst

# likelihood (filter by morph to have only one estimate of bias per participant)
filtro = d_without_outliers_ManyFirst["type"].isin(["morph"])
bias_ManyFirst = d_without_outliers_ManyFirst.loc[filtro, "bias"].to_numpy()
print(bias_ManyFirst)

# run the model
with pm.Model() as model_bias_ManyFirst:
    # prior
    μ = pm.Normal('μ', 0, 10)
    σ = pm.HalfNormal("σ", sigma=1)

    # el likelihood
    y = pm.Normal("y", mu=μ, sigma=σ, observed=bias_ManyFirst)
    idata_bias_ManyFirst = pm.sample(chains=4)

# model figure
pm.model_to_graphviz(model_bias_ManyFirst)

# Chain diagnostics
az.summary(idata_bias_ManyFirst, kind="diagnostics")
az.plot_trace(idata_bias_ManyFirst)
az.plot_rank(idata_bias_ManyFirst)

# Plot 94% hdi
az.plot_posterior(idata_bias_ManyFirst);

# posterior predictive
ppc_bias_ManyFirst = pm.sample_posterior_predictive(idata_bias_ManyFirst, model=model_bias_ManyFirst)
az.plot_ppc(ppc_bias_ManyFirst, num_pp_samples=200);

### Model when Morph was first

filtro = d_without_outliers["order"].isin(["morphFirst"])
d_without_outliers_MorphFirst = d_without_outliers[filtro]
d_without_outliers_MorphFirst.shape

## normal-normal model for dot-morphFirst

# likelihood (filter by morph to have only one estimate of bias per participant)
filtro = d_without_outliers_MorphFirst["type"].isin(["morph"])
bias_MorphFirst = d_without_outliers_MorphFirst.loc[filtro, "bias"].to_numpy()
print(bias_MorphFirst)

# run the model
with pm.Model() as model_bias_MorphFirst:
    # prior
    μ = pm.Normal('μ', 0, 10)
    σ = pm.HalfNormal("σ", sigma=1)

    # el likelihood
    y = pm.Normal("y", mu=μ, sigma=σ, observed=bias_MorphFirst)
    idata_bias_MorphFirst = pm.sample(chains=4)
    
# Chain diagnostics    
az.summary(idata_bias_MorphFirst, kind="diagnostics")
az.plot_trace(idata_bias_MorphFirst)
az.plot_rank(idata_bias_MorphFirst)

# Plot 94% hdi
az.plot_posterior(idata_bias_MorphFirst);

# posterior predictive
ppc_bias_MorphFirst = pm.sample_posterior_predictive(idata_bias_MorphFirst, model=model_bias_MorphFirst)
az.plot_ppc(ppc_bias_MorphFirst, num_pp_samples=200);




