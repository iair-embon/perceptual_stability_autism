#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Sep 20 10:25:45 2023

@author: iair
"""
import arviz as az
import pymc as pm
import preliz as pz
import scipy.stats as stats
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.signal import convolve

################ model for many when Many is first

# number of points distribution perceived by the participants
mean_normal = 52 # mean centered on the true value of dots.
sd_normal = 20 # sd of the number of points distribution perceived by the participants.

# Normal distribution
data_normal = np.random.normal(mean_normal, sd_normal, 10000)

### Model for T = 1

# 100 participant's latent responses
participant_responses = np.random.choice(data_normal, 100)

# run model
with pm.Model() as model_many_ManyFirst_T1:
    # Prior
    μ = pm.Uniform('μ', 0, 100)
    σ = pm.HalfNormal("σ", sigma=30)

    # Likelihood
    y = pm.Normal("y", mu=μ, sigma=σ, observed=participant_responses)

    # posterior samples
    trace_t1 = pm.sample(chains=4)

# Plot 94% hdi for t1
az.plot_posterior(trace_t1);

pm.model_to_graphviz(model_many_ManyFirst_T1)


### Model for T = 2

# New 100 participant's latent responses for T = 2
participant_responses_t2 = np.random.choice(data_normal, 100)

# Mean and std of posterior from T = 1
mean_posterior_t1 = np.mean(trace_t1.posterior['μ'].values)
std_posterior_t1 = np.std(trace_t1.posterior['μ'].values)

# run model 
with pm.Model() as model_many_ManyFirst_T2:
    # Prior using mean and std of posterior from T = 1
    μ_t2 = pm.Normal('μ_t2', mu=mean_posterior_t1, sigma=std_posterior_t1)
    σ_t2 = pm.HalfNormal("σ_t2", sigma=30)  

    # Likelihood with new 100 latent participant's responses
    y_t2 = pm.Normal("y_t2", mu=μ_t2, sigma=σ_t2, observed=participant_responses_t2)

    # sampling from posterior
    trace_t2 = pm.sample(chains=4)

# Now trace_t2 has updated the posterior to T = 2

# Plot 94% hdi for t2
az.plot_posterior(trace_t2);

pm.model_to_graphviz(model_many_ManyFirst_T2)


### Model for T = 3

# New 100 participant's latent responses for T = 3
participant_responses_t3 = np.random.choice(data_normal, 100)

# Mean and std of posterior from T = 2
mean_posterior_t2 = np.mean(trace_t2.posterior['μ_t2'].values)
std_posterior_t2 = np.std(trace_t2.posterior['μ_t2'].values)

# run model 
with pm.Model() as model_many_ManyFirst_T3:
    # Prior using mean and std of posterior from T = 2
    μ_t3 = pm.Normal('μ_t3', mu=mean_posterior_t2, sigma=std_posterior_t2)
    σ_t3 = pm.HalfNormal("σ_t3", sigma=30)

    # Likelihood with new 100 latent participant's responses
    y_t3 = pm.Normal("y_t3", mu=μ_t3, sigma=σ_t3, observed=participant_responses_t3)

    # sampling from posterior
    trace_t3 = pm.sample(chains=4)

# Now trace_t3 has updated the posterior to T = 3

# Plot 94% hdi for t3
az.plot_posterior(trace_t3);

pm.model_to_graphviz(model_many_ManyFirst_T3)


### Model for T = 4

# New 100 participant's REAL responses for T = 4 
participant_responses_t4 = np.random.choice(data_normal, 100)

# Mean and std of posterior from T = 3
mean_posterior_t3 = np.mean(trace_t3.posterior['μ_t3'].values)
std_posterior_t3 = np.std(trace_t3.posterior['μ_t3'].values)

# run model 
with pm.Model() as model_many_ManyFirst_T4:
    # Prior using mean and std of posterior from T = 3
    μ_t4 = pm.Normal('μ_t4', mu=mean_posterior_t3, sigma=std_posterior_t3)
    σ_t4 = pm.HalfNormal("σ_t4", sigma=30)

    # Likelihood with new 100 REAL participant's responses
    y_t4 = pm.Normal("y_t4", mu=μ_t4, sigma=σ_t4, observed=participant_responses_t4)

    # sampling from posterior
    trace_t4 = pm.sample(chains=4)

# Now trace_t4 has updated the posterior to T = 4

# Plot 94% hdi for t4
az.plot_posterior(trace_t4);

pm.model_to_graphviz(model_many_ManyFirst_T4)


######### Is the multiplication of prior and likelihood implicitly performed in this way?
######### ¿Add motor noise? in T = 4
######### the sigma is ok? both in prior, likelihood, and the real destribution


