#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 26 15:33:59 2024

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
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score
from sklearn.utils import resample


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

# calculate bias_yuval 2: few/morph
df_pivot = df_filtered_2.pivot(index='participants', columns='type', values='estimation')
df_pivot['bias_yuval_2'] = (df_pivot['morph'] - df_pivot['few']) / df_pivot['few']
df_pivot.reset_index(inplace=True)

# Fusionar solo la columna 'bias' con df_filtered_2
df_filtered_2 = df_filtered_2.merge(df_pivot[['participants', 'bias_yuval_2']], on='participants')

## Combine both df
df_NotExperimentData_filtered = df_NotExperimentData[df_NotExperimentData['participants'].isin(df_filtered_2['participants'])]
df_filtered_2_selectedColumns = df_filtered_2[['participants', 'bias_yuval_2']]
df_filtered_2_unique = df_filtered_2_selectedColumns.drop_duplicates(subset=['participants'])
df_merged = df_NotExperimentData_filtered.merge(df_filtered_2_unique, on = 'participants')

## exclude some participants that doesnt complete the AQ very well.
participants_to_exclude_3 = list(range(50,59))
df_merged_filtered = df_merged[~df_merged['participants'].isin(participants_to_exclude_3)]


# Model: yi = α + βaq x AQ_scaledi + βage x Age_scaledi + βsex x sex_codei + errori

#Where:  y = bias_yuval_2 = (morph - few) / few 
#        AQ_scaled = AQ - mean(AQ) 
#        Age_scaled = age - mean(age) 
#        sex = {0 = Male ; 1 = Female}

## fake data
fake_df_NotExperimentData = pd.read_csv("/home/iair/Documents/Investigación/perceptual_stability_autism/Big_pilot_4(dot_reference_first)/fake_df_NotExperimentData.csv")


# Definir función para calcular intervalos de confianza bootstrap
def bootstrap_confidence_interval(data, alpha=0.05, n_bootstrap=1000):
    slopes = []
    
    for _ in range(n_bootstrap):
        bootstrap_sample = resample(data)
        X_boot = bootstrap_sample[['AQ_scaled', 'age_scaled', 'sex_code']]
        y_boot = bootstrap_sample['bias_yuval_2']
        model = LinearRegression().fit(X_boot, y_boot)
        slopes.append(model.coef_[0])
        
    slopes.sort()
    lower_idx = int(n_bootstrap * alpha / 2)
    upper_idx = int(n_bootstrap * (1 - alpha / 2))
    return slopes[lower_idx], slopes[upper_idx]


N = 100  # Número de repeticiones

all_results_list = []  # Lista para almacenar los resultados de todas las repeticiones

for _ in range(N):
    # merge 44 random rows from the fake data
    random_rows = fake_df_NotExperimentData.sample(n=len(df_merged_filtered), replace=True)
    columns_to_replace_fake = ['AQ', 'AQ_social', 'AQ_attentional_switches', 'AQ_attention_detail', 'AQ_communication', 'AQ_imagination']
    columns_to_replace_original = ['AQ', 'AQ_social', 'AQ_attentional_switches', 'AQ_attencion_detail', 'AQ_communication', 'AQ_imagination']
    df_merged_filtered.loc[:, columns_to_replace_original] = random_rows.loc[:, columns_to_replace_fake].values

    # morph: 0 ; few: 1
    df_merged_filtered['age_scaled'] = df_merged_filtered['age'] - df_merged_filtered['age'].mean()
    df_merged_filtered['sex_code'] = df_merged_filtered['sex'].replace(['Male'], 0)
    df_merged_filtered['sex_code'] = df_merged_filtered['sex_code'].replace(['Female'], 1)
    df_merged_filtered['AQ_scaled'] = df_merged_filtered['AQ'] - df_merged_filtered['AQ'].mean()
    df_merged_filtered['AQ_social_scaled'] = df_merged_filtered['AQ_social'] - df_merged_filtered['AQ_social'].mean()
    df_merged_filtered['AQ_attentional_switches_scaled'] = df_merged_filtered['AQ_attentional_switches'] - df_merged_filtered['AQ_attentional_switches'].mean()
    df_merged_filtered['AQ_attencion_detail_scaled'] = df_merged_filtered['AQ_attencion_detail'] - df_merged_filtered['AQ_attencion_detail'].mean()
    df_merged_filtered['AQ_communication_scaled'] = df_merged_filtered['AQ_communication'] - df_merged_filtered['AQ_communication'].mean()
    df_merged_filtered['AQ_imagination_scaled'] = df_merged_filtered['AQ_imagination'] - df_merged_filtered['AQ_imagination'].mean()

    results_list = []  # Lista para almacenar los resultados de esta repetición

    for aq_col in columns_to_replace_original:
        # Realizar la regresión lineal para la columna actual de AQ
        X = df_merged_filtered[[aq_col, 'age_scaled', 'sex_code']]
        y = df_merged_filtered['bias_yuval_2']

        regression_model = LinearRegression()
        regression_model.fit(X, y)

        # Calcular la pendiente (slope) y el coeficiente de determinación (R^2)
        slope_AQ = regression_model.coef_[0]
        r2 = r2_score(y, regression_model.predict(X))

        # Calcular los intervalos de confianza para la pendiente (slope)
        lower_ci_slope, upper_ci_slope = bootstrap_confidence_interval(df_merged_filtered)

        # Verificar si los intervalos de confianza contienen el 0
        ci_contains_zero = (lower_ci_slope <= 0) and (upper_ci_slope >= 0)
            
        # Agregar los resultados a la lista
        results_list.append({
            'AQ_type': aq_col,
            'AQ_slope': slope_AQ,
            'R2': r2,
            'lower_ci_slope': lower_ci_slope,
            'upper_ci_slope': upper_ci_slope,
            'ci_contains_zero': ci_contains_zero
        })

    # Agregar los resultados de esta repetición a la lista de resultados de todas las repeticiones
    all_results_list.append(results_list)


from itertools import chain

# Aplanar la lista
flattened_results_list = list(chain.from_iterable(all_results_list))

# Convertir la lista a DataFrame
all_results_df = pd.DataFrame(flattened_results_list)

all_results_df_AQ = all_results_df[all_results_df['AQ_type'] == 'AQ']
all_results_df_AQ_social = all_results_df[all_results_df['AQ_type'] == 'AQ_social']
all_results_df_AQ_attentional_switches = all_results_df[all_results_df['AQ_type'] == 'AQ_attentional_switches']
all_results_df_AQ_attencion_detail = all_results_df[all_results_df['AQ_type'] == 'AQ_attencion_detail']
all_results_df_AQ_communication = all_results_df[all_results_df['AQ_type'] == 'AQ_communication']
all_results_df_AQ_imagination = all_results_df[all_results_df['AQ_type'] == 'AQ_imagination']

all_results_df_AQ.AQ_slope.hist()
all_results_df_AQ_social.AQ_slope.hist()
all_results_df_AQ_attentional_switches.AQ_slope.hist()
all_results_df_AQ_attencion_detail.AQ_slope.hist()
all_results_df_AQ_communication.AQ_slope.hist()
all_results_df_AQ_imagination.AQ_slope.hist()

all_results_df_AQ.AQ_slope.mean()
all_results_df_AQ_social.AQ_slope.mean()
all_results_df_AQ_attentional_switches.AQ_slope.mean()
all_results_df_AQ_attencion_detail.AQ_slope.mean()
all_results_df_AQ_communication.AQ_slope.mean()
all_results_df_AQ_imagination.AQ_slope.mean()


all_results_df.to_csv('/home/iair/Documents/Investigación/perceptual_stability_autism/Big_pilot_4(dot_reference_first)/all_results_df.csv',index = False)

