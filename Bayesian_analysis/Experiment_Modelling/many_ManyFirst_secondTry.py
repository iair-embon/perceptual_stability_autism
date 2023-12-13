# -*- coding: utf-8 -*-
"""
Created on Mon Oct  2 20:16:05 2023

@author: marcosembon
"""
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm, halfnorm

# Configuración de los parámetros verdaderos y la varianza conocida
true_mean = 52
true_stddev = 20

# Número de datos observados en cada iteración
num_data_points = 1

# Número de iteraciones
num_iterations = 5

# Valores para el eje x
x = np.linspace(0, 100, 1000)  # Rango para graficar las distribuciones

# Almacenar los resultados de la posterior para cada iteración
posterior_means = np.zeros(num_iterations)
posterior_stddevs = np.zeros(num_iterations)

# Inicialización del prior con información previa
prior_mean = 52

# Ciclo para cada iteración
for i in range(num_iterations):
    # Generar datos observados
    data = np.random.normal(true_mean, true_stddev, num_data_points)
    
    # Generar el prior de la distribución half-normal
    prior_stddev_distribution = halfnorm(scale=10)
    
    # Obtener una muestra de la distribución half-normal para prior_stddev
    prior_stddev = prior_stddev_distribution.rvs()
    
    # Calcular la distribución prior y posterior usando scipy.stats.norm
    prior_distribution = norm(prior_mean, prior_stddev)
    likelihood_distribution = norm(np.mean(data), true_stddev)
    
    # Actualizar el prior a posterior usando el teorema de Bayes
    posterior_stddev = ((prior_stddev**2) * (true_stddev**2)) / ((np.sum(data) * (prior_stddev**2)) + (true_stddev**2))
    posterior_mean_first_term = prior_mean * ((true_stddev**2)/((np.sum(data) * (prior_stddev**2)) + (true_stddev**2))) 
    posterior_mean_second_term = np.mean(data) * ((np.sum(data) * (prior_stddev**2))/ ((np.sum(data) * (prior_stddev**2))+ (true_stddev**2)))
    posterior_mean = posterior_mean_first_term + posterior_mean_second_term
    
    # Almacenar resultados
    posterior_means[i] = posterior_mean
    posterior_stddevs[i] = posterior_stddev
    
    # El posterior actual se convierte en el prior para la siguiente iteración
    prior_mean = posterior_mean
    
    # Crear subplots
    plt.figure(figsize=(12, 12))

    # Plot Prior
    plt.subplot(311)
    plt.plot(x, prior_distribution.pdf(x), label='Prior', color='blue')
    plt.title(f'Iteración {i + 1}')
    plt.xlabel('Valor')
    plt.ylabel('Densidad de Probabilidad')
    plt.legend()

    # Plot Likelihood
    plt.subplot(312)
    plt.plot(x, likelihood_distribution.pdf(x), label='Likelihood', color='green')
    plt.xlabel('Valor')
    plt.ylabel('Densidad de Probabilidad')
    plt.legend()

    # Plot Posterior
    posterior_distribution = norm(posterior_mean, posterior_stddev)
    plt.subplot(313)
    plt.plot(x, posterior_distribution.pdf(x), label='Posterior', color='red')
    plt.xlabel('Valor')
    plt.ylabel('Densidad de Probabilidad')
    plt.legend()

    plt.tight_layout()
    plt.show()
    
    
# Graficar los resultados
plt.figure(figsize=(12, 6))
plt.subplot(211)
plt.plot(range(1, num_iterations + 1), posterior_means)
plt.title('Posterior Media vs. Iteraciones')
plt.xlabel('Iteración')
plt.ylabel('Posterior Media')
plt.grid()

plt.subplot(212)
plt.plot(range(1, num_iterations + 1), posterior_stddevs)
plt.title('Posterior Desviación Estándar vs. Iteraciones')
plt.xlabel('Iteración')
plt.ylabel('Posterior Desviación Estándar')
plt.grid()

plt.tight_layout()
plt.show()



