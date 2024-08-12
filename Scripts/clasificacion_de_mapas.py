import pandas as pd
import numpy as np
from scipy.cluster.hierarchy import dendrogram, linkage
from scipy.spatial.distance import pdist
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.preprocessing import OneHotEncoder


datos = pd.read_csv('/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Datos/datos_con_metricas_v1.csv')
datos_socio = pd.read_csv('/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Datos/datos_socio.csv')
datos_cod = pd.read_csv('/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Datos/datos_socioeconomicos_codificados.csv')
metricas = ['nodos', 'enlaces', 'receptores', 'transmisores', 'ordinarios', 'prop_pos_neg','grado', 'intermediacion', 'densidad', 'complejidad', 'jerarquia']

Z = linkage(datos[metricas], 'single')
dendrogram_info = dendrogram(Z, no_plot=True)
order = dendrogram_info['leaves']

plt.figure(figsize=(6, 6))
dendrogram(Z, orientation='left')
#plt.title('Dendrograma del agrupamiento jerárquico')
plt.ylabel('Índice del mapa cognitivo')
plt.xlabel('Distancia')
plt.savefig("test.svg")
plt.show()




# Ordenar la base de datos socioeconómicos de acuerdo al agrupamiento
dendrogram_info = dendrogram(Z, no_plot=True)
order = dendrogram_info['leaves']
datos_socio_ordenados = datos_socio.set_index(pd.Index(order))
colormaps = ['coolwarm', 'viridis', 'cividis', 'inferno']
plt.figure(figsize=(10, 8))
for i, col in enumerate(datos_cod.columns):
    sns.heatmap(datos_cod[[col]], cmap=colormaps[i % len(colormaps)], cbar=(i == 0), annot=True, fmt=".2f")
    plt.title('Heatmap con mapas de color diferentes para cada columna')
    plt.xlabel('Columna')
    plt.ylabel('Fila')
    plt.show()


.... no me saleeee