# Es recomendable activar ambiente virtual ´fcm_env' de conda para resolver todas las dependencias del paquete ´fcmpy´
from fcmpy import ExpertFcm, FcmSimulator, FcmIntervention
from fcmpy.simulator.transfer import Sigmoid, Bivalent, Trivalent, HyperbolicTangent
import seaborn as sns
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib.widgets import Button
import os
import csv
# RED 10 NODOS
tabla_enlaces = pd.read_csv("/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Análisis/cytoscape_files/red10_enlaces.csv")
tabla_nodos = pd.read_csv("/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Análisis/cytoscape_files/red10_nodos.csv")
# RED 15 NODOS
tabla_enlaces = pd.read_csv("/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Análisis/cytoscape_files/red15_enlaces.csv")
tabla_nodos = pd.read_csv("/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Análisis/cytoscape_files/red15_nodos.csv")
# RED COMPLETA
tabla_enlaces = pd.read_csv("/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Análisis/cytoscape_files/red_completa_enlaces.csv")
tabla_nodos = pd.read_csv("/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Análisis/cytoscape_files/red_completa_nodos.csv")
# TABLA DE ENLACES A MATRIZ DE ADYACENCIA
matriz_ady = np.zeros((len(tabla_nodos), len(tabla_nodos)))
enlaces_juntos = list(tabla_enlaces['name'])
for i in range(0, len(tabla_enlaces['name'])):
    emisor, receptor = enlaces_juntos[i].split(' (-) ')
    peso = tabla_enlaces['peso'][i]
    pos_x = list(tabla_nodos['name']).index(emisor)
    pos_y = list(tabla_nodos['name']).index(receptor)
    matriz_ady[pos_x, pos_y] = round(peso, 1)

weight_matrix = pd.DataFrame(matriz_ady, columns=tabla_nodos['name'])
init_state = dict()
for concepto in tabla_nodos['name']:
    init_state[concepto] = 0.5

#""" usar en caso de que se quiera crear visualmente un vector inicial
"""init_state = dict()
archivo = "/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Análisis/cytoscape_files/vector_inicial_editable.csv"
with open(archivo, 'r') as csv_file:
    csv_reader = csv.reader(csv_file)
    next(csv_reader)
    for row in csv_reader:
        clave = row[0]
        valor = row[1]
        init_state[clave] = valor
#"""
# SIMULACIÓN
sim = FcmSimulator()
res_mK = sim.simulate(initial_state=init_state, weight_matrix=weight_matrix, transfer='sigmoid', inference='mKosko', thresh=0.001, iterations=50, l=1)
# INTERVENCIÓN
inter = FcmIntervention(FcmSimulator)
inter.initialize(initial_state=init_state, weight_matrix=weight_matrix, transfer='sigmoid', inference='mKosko', thresh=0.001, iterations=50, l=1)
inter.add_intervention('intervention_1', impact={'quemas':-.3, 'selva' : .5}, effectiveness=1)
inter.test_intervention('intervention_1')
inter.test_results['intervention_1']
# GRAFICAR SIMULACIÓN LÍNEAS ANIMADA
colores = plt.cm.tab20(np.linspace(0, 1, 15))
fig, ax = plt.subplots(figsize=(8,6))
def init():
    return []

def update(frame):
    ax.clear()
    for i, columna in enumerate(res_mK.columns):
        ax.plot(res_mK.index[:frame+1], res_mK[columna][:frame+1], label=columna, color=colores[i])
    ax.set_xlabel('Simulación')
    ax.set_ylabel('Valores')
    ax.legend()
    ax.grid(True)

ani_sim = animation.FuncAnimation(fig, update, frames=len(res_mK), interval = 2000)

# Agregar botones
axplay = plt.axes([0.7, 0.01, 0.1, 0.05])
axpause = plt.axes([0.81, 0.01, 0.1, 0.05])
axprev = plt.axes([0.59, 0.01, 0.1, 0.05])
axnext = plt.axes([0.48, 0.01, 0.1, 0.05])

bplay = Button(axplay, 'Play')
bplay.on_clicked(lambda event: ani_sim.event_source.start())

bpause = Button(axpause, 'Pause')
bpause.on_clicked(lambda event: ani_sim.event_source.stop())

bprev = Button(axprev, 'Previous')
bprev.on_clicked(lambda event: ani_sim.frame_seq == max(0, ani_sim.frame_seq - 1))

bnext = Button(axnext, 'Next')
bnext.on_clicked(lambda event: ani_sim.frame_seq == min(ani_sim.frame_seq + 1, len(res_mK) - 1))

plt.show()

# GRAFICAR SIMULACIÓN LÍNEAS NORMAL
colores = plt.cm.tab20(np.linspace(0, 1, 15))
plt.figure(figsize=(8, 6))
for i, columna in enumerate(res_mK.columns):
    plt.plot(res_mK.index, res_mK[columna], label=columna, color=colores[i])

plt.xlabel('Simulación')
plt.ylabel('Valores')
plt.legend()
plt.grid(True)
#plt.savefig("simulacion_fcm.svg")
plt.show()

# GRAFICAR SIMULACIÓN DIFERENCIAS ANIMADO
diferencia_valores = res_mK.iloc[-1] - res_mK.iloc[0]
diferencia_valores = diferencia_valores.sort_values(ascending = False)
fig, ax = plt.subplots(figsize=(5, 5))
bars = ax.bar(diferencia_valores.index, diferencia_valores, color='gray')
def init():
    return bars

def update(frame):
    # Modificar la altura de las barras en cada cuadro
    for bar, height in zip(bars, diferencia_valores):
        bar.set_height(height * frame / len(res_mK))
    return bars

ani_dif = animation.FuncAnimation(fig, update, frames=len(res_mK), init_func=init, interval = 1000)

plt.xlabel('Conceptos')
plt.ylabel('Diferencia')
plt.xticks(rotation=45, ha='right')
plt.grid(axis='y')
plt.show()

# GRAFICAR SIMULACIÓN DIFERENCIAS

diferencia_valores = res_mK.iloc[-1] - res_mK.iloc[0]
diferencia_valores = diferencia_valores.sort_values(ascending = False)
plt.figure(figsize=(16, 8))
plt.bar(diferencia_valores.index, diferencia_valores, width = 0.5, color='gray')

plt.xlabel('Conceptos')
plt.ylabel('Diferencia')
plt.xticks(rotation=45, ha='right')  # Rotar las etiquetas del eje x para mayor legibilidad
plt.grid(axis='y')
plt.savefig("simulacion_fcm_diferencias.svg")
plt.show()

def update(frame):
    # for each frame, update the data stored on each artist.
    x = t[:frame]
    y = z[:frame]
    # update the scatter plot:
    data = np.stack([x, y]).T
    scat.set_offsets(data)
    # update the line plot:
    line2.set_xdata(t[:frame])
    line2.set_ydata(z2[:frame])
    return (scat, line2)



# Aquí se crean datos ficticios para la demostración
np.random.seed(0)
res_mK = pd.DataFrame(np.random.rand(10, 5), columns=[f'Columna_{i}' for i in range(5)])

# Colores para las líneas
colores = plt.cm.tab10(np.linspace(0, 1, 10))

# Crear la figura y el eje
fig, ax = plt.subplots(figsize=(8, 6))

# Función de inicialización (vacía en este caso)
def init():
    return []

# Función de actualización de la animación
def update(frame):
    ax.clear()
    for i, columna in enumerate(res_mK.columns):
        ax.plot(res_mK.index[:frame+1], res_mK[columna][:frame+1], label=columna, color=colores[i])
    ax.set_xlabel('Simulación')
    ax.set_ylabel('Valores')
    ax.legend()
    ax.grid(True)

# Crear la animación
ani = animation.FuncAnimation(fig, update, frames=len(res_mK), init_func=init, blit=False, interval=500)

plt.show()
