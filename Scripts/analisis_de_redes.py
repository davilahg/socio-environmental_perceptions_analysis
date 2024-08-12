# Los paquetes sin conflictos de dependencias están en el ambiente virtual `percepciones` de conda
import os
import numpy as np
import pandas as pd
import networkx as nx
import seaborn as sns
from networkx.algorithms import bipartite
from networkx.algorithms import community
import matplotlib.pyplot as plt
from matplotlib.colors import Normalize
from matplotlib.cm import ScalarMappable
import collections
import math
import random
import matplotlib.pyplot as plt
from scipy.stats import shapiro
from cdlib import algorithms, viz
from IPython.display import display, HTML
import warnings
import graphviz
import time
#import pygraphviz
#import matplotlib
#matplotlib.use('TkAgg')

#### FUNCIONES

def which(condition, iterable):
    return [index for index, item in enumerate(iterable) if condition(item)]

def construir_mapa(lista_de_enlaces):
  #lista_de_enlaces = pd.read_csv(mapa)
  positivos = [which(lambda x: x > 0, lista_de_enlaces.iloc[:,2])] # sustituir valores difusos por -1 y 1 según sean negativos o positivos
  lista_de_enlaces.iloc[positivos,2] = 1
  negativos = [which(lambda x: x < 0, lista_de_enlaces.iloc[:,2])]
  lista_de_enlaces.iloc[negativos,2] = -1
  indices_emisores = lista_de_enlaces.iloc[:,0].index.values
  indices_receptores = lista_de_enlaces.iloc[:,1].index.values
  nodos_emisores = lista_de_enlaces.iloc[:,0].values
  nodos_receptores = lista_de_enlaces.iloc[:,1].values # sustituir índices de los términos por los términos en la base de datos de conceptos
  for i in nodos_emisores:
      indice = [which(lambda x: x == lista_de_terminos[i], lista_de_terminos[nodos_emisores])] # obtener el índice en lista_de_enlaces del nodo que tiene el valor i en lista_de_terminos
      lista_de_enlaces.iloc[indice,0] = lista_de_terminos[i-1] # aquí va la fila que corresponde a i, no la fila
  for j in nodos_receptores:
      indice = [which(lambda x: x == lista_de_terminos[j], lista_de_terminos[nodos_receptores])]
      lista_de_enlaces.iloc[indice,1] = lista_de_terminos[j-1]
  lista_de_enlaces.columns = ['nodo_emisor', 'nodo_receptor', 'peso']
  G = nx.from_pandas_edgelist(lista_de_enlaces, 'nodo_emisor', 'nodo_receptor', edge_attr = 'peso', create_using=nx.MultiDiGraph())
  return G, lista_de_enlaces

def enlaces_duplicados(red):
  enlaces_totales = list(red.edges())
  duplicados = []
  for enlace in enlaces_totales:
    if enlaces_totales.count(enlace) > 1:
      duplicados.append(enlace)
  enlaces_duplicados = dict(collections.Counter(duplicados))
  if enlaces_duplicados == {}:
    print ('No hay enlaces duplicados')
  else:
    return enlaces_duplicados

def graficar(red, pos, colormap = '#c9c6bd', Save = False, Show = True):
  color_map = colormap
  pos = pos
  node_count = {}
  for node in lista_total_nodos:
    node_count[node] = lista_total_nodos.count(node) # lista_total_nodos se define desde el conjunto de listas de enlaces
  link_count = {}
  for edge in red.edges():
    link_count[edge] = red.number_of_edges(edge[0], edge[1])
  max_link_count = max(link_count.values())
  alpha_values = [link_count[edge] / max_link_count for edge in red.edges()]
  max_node_count = max(node_count.values())
  node_alpha_values = [node_count[node] / max_node_count for node in red.nodes()]
  enlaces = red.edges()
  enlaces_l = list(red.edges(data = 'peso'))
  pesos = [enlace[-1] for enlace in enlaces_l]
  color_enlaces = ['green' if peso > 0 else 'red' for peso in pesos]
  node_sizes = [node_count[node]*10 for node in red.nodes()]
  edge_width = [link_count[edge]/max_link_count*2 for edge in red.edges()]
  #plt.figure(figsize = (10,10), dpi = 600)
  nx.draw_networkx_nodes(red, pos, node_size = node_sizes, node_color=color_map, alpha = 0.8)
  nx.draw_networkx_labels(red, pos, alpha = 0.7, font_size = 8)
  nx.draw_networkx_edges(red, pos, edgelist = enlaces, edge_color = color_enlaces, width = edge_width, alpha = 0.2)
  plt.gca().set_aspect('equal')
  plt.axis('off')
  if Save == True:
    plt.savefig('red_completa.pdf', bbox_inches='tight')
  if Show == True:
    plt.show()
  plt.clf()

def graficar_sub(red_total, red, colormap = '#c9c6bd', metrica = 'repeticiones', estilo = 'grosor', NodeAlpha = 0.8, Show = True, Save = False):
  color_map = colormap
  pos = nx.spring_layout(red_total, k = 2, scale = 3.5, seed = 3)
  node_count = {}
  for node in lista_total_nodos:
    node_count[node] = lista_total_nodos.count(node) # lista_total_nodos se define desde el conjunto de listas de enlaces
  link_count = {}
  for edge in red.edges():
    link_count[edge] = red.number_of_edges(edge[0], edge[1])
  max_link_count = max(link_count.values())
  alpha_values = [link_count[edge] / max_link_count for edge in red.edges()]
  max_node_count = max(node_count.values())
  node_alpha_values = [node_count[node] / max_node_count for node in red.nodes()]
  enlaces = red.edges()
  enlaces_l = list(red.edges(data = 'peso'))
  pesos = [enlace[-1] for enlace in enlaces_l]
  color_enlaces = ['green' if peso > 0 else 'red' for peso in pesos]
  plt.figure(figsize = (10,10), dpi = 600)
  if metrica == 'repeticiones':
    node_sizes = [node_count[node]*10 for node in red.nodes()]
    edge_width = [link_count[edge]/max_link_count*2 for edge in red.edges()]
    if estilo == 'grosor':
      nx.draw_networkx_nodes(red, pos, node_size = node_sizes, node_color=color_map, alpha = NodeAlpha)
      nx.draw_networkx_labels(red, pos, alpha = 0.7, font_size = 8)
      nx.draw_networkx_edges(red, pos, edgelist = enlaces, edge_color = color_enlaces, width = edge_width, alpha = 0.2)
    elif estilo == 'alpha':
      nx.draw_networkx_nodes(red, pos, node_size = 200, node_color=color_map, alpha = node_alpha_values)
      nx.draw_networkx_labels(red, pos, alpha = 0.7, font_size = 8)
      nx.draw_networkx_edges(red, pos, edgelist = enlaces, edge_color = color_enlaces, width = 1, alpha = alpha_values)
  elif metrica == 'centralidad':
    centrality = nx.degree_centrality(red_total)
    node_sizes = [centrality[node] * 1000 for node in red.nodes()]
    nx.draw_networkx_nodes(red, pos, node_size = node_sizes, node_color=color_map, alpha = node_alpha_values)
    nx.draw_networkx_labels(red, pos, alpha = 0.7, font_size = 8)
    nx.draw_networkx_edges(red, pos, edgelist = enlaces, edge_color = color_enlaces, width = 1, alpha = alpha_values)
  plt.gca().set_aspect('equal')
  plt.axis('off')
  if Save == True:
    plt.savefig(f'subred.pdf', bbox_inches='tight')
  if Show == True:
    plt.show()
  plt.clf()

def red_init():
  T = nx.MultiDiGraph()
  for r in redes:
    T.add_nodes_from(r.nodes(data = True))
    T.add_edges_from(r.edges(data = True))
  return T

def ponderar_red(G):
  # CONTAR ENLACES
  link_count = {}
  for edge in G.edges():
    link_count[edge] = G.number_of_edges(edge[0], edge[1])
  # CAMBIAR MÚLTIPLES ENLACES POR ENLACES PONDERADOS
  G_ponderada = nx.DiGraph()
  G_pond_pesos_acumulados = {}
  for u, v, data in G.edges(data=True):
      peso_actual = data['peso']
      key = (u, v)
      if key in G_pond_pesos_acumulados:
        G_pond_pesos_acumulados[key].append(peso_actual)
      else:
        G_pond_pesos_acumulados[key] = [peso_actual]
      #if G_ponderada.has_edge(u, v):
      #    G_ponderada[u][v]['peso'] = (G_ponderada[u][v]['peso'] + peso) / 2
      #else:
      #    G_ponderada.add_edge(u, v, peso=peso)
  for (u, v), pesos in G_pond_pesos_acumulados.items():
    peso_promedio = sum(pesos) / len(pesos)
    G_ponderada.add_edge(u, v, peso=peso_promedio)
  return G_ponderada

def indice_jerarquia(red):
  N = len(red.nodes())
  izquierda = 12/((N-1)*(N)*(N+1))
  lista_od = []
  for node in red.nodes():
    od_node = red.out_degree(node)
    lista_od.append(od_node)
  suma_od = sum(lista_od)
  diferencias = []
  for node in red.nodes():
    diferencia = ((red.out_degree(node) - suma_od)/N)**2
    diferencias.append(diferencia)
  derecha = sum(diferencias)
  indice_de_jerarquia = izquierda*derecha
  return indice_de_jerarquia

def efectos_categoria(red, nodos_a_analizar):
  info_enlaces = {}
  pesos = {}
  num_enlaces = {}
  for nodo in nodos_a_analizar:
    if nodo in red.nodes(): # si el nodo está en la red
      vecinos = list(red.neighbors(nodo)) # nodos con los que tiene enlaces nodo
      if len(vecinos) > 0: # si tiene al menos un enlace
        enlaces_del_nodo = [(nodo, vecino) for vecino in vecinos] # lista de enlaces del nodo
        lista_promedios_nodo = [] # una lista con el valor promedio de los enlaces entre A y X - puede ser más de un valor por nodo de la red !! 
        lista_repeticiones_nodo = [] # lista para almacenar el número de veces que se repite el nodo A - X
        for enlace in enlaces_del_nodo: # para cada enlace A - B (par de nodos conectados)
          info_enlace = red.get_edge_data(enlace[0],enlace[1]) # {repeticion: {'peso': valor}}
          pesos_enlace = [] # lista vacía para almacenar todos los pesos de un enlace repetido varias veces - pueden ser varios valores por par de nodos !! Se repite a nivel de red   
          for i in info_enlace.values(): # para cada repetición del enlace A - B
            pesos_enlace.append(i['peso']) # agregar todos los pesos a la lista pesos_enlace
          peso_promedio_enlace = np.mean(pesos_enlace)*len(pesos_enlace) # promedio de todos los pesos repetidos de un par de nodos - un valor por par de nodos A - B
          lista_promedios_nodo.append(peso_promedio_enlace) # agregar todos los valores promedio entre A y X
          lista_repeticiones_nodo.append(len(pesos_enlace)) # agregar todas las n del promedio entre A y X
        promedio_peso_nodo = sum(lista_promedios_nodo)/sum(lista_repeticiones_nodo) # promedio de pesos de todos los enlaces de un nodo - un valor por nodo de la red
        info_enlaces[nodo] = promedio_peso_nodo, sum(lista_repeticiones_nodo)
        pesos[nodo] = promedio_peso_nodo
        num_enlaces[nodo] = sum(lista_repeticiones_nodo)
        pesos[nodo] = promedio_peso_nodo
  return info_enlaces, pesos, num_enlaces

def resumir(red):
  resumen = dict()
  red_ponderada = ponderar_red(red)
  N = len(red_ponderada.nodes())
  transmisores = [node for node in red_ponderada.nodes() if not any(red_ponderada.predecessors(node))]
  receptores = [node for node in red_ponderada.nodes() if not any(red_ponderada.successors(node))]
  neutrales = []
  for node in red_ponderada.nodes():
    if red_ponderada.in_degree(node) > 0:
      if red_ponderada.out_degree(node) > 0:
        neutrales.append(node)
  lista_grados_centralidad = [i for i in red_ponderada.degree()]
  grados = [lista_grados_centralidad[i][1] for i in range(N)]
  lista_grados_intermediacion = nx.betweenness_centrality(red_ponderada)
  enlaces = list(red.edges(data = 'peso'))
  positivos = []
  negativos = []
  for i in range(len(enlaces)):
    if enlaces[i][2] > 0:
      positivos.append(enlaces[i])
    elif enlaces[i][2] < 0:
      negativos.append(enlaces[i])
  densidad = len(red_ponderada.edges())/N**2
  resumen = {
    'número de nodos        ' : len(red.nodes()),
    'número de enlaces      ' : len(red_ponderada.edges()),
    'porc. de transmisores  ' : round(len(transmisores)/len(red_ponderada.nodes()), 3),
    'porc. de receptores    ' : round(len(receptores)/len(red_ponderada.nodes()), 3),
    'porc. de ordinarios    ' : round(len(neutrales)/len(red_ponderada.nodes()), 3),
    'porc. enlaces positivos' : round(len(positivos)/len(red.edges()), 3),
    'porc. enlaces negativos' : round(len(negativos)/len(red.edges()), 3),
    'prop. pos-neg          ' : round(len(positivos)/len(negativos), 3),
    'conect (edges/nodes)   ' : round(len(red_ponderada.edges())/len(red_ponderada.nodes()), 3),
    'centr de grado         ' : round(sum(grados)/N, 3),
    'centr de intermediación' : round(sum(list(lista_grados_intermediacion.values()))/len(lista_grados_intermediacion), 3),
    'densidad               ' : round(densidad, 3),
    'compl (recep/transm)   ' : round(len(receptores)/len(transmisores), 3),
    'indice de jerarquia    ' : round(indice_jerarquia(red_ponderada), 3)
  }
  return resumen

def resumir_categoria(red, categoria):
  resumen = dict()
  red_ponderada = ponderar_red(red)
  N = len(red_ponderada.nodes())
  categoria_efectiva = [nodo for nodo in categoria if nodo in red_ponderada.nodes()]
  transmisores = [node for node in red_ponderada.nodes() if not any(red_ponderada.predecessors(node))]
  transmisores_c = [node for node in transmisores if node in categoria_efectiva]
  receptores = [node for node in red_ponderada.nodes() if not any(red_ponderada.successors(node))]
  receptores_c = [node for node in receptores if node in categoria_efectiva]
  neutrales = []
  for node in red_ponderada.nodes():
    if red_ponderada.in_degree(node) > 0:
      if red_ponderada.out_degree(node) > 0:
        neutrales.append(node)
  neutrales_c = [node for node in neutrales if node in categoria_efectiva]
  grados = red_ponderada.degree()
  grados_categoria = []
  for nodo in categoria_efectiva:
    grado_nodo = grados[nodo]
    grados_categoria.append(grado_nodo)
  intermediacion = nx.betweenness_centrality(red_ponderada)
  intermediacion_categoria = []
  for nodo in categoria_efectiva:
    intermediacion_nodo = intermediacion[nodo]
    intermediacion_categoria.append(intermediacion_nodo)
  _ , pesos_categoria, num_enlaces = efectos_categoria(red, categoria)
  peso_promedio_categoria = np.mean(list(pesos_categoria.values()))
  resumen = {
    'número de nodos        ' : len(categoria_efectiva),
    'número de enlaces      ' : sum(grados_categoria),
    'porc. de transmisores  ' : round(len(transmisores_c)/len(red_ponderada.nodes()), 3),
    'porc. de receptores    ' : round(len(receptores_c)/len(red_ponderada.nodes()), 3),
    'porc. de ordinarios    ' : round(len(neutrales_c)/len(red_ponderada.nodes()), 3),
    'peso promedio          ' : round(peso_promedio_categoria, 3),
    'conect (edges/nodes)   ' : round(sum(grados_categoria)/len(categoria_efectiva), 3),
    'centr de grado         ' : round(sum(grados_categoria)/N, 3),
    'centr de intermediación' : round(sum(intermediacion_categoria)/len(intermediacion_categoria), 3)
  }
  return resumen

def graficar_indicador(indicador, Conteo = True, NodeColor = '#c9c6bd', NodeAlpha = 0.8, FontSize = 9, NodeSize = 600, EdgeAlpha = 0.8, Show = True, Save = True):
  # iniciar red limpia
  T = red_init()
  # obtener nodos, enlaces de entrada y enlaces de salida
  entradas = list(T.in_edges(indicador, data = 'peso'))
  salidas = list(T.out_edges(indicador, data = 'peso'))
  nodos_entradas = []
  for i in range(0,len(entradas)):
    nodo = entradas[i][0]
    nodos_entradas.append(nodo)
  nodos_salidas = []
  for i in range(0, len(salidas)):
    nodo = salidas[i][1]
    nodos_salidas.append(nodo)
  nodos_lista = nodos_salidas + nodos_entradas + [indicador]
  all_edges = entradas+salidas
  # contar la proporción de positivos y negativos en entradas y salidas
  positivos_entradas = []
  negativos_entradas = []
  for entrada in entradas:
    if entrada[2] == 1:
      positivos_entradas.append(entrada[2])
    elif entrada[2] == -1:
      negativos_entradas.append(entrada[2])
    else:
      warnings.warn('Revisar que estén bien contados los enlaces positivos y negativos en las entrasdas')
  positivos_salidas = []
  negativos_salidas = []
  for salida in salidas:
    if salida[2] == 1:
      positivos_salidas.append(salida[2])
    elif salida[2] == -1:
      negativos_salidas.append(salida[2])
    else:
      warnings.warn('Revisar que estén bien contados los enlaces positivos y negativos en las salidas')
  # crear nueva red con nodos vecinos
  I = T.subgraph(nodos_lista)
  J = nx.create_empty_copy(I)
  J.add_weighted_edges_from(entradas)
  J.add_weighted_edges_from(salidas)
  # crear nueva red con nodos vecinos pero sin enlaces, para agregar solamente los que pasan por el indicador
  num_enlaces_repetidos = [] # este se puede borrar
  consolidated_graph = nx.create_empty_copy(I)
  for u, v, data in all_edges:
    if not (u, v) in consolidated_graph.edges():
      enlaces_dict = J.get_edge_data(u,v)
      pesos_u_v = []
      for i in range(0, len(enlaces_dict.values())):
        peso_i = enlaces_dict[i].get('weight')
        pesos_u_v.append(peso_i)
      num_enlaces_repetidos.append(len(pesos_u_v)) # este se puede borrar
      peso_promedio = np.mean(pesos_u_v)
      consolidated_graph.add_edge(u, v, weight = peso_promedio)
    else:
      continue
    print(u, '---', v, ', peso = ', peso_promedio)
  # crear parámetros para graficar
  entradas_solo_nodos = [(u, v) for u, v, peso in entradas]
  num_nodos_entrada = len(set(entradas_solo_nodos))
  salidas_solo_nodos = [(u, v) for u, v, peso in salidas]
  num_nodos_salida = len(set(salidas_solo_nodos))
  max_enlaces = max(num_enlaces_repetidos)
  enlaces_l = list(consolidated_graph.edges(data = 'weight'))
  enlaces = consolidated_graph.edges()
  pesos = [enlace[-1] for enlace in enlaces_l]
  norm = Normalize(vmin=-1, vmax=1)
  color_enlaces = [plt.cm.RdYlGn(norm(peso)) for peso in pesos] # ???
  link_count = {}
  for edge in J.edges(): # conté los enlaces repetidos a partir de J que los conserva, y al contarlos, el orden coincide con los enlaces de consolidated_graph
    link_count[edge] = J.number_of_edges(edge[0], edge[1])
  max_link_count = max(link_count.values())
  edge_width = [1+(link_count[edge]/max_link_count*3) for edge in consolidated_graph.edges()]
  # centrar el indicador y poner las entradas a la izquierda y las salidas a la derecha
  pos = dict()
  pos[indicador] = (0,0)
  promedio_entradas_id = sum(range(0, num_nodos_entrada)) / num_nodos_entrada
  entradas_id_centralizada = [x - promedio_entradas_id for x in range(0, num_nodos_entrada)]
  promedio_salidas_id = sum(range(0, num_nodos_salida)) / num_nodos_salida
  salidas_id_centralizada = [x - promedio_salidas_id for x in range(0, num_nodos_salida)]
  for i in range(0, num_nodos_entrada):
    nodo_entrante = list(set(entradas_solo_nodos))[i][0]
    pos[nodo_entrante] = (-0.3, entradas_id_centralizada[i])
  for i in range(0, num_nodos_salida):
    nodo_entrante = list(set(salidas_solo_nodos))[i][1]
    if not nodo_entrante in pos:
      pos[nodo_entrante] = (0.3, salidas_id_centralizada[i])
  repeticiones_indicador = lista_total_nodos.count(indicador)
  centralidad_grado = nx.degree_centrality(T)
  centralidad_indicador = centralidad_grado[indicador]
  # graficar información del indicador
  info_text = (
    f"Incidencia: {repeticiones_indicador}\n"
    f"Centralidad: {round(centralidad_indicador, 2)}\n"
    f"Entradas: {round(len(positivos_entradas)*100/len(entradas), 2)}% positivas y {round(len(negativos_entradas)*100/len(entradas), 2)}% negativas\n"
    f"Salidas: {round(len(positivos_salidas)*100/len(salidas), 2)}% positivas y {round(len(negativos_salidas)*100/len(salidas), 2)}% negativas"
  )
  plt.figure(figsize = (7, 5))
  nx.draw_networkx_nodes(consolidated_graph, pos, node_size = NodeSize, node_color = NodeColor, alpha = NodeAlpha)
  nx.draw_networkx_labels(consolidated_graph, pos, font_size = FontSize)
  nx.draw_networkx_edges(consolidated_graph, pos, edgelist = enlaces, edge_color = color_enlaces, width = edge_width, alpha = EdgeAlpha, connectionstyle='arc3,rad=0.1')
  if Conteo == True:
    nx.draw_networkx_edge_labels(consolidated_graph, pos, link_count, label_pos = 0.5, bbox=dict(facecolor='none', edgecolor='none'))
  #plt.text(0.5, 1.05, info_text, transform=plt.gca().transAxes, fontsize=10, verticalalignment='top', horizontalalignment = 'center', bbox=dict(facecolor='white', alpha=0.5))
  plt.axis('off')
  if Save == True:
    plt.savefig(f'indicador_{indicador}.svg', bbox_inches='tight')
  if Show == True:
    plt.show()
  plt.clf()

def exportar_red_cyt(red, categoria = False, particiones = None):
  ### EXPORTAR RED COMPLETA PARA CYTOSCAPE
  T = red
  node_count = {} # <- repeticiones de nodos 
  for node in lista_total_nodos:
    node_count[node] = lista_total_nodos.count(node)
  edge_count = dict(T.degree()) # <- repeticiones de enlaces (está calculado sobre la red sin ponderar)
  T_ponderada = ponderar_red(T)
  degree_c = nx.degree_centrality(T_ponderada) # <- centralidad de grado
  betweeness_c = nx.betweenness_centrality(T_ponderada) # <- centralidad de intermediación
  link_count = {} # repeticiones de relaciones
  for edge in T.edges():
    link_count[edge] = T.number_of_edges(edge[0], edge[1])
  transmisores = [node for node in T_ponderada.nodes() if not any(T_ponderada.predecessors(node))]
  receptores = [node for node in T_ponderada.nodes() if not any(T_ponderada.successors(node))]
  ordinarios = []
  for node in T_ponderada.nodes():
    if T_ponderada.in_degree(node) > 0:
      if T_ponderada.out_degree(node) > 0:
        ordinarios.append(node)
  tipo_de_nodo = dict()
  for transmisor in transmisores:
    tipo_de_nodo[transmisor] = 'transmisor'
  for receptor in receptores:
    tipo_de_nodo[receptor] = 'receptor'
  for ordinario in ordinarios:
    tipo_de_nodo[ordinario] = 'ordinario'
  indice_nodos = {node: index + 1 for index, node in enumerate(T_ponderada.nodes())}
  node_count_ord = sorted(node_count.items(), key = lambda x:x[1], reverse = True)
  node_count_idx = {item[0]: index + 1 for index, item in enumerate(node_count_ord)}
  edge_count_ord = sorted(edge_count.items(), key = lambda x:x[1], reverse = True)
  edge_count_idx = {item[0]: index + 1 for index, item in enumerate(edge_count_ord)}
  degree_c_ord = sorted(degree_c.items(), key = lambda x:x[1], reverse = True)
  degree_c_idx = {item[0]: index + 1 for index, item in enumerate(degree_c_ord)}
  betweeness_c_ord = sorted(betweeness_c.items(), key = lambda x:x[1], reverse = True)
  betweeness_c_idx = {item[0]: index + 1 for index, item in enumerate(betweeness_c_ord)}
  link_count_ord = sorted(link_count.items(), key = lambda x:x[1], reverse = True)
  link_count_idx = {item[0]: index + 1 for index, item in enumerate(link_count_ord)}
  if categoria == True: # requiere listas de categorías de conservación cargadas
    categorias = dict()
    for node in T_ponderada.nodes():
      if node in cons_bio_t:
        categorias[node] = 'biodiversidad'
      elif node in cons_agua_t:
        categorias[node] = 'agua'
      elif node in cons_suelo_t:
        categorias[node] = 'suelo'
      elif node in man_par_t+man_gan_t:
        categorias[node] = 'manejo'
    nx.set_node_attributes(T_ponderada, categorias, 'categoria_cons')
  if particiones != None: # requiere diccionario de categorías simplificadas cargado
    comunidades = dict()
    for comunidad in particiones:
      for nodo in comunidad:
        comunidades[nodo] = particiones.index(comunidad)
    nx.set_node_attributes(T_ponderada, comunidades, 'comunidad')
    nx.set_node_attributes(T_ponderada, categorias_simplificadas, 'categoria_simp')
  # indice
  nx.set_node_attributes(T_ponderada, indice_nodos, 'indice')
  # valores
  nx.set_node_attributes(T_ponderada, node_count, 'node_count')
  nx.set_node_attributes(T_ponderada, edge_count, 'edge_count')
  nx.set_node_attributes(T_ponderada, degree_c, 'cent_grado')
  nx.set_node_attributes(T_ponderada, betweeness_c, 'cent_int')
  nx.set_edge_attributes(T_ponderada, link_count, 'repeticiones')
  # índices
  nx.set_node_attributes(T_ponderada, node_count_idx, 'node_count_idx')
  nx.set_node_attributes(T_ponderada, edge_count_idx, 'edge_count_idx')
  nx.set_node_attributes(T_ponderada, degree_c_idx, 'cent_grado_idx')
  nx.set_node_attributes(T_ponderada, betweeness_c_idx, 'cent_int_idx')
  nx.set_edge_attributes(T_ponderada, link_count_idx, 'repeticiones_idx')
  # tipo de nodos e indice
  nx.set_node_attributes(T_ponderada, tipo_de_nodo, 'tipo')
  timestr = time.strftime("%Y%m%d-%H%M%S")
  nx.write_graphml(T_ponderada, 'red_exportada_'+timestr+'.graphml') # exportar para leer con cytoscape


# LEER DATOS Y CREAR RED CONJUNTA

conceptos_est = pd.read_csv('/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Datos/conceptos_v4.csv')
#conceptos_est_pred = conceptos_est.loc[conceptos_est['prediseñado'] == 1] # para quedarse solo con los conceptos prediseñados
""" SI ES EN COLAB
from google.colab import drive
drive.mount('/content/drive')
conceptos_est = pd.read_csv('/content/drive/MyDrive/M 2022 Gerardo Dávila/Datos/conceptos_v4.csv')
"""
lista_de_conceptos = conceptos_est['id'].values # índices de la lista general de conceptos
lista_de_terminos = conceptos_est.iloc[:,1].values
dir_enlaces = '/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Datos/listas_de_enlaces/'
dataframes = []
fil_enlaces = sorted(os.listdir('/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Datos/listas_de_enlaces/'))
for file in fil_enlaces:
  if file.endswith('.csv'):
    file_path = os.path.join(dir_enlaces, file)
    df = pd.read_csv(file_path)
    dataframes.append(df)
redes = []
lista_total_nodos = []
for i in range(len(dataframes)):
  G, _ = construir_mapa(dataframes[i])
  lista_nodos_i = list(G.nodes())
  lista_total_nodos = lista_total_nodos + lista_nodos_i
  redes.append(G)
T = red_init() # red total agregada
T.remove_nodes_from(['monte', 'plagas del ganado', 'infiltración'])
pos = nx.spring_layout(T, k = 2, scale = 3.5, seed = 3)

# Graficar distribuciones de las métricas
informacion_nodos = pd.read_csv('/home/gerardo/Escritorio/Reacomodar/informacion_nodos_red_completa.csv')

fig, axes = plt.subplots(2, 2, figsize=(12, 10))
sns.histplot(informacion_nodos['node_count'], kde=True, ax=axes[0,0])
sns.histplot(informacion_nodos['Indegree'], kde=True, ax=axes[0,1])
sns.histplot(informacion_nodos['Outdegree'], kde=True, ax=axes[1,0])
sns.histplot(informacion_nodos['cent_int'], kde=True, ax=axes[1,1])
plt.tight_layout()
plt.show()


#  CREAR GRANDES CATEGORIAS

#  CONECTAR DATOS SOCIOECONÓMICOS CON MAPAS
datos_socioeconomicos = pd.read_csv('/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Datos/datos_socioeconomicos.csv')
datos_con_mapa = datos_socioeconomicos[datos_socioeconomicos['mapa'] == 1]
datos_con_mapa = datos_con_mapa.drop(21) # quité temporalmente el dato de Pedro Ibarra porque no tengo la matriz de enlaces en la carpeta !! Ahora el número de filas de la base de datos corresponde al orden en el que entraron las matrices a la lista "redes"
datos_con_mapa = datos_con_mapa.set_index(pd.Index(range(0,len(redes))))
names = ['nodos', 'enlaces', 'transmisores', 'receptores', 'ordinarios', 'positivos', 'negativos', 'prop_pos_neg', 'conectividad', 'grado', 'intermediacion', 'densidad', 'complejidad', 'jerarquia']
metricas_por_mapa = pd.DataFrame(columns=names, index=range(0, len(redes)))
for i in range(0, len(redes)):
  metricas_por_mapa.loc[i] = list(resumir(redes[i]).values())
metricas_soc = pd.concat([datos_con_mapa.sort_index(), metricas_por_mapa.sort_index()], axis = 1)
metricas_soc.to_csv('datos_con_metricas_v1.csv')
# métricas promedio
promedios = dict()
for columna in names:
  metrica = columna
  promedio = np.mean(metricas_por_mapa[columna])
  sd = np.std(metricas_por_mapa[columna])
  promedios[columna] = round(promedio, 3), round(sd, 3)

# CREAR SUBCONJUNTOS POR CATEGORÍAS DE CONSERVACIÓN

cons_bio_df_e = conceptos_est.loc[conceptos_est['cons_bio'] == 1] # df de conceptos exclusivos de cons_bio
cons_suelo_df_e = conceptos_est.loc[conceptos_est['cons_suelo'] == 1] # df de conceptos exclusivos de cons_suelo
cons_agua_df_e = conceptos_est.loc[conceptos_est['cons_agua'] == 1] # df de conceptos exclusivos de cons_agua
otros_df = conceptos_est[(conceptos_est['cons_agua'] != 1) & (conceptos_est['cons_bio'] != 1) & (conceptos_est['cons_suelo'] != 1)]
cons_bio_t = list(cons_bio_df_e.loc[:, 'termino']) # conceptos exclusivos de cons_bio
cons_suelo_t = list(cons_suelo_df_e.loc[:, 'termino']) # conceptos exclusivos de cons_bio
cons_agua_t = list(cons_agua_df_e.loc[:, 'termino']) # conceptos exclusivos de cons_bio

### VISUALIZAR SUBBONJUNTOS DE REDES

# filtrar otras categorías -----------------------------------------------------------------
man_par_df = conceptos_est.loc[conceptos_est['man_par'] == 1]
man_gan_df = conceptos_est.loc[conceptos_est['man_gan'] == 1]
man_par_t = list(man_par_df.loc[:, 'termino'])
man_gan_t = list(man_gan_df.loc[:, 'termino'])
manejo_t = man_par_t + man_gan_t

# red de todos los aspectos de conservación + manejo de la parcela
CCC = red_init()
CCC.remove_nodes_from(['monte', 'plagas del ganado', 'tradicion ganadera'])
nodos_conservacion = set(cons_bio_t + cons_agua_t + cons_suelo_t + man_gan_t + man_par_t)
enlaces_cons = []
for u, v, data in CCC.edges(data=True):
  if u in nodos_conservacion and v in nodos_conservacion:
    enlaces_cons.append((u, v, {'peso':data['peso']}))
nodos_elim = [nodo for nodo in CCC.nodes() if nodo not in nodos_conservacion]
CCC.remove_nodes_from(nodos_elim)
CCC.remove_edges_from(list(CCC.edges()))
CCC.add_edges_from(enlaces_cons) # <--- red conservación
categoria = dict()
for node in CCC.nodes():
  if node in cons_bio_t:
    categoria[node] = 'biodiversidad'
  elif node in cons_agua_t:
    categoria[node] = 'agua'
  elif node in cons_suelo_t:
    categoria[node] = 'suelo'
  elif node in man_par_t+man_gan_t:
    categoria[node] = 'manejo'
nx.set_node_attributes(CCC, categoria, 'categoria')


# crear red de conservación de la biodiversidad --------------------------------------------
B = red_init()
B.remove_nodes_from(['monte', 'plagas del ganado'])
nodos_cons_bio = set(cons_bio_t + man_par_t + man_gan_t)
enlaces_cons_bio = []
for u, v, data in B.edges(data=True):
  if u in nodos_cons_bio and v in nodos_cons_bio:
    enlaces_cons_bio.append((u, v, {'peso':data['peso']}))
nodos_elim_bio = [nodo for nodo in B.nodes() if nodo not in nodos_cons_bio]
B.remove_nodes_from(nodos_elim_bio)
B.remove_edges_from(list(B.edges()))
B.add_edges_from(enlaces_cons_bio) # <--- red biodiversidad
color_map_b = []
for node in list(B.nodes()):
  if node in cons_bio_t:
    color_map_b.append('#bcbc8d') # verdecito
  elif (node in man_par_t) | (node in man_gan_t):
    color_map_b.append('#c0c8e0') # azulito
graficar_sub(T, B, colormap = color_map_b, Show = False, Save = True)
#plt.savefig('red_bio.png', dpi = 300, format = 'png', bbox_inches='tight')
# crear red de conservación del suelo -----------------------------------------------------
S = red_init()
S.remove_nodes_from(['monte', 'plagas del ganado', 'infiltración'])
nodos_cons_sue = set(cons_suelo_t + man_par_t + man_gan_t)
enlaces_cons_sue = []
for u, v, data in S.edges(data=True):
  if u in nodos_cons_sue and v in nodos_cons_sue:
    enlaces_cons_sue.append((u, v, {'peso':data['peso']}))
nodos_elim_sue = [nodo for nodo in S.nodes() if nodo not in nodos_cons_sue]
S.remove_nodes_from(nodos_elim_sue)
S.remove_edges_from(list(S.edges()))
S.add_edges_from(enlaces_cons_sue)# <--- red suelo
color_map_s = []
for node in list(S.nodes()):
  if node in cons_suelo_t:
    color_map_s.append('#bcbc8d')
  elif (node in man_par_t) | (node in man_gan_t):
    color_map_s.append('#c0c8e0')
graficar_sub(T, S, color_map_s, Show = False, Save = True)
# crear red de conservación del agua -------------------------------------------------------
A = red_init()
A.remove_nodes_from(['monte', 'plagas del ganado', 'infiltración'])
nodos_cons_agua = set(cons_agua_t + man_par_t + man_gan_t)
enlaces_cons_agua = []
for u, v, data in A.edges(data=True):
  if u in nodos_cons_agua and v in nodos_cons_agua:
    enlaces_cons_agua.append((u, v, {'peso':data['peso']}))
nodos_elim_agua = [nodo for nodo in A.nodes() if nodo not in nodos_cons_agua]
A.remove_nodes_from(nodos_elim_agua)
A.remove_edges_from(list(A.edges()))
A.add_edges_from(enlaces_cons_agua)
color_map_a = []
for node in list(A.nodes()):
  if node in cons_agua_t:
    color_map_a.append('#bcbc8d')
  elif (node in man_par_t) | (node in man_gan_t):
    color_map_a.append('#c0c8e0')
graficar_sub(T, A, color_map_a, Show = False, Save = True)

### CALCULAR MÉTRICAS DE REDES

# nodos transmisores y receptores por categoría --------------------------------------------------------------------

T = red_init()
T.remove_nodes_from(list(nx.isolates(T)))
transmisores = [node for node in T.nodes() if not any(T.predecessors(node))]
receptores = [node for node in T.nodes() if not any(T.successors(node))]
neutrales = []
for node in T.nodes():
  if T.in_degree(node) > 0:
    if T.out_degree(node) > 0:
      neutrales.append(node)
nombres_cat = list(conceptos_est.columns.values)[4:] # los nombres de las categorías empiezan en la columna 5
tr_por_categoria = dict()
for categoria in nombres_cat:
  cat_df = conceptos_est.loc[conceptos_est[categoria] == 1]
  cat_t = list(cat_df.loc[:, 'termino'])
  cat_t_transmisores = [termino for termino in cat_t if termino in transmisores]
  cat_t_neutros = [termino for termino in cat_t if termino in neutrales]
  cat_t_receptores = [termino for termino in cat_t if termino in receptores]
  tr_por_categoria[categoria] = [len(cat_t_transmisores), len(cat_t_neutros), len(cat_t_receptores)]

# enlaces positivos y negativos de manejo a conservación ----------------------------------------------

efectos_categoria(B, cons_bio_t)
efectos_categoria(S, cons_suelo_t)
efectos_categoria(A, cons_agua_t)

# conceptos usados en las redes (del total de conceptos disponibles por categoría)---------------------

t_usados_b = []
for i in range(len(cons_bio_t)):
  if cons_bio_t[i] in B.nodes():
    t_usados_b.append(cons_bio_t[i])
t_usados_s = []
for i in range(len(cons_suelo_t)):
  if cons_suelo_t[i] in S.nodes():
    t_usados_s.append(cons_suelo_t[i])
t_usados_a = []
for i in range(len(cons_agua_t)):
  if cons_agua_t[i] in A.nodes():
    t_usados_a.append(cons_agua_t[i])
t_usados_m = []
for i in range(len(manejo_t)):
  if manejo_t[i] in A.nodes():
    t_usados_a.append(cons_agua_t[i])

# grados por partes --------------------------------------------------------------------------------------

# biodiversidad
grados_todos_b = nx.degree_centrality(B)
grados_manejo_b = []
grados_biodiversidad = []
for nodo in B.nodes():
  if nodo in cons_bio_t:
    grados_biodiversidad.append(grados_todos_b[nodo])
  elif nodo in manejo_t:
    grados_manejo_b.append(grados_todos_b[nodo])
grado_manejo_b_prom = np.mean(grados_manejo_b)
grado_biodiversidad_prom = np.mean(grados_biodiversidad)


### MODULARIDAD

conservacion = ['cons_agua', 'cons_suelo', 'cons_bio']
biofisicas = ['cond_amb', 'rec_nat', 'rec_no_nat']
ganaderia = ['man_par', 'man_gan']
sociales = ['cond_soc', 'subj', 'actores', 'proc_ext']
# Louvain Directed
T = red_init()
T.remove_nodes_from(['monte', 'plagas del ganado', 'tradicion ganadera']) 
T_ponderada = ponderar_red(T)
coms_louvain = nx.community.louvain_communities(T_ponderada, weight = 'repeticiones', resolution = 1)
coms_louvain_communities = list()
for i in range(0, len(coms_louvain)):
  coms_louvain_communities.append(list(coms_louvain[i]))
# crear matriz
nrow = len(T_ponderada.nodes()) # corresponde a los conceptos
ncol = len(coms_louvain_communities) # corresponde a las comunidades
matriz = np.full([nrow, ncol], np.nan)
nodos_ordenados = []
for comunidad in coms_louvain_communities:
  for nodo in comunidad:
    nodos_ordenados.append(nodo)
categorias_etiquetas = [f"Categoría {i}" for i in range(1, ncol+1)]
# meter datos a la matriz
categorias_simplificadas = dict()
for comunidad in coms_louvain_communities:
  for concepto in comunidad:
    concepto_info = conceptos_est.loc[conceptos_est['termino'] == concepto]
    concepto_info_categorias = concepto_info.iloc[:,4:] # las categorías empiezan desde la quinta columna
    concepto_info_categorias_col_index = np.where(concepto_info_categorias == 1)[1]
    categoria_original = concepto_info_categorias.columns[concepto_info_categorias_col_index][0]
    if categoria_original in conservacion:
      categoria = 1
      categorias_simplificadas[concepto] = 'conservación'
    elif categoria_original in biofisicas:
      categoria = 2
      categorias_simplificadas[concepto] = 'biofisicas'
    elif categoria_original in ganaderia:
      categoria = 3
      categorias_simplificadas[concepto] = 'ganadería'
    elif categoria_original in sociales:
      categoria = 4
      categorias_simplificadas[concepto] = 'sociales'
    fila = nodos_ordenados.index(concepto)
    columna = coms_louvain_communities.index(comunidad)
    matriz[fila, columna] = categoria
    matriz_grafica = matriz.transpose()
exportar_red_cyt(T, categoria = True, particiones = coms_louvain_communities)
# graficar
fig, ax = plt.subplots()
im = ax.imshow(matriz_grafica, cmap = 'Accent')
ax.grid(False)
ax.set_yticks(np.arange(ncol), labels=categorias_etiquetas)
ax.set_xticks(np.arange(len(nodos_ordenados)), labels=nodos_ordenados)
plt.setp(ax.get_yticklabels(), rotation=45, ha="right", rotation_mode="anchor")
plt.setp(ax.get_xticklabels(), rotation=90, ha="right", rotation_mode="anchor")
fig.set_size_inches(14, 8)  # Ancho x Alto en pulgadas
plt.savefig("comunidades.svg")
plt.show()


### GRAFICAR ESPCIALMENTE POR COMUNIDADES
communities = nx.community.louvain_communities(T_ponderada, weight = 'repeticiones', resolution = 1)
# Asignar colores a cada comunidad
color_map = {}
for i, com in enumerate(communities):
    for node in com:
        color_map[node] = i

# Definir posiciones iniciales para las comunidades
pos = {}
for i, com in enumerate(communities):
    theta = np.linspace(0, 2*np.pi, len(com))
    radius = i*2  # Espaciado radial entre comunidades
    for j, node in enumerate(com):
        pos[node] = (radius*np.cos(theta[j]), radius*np.sin(theta[j]))

# Dibujar el grafo con posiciones de las comunidades
plt.figure(figsize=(8, 8))
nx.draw(T_ponderada, pos, node_color=[color_map[node] for node in T_ponderada.nodes()], with_labels=True, node_size=300)
plt.show()



### ANÁLISIS POR INDICADORES
# graficar y guardar
indicadores = list(conceptos_est.query('indicadores==1')['termino'])
for indicador in indicadores:
  graficar_indicador(indicador)
  plt.savefig(f'inficador_{indicador}.png')
  plt.clf()
plt.close()


### Análisis de centralidad para todos los nodos
# Crear una red no dirigida ponderada a partir de la red dirigida
T = red_init()
T_ponderada = nx.Graph()
for u, v, data in T.edges(data=True):
    peso = data['peso']
    if T_ponderada.has_edge(u, v):
        T_ponderada[u][v]['peso'] = (T_ponderada[u][v]['peso'] + peso) / 2
    else:
        T_ponderada.add_edge(u, v, peso=peso)
eigenvector_centrality = nx.eigenvector_centrality_numpy(T_ponderada, weight='peso')
degree_centrality = nx.degree_centrality(T)
node_counts = dict(T.degree())
nx.set_node_attributes(T, degree_centrality, 'degree_centrality')
nx.set_node_attributes(T, eigenvector_centrality, 'eigenvector_centrality')
nx.set_node_attributes(T, node_counts, 'node_count')
nx.write_graphml(T, 'T_con_atributos.graphml') # exportar para leer con cytoscape


### EXPORTAR RED COMPLETA PARA CYTOSCAPE
T = red_init()
T.remove_nodes_from(['monte', 'plagas del ganado', 'tradicion ganadera']) # quitar conceptos obsoletos
node_count = {} # <- repeticiones de nodos 
for node in lista_total_nodos:
  node_count[node] = lista_total_nodos.count(node)
edge_count = dict(T.degree()) # <- repeticiones de enlaces (está calculado sobre la red sin ponderar)
T_ponderada = ponderar_red(T)
degree_c = nx.degree_centrality(T_ponderada) # <- centralidad de grado
betweeness_c = nx.betweenness_centrality(T_ponderada) # <- centralidad de intermediación
link_count = {} # repeticiones de relaciones
for edge in T.edges():
  link_count[edge] = T.number_of_edges(edge[0], edge[1])
transmisores = [node for node in T_ponderada.nodes() if not any(T_ponderada.predecessors(node))]
receptores = [node for node in T_ponderada.nodes() if not any(T_ponderada.successors(node))]
ordinarios = []
for node in T_ponderada.nodes():
  if T_ponderada.in_degree(node) > 0:
    if T_ponderada.out_degree(node) > 0:
      ordinarios.append(node)
tipo_de_nodo = dict()
for transmisor in transmisores:
  tipo_de_nodo[transmisor] = 'transmisor'
for receptor in receptores:
  tipo_de_nodo[receptor] = 'receptor'
for ordinario in ordinarios:
  tipo_de_nodo[ordinario] = 'ordinario'
indice_nodos = {node: index + 1 for index, node in enumerate(T_ponderada.nodes())}
node_count_ord = sorted(node_count.items(), key = lambda x:x[1], reverse = True)
node_count_idx = {item[0]: index + 1 for index, item in enumerate(node_count_ord)}
edge_count_ord = sorted(edge_count.items(), key = lambda x:x[1], reverse = True)
edge_count_idx = {item[0]: index + 1 for index, item in enumerate(edge_count_ord)}
degree_c_ord = sorted(degree_c.items(), key = lambda x:x[1], reverse = True)
degree_c_idx = {item[0]: index + 1 for index, item in enumerate(degree_c_ord)}
betweeness_c_ord = sorted(betweeness_c.items(), key = lambda x:x[1], reverse = True)
betweeness_c_idx = {item[0]: index + 1 for index, item in enumerate(betweeness_c_ord)}
link_count_ord = sorted(link_count.items(), key = lambda x:x[1], reverse = True)
link_count_idx = {item[0]: index + 1 for index, item in enumerate(link_count_ord)}
# valores
nx.set_node_attributes(T_ponderada, node_count, 'node_count')
nx.set_node_attributes(T_ponderada, edge_count, 'edge_count')
nx.set_node_attributes(T_ponderada, degree_c, 'cent_grado')
nx.set_node_attributes(T_ponderada, betweeness_c, 'cent_int')
nx.set_edge_attributes(T_ponderada, link_count, 'repeticiones')
# índices
nx.set_node_attributes(T_ponderada, node_count_idx, 'node_count_idx')
nx.set_node_attributes(T_ponderada, edge_count_idx, 'edge_count_idx')
nx.set_node_attributes(T_ponderada, degree_c_idx, 'cent_grado_idx')
nx.set_node_attributes(T_ponderada, betweeness_c_idx, 'cent_int_idx')
nx.set_edge_attributes(T_ponderada, link_count_idx, 'repeticiones_idx')
# tipo de nodos e indice
nx.set_node_attributes(T_ponderada, tipo_de_nodo, 'tipo')
nx.set_node_attributes(T_ponderada, indice_nodos, 'indice')
nx.write_graphml(T_ponderada, 'red_completa.graphml') # exportar para leer con cytoscape


### CREAR RED SIMPLIFICADA PARA CYTOSCAPE
T = red_init()
num_nodos_incluir = 15
# FILTRAR MÁS REPETICIONES
link_count = {} # repeticiones de relaciones
for edge in T.edges():
  link_count[edge] = T.number_of_edges(edge[0], edge[1])
most_linked = {k:v for k, v in sorted(link_count.items(), key = lambda item: item[1], reverse = True)}
enlaces_mas_conectados = {clave: valor for clave, valor in most_linked.items() if valor >= 6}
nodos_unicos = set()
for enlace in enlaces_mas_conectados:
    nodos_unicos.update(enlace)
G = T.subgraph(nodos_unicos)
# FILTRAR NODOS CON MÁS REPETICIONES
node_count = {}
for node in lista_total_nodos:
  node_count[node] = lista_total_nodos.count(node)
most_counted = sorted(node_count.items(), key = lambda x:x[1], reverse = True)
diccionario_ordenado = {item[0]: index + 1 for index, item in enumerate(most_counted)}
ten_most_counted = []
for var in most_counted[:num_nodos_incluir]:
  ten_most_counted.append(var[0])
G = T.subgraph(ten_most_counted)
## O POR CRITERIO DE REPETICIONES
top_T_count = {key:val for key, val in node_count.items() if val > 8}
simple_nodos = [key for key, val in top_T_count.items()]
G = T.subgraph(simple_nodos)
# FILTRAR NODOS CON MÁS CENTRALIDAD
node_degree = dict(T.degree())
most_degree = sorted(node_degree.items(), key=lambda x:x[1], reverse = True)
ten_most_degree = []
for var in most_degree[:num_nodos_incluir]:
  ten_most_degree.append(var[0])
G = T.subgraph(ten_most_degree)
## O POR CRITERIO DE CENTRALIDAD
top_T_degree = {key:val for key, val in node_degree.items() if val > 8}
simple_nodos = [key for key, val in top_T_count.items()]
G = T.subgraph(simple_nodos)
# CONTAR ENLACES
link_count = {}
for edge in G.edges():
  link_count[edge] = G.number_of_edges(edge[0], edge[1])
# CAMBIAR MÚLTIPLES ENLACES POR ENLACES PONDERADOS
G_ponderada = nx.DiGraph()
G_pond_pesos_acumulados = {}
for u, v, data in G.edges(data=True):
    peso_actual = data['peso']
    key = (u, v)
    if key in G_pond_pesos_acumulados:
      G_pond_pesos_acumulados[key].append(peso_actual)
    else:
      G_pond_pesos_acumulados[key] = [peso_actual]
    #if G_ponderada.has_edge(u, v):
    #    G_ponderada[u][v]['peso'] = (G_ponderada[u][v]['peso'] + peso) / 2
    #else:
    #    G_ponderada.add_edge(u, v, peso=peso)
for (u, v), pesos in G_pond_pesos_acumulados.items():
  peso_promedio = sum(pesos) / len(pesos)
  G_ponderada.add_edge(u, v, peso=peso_promedio)

# ASIGNAR NUEVOS ATRIBUTOS DE LA RED
nx.set_edge_attributes(G_ponderada, link_count, 'repeticiones')
#eigenvector_centrality = nx.eigenvector_centrality_numpy(G_ponderada, weight='peso')
degree_centrality = nx.degree_centrality(G_ponderada)
nx.set_node_attributes(G_ponderada, degree_centrality, 'degree_centrality')
#nx.set_node_attributes(G_ponderada, eigenvector_centrality, 'eigenvector_centrality')
nx.set_node_attributes(G_ponderada, node_count, 'node_count')
nx.write_graphml(G_ponderada, 'red_simplificada.graphml') # exportar para leer con cytoscape


# LEER E IMPORTAR RED EJEMPLO PARA TALLER 
archivo = "/home/gerardo/Descargas/bio-CE-LC/bio-CE-LC.edges"
G = nx.Graph()

with open(archivo, 'r') as f:
    for linea in f:
        nodo1, nodo2, valor_enlace = linea.strip().split()
        G.add_edge(nodo1, nodo2, weight=float(valor_enlace))  # Añade el enlace con su peso como un número flotante

nx.write_graphml(G, 'red_ejemplo_taller.graphml') # exportar para leer con cytoscape















### COSAS PENDIENTES
# GRAFICAR COMO RED BIPARTITA
#pos_b = {}
#for node in cons_bio_t:
#    pos_b[node] = (cons_bio_t.index(node), 2 + random.uniform(-0.4,-0.4))  # Posición en el nivel superior
#for node in manejo_t:
#    pos_b[node] = (manejo_t.index(node), 0 + random.uniform(-0.4,-0.4))  # Posición en el nivel inferior
# suelo ---------------------------------------------------------------------------------------------------
grados_todos_s = nx.degree_centrality(S)
grados_manejo_s = []
grados_suelo = []
for nodo in S.nodes():
  if nodo in cons_suelo_t:
    grados_suelo.append(grados_todos_s[nodo])
  elif nodo in man_par_t + man_gan_t:
    grados_manejo_s.append(grados_todos_s[nodo])
grado_manejo_s_prom = np.mean(grados_manejo_s)
grado_suelo_prom = np.mean(grados_suelo)
# agua ----------------------------------------------------------------------------------------------------
grados_todos_a = nx.degree_centrality(A)
grados_manejo_a = []
grados_agua = []
for nodo in S.nodes():
  if nodo in cons_agua_t:
    grados_agua.append(grados_todos_a[nodo])
  elif nodo in man_par_t + man_gan_t:
    grados_manejo_a.append(grados_todos_a[nodo])
grado_manejo_a_prom = np.mean(grados_manejo_a)
grado_agua_prom = np.mean(grados_agua)
###
# MEJORAR GRÁFICA (INCOMPLETO) !!!
node_count = {}
for node in lista_total_nodos:
  node_count[node] = lista_total_nodos.count(node) # lista_total_nodos se define desde el conjunto de listas de enlaces
nodos_chidos = [key for key, value in node_count.items() if value > 2]
Counter(list(node_count.values()))
N = T.subgraph(nodos_chidos)
link_count = {}
for edge in N.edges():
  link_count[edge] = N.number_of_edges(edge[0], edge[1])
max_link_count = max(link_count.values())
alpha_values = [link_count[edge] / max_link_count for edge in N.edges()]
max_node_count = max(node_count.values())
node_alpha_values = [node_count[node] / max_node_count for node in N.nodes()]
enlaces = N.edges()
enlaces_l = list(N.edges(data = 'peso'))
pesos = [enlace[-1] for enlace in enlaces_l]
color_enlaces = ['green' if peso > 0 else 'red' for peso in pesos]
node_sizes = [node_count[node]*30 for node in N.nodes()]
edge_width = [link_count[edge]/max_link_count*2 for edge in N.edges()]
plt.figure(figsize=(10, 10))
nx.draw_networkx_nodes(N, pos, node_size = node_sizes, node_color='#cbdfef', alpha = 0.5)
nx.draw_networkx_labels(N, pos, alpha = 0.9, font_size = 8)
nx.draw_networkx_edges(N, pos, edgelist = enlaces, edge_color = color_enlaces, width = edge_width, alpha = 0.7)
plt.gca().set_aspect('equal')
plt.axis('off')
plt.savefig("networkx_graph.png", dpi = 800, bbox_inches='tight', pad_inches=0)
###   