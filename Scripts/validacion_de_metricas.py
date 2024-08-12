import networkx as nx
import numpy as np
import matplotlib.pyplot as plt

# Generar redes aleatorias usando el modelo de configuración para redes dirigidas
in_degrees = [d for n, d in G.in_degree()]
out_degrees = [d for n, d in G.out_degree()]
# Crear una función para generar una red aleatoria dirigida con la misma secuencia de grados
def generate_random_directed_graph(in_degrees, out_degrees):
    while True:
        try:
            random_graph = nx.directed_configuration_model(in_degrees, out_degrees)
            random_graph = nx.DiGraph(random_graph)  # Convertir a DiGraph para eliminar bucles y aristas múltiples
            random_graph.remove_edges_from(nx.selfloop_edges(random_graph))
            return random_graph
        except nx.NetworkXError:
            continue
random_graphs = [generate_random_directed_graph(in_degrees, out_degrees) for _ in range(10000)]
def calculate_metrics(G):
    nodes_tot = len(G.nodes())
    in_degree_centrality = nx.in_degree_centrality(G)
    out_degree_centrality = nx.out_degree_centrality(G)
    density = nx.density(G)
    transmisores = [node for node in G.nodes() if not any(G.predecessors(node))]
    receptores = [node for node in G.nodes() if not any(G.successors(node))]
    ordinarios = []
    for node in G.nodes():
        if G.in_degree(node) > 0:
            if G.out_degree(node) > 0:
                ordinarios.append(node)
    lista_grados_intermediacion = nx.betweenness_centrality(G)
    return (
        len(receptores)/nodes_tot, # proporción de nodos receptores
        len(transmisores)/nodes_tot, # proporción de nodos transmisores
        len(ordinarios)/nodes_tot, # proporción de nodos ordinarios
        np.mean(list(in_degree_centrality.values())), # centralidad de grado de entrada
        np.mean(list(out_degree_centrality.values())), # centralidad de grado de salida
        sum(list(lista_grados_intermediacion.values()))/len(lista_grados_intermediacion), # intermediacion promedio
        len(G.edges())/len(G.nodes())**2, # densidad
        len(receptores)/len(transmisores), # complejidad
        indice_jerarquia(G) # jerarquía
        )
# Calcular métricas para la red original
original_metrics = calculate_metrics(G)
receptores, transmisores, ordinarios, grado_entrada, grado_salida, intermediacion, densidad, complejidad, jerarquia = original_metrics
# Calcular métricas para las redes aleatorias
random_metrics = np.array([calculate_metrics(R) for R in random_graphs])
r_receptores, r_transmisores, r_ordinarios, r_grado_entrada, r_grado_salida, r_intermediacion, r_densidad, r_complejidad, r_jerarquia = random_metrics[:, 0], random_metrics[:, 1], random_metrics[:, 2], random_metrics[:, 3], random_metrics[:, 4], random_metrics[:, 5], random_metrics[:, 6], random_metrics[:, 7], random_metrics[:, 8]
# Comparación estadística (dos colas)
def two_tailed_p_value(random_values, original_value):
    more_extreme_high = np.sum(random_values >= original_value) / len(random_values)
    more_extreme_low = np.sum(random_values <= original_value) / len(random_values)
    return more_extreme_high + more_extreme_low - 1
# Comparación estadística
receptores_p = two_tailed_p_value(r_receptores, receptores)
transmisores_p = two_tailed_p_value(r_transmisores, transmisores)
ordinarios_p = two_tailed_p_value(r_ordinarios, ordinarios)
grado_entrada_p = two_tailed_p_value(r_grado_entrada, grado_entrada)
grado_salida_p = two_tailed_p_value(r_grado_salida, grado_salida)
intermediacion_p = two_tailed_p_value(r_intermediacion, intermediacion)
densidad_p = two_tailed_p_value(r_densidad, densidad)
complejidad_p = two_tailed_p_value(r_complejidad, complejidad)
jerarquia_p = two_tailed_p_value(r_jerarquia, jerarquia)
valores_de_p = [receptores_p, transmisores_p, ordinarios_p, grado_entrada_p, grado_salida_p, intermediacion_p, densidad_p, complejidad_p, jerarquia_p]
metrics = [
    ('proporción de nodos receptores', r_receptores, receptores, receptores_p),
    ('proporción de nodos transmisores', r_transmisores, transmisores, transmisores_p),
    ('proporción de nodos ordinarios', r_ordinarios, ordinarios, ordinarios_p),
    ('centralidad de grado de entrada', r_grado_entrada, grado_entrada, grado_entrada_p),
    ('centralidad de grado de salida', r_grado_salida, grado_salida, grado_salida_p),
    ('centralidad de intermediación', r_intermediacion, intermediacion, intermediacion_p),
    ('densidad', r_densidad, densidad, densidad_p),
    ('complejidad', r_complejidad, complejidad, complejidad_p),
    ('índice de jerarquía', r_jerarquia, jerarquia, jerarquia_p)
]
# Visualización
plt.figure(figsize=(10, 8))
for i, (title, random_metric, original_metric, p_value) in enumerate(metrics):
    plt.subplot(3, 3, i+1)
    plt.hist(random_metric, bins=30, alpha=0.7, label='Redes aleatorias')
    plt.axvline(original_metric, color='r', linestyle='dashed', linewidth=2, label='Mapa cognitivo conjunto')
    plt.title(title)
    plt.legend()
    plt.text(0.95, 0.95, f'p: {p_value:.4f}', ha='right', va='top', transform=plt.gca().transAxes, fontsize=12)
plt.tight_layout()
plt.savefig('dsitribucion_de_metricas.svg')
plt.show()

