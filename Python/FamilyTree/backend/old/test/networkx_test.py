import networkx as nx
import matplotlib.pyplot as plt

G = nx.Graph()

#add individual nodes
G.add_node(1)

#add nodes from an iterable, e.g. a list
G.add_nodes_from([2,3])


#add nodes with attributes (this can only be made with iterables)
#G.add_node((4, {"color": "blue"})) # this gives an error
G.add_nodes_from([(5, {"color":"red"}), (6,{"color":"green"})])

#automatically generate a graph with 10 nodes
H = nx.path_graph(10)
#subax1=plt.subplot(221)
#nx.draw(H, with_labels=True)

#incorporate the nodes of H into G
G.add_nodes_from(H)
#subax2=plt.subplot(222)
#nx.draw(G, with_labels=True)

#the graph H could be used as a node in G
G.add_node(H)
#subax3=plt.subplot(223)

#remove a node from a graph
G.remove_node(H)
G.remove_nodes_from([7,8,9])

#add an edge
nodes = list(G.nodes) #returns a list with the nodes of a graph
edges = list(G.edges) #returns a list with the edges of a graph
for i in range(len(nodes)-1):
    G.add_edge(nodes[i], nodes[i+1]) #individual addition of edges
    
G.add_edges_from([(1,4),(2,4, {"weight": 0.5})])

#information about the graph
nodes = list(G.nodes) #returns a list with the nodes of a graph
n_nodes = G.number_of_nodes()
edges = list(G.edges) #returns a list with the edges of a graph
n_edges = G.number_of_edges()
print(nodes, n_nodes)
print(edges, n_edges)

for i in nodes:
    print("%d -> %s" % (i,G.nodes[i]))

    
nx.draw(G, with_labels=True)
plt.show()
