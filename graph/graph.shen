\* graph.shen --- a library for graph definition and manipulation

Copyright (C) 2011  Eric Schulte

*** License:

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

*** Commentary:

Graphs are represented as two dictionaries one for vertices and one
for edges.  It is important to note that the dictionary implementation
used is able to accept arbitrary data structures as keys.  This
structure technically encodes hypergraphs (a generalization of graphs
in which each edge may contain any number of vertices).  Examples of a
regular graph G and a hypergraph H with the corresponding data
structure are given below.


--G=<graph Vertices Edges>------------------------------------------------
                            Vertices                  Edges
                           ----------                -------
 +----Graph G-----+      hash | key -> value      hash |  key  -> value
 |                |      -----+------>--------    -----+-------->---------
 | a---b---c   g  |         1 |  a  -> [1]           1 | [a b] -> [1 2]
 |     |   |      |         2 |  b  -> [1 2 3]       2 | [b c] -> [2 3]
 |     d---e---f  |         3 |  c  -> [2 4]         3 | [b d] -> [2 4]
 |                |         4 |  d  -> [3 5]         4 | [c e] -> [3 5]
 +----------------+         5 |  e  -> [4 5 6]       5 | [d e] -> [4 5]
                            6 |  f  -> [6]           6 | [e f] -> [5 6]
                            7 |  g  -> []


--H=<graph Vertices Edges>------------------------------------------------
                            Vertices                  Edges
                           ----------                -------
                         hash | key -> value      hash |  key  -> value
 +-- Hypergraph H----+   -----+------>--------    -----+-------->---------
 |                   |      1 |  a  -> [1]           1 | [a b     [1 2
 |        +------+   |      2 |  b  -> [1]             |  c d  ->  3 4
 | +------+------+   |      3 |  c  -> [1]             |  e f]     5 6]
 | |a b c |d e f |   |      4 |  d  -> [1 2]           |
 | +------+------+   |      5 |  e  -> [1 2]         2 | [d e     [4 5
 |        |g h i | j |      6 |  f  -> [1 2]           |  f g  ->  6 7
 |        +------+   |      7 |  g  -> [2]             |  h i]     8 9]
 |                   |      8 |  h  -> [2]
 +-------------------+      9 |  i  -> [2]
                           10 |  j  -> []

V = # of vertices
E = # of edges
M = # of vertex edge associations

size = size of all vertices +            all vertices stored in Vertices dict
       M * sizeof(int) * 4 +             indices into Vertices & Edge dicts
       V * sizeof(dict entry) +          storage in the Vertex dict
       E * sizeof(dict entry) +          storage in the Edge dict
       2 * sizeof(dict)                  the Vertices and Edge dicts

*** Code: *\
(require dict)
(require sequence)

(datatype graph
  Vertices : dictionary;
     Edges : dictoinary;
  ===================
  (vector symbol Vertices Edges);)

(define graph?
  X -> (= graph (<-address X 0)))

(define make-graph
  \* create a graph with specified sizes for the vertex dict and edge dict *\
  {number --> number --> graph}
  Vertsize Edgesize ->
    (let Graph (absvector 3)
      (do (address-> Graph 0 graph)
          (address-> Graph 1 (dict Vertsize))
          (address-> Graph 2 (dict Edgesize))
          Graph)))

(defmacro graph-macro
  \* return a graph taking optional sizes for the vertex and edge dicts *\
  [graph] -> [make-graph 1024 1024]
  [graph N] -> [make-graph N 1024]
  [graph N M] -> [make-graph N M])

(define vert-dict Graph -> (<-address Graph 1))

(define edge-dict Graph -> (<-address Graph 2))

(define vertices
  {graph --> (list A)}
  Graph -> (keys (vert-dict Graph)))

(define edges
  {graph --> (list (list A))}
  Graph -> (keys (edge-dict Graph)))

(define add-vertex
  \* add a vertex to a graph *\
  {graph --> A --> B --> A}
  Graph V Data -> (let Edges (trap-error (snd (<-dict (vert-dict Graph) V))
                                         (/. E {}))
                    (dict-> (vert-dict Graph) V (@p Data []))))

(define update-vert
  {vector --> (@p number number) --> A --> number}
  Vs Edge V -> (let Store (<-address Vs 2)
                    N (hash V (limit Store))
                    VertLst (trap-error (<-vector Store N) (/. E []))
                    Contents (trap-error (<-dict Vs V) (/. E (@p true [])))
                 (do (dict-> Vs V (@p (fst Contents)
                                      (adjoin Edge (snd Contents))))
                     (@p N (length VertLst)))))

(define add-edge
  \* add an edge to a graph *\
  {graph --> (list A) --> B --> (list A)}
  Graph Edge Data ->
  (let Edges (edge-dict Graph)
    (if (key? Edges Edge)
        Edge
        (let Store (<-address Edges 2)
             EdgeID (hash Edge (limit Store))
             EdgeLst (trap-error (<-vector Store EdgeID) (/. E []))
             Verts (vert-dict Graph)
             Places (map (update-vert Verts (@p EdgeID (length EdgeLst))) Edge)
          (do (dict-> Edges Edge [Places|EdgeLst]) Edge)))))

(define has-edge?
  {graph --> (list A) --> boolean}
  Graph Edge -> (key? (edge-dict Graph) Edge))

(define has-vertex
  {graph --> A --> boolean}
  Graph Vertex -> (key? (vert-dict Graph) Vertex))

(define resolve
  {(vector (list A)) --> (@p number number) --> A}
  Vector (@p Index Place) -> (nth (+ 1 Place) (<-vector Vector Index)))

(define resolve-vert
  {graph --> (@p number number) --> A}
  Graph Place -> (resolve (<-address (vert-dict Graph) 2) Place))

(define resolve-edge
  {graph --> (@p number number) --> A}
  Graph Place -> (resolve (<-address (edge-dict Graph) 2) Place))

(define edges-for
  {graph --> A --> (list (list A))}
  Graph Vert -> (map (/. X (fst (resolve-edge Graph X)))
                     (snd (<-dict (vert-dict Graph) Vert))))

(define neighbors
  \* Return the neighbors of a vertex *\
  {graph --> A --> (list A)}
  Graph Vert -> (unique (mapcon (remove-first Vert) (edges-for Graph Vert))))

(define connected-to-
  {graph --> (list A) --> (list A) --> (list A)}
  Graph [] Already -> Already
  Graph New Already ->
  (let Reachable (unique (mapcon (neighbors Graph) New))
       New (difference Reachable Already)
    (connected-to- Graph New (append New Already))))

(define connected-to
  \* return all vertices connected to the given vertex, including itself *\
  {graph --> A --> (list A)}
  Graph V -> (connected-to- Graph [V] [V]))

(define connected?
  \* return if a graph is fully connected *\
  {graph --> boolean}
  Graph -> (reduce (/. V Acc
                       (and Acc
                            (subset? (vertices Graph) (connected-to Graph V))))
                   true (vertices Graph)))

(define connected-components-
  \* given a graph return a list of connected components *\
  {graph --> (list A) --> (list (list A)) --> (list graph)}
  Graph [] _ -> []
  Graph VS [] -> (map (/. V (let Component (graph 1 0)
                              (do (add-vertex Component V true) Component)))
                      VS)
  Graph [V|VS] ES ->
    (let Con-verts (connected-to Graph V)
         Con-edges (filter (/. E (subset? E Con-verts)) ES)
         Component (graph (length Con-verts) (length Con-edges))
      (do (map (/. X (add-edge Component X true)) Con-edges)
          (cons Component (connected-components- Graph
                                                 (difference VS Con-verts)
                                                 (difference ES Con-edges))))))

(define connected-components
  {graph --> (list graph)}
  Graph -> (connected-components- Graph (vertices Graph) (edges Graph)))

(define place-vertex
  \* given a graph, vertex and list of vertex partitions, partition the vertex *\
  {graph --> A --> (list (list A)) --> (list (list A))}
  Graph V [] -> (if (element? V (neighbors Graph V))
                    (simple-error
                     (make-string "self-loop ~S, no vertex partition" V))
                    [[V]])
  Graph V [C|CS] -> (let Neighbors (neighbors Graph V)
                      (if (element? V Neighbors)
                          (simple-error
                           (make-string "self-loop ~S, no vertex partition" V))
                          (if (empty? (intersection C Neighbors))
                              [[V|C]|CS]
                              [C|(place-vertex Graph V CS)]))))

(define vertex-partition
  \* partition the vertices of a graph *\
  {graph --> (list (list A))}
  Graph -> (reduce (place-vertex Graph) [] (vertices Graph)))

(define bipartite?
  \* check if a graph is bipartite *\
  {graph --> boolean}
  Graph -> (= 2 (length (vertex-partition Graph))))

\* simple tests

(set g (graph))
(add-edge (value g) [chris patton] true)
(add-edge (value g) [eric chris] true)
(add-vertex (value g) nobody true)
(has-edge? (value g) [patton chris])
(edges-for (value g) chris)
(neighbors (value g) chris)
(neighbors (value g) nobody)
(connected-to (value g) chris)
(connected? (value g))
(connected-components (value g))
(map (function vertices) (connected-components (value g)))

*\
