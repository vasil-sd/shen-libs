\* graph.shen --- functions for manipulating graphs

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

(2-) (set g (@p [1 2 3 4 5 6] [[1 2 3] [1 2] [2 3] [2 4] [2 5] [1 1]]))
(@p [1 2 3 4 5 6] [[1 2 3] [1 2] [2 3] [2 4] [2 5] [1 1]])

(3-) (neighbors (value g) 1)
[2 3]

(4-) (connected-to (value g) 1)
[4 5 2 3 1]

*** Code: *\
(load "../sequence/sequence.shen")

\*** Types ***\
(datatype hypergraph
  \* vertices are a set and edges are subset of the power set P(V) *\
  Vertices : (list A); Edges : (list (list A));
  ==============================
  (@p Vertices Edges) : hypergraph;)

(datatype edge
  ____________
  [] : (edge 0);

  Edge : (edge N);
  ==================================
  [Vertex | Edge] : (edge (succ N));)

(datatype graph
  Vertices : (list A); Edges : (list (edge (succ (succ 0))));
  ===========================================================
  (@p Vertices Edges) : graph;)

(define vertices
  \* Return a list of vertices *\
  {hypergraph --> (list A)}
  (@p V _) -> V)

(define edges
  \* Return a list of edges *\
  {hypergraph --> (list (list A))}
  (@p _ E) -> E)

(define neighbors
  \* Return the neighbors of a vertex *\
  {hypergraph --> A --> (list A)}
  (@p VS ES) V -> (unique (mapcon (remove-first V) (filter (element? V) ES))))

(define connected-to-
  {hypergraph --> (list A) --> (list A) --> (list A)}
  Graph [] Already -> Already
  Graph New Already ->
  (let Reachable (unique (mapcon (neighbors Graph) New))
       New (difference Reachable Already)
    (connected-to- Graph New (append New Already))))

(define connected-to
  \* return all vertices connected to the given vertex, including itself *\
  {hypergraph --> A --> (list A)}
  Graph V -> (connected-to- Graph [V] [V]))

(define connected?
  \* return if a graph is fully connected *\
  {hypergraph --> boolean}
  Graph -> (reduce (/. V Acc
                       (and Acc
                            (subset? (vertices Graph) (connected-to Graph V))))
                   true (vertices Graph)))

(define connected-components
  \* given a graph return a list of connected components *\
  {hypergraph --> (list hypergraph)}
  (@p [] _)  -> []
  (@p VS []) -> (map (lambda V (@p [V] [])) VS)
  (@p [V|VS] ES) ->
    (let Con-verts (connected-to (@p [V|VS] ES) V)
         Con-edges (filter (/. E (subset? E Con-verts)) ES)
      (cons (@p Con-verts Con-edges)
            (connected-components (@p (difference VS Con-verts)
                                      (difference Con-edges ES))))))

(define place-vertex
  \* given a graph, vertex and list of vertex partitions, partition the vertex *\
  {hypergraph --> A --> (list A) --> (list A)}
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
  {hypergraph --> (list (list A))}
  Graph -> (reduce (place-vertex Graph) [] (vertices Graph)))

(define bipartite?
  \* check if a graph is bipartite *\
  {hypergraph --> boolean}
  Graph -> (= 2 (length (vertex-partition Graph))))
