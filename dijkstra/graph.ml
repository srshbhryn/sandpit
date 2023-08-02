
include Set
include Stdlib

module IntSet = Set.Make(Int)
module IntSetSet = Set.Make(IntSet)

include IntSet
include IntSetSet

type graph = {vertices : IntSet.t; edges: IntSetSet.t}

let empty_graph = {vertices = IntSet.empty; edges = IntSetSet.empty}


let add_vertex g v =  {vertices = IntSet.add v g.vertices; edges = g.edges}

exception EdgeShouldBeSubsetOfVertices

exception NoLoopForSimpleGraph

let add_edge g u v = match (u == v, not (IntSet.mem u g.vertices) || not (IntSet.mem u g.vertices)) with
  | (true, _) -> raise NoLoopForSimpleGraph
  | (false, true) -> raise EdgeShouldBeSubsetOfVertices
  | _ -> {vertices = g.vertices; edges = IntSetSet.add (IntSet.add v (IntSet.singleton u)) g.edges}

let is_edge g u v = match (u == v, not (IntSet.mem u g.vertices) || not (IntSet.mem u g.vertices)) with
  | (true, _) -> raise NoLoopForSimpleGraph
  | (false, true) -> raise EdgeShouldBeSubsetOfVertices
  | _ -> IntSetSet.mem (IntSet.add v (IntSet.singleton u)) g.edges

let neighbors g v = IntSet.filter (is_edge g v) g.vertices
