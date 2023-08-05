include Set
include Stdlib

module IntSet = Set.Make(Int)
module IntSetSet = Set.Make(IntSet)

include IntSet
include IntSetSet

type graph = {vertices: IntSet.t; edges: IntSetSet.t}

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


let rec set_neighbors g v_set = if  v_set = IntSet.empty
  then IntSet.empty
  else let e = IntSet.choose v_set in IntSet.union (neighbors g e) (set_neighbors g (IntSet.remove e v_set))

let has_neighbors_in g v_set u = u |> neighbors g |> IntSet.inter v_set |> IntSet.is_empty |> not


let a_u_neighber_in_a_set g u set = IntSet.find_first (is_edge g u)

