open! Core
open! Core
module City = String

module Interstate = struct
  include String

  let default = ""
end

let replace_period c = match c with '.' -> 'm' | ' ' -> 'm' | _ -> c

(* We separate out the [Network] module to represent our social network in
   OCaml types. *)
module Network = struct
  (* We can represent our social network graph as a set of connections, where
     a connection represents a friendship between two people. *)
  module Connection = struct
    module T = struct
      type t = City.t * Interstate.t * City.t [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s.
       This is needed to defined our [Network.t] type later. Using this
       [Comparable.Make] functor also gives us immutable maps, which might
       come in handy later. *)
    include Comparable.Make (T)

    let of_string s =
      String.split s ~on:',' |> List.map ~f:(fun elem -> City.of_string elem)
    ;;
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  let of_file input_file =
    let connections =
      let lines = In_channel.read_lines (File_path.to_string input_file) in
      List.concat
        (List.concat_map lines ~f:(fun string ->
           match Connection.of_string string with
           | inter :: sec :: tail ->
             let get_tups i b =
               let interstate = Interstate.of_string inter in
               let a = List.nth_exn (sec :: tail) i in
               let a = String.map a ~f:replace_period in
               let b = String.map b ~f:replace_period in
               [ a, interstate, b ]
             in
             List.mapi tail ~f:get_tups
           | _ :: [] | [] ->
             printf
               "ERROR: Could not parse line as connection; dropping. %s\n"
               string;
             []))
    in
    Connection.Set.of_list connections
  ;;
end

(* In order to visualize the social network, we use the ocamlgraph library to
   create a [Graph] structure whose vertices are of type [Person.t].

   The ocamlgraph library exposes lots of different ways to construct
   different types of graphs. Take a look at
   https://github.com/backtracking/ocamlgraph/blob/master/src/imperative.mli
   for documentation on other types of graphs exposed by this API. *)
module G =
  Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (City) (Interstate)

(* We extend our [Graph] structure with the [Dot] API so that we can easily
   render constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
  include G

  (* These functions can be changed to tweak the appearance of the generated
     graph. Check out the ocamlgraph graphviz API
     (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
     for examples of what values can be set here. *)
  let edge_attributes _ = [ `Dir `None ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let network = Network.of_file input_file in
        (* This special syntax can be used to easily sexp-serialize values
           (whose types have [sexp_of_t] implemented). *)
        printf !"%{sexp: Network.t}\n" network]
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let network = Network.of_file input_file in
        let graph = G.create () in
        Set.iter network ~f:(fun g -> G.add_edge_e graph g);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
