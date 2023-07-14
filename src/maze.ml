open! Core
open! Core

module Space = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include Comparable.Make (T)
end

module Board = struct
  type t = char Space.Map.t [@@deriving sexp_of]

  let of_file input_file =
    let lines = In_channel.read_lines (File_path.to_string input_file) in
    let chars_to_tup (line : string) (y : int) : (int * int * char) list =
      let char_list = String.to_list line in
      List.foldi char_list ~init:[] ~f:(fun x li char -> li @ [ x, y, char ])
    in
    let position_chars =
      List.foldi
        (lines : string list)
        ~init:[]
        ~f:(fun y li st -> li @ [ chars_to_tup st y ])
      |> List.concat
    in
    List.fold position_chars ~init:Space.Map.empty ~f:(fun map position ->
      let x, y, char = position in
      Map.set map ~key:(x, y) ~data:char)
  ;;
end

(* We separate out the [Network] module to represent our social network in
   OCaml types. *)
module Connection = struct
  module T = struct
    type t = Space.T.t * Space.T.t [@@deriving compare, sexp]
  end

  (* This funky syntax is necessary to implement sets of [Connection.t]s.
     This is needed to defined our [Network.t] type later. Using this
     [Comparable.Make] functor also gives us immutable maps, which might come
     in handy later. *)
  include Comparable.Make (T)
end

module Network = struct
  (* We can represent our social network graph as a set of connections, where
     a connection represents a friendship between two people. *)

  type t = Connection.T.t list [@@deriving sexp_of]

  let of_board (board : Board.t) : Connection.T.t list =
    Map.fold board ~init:[] ~f:(fun ~key:(x, y) ~data:_d li ->
      let up = Map.find board (x, y + 1) in
      let down = Map.find board (x, y - 1) in
      let left = Map.find board (x - 1, y) in
      let right = Map.find board (x + 1, y) in
      let li =
        match up with
        | None -> li
        | Some '.' -> li @ [ (x, y), (x, y + 1) ]
        | Some 'S' -> li @ [ (x, y), (x, y + 1) ]
        | Some 'E' -> li @ [ (x, y), (x, y + 1) ]
        | _ -> li
      in
      let li =
        match down with
        | None -> li
        | Some '.' -> li @ [ (x, y), (x, y - 1) ]
        | Some 'S' -> li @ [ (x, y), (x, y - 1) ]
        | Some 'E' -> li @ [ (x, y), (x, y - 1) ]
        | _ -> li
      in
      let li =
        match left with
        | None -> li
        | Some '.' -> li @ [ (x, y), (x - 1, y) ]
        | Some 'S' -> li @ [ (x, y), (x - 1, y) ]
        | Some 'E' -> li @ [ (x, y), (x - 1, y) ]
        | _ -> li
      in
      let li =
        match right with
        | None -> li
        | Some '.' -> li @ [ (x, y), (x + 1, y) ]
        | Some 'S' -> li @ [ (x, y), (x + 1, y) ]
        | Some 'E' -> li @ [ (x, y), (x + 1, y) ]
        | _ -> li
      in
      li)
  ;;
end

let get_children (network : Network.t) (node : Space.T.t) =
  List.fold network ~init:[] ~f:(fun li (origin, dest) ->
    if Space.equal origin node then li @ [ dest ] else li)
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let board = Board.of_file input_file in
        let network = Network.of_board board in
        let start_node, _ =
          List.find_exn network ~f:(fun (start_node, _) ->
            Char.equal (Map.find_exn board start_node) 'S')
        in
        let _, end_node =
          List.find_exn network ~f:(fun (_, end_node) ->
            Char.equal (Map.find_exn board end_node) 'E')
        in
        let rec dfs curr_node (path : Space.T.t list) =
          match Space.equal curr_node end_node with
          | true -> path
          | false ->
            let children = get_children network curr_node in
            List.fold children ~init:[] ~f:(fun li space ->
              let list = dfs space li in
              li @ list)
        in
        let list = dfs start_node [] in
        print_s [%message "Path: " (list : Space.T.t list)]]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
