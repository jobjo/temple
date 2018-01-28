open Printf

type file = { name : string; text : string }

type dir = { name : string; content : t }

and t =
  | Empty
  | File of file
  | Dir of dir
  | Merge of t * t

let file ~name text = File { name; text}

let combine = List.fold_left (fun x y -> Merge (x,y)) Empty

let dir name contents = 
  let content = combine contents in
  Dir {name; content; }

type config =
  {
    make_file : file -> unit;
    make_dir  : string -> unit;
  }

let merge_path cd name = sprintf "%s/%s" cd name

let make_file cd {name; text} =
  let file = merge_path cd name in
  print_endline file;
  let oc = open_out file in
  fprintf oc "%s\n" text;
  close_out oc   
 
let is_dir dir =
  try
    Sys.is_directory dir
  with
  | _ ->
    false

let make_dir cd name = 
  let dir = merge_path cd name in
  if not @@ is_dir dir then
    Unix.mkdir dir 0o766

let create =
  let rec aux cd = function
    | Empty               -> 
      ()
    | File f              -> 
      make_file cd f
    | Dir {name; content} ->
      make_dir cd name;
      aux (merge_path cd name) content
    | Merge (r1, r2) ->
      aux cd r1;
      aux cd r2
  in
  aux "."

 (**************************************************************************
  * Example
  *************************************************************************)

type config = { library_name : string }

(*
let trim s =
  let lines = Str.split (Str.regexp "\n") s in
  let min_space ms line =
    let ls = failwith "TODO" in 
    min ms ls
  in
  let min_space = List.fold_left min_space 0 lines in
*)

let makefile { library_name} = 
  {|
    |}^library_name^{|
  |}

let repo config =
  dir "output" [
    file ~name:"Makefile" @@ makefile config;
    file ~name:"README" "Here we go";
    dir "src" [
      file ~name:"main.ml" "";
    ];
  ]

let () = 
  Sys.command "rm -rf ./output";
  let config = { library_name = "my_lib" } in
  create @@ repo config


