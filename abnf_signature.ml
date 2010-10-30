(*
 * Copyright (c) 2010 Thomas Gazagnaire <thomas@gazagnaire.org>
 * See the COPYING file included in this distribution for licensing details
 *)

open Abnf_syntaxtree
open Printf

type t =
  | T_char
  | T_string
  | T_constant of string
  | T_var of string
  | T_mu of string * t
  | T_tuple of t list
  | T_sum of t list
  | T_list of t
  | T_array of t

let rec pp = function
  | T_char -> "CHAR"
  | T_string -> "STRING"
  | T_constant s -> sprintf "CONSTANT(%s)" s
  | T_var s -> sprintf "VAR(%s)" s
  | T_mu (s, t) -> sprintf "MU(%s, %s)" s (pp t)
  | T_tuple s -> sprintf "TUPLE(%s)" (String.concat ", " (List.map pp s))
  | T_sum s -> sprintf "SUM(%s)" (String.concat ", " (List.map pp s))
  | T_list s -> sprintf "LIST(%s)" (pp s)
  | T_array s -> sprintf "ARRAY(%s)" (pp s)

let is_char = function
  | T_mu (_, T_char)
  | T_char -> true
  | T_constant str when String.length str = 1 -> true
  | _ -> false
 
let is_constant = function
  | T_mu (_, T_constant _)
  | T_constant _ -> true
  | _ -> false

let string_of_constant = function
  | T_mu (_, T_constant u)
  | T_constant u -> u
  | _ -> failwith "string_of_constant"

let is_sum = function
  | T_mu (_, T_sum _)
  | T_sum _ -> true
  | _ -> false

let is_nice_variant = function
  | T_tuple ( T_constant u :: _ )
  | T_tuple ( T_var u :: _ )
  | T_tuple ( T_mu (u, _) :: _ )
  | T_constant u
  | T_mu (u, _)
  | T_var u -> true
  | _ -> false

let is_nice_sum s =
  let l = ref [] in
  let aux = function
  | T_var u | T_mu (u,_)
      when not (List.mem u !l) ->
    l := u :: !l;
    true
  | T_var u | T_mu (u,_) -> false
  | x -> is_nice_variant x in
  List.for_all aux s

let rec fold_left f init t =
  let res = f init t in
  match t with
    | T_char | T_string | T_constant _ | T_var _ | T_mu _ -> res
    | T_tuple tl | T_sum tl -> List.fold_left f res tl
    | T_list t | T_array t -> f res t

let exists f t =
  fold_left (fun accu t -> accu || f t) false t

type decl = {
  name : string;
  t    : t;
}

type env = {
  rules       : (string, rule) Hashtbl.t;
  mutable top : string list; (* Top-level rule name(s) *)
}

(* XXX: my first guess was to add all the top nodes + nodes corresponding to
   variant types; however this doesn't work because of possible cycles.
   So need to think a bit more about that ... *)
let make_env rules =
  (* XXX: let top = Abnf_ops.Graph.top_nodes rules in *)
  let top = Abnf_ops.Graph.topological_sort rules in
  {
    rules = rules;
    top   = top;
  }

let find_rule env name =
  Hashtbl.find env.rules name

let is_toplevel env name =
  List.mem name env.top

let add_toplevel env name =
  if not (List.mem name env.top) then
    env.top <- name :: env.top

let t_of_terminal = function
  | UPALPHA
  | LOALPHA
  | ALPHA
  | BIT 
  | CHAR
  | CR
  | CTL
  | DIGIT
  | DQUOTE
  | HEXDIGIT
  | HTAB
  | LF
  | OCTET
  | SP
  | VCHAR
  | WSP  -> T_char
  | LWSP
  | CRLF -> T_string

let t_of_alt r s = match r,s with
  | T_sum u, T_sum v when List.for_all is_char u && List.for_all is_char v ->
    T_char
  | T_sum u, T_sum v ->
    T_sum (u @ v)

  | T_sum u, v when List.for_all is_char u && is_char v ->
    T_char
  | T_sum u, v ->
    T_sum (u @ [v])

  | u, T_sum v when is_char u && List.for_all is_char v ->
    T_char
  | u, T_sum v ->
    T_sum (u :: v)

  | u, v when is_char u && is_char v ->
    T_char
  | u, v ->
    T_sum [u; v]

let rec t_of_rule ?root env = function
  | S_terminal t             -> t_of_terminal t
  | S_string s               -> T_constant s
  | S_concat (r,s)           ->
    (match t_of_rule ?root env r, t_of_rule ?root env s with
      | T_tuple u, T_tuple v -> T_tuple (u @ v)
      | T_tuple u, v         -> T_tuple (u @ [v])
      | u        , T_tuple v -> T_tuple (u :: v)
      | T_char   , T_char
      | T_char   , T_string
      | T_string , T_char    -> T_string
      | u        , v         -> T_tuple [u; v])
  | S_reference str ->
    if root = Some str then
      T_var str
    else if is_toplevel env str then begin
      let new_env = { env with top = [ str ] } in
(*      eprintf "REF: %s (%s)\n" str (String.concat ", " new_env.top); *)
      let t = t_of_rule ~root:str new_env (find_rule env str) in
      T_mu (str, t)
    end else begin
      let new_env = if root <> None then { env with top = str :: env.top } else env in
(*      eprintf "VAR: %s (%s)\n" str (String.concat ", " new_env.top); *)
      t_of_rule ?root new_env (find_rule env str)
    end
  | S_alt (r,s)              -> t_of_alt (t_of_rule ?root env r) (t_of_rule ?root env s)
  | S_bracket r              -> t_of_rule ?root env r
  | S_element_list (_,None,r)
  | S_repetition (_,None,r)  ->
    (match t_of_rule ?root env r with
       | T_char -> T_string
       | t      -> T_list t)
  | S_element_list (_,_,r)
  | S_repetition (_,_,r)     ->
    (match t_of_rule ?root env r with
       | T_char -> T_string
       | t      -> T_array t)
  | S_hex_range _            -> T_char
  | S_any_except (r,_)       ->
    (match t_of_rule ?root env r with
       | T_char -> T_string
       | t      -> T_list t)

(* WARNING: This function modifies the environnement *)
let decl_of_rd env name rule =
  let t = t_of_rule env rule in
  (* XXX: If the rule is a variant, then add it the the top-level rules *)
  (* if not (is_toplevel env name) && is_sum t then
     add_toplevel env name; *)
  {
    name = name;
    t    = t;
  }

let ocamlify name =
  (* Need to copy the string if it is used by someone else; but don't think it's the case here *)
  for i = 0 to String.length name - 1 do
    if name.[i] >= 'A' && name.[i] < 'Z' then
      name.[i] <- Char.lowercase name.[i]
    else if name.[i] = '-' then
      name.[i] <- '_'
  done;
  name

let skip_constant l = List.filter (fun x -> not (is_constant x)) l

let string_of_nice_variant ss = function
  | T_tuple (T_constant u :: v)
  | T_tuple (T_mu (u,_) :: v)  ->
    (match List.map ss (skip_constant v) with
       | []    -> sprintf "'%s" (ocamlify u)
       | args  -> sprintf "'%s of %s" (ocamlify u) (String.concat " * " args))
  | T_constant u ->
    sprintf "'%s" (ocamlify u)
  | T_mu (u,t) ->
    if is_sum t then
      sprintf "%s" (ocamlify u)
    else
      sprintf "'%s of %s" (ocamlify u) (ocamlify u)
  | _ ->
    failwith "string_of_nice_sum is not up-to-date with is_nice_sum"

let string_of_nice_sum ss s =
  sprintf "[ %s ]" (String.concat "  | " (List.map (string_of_nice_variant ss) s))

(* XXX: need to verify that there is no name clashes with variant names *)
let string_of_ugly_sum ss s =
  let aux (i, l) s =
    let r =
      if is_nice_variant s then
        string_of_nice_variant ss s
      else
        if is_constant s then
          sprintf "%s\n" (ocamlify (string_of_constant s))
        else
          sprintf "`a%i of %s\n" i (pp s) in
    (i+1, r :: l) in
  let _, l = List.fold_left aux (1,[]) s in
  sprintf "[ %s ]" (String.concat "  | " (List.rev l))

let rec string_of_t = function
  | T_char       -> "char"
  | T_string     -> "string"
  | T_constant _ -> "unit" (* constant are skipped *)
  | T_mu (v,_)   -> ocamlify v
  | T_var v      -> ocamlify v
  | T_list l     -> sprintf "%s list" (string_of_t l)
  | T_array a    -> sprintf "%s array" (string_of_t a)

  | T_tuple t    ->
    (* XXX: here we can do better in some cases, ie. create a record if the names are meanigful *)
    sprintf "(%s)" (String.concat " * " (List.map string_of_t (skip_constant t)))

  | T_sum s when is_nice_sum s ->
    string_of_nice_sum string_of_t s

  | T_sum s ->
    string_of_ugly_sum string_of_t s

let string_of_decl d =
  Printf.sprintf "%s = %s" (ocamlify d.name) (string_of_t d.t)

let to_string rules =
  let env   = make_env rules in
  let decls = Hashtbl.fold (fun name r accu -> decl_of_rd env name r :: accu) rules [] in
  let aux d =
    if is_toplevel env d.name then
      string_of_decl d
    else
      "" in
  sprintf "type %s" (String.concat "\n and " (List.map aux decls))

let dump rules =
  printf "(* autogenerated file *)\n\n%s" (to_string rules)
