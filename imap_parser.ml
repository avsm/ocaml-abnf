(** Copyright (c) 2008,2009 Anil Madhavapeddy <anil@recoil.org>
 ** See the COPYING file included in this distribution for licensing details *)

open Printf

type t = {
    buf: string;
    pos: int;
}

let t buf = { buf=buf; pos=0 }

(* Apply a function to a parser state *)
let apply fn t =
    fn t.buf.[t.pos]

(* Move the parser forward *)
let advance p n =
    eprintf "advance: %d (%s) by %d\n" p.pos p.buf n;
    { p with pos=p.pos+n }

(* Consume one terminal and advance parser *)
let one testfn charfn p =
    if apply testfn p then begin 
        let c = apply charfn p in
        Some (advance p 1, c)
    end
    else None

(* Keep grabbing from the fn until the test fails, 
   and return the list *)
exception Repeat_failed
exception Repeat_done
let repeat ~min ~max fn p =
    let p = ref p in
    let ret = ref [] in
    try
        for i = 0 to min do
            match fn !p with
            |Some (p',x) -> ret := x :: !ret; p := p'
            |None -> raise Repeat_failed
        done;
        begin match max with
        |0 (* continue until it fails *) ->
            while true do
                match fn !p with
                |Some (p',x) -> ret := x :: !ret; p := p'
                |None -> raise Repeat_done
            done
        |max -> (* maximum number *)
            for i = min to max do
                match fn !p with
                |Some (p',x) -> ret := x :: !ret; p := p'
                |None -> raise Repeat_done
            done
        end; None
    with
    |Repeat_failed -> None
    |Repeat_done -> Some (!p, List.rev !ret)
