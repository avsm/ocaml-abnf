(** Copyright (c) 2008,2009 Anil Madhavapeddy <anil@recoil.org>
 ** See the COPYING file included in this distribution for licensing details *)

open Imap_parser
open Imap_terms

module Rules = struct
    module Number = struct
        (* 1*DIGIT *)
        type t = int32

        let test = Terminal.DIGIT.test
        let one p =
            (* Grab the digit characters *)
            match repeat ~min:1 ~max:0 Terminal.DIGIT.one p with
            |None -> None
            |Some (p,dl) ->
                let v = ref 0l in
                let mult = ref 1 in
                for i = List.length dl - 1 downto 0 do
                    let digit = Terminal.DIGIT.to_int (List.nth dl i) in
                    v := Int32.add !v (Int32.of_int (digit * !mult));
                    mult := !mult * 10; 
                done;
                Some (p,!v)
    end
    
    module Body_fld_lines = struct
       (* number *)
       type t = Number.t
       let test = Number.test
       let one = Number.one 
    end
    
    module Quoted_specials = struct
       (* DQUOTE / 'blackslash *)
    end
end
