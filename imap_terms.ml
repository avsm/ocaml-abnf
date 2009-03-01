open Printf

exception Parse_error of string

module Terminal = struct
    
    module DIGIT = struct
        (* Decimal digits (0–9) *)
        type t = int

        let of_char = function
        |'\x30' -> 0 |'\x31' -> 1 |'\x32' -> 2
        |'\x33' -> 3 |'\x34' -> 4 |'\x35' -> 5
        |'\x36' -> 6 |'\x37' -> 7 |'\x38' -> 8
        |'\x39' -> 9
        |x -> raise (Parse_error (sprintf "Terminal.DIGIT.of_char %c" x))
        
        let to_int x = x
        let of_string = int_of_string
        let to_string (x:t) = string_of_int x
        
        let test = function
        |'\x30'..'\x39' -> true
        |_ -> false
        
        let one = Imap_parser.one test of_char
    end

    module SP = struct
        type t = unit
        
        let test = function
        |'\x22' -> true
        |_ -> false

        let of_char (x:char) : t = ()
        let to_string (x:t) = "SP"
        let one = Imap_parser.one test of_char
    end
    
    module DQUOTE = struct
        type t = unit
        
        let test = function
        |'\x22' -> true
        |_ -> false
            
        let of_char (x:char) : t = ()
        let to_string (x:t) = "DQUOTE"
        let one = Imap_parser.one test of_char
    end
    
    module ALPHA = struct
        type t = char
        
        let test = function
        |'\x41'..'\x5A' -> true
        |'\x61'..'\x7A' -> true
        |_ -> false
        
        let of_char (x:char) : t = x
        let to_string (x:t) = sprintf "%c" x
        let one = Imap_parser.one test of_char
    end
    
    module CHAR = struct
        type t = char

        let test = function
        |'\x01'..'\x7f' -> true
        |_ -> false

        let of_char (x:char) : t = x
        let to_string (x:t) = sprintf "%c" x
        let one = Imap_parser.one test of_char
    end
    
    module CTL = struct
        type t = char
        
        let test = function
        |'\x00'..'\x1f' | '\x7f' -> true
        |_ -> false
        
        let of_char (x:char) : t = x
        let to_string (x:t) = "CTL"
        let one = Imap_parser.one test of_char
    end

    module CRLF = struct
        type t = unit
        
        let test = function
        |'\x0D' -> true
        |'\x0a' -> true
        |_ -> false

        let of_char (x:char) : t = ()
        let to_string (x:t) = "CRLF"
        let one = Imap_parser.one test of_char
    end

    type t =
    | CRLF
    | CTL of CTL.t
    | CHAR of CHAR.t
    | ALPHA of ALPHA.t
    | DQUOTE
    | SP
    | DIGIT of DIGIT.t
    
end
