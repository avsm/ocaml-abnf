exception Parse_error of string
module Terminal :
  sig
    module DIGIT :
      sig
        type t 
        val of_char : char -> t
        val of_string : string -> t
        val to_string : t -> string
        val to_int : t -> int
        val test : char -> bool
        val one : Imap_parser.t -> (Imap_parser.t * t) option
      end
    module SP :
      sig
        type t
        val test : char -> bool
        val of_char : char -> t
        val to_string : t -> string
        val one : Imap_parser.t -> (Imap_parser.t * t) option
      end
    module DQUOTE :
      sig
        type t
        val test : char -> bool
        val of_char : char -> t
        val to_string : t -> string
        val one : Imap_parser.t -> (Imap_parser.t * t) option
      end
    module ALPHA :
      sig
        type t
        val test : char -> bool
        val of_char : char -> t
        val to_string : t -> string
        val one : Imap_parser.t -> (Imap_parser.t * t) option
      end
    module CHAR :
      sig
        type t
        val test : char -> bool
        val of_char : char -> t
        val to_string : t -> string
        val one : Imap_parser.t -> (Imap_parser.t * t) option
      end
    module CTL :
      sig
        type t
        val test : char -> bool
        val of_char : char -> t
        val to_string : t -> string
        val one : Imap_parser.t -> (Imap_parser.t * t) option
      end
    module CRLF :
      sig
        type t
        val test : char -> bool
        val of_char : char -> t
        val to_string : t -> string
        val one : Imap_parser.t -> (Imap_parser.t * t) option
      end
    type t =
        CRLF
      | CTL of CTL.t
      | CHAR of CHAR.t
      | ALPHA of ALPHA.t
      | DQUOTE
      | SP
      | DIGIT of DIGIT.t
  end
