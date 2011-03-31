
val encode_unsafe_char : string -> string
(** The encoder maps strings to HTML and {e must} encode the unsafe characters
    ['<'], ['>'], ['"'], ['&'] and the control characters 0-8, 11-12, 14-31, 127
    to HTML entities.  [encode_unsafe] is the default for [?encode] in [output]
    and [pretty_print] below.  Other implementations are provided by the module
    [Netencoding] in the
    {{:http://www.ocaml-programming.de/programming/ocamlnet.html}OcamlNet}
    library, e.g.:
    [let encode = Netencoding.Html.encode ~in_enc:`Enc_iso88591 ~out_enc:`Enc_usascii ()],
    Where national characters are replaced by HTML entities.
    The user is of course free to write her own implementation.
    @see <http://www.ocaml-programming.de/programming/ocamlnet.html> OcamlNet *)

val encode_unsafe_char_and_at : string -> string
(** In addition, encode ["@"] as ["&#64;"] in the hope that this will fool
    simple minded email address harvesters. *)

val compose_decl : ?version:string -> ?encoding:string -> unit -> string
(** [encoding] is the name of the character encoding, e.g. ["US-ASCII"] *)

val compose_doctype : string -> string list -> string

module Make(XML : XML_sigs.Iterable)
           (I : XML_sigs.Info)
           (O : XML_sigs.Output)
     : XML_sigs.Printer(XML)(O).T

module MakeTyped(XML : XML_sigs.Iterable)
                (TypedXML : XML_sigs.TypedXML(XML).T)
                (O : XML_sigs.Output)
     : XML_sigs.TypedPrinter(XML)(TypedXML)(O).T

module MakeSimple(XML : XML_sigs.Iterable)(F : XML_sigs.Info)
     : XML_sigs.SimplePrinter(XML).T

module MakeTypedSimple(XML : XML_sigs.Iterable)
                      (TypedXML : XML_sigs.TypedXML(XML).T)
     : XML_sigs.TypedSimplePrinter(XML)(TypedXML).T
