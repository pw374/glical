(* -*- coding: utf-8; -*- *)
(* ********************************************************************* *)
(* glical: A library to glance at iCal data using OCaml                  *)
(* ********************************************************************* *)
(* (c) 2013/2014, Philippe Wang <philippe.wang@cl.cam.ac.uk>             *)
(* ********************************************************************* *)
(* Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   THE SOFTWARE IS PROVIDED “AS IS” AND ISC DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL ISC BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY
   DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
   WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
   ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
   OF THIS SOFTWARE.                                                     *)
(* ********************************************************************* *)

(**
   Important note: this library needs OCaml >= 4.1.0

*)

module Ical : sig
  type 'a t = ([> `Raw of location * string ] as 'a) element list 
  and 'a element =
    | Block of location * name * 'a t
    | Assoc of location * key * ([> `Raw of location * string ] as 'a)
  and location = int * int
  and name = string
  and key = string
end

type location = Ical.location
and name = Ical.name
and key = Ical.key


(* Syntax errors *)
(* ******************************************************************** *)
exception Syntax_error of string
val syntax_error : string -> int -> int -> 'a
val syntax_assert : bool -> string -> int -> int -> unit
(* ******************************************************************** *)


(* Lexing *)
(* ******************************************************************** *)
type line = {
  name : string;
  value : string;
  name_start : int * int;
  value_start : int * int;
  value_end : int * int;
}

val lex_ical : string -> line list
(* ******************************************************************** *)

(* Parsing *)
(* ******************************************************************** *)
val parse_ical : line list -> [> `Raw of location * string ] Ical.t
(* ******************************************************************** *)

(* Data processing *)
(* ******************************************************************** *)
val tree_map :
  (([> `Raw of location * string ] as 'a) ->
   ([> `Raw of location * string ] as 'b)) ->
  'a Ical.t -> 'b Ical.t
val tree_transform :
  (([> `Raw of location * string ] as 'a) Ical.element ->
   ([> `Raw of location * string ] as 'b) Ical.element) ->
  'a Ical.t -> 'b Ical.t
(* ******************************************************************** *)

(* Value formats *)
(* ******************************************************************** *)
val text_of_raw :
  ([> `Raw of (int * int) * string
   | `Text of (int * int) * string list ] as 'a)
  -> 'a


module Datetime :
  sig
    type 'a timezone = 'a
      constraint 'a = [> `Local | `String of string | `UTC ]
    type 'a t = {
      timezone : 'a timezone;
      year : int;
      month : int;
      day : int;
      hours : int;
      minutes : int;
      seconds : int;
    } constraint 'a = [> `Local | `String of string | `UTC ]
    val validate : [ `Local | `String of string | `UTC ] t -> bool
    val to_string : [ `Local | `String of string | `UTC ] t -> string
    val parse :
      int * int -> string -> [> `Local | `String of string | `UTC ] t
    val parse_datetime :
      ([> `Datetime of [> `Local | `String of string | `UTC ] t
       | `Raw of location * string
       | `Text of string ]
       as 'a) Ical.t -> 'a Ical.t
  end
module Date :
  sig
    type t = { year : int; month : int; day : int; }
    val validate : t -> bool
    val to_string : t -> string
    val parse : int * int -> string -> t
  end
(* ******************************************************************** *)


(* Formatting *)
(* ******************************************************************** *)
val limit_lines_to_75_bytes : string -> string

(* To string *)
(* ******************************************************************** *)
val to_string :
  ('a -> string) -> [> `Raw of location * string | `Text of string ] 
    Ical.t -> string


(* ******************************************************************** *)
