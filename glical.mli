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
(** Type to represent a line. *)
type line = {
  name : string;
  value : string;
  name_start : int * int;
  value_start : int * int;
  value_end : int * int;
}

(** [lex_ical s] reads iCal data from the string [s] and returns a
    list of [line]s. A [line] is not series of bytes separated by some
    CRLF in [s] but a line in what the contents of [s] represents. *)
val lex_ical : string -> line list
(* ******************************************************************** *)

(* Parsing *)
(* ******************************************************************** *)
(** [parse_ical l] returns the iCalendar tree that's encoded in [l] *)
val parse_ical : line list -> [> `Raw of location * string ] Ical.t
(* ******************************************************************** *)

(* Data processing *)
(* ******************************************************************** *)
(** [tree_map f ical] is a map over [ical], where [f] is applied to
    each pair of "key x value". Locations are preserved and cannot be
    changed. *)
val tree_map :
  (
    string -> 
    ([> `Raw of location * string ] as 'a) ->
    (string * ([> `Raw of location * string ] as 'b))
  ) -> 'a Ical.t -> 'b Ical.t

(** [tree_transform] is like [tree_map] except that the function
    is applied to the whole [Assoc(_)] node. *)
val tree_transform :
  (([> `Raw of location * string ] as 'a) Ical.element ->
   ([> `Raw of location * string ] as 'b) Ical.element) ->
  'a Ical.t -> 'b Ical.t
(* ******************************************************************** *)

(* Value formats *)
(* ******************************************************************** *)
(** [text_of_raw label value] converts [`Raw(loc, s)] to [`Text(loc,sl)]
    where [sl] is the list of text values in [s]. Note that it is a list
    because in iCalendar a label can be associated with multiple values.
    Note that [loc] isn't changed, and [sl] is a [string list].
    This conversion interpretes backslash-escaped characters and
    commas, the latter are used for separating multiple values.
    (http://tools.ietf.org/html/rfc5545#section-3.3.11) *)
val text_of_raw :
  string ->
  ([> `Raw of (int * int) * string
   | `Text of (int * int) * string list ] as 'a)
  -> string * 'a


(** A module to represent date-time values. Since there are way
    too many possible values for timezones, this modules distinguishes
    3 possibilities: [`Local] represents localtime, in practice this
    means that there's no timezone indicator; [`UTC] represents UTC,
    in practice it means that the values ens with an additional Z character
    if you compare to localtime; and [`String s] where the timezone
    is encoded in the string [s]. [`String s] is used when the date-time
    value has a field "TZID=".

   (http://tools.ietf.org/html/rfc2445#section-4.3.5) *)
module Datetime :
  sig
    type 'a t = {
      timezone : 'a timezone;
      year : int;
      month : int;
      day : int;
      hours : int;
      minutes : int;
      seconds : int;
    } constraint 'a = [> `Local | `String of string | `UTC ]
    and 'a timezone = 'a
      constraint 'a = [> `Local | `String of string | `UTC ]

    (** [validate s] returns [true] if the format of [s] satisfies a 
        set of criteria (which is not enough to ensure that [s] is valid
        but a lot of errors can be detected). 
        Otherwise it returns [false]. *)
    val validate : [ `Local | `String of string | `UTC ] t -> bool

    (** [to_string] performs a conversion to a string *)
    val to_string : [ `Local | `String of string | `UTC ] t -> string

    (** [parse loc s] extract a date-time from [s]. If it fails to
        read a date-time, it raises a [Syntax_error _] exception.
        Note that [loc] is only used to know the location when the
        parsing fails. *)
    val parse :
      location -> string -> [> `Local | `String of string | `UTC ] t

    (** [parse_datetime ical] applies [parse] to each element
        of [ical] that satisfies the following pattern
        [("DTSTAMP", (`Text [_] | `Raw _))] *)
    val parse_datetime :
      ([> `Datetime of [> `Local | `String of string | `UTC ] t
       | `Raw of location * string
       | `Text of string list ]
       as 'a) Ical.t -> 'a Ical.t
  end


(** 
(http://tools.ietf.org/html/rfc2445#section-4.3.4)
*)
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
(** [ical_format s] returns a string that satisfies the format contraints
    of iCalendar: lines are limited to at most 75 bytes
    (http://tools.ietf.org/html/rfc5545#section-3.1),
    and some characters are backslash-escaped
    (http://tools.ietf.org/html/rfc5545#section-3.3.11). *)
val ical_format : string -> string

(* To string *)
(* ******************************************************************** *)
(** [to_string f ical] returns the string that represents [ical].
    [ical] shall have any value of type 
    [> `Raw of location * string | `Text of string ]
    union the type of any value that the function [f] can convert to string.
    Elements that couldn't be properly converted to a string are converted
    to the empty string. 
    [f x] shall return [None] when [f] cannot convert [x] to a string,
    and [Some s] when [s] is the string representation for [x]; and it
    would be very wrong to return [Some ""] when the proper value should
    be [None].
    Note that [f] can override the default conversion semantics for elements of
    the type [`Raw of location * string | `Text of string ].
*)
val to_string :
  (([> ] as 'a) -> string option) ->
  ([> `Raw of location * string | `Text of string ] as 'a)
    Ical.t -> string


(* ******************************************************************** *)
