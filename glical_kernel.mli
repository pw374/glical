(* -*- coding: utf-8; -*- *)
(* ********************************************************************* *)
(* glical: A library to glance at iCal data using OCaml                  *)
(* (c) 2013/2014, Philippe Wang <philippe.wang@cl.cam.ac.uk>             *)
(* Licence: ISC                                                          *)
(* ********************************************************************* *)

(* A location is a line number and a column number. *)
type location = int * int

(* A name is a string. *)
and name = string

(* A key is a string. *)
and key = string


(** Module for the representation of iCalendar data. *)
module Ical : sig
  (** iCalendar data is a list of elements. *)
  type 'a t = 'a element list
    constraint 'a = [> `Raw of string ]
  (** An element is either a block, which is a characterized by a name
      and a list of elements, or a pair, which associates a key to a
      value. A value can be pretty much anything, that depends on the
      context in which it's found, however any value has a "raw"
      representation, which is the basically the bytes in the
      iCalendar data. *)
  and 'a element =
    | Block of location * name * 'a t
    | Assoc of location * key * 'a parameters * 'a value
    constraint 'a = [> `Raw of string ]
  and 'a parameters = (key * 'a value) list
    constraint 'a = [> `Raw of string ]

  and 'a value = {
    location : location;
    to_string : (unit -> string);
    to_pretty: (unit -> string);
    value : 'a;
  } constraint 'a = [> `Raw of string ]
end

(* Syntax errors *)
(* ******************************************************************** *)
exception Syntax_error of string
val syntax_error : string -> int -> int -> 'a
val syntax_assert : bool -> string -> int -> int -> unit
(* ******************************************************************** *)


(* Lexing *)
(* ******************************************************************** *)
module Lexing : sig
  (** A [line] is made of a name and a value. Sometimes, the value of a [line]
      in a file is better off being on several lines, in which case the [\n]
      has to be backslash-escaped. *)
  type line = {
    name : string;
    parameters : (string * string) list;
    value : string;
    name_start : location;
    value_start : location;
    value_end : location;
  }
  
  (** [lex_ical s] reads iCal data from the string [s] and returns a
      list of [line]s. A [line] is not series of bytes separated by some
      CRLF in [s] but a line in what the contents of [s] represents. *)
  val lex_ical : string -> line list
end
(* ******************************************************************** *)

(* Parsing *)
(* ******************************************************************** *)
(** [parse_ical l] returns the iCalendar tree that's encoded in [l] *)
val parse_ical : Lexing.line list -> [> `Raw of string ] Ical.t
(* ******************************************************************** *)

(* Data processing *)
(* ******************************************************************** *)
(** [map_values f ical] is a map over [ical], where [f] is applied to
    values only. Locations and keys are preserved. *)
val map_values :
  (
    string ->
    (([> `Raw of string ] as 'a) Ical.value) ->
    (string * ([> `Raw of string ] as 'a) Ical.value)
  ) -> 'a Ical.t -> 'a Ical.t

(** [map f t] is like [map f t] except that [f] is applied to the
    whole [Assoc(_)]. Note that [map] is equivalent to [List.map] if
    and only if there's no block element in [t], indeed the difference
    is that [map] is recursively called over all elements of all
    blocks. *)
val map :
  (([> `Raw of string ] as 'a) Ical.element ->
   ([> `Raw of string ] as 'b) Ical.element) ->
  'a Ical.t -> 'b Ical.t


(** [iter f ical] applies the function [f] to all [Assoc(loc, s, r)] elements
    of [ical]. Note that [f] doesn't process any [Block _]. If you don't want
    to iter only on [Assoc _] elements, you might want to use [List.iter]
    instead. It doesn't make much sense to process all [Block _] and [Assoc _]
    elements in an iter function because children of [Block _] elements are
    passed to [f] multiple times. *)
val iter :
  (([> `Raw of string ] as 'a) Ical.element -> unit) ->
  'a Ical.t -> unit

(**  *)
val sort : (([> `Raw of string ] as 'a) Ical.element -> 'a Ical.element -> int) ->
  'a Ical.t -> 'a Ical.t

(** [filter f t] returns all elements of [t] that satisfy the
    predicate [f]. Your function should return [true] for elements
    matching [Block _] if you want the elements of the block to be
    filtered, otherwise the entire block will be discarded.
    If you want to make sure that you don't end up with empty
    blocks, you should apply [filter] twice (the second time to
    remove empty blocks). *)
val filter : ('a Ical.element -> bool) -> 'a Ical.t -> 'a Ical.t


(** [fold_on_assocs f accu ical] returns [f a k v] applied to all
    pairs of (key*value) of [ical] where [a] is the accumulator.
    The first [k] and [v] given to [f] are the first pair
    of (key*value) met when browsing [ical]. It's similar to
    [List.fold_left]. *)
val fold_on_assocs :
  ('accu -> key -> 'a Ical.value -> 'accu) -> 'accu -> 'a Ical.t -> 'accu


(** [is_empty_block t] returns [true] if [t] is an empty block,
    false otherwise. *)
val is_empty_block : 'a Ical.element -> bool

(** [is_nonempty_block t] returns [true] if [t] isn't an empty block,
    false otherwise. *)
val is_nonempty_block : 'a Ical.element -> bool



(* ******************************************************************** *)

(* Value formats *)
(* ******************************************************************** *)
(** [text_of_raw location s] converts [s] to [sl]
    where [sl] is the list of text values in [s]. Note that it is a list
    because in iCalendar a label can be associated with multiple values.
    Note that [location] is only used when there's a syntax error.
    This conversion interpretes backslash-escaped characters and
    commas, the latter are used for separating multiple values.
    (http://tools.ietf.org/html/rfc5545#section-3.3.11) *)
val text_of_raw :
  location -> string -> string list


(** A module to represent date-time values. Since there are way
    too many possible values for timezones, this modules distinguishes
    3 possibilities: [`Local] represents localtime, in practice this
    means that there's no timezone indicator; [`UTC] represents UTC,
    in practice it means that the values ens with an additional Z character
    if you compare to localtime; and [`TZID s] where the timezone
    is encoded in the string [s]. [`TZID s] is used when the date-time
    value has a field "TZID=".

    (http://tools.ietf.org/html/rfc5545#section-3.3.5) *)
module Datetime :
  sig
    type t = {
      year : int;
      month : int;
      day : int;
      hours : int;
      minutes : int;
      seconds : int;
      timezone : timezone;
    }
    and timezone = [ `Local | `UTC | `TZID of string ]

    (** [validate s] returns [true] if the format of [s] satisfies a
        set of criteria (which is not enough to ensure that [s] is
        valid but a lot of errors can be detected).  Otherwise it
        returns [false]. Leap seconds are accepted on any year because
        there isn't a list of existing or future leap seconds in this
        library. However leap seconds are only accepted for June and
        December. *)
    val validate : t -> bool

    (** [to_string] performs a conversion to a string *)
    val to_string : t -> string

    (** [parse loc s] extract a date-time from [s]. If it fails to
        read a date-time, it raises a [Syntax_error _] exception.
        Note that [loc] is only used to know the location when the
        parsing fails. *)
    val parse :
      location -> string -> t

    (** [parse_datetime ical] applies [parse] to each element
        of [ical] that satisfies the following pattern
        [("DTSTAMP", (`Text _ | `Raw _))] *)
    val parse_datetime :
      ([> `Datetime of t
       | `Raw of string ]
       as 'a) Ical.t -> 'a Ical.t
  end


(** A module to represent date values.
    (http://tools.ietf.org/html/rfc5545#section-3.3.4) *)
module Date :
  sig
    (** type to represent a date *)
    type t = { year : int; month : int; day : int; }

    (** [validate d] returns [true] if [d] looks valid, i.e., the year
        has to hold in 4 decimal digits, the month has to be valid and
        the day has to be valid according to the month and year
        (leap years are taken into account). *)
    val validate : t -> bool

    (** [to_string d] returns [d] in a string format that can be used
        for iCalendar output. *)
    val to_string : t -> string

    (** [parse loc s] parses a date from the string [s], it raises
        a [Syntax_error _] exception if it fails and uses [loc] as the
        location for the error message. *)
    val parse : location -> string -> t
  end
(* ******************************************************************** *)


(* Formatting *)
(* ******************************************************************** *)
(** [limit_to_75_bytes sl] returns a string that satisfies the format
    contraints of iCalendar: lines are limited to at most 75 bytes
    (http://tools.ietf.org/html/rfc5545#section-3.1).   Elements of
    [sl] are comma-separated. Note that no character is backslash escaped.
    See also [ical_format].
*)
val limit_to_75_bytes : string list -> string


(** [ical_format sl] returns a string that satisfies the format
    contraints of iCalendar: lines are limited to at most 75 bytes
    (http://tools.ietf.org/html/rfc5545#section-3.1), and some
    characters are backslash-escaped
    (http://tools.ietf.org/html/rfc5545#section-3.3.11).  Elements of
    [sl] are comma-separated.
    See also [limit_to_75_bytes].
*)
val ical_format : string list -> string

(* To string *)
(* ******************************************************************** *)
(** [to_string f ical] returns the string that represents [ical].
    [ical] shall have any value of type
    [> `Raw of string | `Text of location * string list ]
    union the type of any value that the function [f] can convert to string.
    Elements that couldn't be properly converted to a string are converted
    to the empty string.
    [f x] shall return [None] when [f] cannot convert [x] to a string,
    and [Some s] when [s] is the string representation for [x]; and it
    would be very wrong to return [Some ""] when the proper value should
    be [None].
    Note that [f] can override the default conversion semantics for elements of
    the type [`Raw of string | `Text of location * string list ].
*)
val to_string :
  ?f:(([> ] as 'a) Ical.value -> string option) ->
  ([> `Raw of string | `Text of string list ] as 'a)
    Ical.t -> string


(* ******************************************************************** *)



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
