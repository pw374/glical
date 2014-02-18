(* -*- coding: utf-8; -*- *)
(* ********************************************************************* *)
(* glical: A library to glance at iCal data using OCaml                  *)
(* (c) 2013/2014, Philippe Wang <philippe.wang@cl.cam.ac.uk>             *)
(* Licence: ISC                                                          *)
(* ********************************************************************* *)

include module type of Glical_kernel

(** SSet = Set.Make(String) *)
module SSet :
  sig
    type elt = String.t
    type t = Set.Make(String).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end

(** [channel_contents ic] eats all contents of [ic] and returns it as
    a string. Beware: if the contents is very big, it might fail,
    cf. [Sys.max_string_length] *)
val channel_contents : in_channel -> string

(** [simple_cat ic oc] reads some iCalendar data from [ic] and outputs
    it on [oc]. Note that this fails if there are syntax errors. *)
val simple_cat : in_channel -> out_channel -> unit

(** [extract_assocs ?kl ?ks ?k ical] returns the iCalendar values that
    are associated with the keys specified in [kl], [ks] and/or [k].
    Empty blocks are not kept. *)
val extract_assocs :
  ?kl:key list ->
  ?ks:SSet.t ->
  ?k:string ->
  ([> `Raw of location * string ] as 'a) Ical.t ->
  'a Ical.t

(** [extract_values ?kl ?ks ?k ical] is like [extract_assocs ?kl ?ks
    ?k ical] except that it returns a list of values instead. *)
val extract_values :
  ?kl:key list ->
  ?ks:SSet.t ->
  ?k:string ->
  ([> `Raw of location * string ] as 'a) Ical.t ->
  'a list

(** [list_keys_rev ical] is like [list_keys ical] except that the 
    result is reversed and performs faster. *)
val list_keys_rev :
  [> `Raw of location * string ] Ical.t -> string list

(** [list_keys ical] returns the list of keys from [ical].
    Each key only appears once. *)
val list_keys :
  [> `Raw of location * string ] Ical.t -> string list

(** [list_keys_ordered ?compare ical] is like [list_keys ical] except
    that the result is ordered according to [String.compare] unless
    [~compare] is specified. *)
val list_keys_ordered :
  ?compare:(string -> string -> int) ->
  [> `Raw of location * string ] Ical.t -> string list

(** [combine ical1 ical2] returns the combination of 
    the two iCalendars [ical1] and [ical2]. *)
val combine :
  ([> `Raw of location * string ] as 'a) Ical.t ->
  'a Ical.t -> 'a Ical.t


(** [combine_many icals] returns the combination of 
    the all iCalendars of [icals]. *)
val combine_many :
  ([> `Raw of location * string ] as 'a) Ical.t list ->
  'a Ical.t

(** [to_socaml f ical] returns the OCaml program that represents [ical].
    [ical] shall have any value of type
    [> `Raw of location * string | `Text of location * string list ]
    union the type of any value that the function [f] can convert to string.
    Elements that couldn't be properly converted to a string are converted
    to the empty string.
    [f x] shall return [None] when [f] cannot convert [x] to a string,
    and [Some s] when [s] is the string representation for [x]; and it
    would be very wrong to return [Some ""] when the proper value should
    be [None].
    Note that [f] can override the default conversion semantics for elements of
    the type [`Raw of location * string | `Text of location * string list ].
*)
val to_socaml :
  ?f:(([> ] as 'a) -> string option) ->
  ([> `Raw of location * string | `Text of location * string list
   |  `Datetime of
        [ `Local | `String of string | `UTC ] Datetime.t ] as 'a)
    Ical.t -> string


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
