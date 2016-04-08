(* -*- coding: utf-8; -*- *)
(* ********************************************************************* *)
(* glical: A library to glance at iCal data using OCaml                  *)
(* (c) 2013/2014, Philippe Wang <philippe.wang@cl.cam.ac.uk>             *)
(* Licence: ISC                                                          *)
(* ********************************************************************* *)

module Kernel : module type of Glical_kernel
include module type of Kernel

module SSet : Set.S with type elt = String.t
(** Set of strings. *)

(** [channel_contents ic] eats all contents of [ic] and returns it as
    a string. Beware: if the contents is very big, it might fail,
    cf. [Sys.max_string_length] *)
val channel_contents : in_channel -> string

(** [file_contents filename] eats all contents of [filename] and returns it as
    a string. Beware: if the contents is very big, it might fail,
    cf. [Sys.max_string_length] *)
val file_contents : string -> string

(** [simple_cat ic oc] reads some iCalendar data from [ic] and outputs
    it on [oc]. Note that this fails if there are syntax errors. *)
val simple_cat : in_channel -> out_channel -> unit

(** [get ?maxdepth ?kl ?ks ?k ical] returns the iCalendar elements
    (blocks and associations) that are associated with the names or keys
    specified in [kl], [ks] and/or [k].
    [maxdepth] is the number of authorized traversals of [Block _] elements.
    If [maxdepth = 0] then no [Block _] will ever be returned.
    The default value of [maxdepth] is [max_int]. *)
val get :
  ?maxdepth:int ->
  ?kl:key list ->
  ?ks:SSet.t ->
  ?k:string ->
  ([> `Raw of string ] as 'a) Ical.t ->
  'a Ical.t

(** [extract_assocs ?maxdepth ?kl ?ks ?k ical] returns the iCalendar values that
    are associated with the keys specified in [kl], [ks] and/or [k].
    Empty blocks are not kept.
    [maxdepth] is the number of authorized traversals of [Block _] elements.
    If [maxdepth = 0] then no [Block _] will ever be returned. *)
val extract_assocs :
  ?maxdepth:int ->
  ?kl:key list ->
  ?ks:SSet.t ->
  ?k:string ->
  ([> `Raw of string ] as 'a) Ical.t ->
  'a Ical.t

(** [extract_values ?kl ?ks ?k ical] is like [extract_assocs ?kl ?ks
    ?k ical] except that it returns a list of values instead. *)
val extract_values :
  ?kl:key list ->
  ?ks:SSet.t ->
  ?k:string ->
  ([> `Raw of string ] as 'a) Ical.t ->
  'a Ical.value list

(** [list_keys_rev ical] is like [list_keys ical] except that the
    result is reversed and performs faster. *)
val list_keys_rev :
  [> `Raw of string ] Ical.t -> string list

(** [list_keys ical] returns the list of keys from [ical].
    Each key only appears once. *)
val list_keys :
  [> `Raw of string ] Ical.t -> string list

(** [list_keys_ordered ?compare ical] is like [list_keys ical] except
    that the result is ordered according to [String.compare] unless
    [~compare] is specified. *)
val list_keys_ordered :
  ?compare:(string -> string -> int) ->
  [> `Raw of string ] Ical.t -> string list

(** [combine ical1 ical2] returns the combination of
    the two iCalendars [ical1] and [ical2]. *)
val combine :
  ([> `Raw of string ] as 'a) Ical.t ->
  'a Ical.t -> 'a Ical.t


(** [combine_many icals] returns the combination of
    the all iCalendars of [icals]. *)
val combine_many :
  ([> `Raw of string ] as 'a) Ical.t list ->
  'a Ical.t



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
