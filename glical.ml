(* -*- coding: utf-8; -*- *)
(* ********************************************************************* *)
(* glical: A library to glance at iCal data using OCaml                  *)
(* (c) 2013/2014, Philippe Wang <philippe.wang@cl.cam.ac.uk>             *)
(* Licence: ISC                                                          *)
(* ********************************************************************* *)

open Printf
include Glical_kernel
open Ical

module SSet = Set.Make(String)

let channel_contents ic =
  let b = Buffer.create 42 in
  begin
    try
      while true do
        Buffer.add_char b (input_char ic) 
      done with End_of_file -> ()
  end;
  Buffer.contents b

let simple_cat ic oc =
  let s = channel_contents ic in
  let l = lex_ical s in
  let p : 'a Ical.t = parse_ical l in
  let d = Datetime.parse_datetime p in
  let o =
    to_string
      ~f:(function
          | (`Text _ | `Raw _) -> None
          | `Datetime d -> Some(Datetime.to_string d) 
        ) d in
  fprintf oc "%s%!" o

let extract_assocs ?(kl=[]) ?(ks=SSet.empty) ical : 'a t =
  let i =
    filter
      (function
        | Block _ -> true
        | Assoc(_, k, _) -> SSet.mem k ks || List.mem k kl)
      ical
  in
  i

let extract_values ?(kl=[]) ?(ks=SSet.empty) ical : 'value list =
  fold_on_assocs
    (fun accu key value -> value::accu)
    []
    (extract_assocs ~kl ~ks ical)


let list_keys_rev ical : string list =
  let ks = ref SSet.empty in
  let res = ref [] in
  iter
    (function
        Block _ -> ()
      | Assoc(loc, k, _) ->
        if SSet.mem k !ks then
          ()
        else
          (ks := SSet.add k !ks;
           res := k :: !res))
    ical;
  !res

let list_keys ical : string list =
  List.rev (list_keys_rev ical)


let list_keys_ordered ical : string list =
  let ks = ref SSet.empty in
  iter
    (function
        Block _ -> ()
      | Assoc(loc, k, _) ->
        if SSet.mem k !ks then
          ()
        else
          ks := SSet.add k !ks)
    ical;
  SSet.elements !ks
        

let combine ical1 ical2 : 'a t =
  match ical1, ical2 with
  | [Block(locx, x, xc)], [Block(locy, y, yc)] ->
    [Block(locx, x, xc@yc)]
  | [], _ -> ical2
  | _, [] -> ical1
  | _ -> ical1 @ ical2











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
