(* -*- coding: utf-8; -*- *)
(* ********************************************************************* *)
(* glical: A library to glance at iCal data using OCaml                  *)
(* (c) 2013/2014, Philippe Wang <philippe.wang@cl.cam.ac.uk>             *)
(* Licence: ISC                                                          *)
(* ********************************************************************* *)

open Printf
module Kernel = Glical_kernel
include Kernel
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

let file_contents filename =
  let i = open_in filename in
  let r = channel_contents i in
  close_in i;
  r

let simple_cat ic oc =
  let s = channel_contents ic in
  let l = Lexing.lex_ical s in
  let p : 'a Ical.t = parse_ical l in
  let d = Datetime.parse_datetime p in
  let o = to_string ~f:(fun _ -> None) d in
  fprintf oc "%s%!" o


let get ?(maxdepth=max_int) ?(kl=[]) ?(ks=SSet.empty) ?k ical : 'a t =
  let res = ref [] in
  let rec loop_one maxdepth = function
    | Block (_, name, contents) as b ->
      if maxdepth = 0 then
        ()
      else if (Some name = k || SSet.mem name ks || List.mem name kl) then
        res := b :: !res
      else
        loop (pred maxdepth) contents
    | Assoc(_, key, _, _) as a ->
      if (Some key = k || SSet.mem key ks || List.mem key kl) then
        res := a :: !res
      else
        ()
  and loop maxdepth ical = List.iter (loop_one maxdepth) ical in
  loop maxdepth ical;
  List.rev !res


let extract_assocs ?(maxdepth=max_int) ?(kl=[]) ?(ks=SSet.empty) ?k ical : 'a t =
  (* [block] is necessary for performance issues, otherwise
     calling [extract_assocs] would have been sufficient. *)
  let rec block ~maxdepth ?(kl=[]) ?(ks=SSet.empty) ?(k=None) = function
    | [] -> false
    | Block(_, _, l) :: tl ->
      let maxdepth = pred maxdepth in
      maxdepth > -1 &&
      (block ~maxdepth ~kl ~ks ~k l || block ~maxdepth ~kl ~ks ~k tl)
    | Assoc(_, key, _, _)::tl ->
      let maxdepth = pred maxdepth in
      (Some key = k || SSet.mem key ks || List.mem key kl
       || (maxdepth > -1 && block ~maxdepth ~kl ~ks ~k tl))
  in
  let i =
    filter
      (function
        | Block(_, _, l) -> block ~maxdepth:(pred maxdepth) ~kl ~ks ~k l
        | Assoc(_, key, _, _) ->
          Some key = k || SSet.mem key ks || List.mem key kl)
      ical
  in
  i

let extract_values ?(kl=[]) ?(ks=SSet.empty) ?k ical : 'value list =
  match k with
  | None ->
    fold_on_assocs
      (fun accu key value -> value::accu)
      []
      (extract_assocs ~kl ~ks ical)
  | Some k ->
    fold_on_assocs
      (fun accu key value -> value::accu)
      []
      (extract_assocs ~kl ~ks ~k ical)

let list_keys_rev ical : string list =
  let ks = ref SSet.empty in
  let res = ref [] in
  iter
    (function
        Block _ -> ()
      | Assoc(loc, k, p, _) ->
        if SSet.mem k !ks then
          ()
        else
          (ks := SSet.add k !ks;
           res := k :: !res))
    ical;
  !res

let list_keys ical : string list =
  List.rev (list_keys_rev ical)


let list_keys_ordered ?(compare=String.compare) ical : string list =
  let
    module SSet = Set.Make(struct type t = string let compare = compare end)
  in
  let ks = ref SSet.empty in
  iter
    (function
        Block _ -> ()
      | Assoc(loc, k, p, _) ->
        if SSet.mem k !ks then
          ()
        else
          ks := SSet.add k !ks)
    ical;
  SSet.elements !ks


let combine ical1 ical2 : 'a t =
  match ical1, ical2 with
  | [Block(locx, x, xc)], [Block(locy, y, yc)] when x = y ->
    [Block(locx, x, xc@yc)]
  | [], _ -> ical2
  | _, [] -> ical1
  | _ -> ical1 @ ical2


let rec combine_many = function
  | [] -> []
  | [ical] -> ical
  | ical1::ical2::tl ->
    combine_many ((combine ical1 ical2)::tl)


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
