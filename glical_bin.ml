(* -*- coding: utf-8; -*- *)
(* ********************************************************************* *)
(* glical: A library to glance at iCal data using OCaml                  *)
(* (c) 2013/2014, Philippe Wang <philippe.wang@cl.cam.ac.uk>             *)
(* Licence: ISC                                                          *)
(* ********************************************************************* *)

open Printf
open Glical
open Ical

let ical = ref true
let inputs = ref []
let template = ref None
let out = ref stdout

let assert_ a m =
  if not a then
    begin
      eprintf "Error: %s\n%!" m;
      exit 1
    end

let wrap f x =
  try f x
  with e ->
    assert_ false (Printexc.to_string e);
    assert false

let () =
  Arg.(
    parse
    [
      ("-input",
       String(wrap(fun s -> inputs := open_in_bin s :: !inputs)),
       "f use/add f as an input");
      ("-ical",
       Set(ical),
       " output in iCalendar format");
      ("-tpl",
       String(wrap(fun s -> template := Some(open_in_bin s))),
       "t output according to the template t");
      ("-o",
       String(wrap(fun s -> out := open_out_bin s)),
       "f use file f as output (default is stdout)");
    ]
    (wrap(fun s -> inputs := open_in_bin s :: !inputs))
    "glical takes some iCalendar data and allows you to play a little with it"
  )

let _ =
  if !template <> None then
    assert_ (!ical = false) "You can't have both -ical and -tpl";
  if !inputs = [] then
    inputs := [stdin];













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
