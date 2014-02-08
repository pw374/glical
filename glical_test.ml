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

open Glical

let channel_contents ic =
  let b = Buffer.create 42 in
  begin
    try
      while true do
        Buffer.add_char b (input_char ic) 
      done with End_of_file -> ()
  end;
  Buffer.contents b

let _ =
  let s = channel_contents stdin in
  let l = lex_ical s in
  let p : 'a Ical.t = parse_ical l in
  let d = Datetime.parse_datetime p in
  let o =
    to_string
      ~f:(function
          | (`Text _ | `Raw _) -> None
          | `Datetime d -> Some(Datetime.to_string d) 
        ) d in
  Printf.printf "%s%!" o

