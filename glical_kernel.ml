(* -*- coding: utf-8; -*- *)
(* ********************************************************************* *)
(* glical: A library to glance at iCal data using OCaml                  *)
(* (c) 2013/2014, Philippe Wang <philippe.wang@cl.cam.ac.uk>             *)
(* Licence: ISC                                                          *)
(* ********************************************************************* *)

type location = int * int
and name = string
and key = string


type line = { (* output of the lexer *)
  name: string;
  value: string;
  (* Locations *)
  name_start: int * int;
  value_start: int * int;
  value_end: int * int;
}

class type ['a] value = object
  constraint 'a = [> `Raw of string ]
  method location : location
  method to_string : string
  method to_pretty : string
  method value : 'a
end


module Ical =
struct
  type 'a t = 'a element list
    constraint 'a = [> `Raw of string ]
  and 'a element =
    | Block of location * name * 'a t
    | Assoc of location * key * 'a parameters * 'a value
    constraint 'a = [> `Raw of string ]
  and 'a parameters = (key * 'a value) list
    constraint 'a = [> `Raw of string ]
end
open Ical

open Printf

exception Syntax_error of string

let syntax_error s ln cn =
  raise (Syntax_error (sprintf "(%d:%d): %s." ln cn s))

let syntax_assert b s ln cn =
  if not b then syntax_error s ln cn

let syntax_warning s ln cn =
  eprintf "Warning (%d:%d): %s.\n" ln cn s

let syntax_warning_if cond s ln cn =
  if cond then
    syntax_warning s ln cn
  else
    ()


(** http://tools.ietf.org/html/rfc5545#section-3.3.11 (TEXT) *)
let text_of_raw (ln, cn) s =
    let sl = String.length s in
    let open Buffer in
    let b = create sl in
    let rec loop accu i =
      if i = sl then contents b :: accu else
        match s.[i] with
        | _ when false -> assert false

        (* Escaped char *)
        | '\\' ->
          if i+1 = sl then
            syntax_error "raw data ends with an unescaped backslash" ln cn
          else
            begin match s.[i+1] with
              | 'N' | 'n' -> add_char b '\n'
              | '\\' -> add_char b '\\'
              | ';' -> add_char b ';'
              | ',' -> add_char b ','
              | c -> syntax_error (sprintf "backslash-escaping %c" c) ln cn
            end;
          loop accu (i+2)

        (* For multiple text values *)
        | ',' ->
          let bc = contents b in
          clear b;
          loop (bc :: accu) (i+1)

        | ';' -> syntax_error "character ; is not allowed" ln cn

        (* The risk here is to allow some control characters that
           shouldn't be allowed. I'm taking a risk by assuming that
           feeds read by this program do comply with RFC 5545. *)
        | c -> add_char b c; loop accu (i+1)
    in
    List.rev (loop [] 0)


let lex_ical s =
  let name = Buffer.create 42
  and value = Buffer.create 42
  and name_start = ref (0,0)
  and value_start = ref (0,0)
  in
  let sl = String.length s in
  let rec loop lines i (colon:bool) (dquotes:bool) (lc:int) (cc:int) =
    if i >= sl then
      { name=Buffer.contents name;
        value=Buffer.contents value;
        name_start = !name_start;
        value_start = !value_start;
        value_end = lc, cc;}
      ::lines
    else
      match s.[i] with
      | '"' as c ->
        if colon then
          begin
            Buffer.add_char value c;
            loop lines (i+1) colon (not dquotes) lc (cc+1)
          end
        else
          begin
            Buffer.add_char name c;
            loop lines (i+1) colon (not dquotes) lc (cc+1)
          end
      | '\n' ->
        begin
          if i >= sl-1 then
            { name=Buffer.contents name;
              value=Buffer.contents value;
              name_start = !name_start;
              value_start = !value_start;
              value_end = lc, cc;}
            ::lines
          else if s.[i+1] <> ' ' then
            let nv = {
              name=Buffer.contents name;
              value=Buffer.contents value;
              name_start = !name_start;
              value_start = !value_start;
              value_end = lc, cc;
            }
            in
            Buffer.clear name; Buffer.clear value;
            name_start := (lc+1,0);
            loop (nv::lines) (i+1) false dquotes (lc+1) 0
          else
            begin
              (* syntax_assert colon "unexpected end of line" lc cc; *)
              loop lines (i+2) colon dquotes (lc+1) 0
            end
        end
      | '\r' -> (* just ignore \r for now *)
        loop lines (i+1) colon dquotes lc cc
      | ' ' as c ->
        if colon then
          Buffer.add_char value c
        else if dquotes then
          Buffer.add_char name c
        else
          syntax_error "unexpected space before colon" lc cc;
        loop lines (i+1) colon dquotes lc (cc+1)
      | ':' as c ->
        if colon then
          begin
            Buffer.add_char value c;
            loop lines (i+1) true dquotes lc (cc+1)
          end
        else
          begin
            value_start := (lc,cc);
            loop lines (i+1) true dquotes lc (cc+1)
          end
      | c ->
        if colon then
          begin
            Buffer.add_char value c;
            loop lines (i+1) colon dquotes lc (cc+1)
          end
        else
          begin
            Buffer.add_char name c;
            loop lines (i+1) colon dquotes lc (cc+1)
          end
  in
  List.rev (loop [] 0 false false 1 0)


let ical_format ls = (* limit lines to 75 bytes *)
  let b = Buffer.create 42 in
  let rec loop i l s ls =
    let sl = String.length s in
    if i = sl then
      match ls with
      | [] -> ()
      | s :: tl ->
        if Buffer.length b <> 0 then
          (Buffer.add_char b ',';
           loop 0 (l+1) s tl)
        else
          loop 0 l s tl
    else match s.[i] with
      | '\n' ->
        if l > 73 then
          (Buffer.add_string b "\r\n ";
           loop i 1 s ls)
        else
          (Buffer.add_string b "\\n";
           loop (i+1) (l+2) s ls)
      | '\\' ->
        if l > 73 then
          (Buffer.add_string b "\r\n ";
           loop i 1 s ls)
        else
          (Buffer.add_string b "\\\\";
           loop (i+1) (l+2) s ls)
      | ';' ->
        if l > 73 then
          (Buffer.add_string b "\r\n ";
           loop i 1 s ls)
        else
          (Buffer.add_string b "\\;";
           loop (i+1) (l+2) s ls)
      | ',' ->
        if l > 73 then
          (Buffer.add_string b "\r\n ";
           loop i 1 s ls)
        else
          (Buffer.add_string b "\\,";
           loop (i+1) (l+2) s ls)
      | c when (int_of_char c land 0b1110_0000 = 0b1100_0000) ->
        if l > 73 then
          (Buffer.add_string b "\r\n ";
           loop i 1 s ls)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1) s ls)
      | c when (int_of_char c land 0b1111_0000 = 0b1110_0000) ->
        if l > 72 then
          (Buffer.add_string b "\r\n ";
           loop i 1 s ls)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1) s ls)
      | c when (int_of_char c land 0b1111_1000 = 0b1111_0000) ->
        if l > 71 then
          (Buffer.add_string b "\r\n ";
           loop i 1 s ls)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1) s ls)
      | c when (int_of_char c land 0b1111_1100 = 0b1111_1000) ->
        if l > 70 then
          (Buffer.add_string b "\r\n ";
           loop i 1 s ls)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1) s ls)
      | c when (int_of_char c land 0b1111_1110 = 0b1111_1100) ->
        if l > 69 then
          (Buffer.add_string b "\r\n ";
           loop i 1 s ls)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1) s ls)
      | c when (int_of_char c land 0b1111_1111 = 0b1111_1110) ->
        if l > 68 then
          (Buffer.add_string b "\r\n ";
           loop i 1 s ls)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1) s ls)
      | c ->
        if l > 74 then
          (Buffer.add_string b "\r\n ";
           loop i 1 s ls)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1) s ls)
  in
  loop 0 0 "" ls;
  Buffer.contents b


let make_raw location value : 'a value = object
  method location = location
  method to_string = value
  method to_pretty = value
  method value : [> `Raw of string ] = `Raw value
end

let make_text location value : 'a value = object
  method location = location
  method to_string =
    ical_format value
  method to_pretty =
    String.concat "," value
  method value : [> `Text of string list ] = `Text value
end


let parse_ical l =
  (* TODO: parse the [name] to separate the parameters that are still
     inside. *)
  (* ob = opened blocks *)
  let rec loop (res:'a list) (ob:string option) = function
    | [] ->
      begin match ob with
        | Some e -> syntax_error (sprintf "unclosed block %s" e) (-1) (-1);
        | None -> res, []
      end
    | {name="BEGIN"; value=e} as v::tl ->
      let block, tl = loop_rev [] (Some e) tl in
      loop ((Block(v.name_start, e, block))::res) ob tl
    | {name="END"; value=e} as v::tl ->
      begin match ob with
        | Some x when x = e ->
          res, tl
        | Some x ->
          syntax_error (sprintf "unexpected end of block %s, \
                                 expected end of block %s" x e)
            (fst v.name_start) (snd v.name_start)
        | None ->
          syntax_error (sprintf "unexpected end of block %s" e)
            (fst v.name_start) (snd v.name_start)
      end
    | {name; value} as v::tl ->
      loop
        ((Assoc(v.name_start, name, [], make_raw v.value_start value))::res)
        ob
        tl
  and loop_rev res ob l =
    let f,s = loop res ob l in
    List.rev f, s
  in
  match loop_rev [] None l with
  | res, [] ->
    res
  | _, v::_ ->
    syntax_error (sprintf "unexpected data")
      (fst v.name_start) (snd v.name_start)


(** [map] keeps location and section names, it applies the
    function [f] only to the values. *)
let rec map_values f = function
  | Block(loc, s, v)::tl ->
    Block(loc, s, map_values f v)::map_values f tl
  | Assoc(loc, s, p, r)::tl ->
    let new_s, new_r = f s r in
    Assoc(loc, new_s, p, new_r)::map_values f tl
  | [] -> []


(** [map] keeps location and section names, it applies the
    function [f] to all [Assoc(loc, s, p, r)] elements. *)
let rec map f = function
  | Block(loc, s, v)::tl ->
    Block(loc, s, map f v)::map f tl
  | (Assoc(_, _, _, _) as e)::tl ->
    f e::map f tl
  | [] -> []


(** [iter f ical] applies the function [f] to all [Assoc(loc, s, r)] elements
    of [ical]. *)
let rec iter f = function
  | Block(loc, s, v)::tl ->
    iter f v;
    iter f tl
  | (Assoc(_, _, _, _) as e)::tl ->
    f e;
    iter f tl
  | [] -> ()


let rec sort compare = function
  | Block(loc, s, v)::tl ->
    List.sort compare (Block(loc, s, sort compare v)::(sort compare tl))
  | (Assoc(loc, s, p, r) as e)::tl ->
    List.sort compare (e::sort compare tl)
  | [] -> []

let rec filter f = function
  | (Block(loc, s, v) as e)::tl ->
    if f e then
      Block(loc, s, filter f v)::filter f tl
    else
      filter f tl
  | (Assoc(loc, s, p, r) as e)::tl ->
    if f e then
      e::filter f tl
    else
      filter f tl
  | [] -> []

let rec fold_on_assocs f accu = function
  | Block(_, _, v)::tl -> fold_on_assocs f (fold_on_assocs f accu v) tl
  | Assoc(_, k, _, v)::tl -> fold_on_assocs f (f accu k v) tl
  | [] -> accu

let is_nonempty_block = function
    | Block(_, _, []) -> false
    | _ -> true

let is_empty_block = function
    | Block(_, _, []) -> true
    | _ -> false



let limit_to_75_bytes ls = (* limit lines to 75 bytes *)
  (* Note : Apple Calendar seems to limit to less than 75 bytes, which
     is also valid but less space-efficient. *)
  let b = Buffer.create 42 in
  let rec loop i l s ls =
    let sl = String.length s in
    if i = sl then
      match ls with
      | [] -> ()
      | s :: tl ->
        if Buffer.length b <> 0 then
          (Buffer.add_char b ',';
           loop 0 (l+1) s tl)
        else
          loop 0 l s tl
    else match s.[i] with
      | c when (int_of_char c land 0b1110_0000 = 0b1100_0000) ->
        if l > 73 then
          (Buffer.add_string b "\r\n ";
           loop i 1 s ls)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1) s ls)
      | c when (int_of_char c land 0b1111_0000 = 0b1110_0000) ->
        if l > 72 then
          (Buffer.add_string b "\r\n ";
           loop i 1 s ls)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1) s ls)
      | c when (int_of_char c land 0b1111_1000 = 0b1111_0000) ->
        if l > 71 then
          (Buffer.add_string b "\r\n ";
           loop i 1 s ls)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1) s ls)
      | c when (int_of_char c land 0b1111_1100 = 0b1111_1000) ->
        if l > 70 then
          (Buffer.add_string b "\r\n ";
           loop i 1 s ls)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1) s ls)
      | c when (int_of_char c land 0b1111_1110 = 0b1111_1100) ->
        if l > 69 then
          (Buffer.add_string b "\r\n ";
           loop i 1 s ls)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1) s ls)
      | c when (int_of_char c land 0b1111_1111 = 0b1111_1110) ->
        if l > 68 then
          (Buffer.add_string b "\r\n ";
           loop i 1 s ls)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1) s ls)
      | c ->
        if l > 74 then
          (Buffer.add_string b "\r\n ";
           loop i 1 s ls)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1) s ls)
  in
  loop 0 0 "" ls;
  Buffer.contents b




let to_string ?(f=(fun _ -> None)) t =
  let string_of_p p =
    let b = Buffer.create 42 in
    List.iter
      (fun (k, v) -> bprintf b ";%s:%s" k v#to_string)
      p;
    Buffer.contents b
  in
  let b = Buffer.create 42 in
  let rec loop = function
    | [] -> ()
    | Block(_, s, v)::tl ->
      bprintf b "BEGIN:%s\r\n" s;
      loop v;
      bprintf b "END:%s\r\n" s;
      loop tl
    | Assoc(_, s, p, r)::tl ->
      Buffer.add_string b
        (match f r with
         | Some x ->
           ical_format [s ^ string_of_p p ^ ":" ^ x]
         | None ->
           limit_to_75_bytes [s ^ string_of_p p ^ ":" ^ r#to_string]);
      Buffer.add_string b "\r\n";
      loop tl
  in
  loop t;
  Buffer.contents b




module Datetime =
struct
  (** http://tools.ietf.org/html/rfc2445#section-4.3.5 *)

  type timezone = [ `Local | `UTC | `TZID of string ]

  type t = {
    year     : int;
    month    : int;
    day      : int;
    hours    : int;
    minutes  : int;
    seconds  : int;
    timezone : timezone;
  }

  (** Allows the time to be equal to 23:59:60 on any day because
      there are too many days to check and some days are yet to
      be determined in the future. *)
  let validate { timezone; hours; minutes; seconds; year; month; day } =
    hours >= 0 && hours <= 24
    && minutes >= 0 && minutes <= 59
    && seconds >= 0 && seconds <= 60
    && year >= 0 && year <= 9999
    && month > 0 && month < 13
    && day > 0 && day < 31
    && (seconds < 60 || (minutes = 59 && hours = 23 &&
                         (month = 6 || month = 12)))
    && (match month with
        | 2 -> day < 29 ||
               (day = 29 && (year mod 4 = 0 && year mod 400 <> 0))
        | 4 | 6 | 9 | 11 -> day <= 30
        | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> true
        | _ -> assert false)
    && (match timezone with
        | `Local | `UTC -> true
        | `TZID t ->
          let module X = struct exception Break end in
          try
            for i = 0 to String.length t - 1 do
              match t.[i] with
              | 'A' .. 'Z' | 'a' .. 'z' | '/' -> ()
              | _ -> raise X.Break
            done;
            true
          with X.Break -> false)

  let to_string = function
    | { year; month; day; hours; minutes; seconds; timezone = `Local } ->
      sprintf "%04d%02d%02dT%02d%02d%02d" year month day hours minutes seconds
    | { year; month; day; hours; minutes; seconds; timezone = `UTC } ->
      sprintf "%04d%02d%02dT%02d%02d%02dZ" year month day hours minutes seconds
    | { year; month; day; hours; minutes; seconds;
        timezone = `TZID timezone } ->
      sprintf "TZID=%s:%04d%02d%02dT%02d%02d%02dZ"
        timezone
        year month day hours minutes seconds

  let parse (ln, cn : int*int) (s : string) =
    let d = "19980130T134500" in
    let l = String.length d in
    let t = String.index d 'T' (* Won't fail. *) in
    if String.length s < l
    then
      syntax_error (sprintf "invalid date-time format for %S" s) ln cn
    else
      let timezone, offset =
        if String.sub s 0 5 = "TZID=" then
          if String.contains s ':' then
            let i = String.index s ':' in
            let tz = String.sub s 5 (i - 5) in
            tz, i+1
          else
            syntax_error (sprintf "invalid date-time format for %S" s) ln cn
        else
          "", 0
      in
      begin
        for i = offset to l - 1 do
          if i = t && s.[i] = 'T'
          || match s.[i] with '0' .. '9' -> i <> t | _ -> false
          then
            ()
          else
            syntax_error (sprintf "invalid date-time format for %S, \
                                   character #%d is wrong" s i) ln cn
        done;
        let year = int_of_string (String.sub s offset 4)
        and month = int_of_string (String.sub s (offset+4) 2)
        and day = int_of_string (String.sub s (offset+6) 2)
        and hours = int_of_string (String.sub s (offset+9) 2)
        and minutes = int_of_string (String.sub s (offset+11) 2)
        and seconds = int_of_string (String.sub s (offset+13) 2)
        and utc =
          if String.length s = offset + 15 || String.length s = offset + 16
          then
            s.[String.length s - 1] = 'Z'
          else
            syntax_error (sprintf "invalid date-time format for %S" s) ln cn
        in
        {
          year;
          month;
          day;
          hours;
          minutes;
          seconds;
          timezone =
            (if utc then `UTC
             else if timezone <> "" then `TZID(timezone)
             else `Local);
        }
      end

  let make location t =
    object
      method location = location
      method to_string = to_string t
      method to_pretty = to_string t
      method value = `Datetime t
    end

  let parse_datetime t =
    map
      (function
        | Assoc(loc, ("DTSTAMP"|"DTSTART"|"DTEND" as l), p, v) ->
          Assoc(loc, l, p, make v#location (parse v#location v#to_string))
        | x -> x)
      t
end

module Date =
struct
  type t = {
    year     : int;
    month    : int;
    day      : int;
  }

  let validate { year; month; day } =
    year >= 0 && year <= 9999
    && month > 0 && month < 13
    && day > 0 && day < 31
    && (match month with
        | 2 -> day < 29 ||
               (day = 29 && (year mod 4 = 0 && year mod 400 <> 0))
        | 4 | 6 | 9 | 11 -> day <= 30
        | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> true
        | _ -> assert false)
  let to_string { year; month; day } = sprintf "%04d%02d%02d" year month day

  let parse (ln, cn : int*int) (s : string) =
    let d = "19980130" in
    let l = String.length d in
    if String.length s <> l
    then
      syntax_error (sprintf "invalid date format for %S" s) ln cn
    else
      begin
        (try ignore(int_of_string s)
         with Failure "int_of_string" ->
           syntax_error (sprintf "invalid date format for %S" s) ln cn);
        let year = int_of_string (String.sub s 0 4)
        and month = int_of_string (String.sub s 4 2)
        and day = int_of_string (String.sub s 6 2)
        in
        { year; month; day; }
      end
end





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
