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

(** Important note: this library needs OCaml >= 4.1.0 *)

module Ical =
struct
type 'a t = 'a element list constraint 'a = [> `Raw of location * string ]
and 'a element =
  | Block of location * name * 'a t
  | Assoc of location * key * 'a constraint 'a = [> `Raw of location * string ]
and location = int * int
and name = string
and key = string



(** A [line] is made of a name and a value. Sometimes, the value of a [line] 
    in a file is better off being on several lines, in which case the [\n]
    has to be backslash-escaped. *)
type line = { (* output of the lexer *)
  (*  *)
  name: string;
  (* *)
  value: string;
  (* Locations *)
  name_start: int * int;
  value_start: int * int;
  value_end: int * int;
}
end
include Ical



open Printf

exception Syntax_error of string
let syntax_error s ln cn = raise (Syntax_error (sprintf "(%d:%d): %s." ln cn s))
let syntax_assert b s ln cn  = if not b then syntax_error s ln cn

(** http://tools.ietf.org/html/rfc5545#section-3.3.11 (TEXT) *)
let text_of_raw = function
  | `Raw((ln, cn) as location, s) ->
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
      `Text(location, List.rev (loop [] 0))
  | x -> x

let lex_ical s =
  let name = Buffer.create 42
  and value = Buffer.create 42 
  and name_start = ref (0,0)
  and value_start = ref (0,0)
  in
  let sl = String.length s in
  let rec loop lines i (colon:bool) (nl:bool) (lc:int) (cc:int) =
    if i >= sl then
      { name=Buffer.contents name;
        value=Buffer.contents value;
        name_start = !name_start;
        value_start = !value_start;
        value_end = lc, cc;}
      ::lines
    else
      match s.[i] with
      | '\n' ->
            begin
              syntax_assert (not nl) "unexpected double newline" lc cc;
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
                  loop
                    (nv::lines)
                    (i+1) false true (lc+1) 0
              else
                begin
                  syntax_assert colon "unexpected end of line" lc cc;
                  loop
                    lines
                    (i+2) colon false (lc+1) 0
                end
            end
      | '\r' -> (* just ignore \r for now *)
        loop lines (i+1) colon nl lc cc
      | ' ' as c ->
        syntax_assert colon "unexpected space before colon" lc cc;
        Buffer.add_char value c;
        loop lines (i+1) colon false lc (cc+1)
      | ':' as c ->
        if colon then
          begin
            Buffer.add_char value c;
            loop lines (i+1) true false lc (cc+1)
          end
        else
          begin
            value_start := (lc,cc);
            loop lines (i+1) true false lc (cc+1)
          end
      | c ->
        if colon then
          begin
            Buffer.add_char value c;
            loop lines (i+1) colon false lc (cc+1)
          end
        else
          begin
            Buffer.add_char name c;
            loop lines (i+1) colon false lc (cc+1)
          end
  in
  List.rev (loop [] 0 false true 1 0)
;;

let parse_ical l =
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
        ((Assoc(v.name_start, name, `Raw(v.value_start, value)))::res)
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


(** [tree_map] keeps location and section names, it applies the
    function [f] only to the values. *)
let rec tree_map f = function
  | Block(loc, s, v)::tl ->
    Block(loc, s, tree_map f v)::tree_map f tl
  | Assoc(loc, s, r)::tl ->
    let new_s, new_r = f s r in
    Assoc(loc, new_s, new_r)::tree_map f tl
  | [] -> []


(** [tree_transform] keeps location and section names, it applies the
    function [f] to all [Assoc(loc, s, r)] elements. *)
let rec tree_transform f = function
  | Block(loc, s, v)::tl ->
    Block(loc, s, tree_transform f v)::tree_transform f tl
  | (Assoc(loc, s, r) as e)::tl ->
    f e::tree_transform f tl
  | [] -> []




let ical_format s = (* limit lines to 75 bytes *)
  let b = Buffer.create (2 * String.length s) in
  let sl = String.length s in
  let rec loop i l =
    if i = sl then ()
    else match s.[i] with
      | '\n' ->
        if l > 73 then
          (Buffer.add_string b "\r\n ";
           loop i 1)
        else
          Buffer.add_string b "\\n"
      | '\\' ->
        if l > 73 then
          (Buffer.add_string b "\r\n ";
           loop i 1)
        else
          Buffer.add_string b "\\\\"
      | ';' ->
        if l > 73 then
          (Buffer.add_string b "\r\n ";
           loop i 1)
        else
          Buffer.add_string b "\\;"
      | ',' ->
        if l > 73 then
          (Buffer.add_string b "\r\n ";
           loop i 1)
        else
          Buffer.add_string b "\\,"
      | c when (int_of_char c land 0b1110_0000 = 0b1100_0000) ->
        if l > 73 then
          (Buffer.add_string b "\r\n ";
           loop i 1)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1))
      | c when (int_of_char c land 0b1111_0000 = 0b1110_0000) ->
        if l > 72 then
          (Buffer.add_string b "\r\n ";
           loop i 1)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1))
      | c when (int_of_char c land 0b1111_1000 = 0b1111_0000) ->
        if l > 71 then
          (Buffer.add_string b "\r\n ";
           loop i 1)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1))
      | c when (int_of_char c land 0b1111_1100 = 0b1111_1000) ->
        if l > 70 then
          (Buffer.add_string b "\r\n ";
           loop i 1)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1))
      | c when (int_of_char c land 0b1111_1110 = 0b1111_1100) ->
        if l > 69 then
          (Buffer.add_string b "\r\n ";
           loop i 1)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1))
      | c when (int_of_char c land 0b1111_1111 = 0b1111_1110) ->
        if l > 68 then
          (Buffer.add_string b "\r\n ";
           loop i 1)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1))
      | c ->
        if l > 74 then
          (Buffer.add_string b "\r\n ";
           loop i 1)
        else
          (Buffer.add_char b c;
           loop (i+1) (l+1))
  in
  loop 0 0;
  Buffer.contents b


let to_string f t =
  let b = Buffer.create 42 in
  let rec loop = function
    | [] -> ()
    | Block(_, s, v)::tl ->
      bprintf b "BEGIN:%s\n" s;
      loop v;
      bprintf b "END:%s\n" s;
      loop tl
    | Assoc(_, s, r)::tl ->
      Buffer.add_string b
        (match f r with
         | Some x -> ical_format (s ^ ":" ^ x)
         | None ->
           match r with
           | `Text x -> ical_format (s ^ ":" ^ x)
           | `Raw(loc, x) -> s ^ ":" ^ x
           | _ -> "");
      loop tl
  in
  loop t;
  Buffer.contents b




module Datetime =
struct
  (** http://tools.ietf.org/html/rfc2445#section-4.3.5 *)
  
  type 'a timezone = [> `Local | `UTC | `String of string ] as 'a

  type 'a t = {
    timezone : 'a timezone;
    year     : int;
    month    : int;
    day      : int;
    hours    : int;
    minutes  : int;
    seconds  : int;
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
    && (seconds < 60 || (minutes = 59 && hours = 23))
    && (match month with
        | 2 -> day < 29 ||
               (day = 29 && (year mod 4 = 0 && year mod 400 <> 0))
        | 4 | 6 | 9 | 11 -> day <= 30
        | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> true
        | _ -> assert false)
    && (match timezone with
        | `Local | `UTC -> true
        | `String t ->
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
        timezone = `String timezone } ->
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
          timezone =
            (if utc then `UTC
             else if timezone <> "" then `String(timezone)
             else `Local);
          year = year;
          month = month;
          day = day;
          hours = hours;
          minutes = minutes;
          seconds = seconds;
        }
      end

    let f = (function
          | Assoc(loc, "DTSTAMP", (`Text d | `Raw(_, d))) ->
            Assoc(loc, "DTSTAMP", `Datetime(parse loc d))
          | x -> x)
    let parse_datetime t =
      tree_transform
        f
        t
end

module Date =
struct
  (** http://tools.ietf.org/html/rfc2445#section-4.3.4 *)

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
        {
          year = year;
          month = month;
          day = day;
        }
      end
end



(* (\* ********************************************************************* *\) *)
(* (\* Testing junk below *\) *)
(* let _x1 = *)
(*   lex_ical "BEGIN:VCALENDAR *)
(* VERSION:2.0 *)
(* PRODID:-//ABC Corporation//NONSGML My Product//EN *)
(* BEGIN:VTODO *)
(* DTSTAMP:19980130T134500Z *)
(* SEQUENCE:2 *)
(* UID:uid4@host1.com *)
(* ACTION:AUDIO *)
(* TRIGGER:19980403T120000 *)
(* ATTACH;FMTTYPE=audio/basic:http://example.com/pub/audio- *)
(*  files/ssbanner.aud *)
(* REPEAT:4 *)
(* DURATION:PT1H *)
(* END:VTODO *)
(* END:VCALENDAR *)
(* ";; *)

(* let _y1 = parse_ical _x1;; *)

(* let _x2 = *)
(*   lex_ical "BEGIN:VCALENDAR *)
(* VERSION:2.0 *)
(* PRODID:-//ABC Corporation//NONSGML My Product//EN *)
(* BEGIN:VJOURNAL *)
(* DTSTAMP:19970324T120000Z *)
(* DTSTAMP:TZID=America/New_York:19980119T020000 *)
(* UID:uid5@host1.com *)
(* ORGANIZER:MAILTO:jsmith@example.com *)
(* STATUS:DRAFT *)
(* CLASS:PUBLIC *)
(* CATEGORIES:Project Report, XYZ, Weekly Meeting *)
(* DESCRIPTION:Project xyz Review Meeting Minutes\\n *)
(*  Agenda\\n1. Review of project version 1.0 requirements.\\n2. *)
(*  Definition *)
(*  of project processes.\\n3. Review of project schedule.\\n *)
(*  Participants: John Smith, Jane Doe, Jim Dandy\\n-It was *)
(*   decided that the requirements need to be signed off by *)
(*   product marketing.\\n-Project processes were accepted.\\n *)
(*  -Project schedule needs to account for scheduled holidays *)
(*   and employee vacation time. Check with HR for specific *)
(*   dates.\\n-New schedule will be distributed by Friday.\\n- *)
(*  Next weeks meeting is cancelled. No meeting until 3/23. *)
(* END:VJOURNAL *)
(* END:VCALENDAR *)
(* " *)
(* ;; *)

(* let _y2 = parse_ical _x2;; *)

(* let () = () ;; *)


(* let _ = tree_map text_of_raw _y2;; *)

(* let _ = tree_transform text_of_raw _y2;; *)

(* let _ =  *)
(*   Datetime.parse_datetime _y2 *)
