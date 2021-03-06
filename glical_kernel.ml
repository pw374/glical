(* -*- coding: utf-8; -*- *)
(* ********************************************************************* *)
(* glical: A library to glance at iCal data using OCaml                  *)
(* (c) 2013/2014, Philippe Wang <philippe.wang@cl.cam.ac.uk>             *)
(* Licence: ISC                                                          *)
(* ********************************************************************* *)

(* Based on RFC5545. Beware: RFC2445 is deprecated. *)

type location = int * int
and name = string
and key = string

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
  and 'a value = {
    location : location;
    to_string : unit -> string;
    to_pretty: unit -> string;
    value : 'a;
  } constraint 'a = [> `Raw of string ]
end
(* open Ical *)

open Printf

exception Syntax_error of string

let syntax_error s ln cn =
  raise (Syntax_error (sprintf "(%d:%d): %s." ln cn s))

let syntax_error_ s (ln, cn) =
  raise (Syntax_error (sprintf "(%d:%d): %s." ln cn s))

let syntax_assert b s ln cn =
  if not b then syntax_error s ln cn

let syntax_assert_ b s (ln, cn) =
  if not b then syntax_error s ln cn

let syntax_warning s ln cn =
  eprintf "Warning (%d:%d): %s.\n" ln cn s

let syntax_warning_ s (ln, cn) =
  eprintf "Warning (%d:%d): %s.\n" ln cn s

let syntax_warning_if cond s ln cn =
  if cond then
    syntax_warning s ln cn
  else
    ()

let syntax_warning_if_ cond s (ln, cn) =
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

module Lexing = struct
type line = { (* output of the lexer *)
  (* Based on "contentline = name *(";" param ) ":" value CRLF"
     from page 9 of http://tools.ietf.org/html/rfc5545#section-3.1 *)
  name: string;
  parameters: (string * string) list;
  value: string;
  (* Locations *)
  name_start: int * int;
  value_start: int * int;
  value_end: int * int;
}

let lex_ical s =
  (* Based on pages 9 and 10 of
     http://tools.ietf.org/html/rfc5545#section-3.1
     contentline   = name *(";" param ) ":" value CRLF
     param         = param-name "=" param-value *("," param-value)
     param-name    = iana-token / x-name
     param-value   = paramtext / quoted-string
     paramtext     = *SAFE-CHAR
     value         = *VALUE-CHAR
     quoted-string = DQUOTE *QSAFE-CHAR DQUOTE
     QSAFE-CHAR    = WSP / %x21 / %x23-7E / NON-US-ASCII
     ; Any character except CONTROL and DQUOTE
     SAFE-CHAR     = WSP / %x21 / %x23-2B / %x2D-39 / %x3C-7E / NON-US-ASCII
     ; Any character except CONTROL, DQUOTE, ";", ":", ","
     VALUE-CHAR    = WSP / %x21-7E / NON-US-ASCII
     ; Any textual character
     NON-US-ASCII  = UTF8-2 / UTF8-3 / UTF8-4
     ; UTF8-2, UTF8-3, and UTF8-4 are defined in [RFC3629]
     CONTROL       = %x00-08 / %x0A-1F / %x7F
     ; All the controls except HTAB *)
  let name = Buffer.create 42
  and value = Buffer.create 42
  and name_start = ref (0,0)
  and value_start = ref (0,0)
  in
  let extract_np s =
    let b = Buffer.create (String.length s) in
    let name = ref s in
    let k = ref "" in
    let parameters = ref [] in
    let rec loop (dq:bool) (p:bool) (eq:bool) i =
      let loop ?(dq=dq) ?(p=p) ?(eq=eq) i = loop dq p eq i in
      if i = String.length s then
        ()
      else match s.[i] with
        | '"' ->
          Buffer.add_char b s.[i];
          loop ~dq:(not dq) (succ i)
        | ';'
        | '='
          when dq
          ->
          Buffer.add_char b s.[i];
          loop (succ i)
        | ';' ->
          if p then (* end of p value *)
            begin
              parameters := (!k, Buffer.contents b) :: !parameters;
              Buffer.clear b;
              loop ~p:true ~eq:false (succ i)
            end
          else (* not p && not dq ==> end of name *)
            begin
              name := Buffer.contents b;
              Buffer.clear b;
              loop ~p:true (succ i)
            end
        | '=' when not eq ->
              k := Buffer.contents b;
              Buffer.clear b;
              loop ~eq:true (succ i)
        | _ ->
          Buffer.add_char b s.[i];
          loop (succ i)
    in
    loop false false false 0;
    !name, List.rev !parameters
  in
  let sl = String.length s in
  let rec loop lines i ~colon ~dquotes ~sc (lc:int) (cc:int) =
    (* colon: separator between  *)
    if i >= sl then
      let name, parameters = extract_np (Buffer.contents name) in
      { name        = name;
        parameters  = parameters;
        value       = Buffer.contents value;
        name_start  = !name_start;
        value_start = !value_start;
        value_end   = lc, cc;}
      ::lines
    else
      match s.[i] with
      | '"' as c ->
        if colon then
          begin
            Buffer.add_char value c;
            loop lines (i+1) ~colon ~dquotes:(not dquotes) ~sc lc (cc+1)
          end
        else
          begin
            Buffer.add_char name c;
            loop lines (i+1) ~colon ~dquotes:(not dquotes) ~sc lc (cc+1)
          end
      | '\n' ->
        begin
          if i >= sl-1 then
            let name, parameters = extract_np (Buffer.contents name) in
            { name        = name;
              parameters  = parameters;
              value       = Buffer.contents value;
              name_start  = !name_start;
              value_start = !value_start;
              value_end   = lc, cc;}
            ::lines
          else if s.[i+1] <> ' ' then
            let nv =
              let name, parameters = extract_np (Buffer.contents name) in
              { name        = name;
                parameters  = parameters;
                value       = Buffer.contents value;
                name_start  = !name_start;
                value_start = !value_start;
                value_end   = lc, cc;
              }
            in
            Buffer.clear name; Buffer.clear value;
            name_start := (lc+1,0);
            loop (nv::lines) (i+1) ~colon:false ~dquotes ~sc (lc+1) 0
          else
            begin
              (* syntax_assert colon "unexpected end of line" lc cc; *)
              loop lines (i+2) ~colon ~dquotes ~sc (lc+1) 0
            end
        end
      | '\r' -> (* just ignore \r for now *)
        loop lines (i+1) ~colon ~dquotes ~sc lc cc
      | ' ' as c ->
        if colon then
          Buffer.add_char value c
        else if dquotes then
          Buffer.add_char name c
        else if not sc then
          syntax_error "unexpected space before colon" lc cc
        else
          Buffer.add_char name c;
        loop lines (i+1) ~colon ~dquotes ~sc lc (cc+1)
      | ':' as c ->
        if dquotes then
          begin
            Buffer.add_char value c;
            loop lines (i+1) ~colon ~dquotes ~sc lc (cc+1)
          end
        else if colon then
          begin
            Buffer.add_char value c;
            loop lines (i+1) ~colon:true ~dquotes ~sc lc (cc+1)
          end
        else
          begin
            value_start := (lc,cc);
            loop lines (i+1) ~colon:true ~dquotes ~sc lc (cc+1)
          end
      | c ->
        let sc = sc || c = ';' in
        if colon then
          begin
            Buffer.add_char value c;
            loop lines (i+1) ~colon ~dquotes ~sc lc (cc+1)
          end
        else
          begin
            Buffer.add_char name c;
            loop lines (i+1) ~colon ~dquotes ~sc lc (cc+1)
          end
  in
  List.rev (loop [] 0 ~colon:false ~dquotes:false ~sc:false 1 0)
end


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


let make_raw location (value:string) : 'a Ical.value = {
  location;
  to_string = (fun () -> value);
  to_pretty = (fun () -> value);
  Ical.value = `Raw value;
}

let make_text location (value:string list) : 'a Ical.value = {
 location;
 to_string = (fun () -> ical_format value);
 to_pretty = (fun () -> String.concat "," value);
 Ical.value = `Text value;
}

let parse_ical l =
  let open Ical in
  let open Lexing in
  let rec loop (res:'a list) (ob:string option) = function
    (* ob = opened blocks *)
    | [] ->
      begin match ob with
        | Some e -> syntax_error (sprintf "unclosed block %s" e) (-1) (-1);
        | None -> res, []
      end
    | {name="BEGIN"; value} as v::tl ->
      syntax_assert_ (v.parameters = []) "unexpected parameters for BEGIN"
        v.name_start;
      let block, tl = loop_rev [] (Some value) tl in
      loop ((Block(v.name_start, value, block))::res) ob tl
    | {name="END"; value} as v::tl ->
      syntax_assert_ (v.parameters = []) "unexpected parameters for START"
        v.name_start;
      assert (v.parameters = []);
      begin match ob with
        | Some x when x = value ->
          res, tl
        | Some x ->
          syntax_error_ (sprintf "unexpected end of block %s, \
                                 expected end of block %s" x value)
            v.name_start
        | None ->
          syntax_error_ (sprintf "unexpected end of block %s" value)
            v.name_start
      end
    | {name; parameters; value} as v::tl ->
      let p =
        List.map (fun (pk, pv) -> pk, make_raw v.value_start pv) parameters
      in
      loop
        ((Assoc(v.name_start, name, p, make_raw v.value_start value))::res)
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
    syntax_error_ (sprintf "unexpected data") v.name_start


(** [map] keeps location and section names, it applies the
    function [f] only to the values. *)
let rec map_values f = let open Ical in function
  | Block(loc, s, v)::tl ->
    Block(loc, s, map_values f v)::map_values f tl
  | Assoc(loc, s, p, r)::tl ->
    let new_s, new_r = f s r in
    Assoc(loc, new_s, p, new_r)::map_values f tl
  | [] -> []


(** [map] keeps location and section names, it applies the
    function [f] to all [Assoc(loc, s, p, r)] elements. *)
let rec map f = let open Ical in function
  | Block(loc, s, v)::tl ->
    Block(loc, s, map f v)::map f tl
  | (Assoc(_, _, _, _) as e)::tl ->
    f e::map f tl
  | [] -> []


(** [iter f ical] applies the function [f] to all [Assoc(loc, s, r)] elements
    of [ical]. *)
let rec iter f = let open Ical in function
  | Block(loc, s, v)::tl ->
    iter f v;
    iter f tl
  | (Assoc(_, _, _, _) as e)::tl ->
    f e;
    iter f tl
  | [] -> ()


let rec sort compare = let open Ical in function
  | Block(loc, s, v)::tl ->
    List.sort compare (Block(loc, s, sort compare v)::(sort compare tl))
  | (Assoc(loc, s, p, r) as e)::tl ->
    List.sort compare (e::sort compare tl)
  | [] -> []

let rec filter f = let open Ical in function
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

let rec fold_on_assocs f accu =  let open Ical in function
  | Block(_, _, v)::tl -> fold_on_assocs f (fold_on_assocs f accu v) tl
  | Assoc(_, k, _, v)::tl -> fold_on_assocs f (f accu k v) tl
  | [] -> accu

let is_nonempty_block = let open Ical in function
    | Block(_, _, []) -> false
    | _ -> true

let is_empty_block = let open Ical in function
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
 let open Ical in
  let string_of_p p =
    let b = Buffer.create 42 in
    List.iter
      (fun (k, v) -> bprintf b ";%s:%s" k (v.to_string()))
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
           limit_to_75_bytes [s ^ string_of_p p ^ ":" ^ r.to_string()]);
      Buffer.add_string b "\r\n";
      loop tl
  in
  loop t;
  Buffer.contents b




module Datetime =
struct
  (** http://tools.ietf.org/html/rfc5545#section-3.3.5 *)

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

  let make location t = {
    location;
    to_string = (fun () -> to_string t);
    to_pretty = (fun () -> to_string t);
    Ical.value = `Datetime t;
  }

  let parse_datetime t = let open Ical in
    map
      (function
        | Assoc(loc, ("DTSTAMP"|"DTSTART"|"DTEND" as l), p, v) ->
          Assoc(loc, l, p, make v.location (parse v.location (v.to_string())))
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
