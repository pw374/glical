Glical: glancing at iCal data using OCaml
=========================================

Description
-----------

Glical  is  a rather  small  library  for  OCaml \[1]  programmers  to
manipulate iCal data.  Since iCalendar  \[2] is gigantic and very hard
if not  virtually impossible  to address  fully, we  here call  iCal a
subset of  iCalendar.  And  since even  iCal is too  big to  really be
addressed, we state that we provide a library that allows to glance at
iCal data.

By "glancing iCal data" we mean  "processing iCal data" with as little
knowledge of  the iCal(endar)  format as  possible.  This  library has
been developed  with the  idea that  very few  properties of  the iCal
format are sufficient  for most people to transform some  iCal data or
extract the information that they want from it.

This library is implemented in pure  OCaml. This means that any direct
user of  this library should not  have to worry about  any dependency.
This library requires  some features introduced in  OCaml 4.01.0 \[3],
so it will not compile with previous versions of the OCaml compiler.


\[1]: The OCaml community web site: <http://ocaml.org>

\[2]: F. Dawson  and D. Stenerson, *RFC2445:  Internet Calendaring and
Scheduling    Core    Object   Specification    (iCalendar)*,    1998,
<http://tools.ietf.org/html/rfc2445>

\[3]: OCaml 4.01, <http://caml.inria.fr/pub/distrib/ocaml-4.01/>
<http://caml.inria.fr/pub/distrib/ocaml-4.01/notes/Changes>


Please see PLAY.md for instructions. 

Distribution
------------

The development of this library is at the early stage. 
Do feel free to try it and give feedbacks.
There should be just a single `.ml` file for this library,
and a `.mli` file is on its way if it's not already there.

Contribution
------------

Please feel free to fork this library.
Pull requests are welcome.

Issues
------

Please do feel free to report any bug or any issue on Github's issues
tracker: <https://github.com/pw374/glical/issues>


Licence
-------

This library is  freely distributed under the ISC licence,  which is a
BSD-like licence.


Author
------

Philippe Wang <Philippe.Wang@cl.cam.ac.uk>




