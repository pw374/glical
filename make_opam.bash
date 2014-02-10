#!/bin/bash


######################################################################
# generate the distribution package

p=glical-$V
mkdir $p

cat Makefile > $p/Makefile
cat >> $p/Makefile <<EOF
build:
	${MAKE}

install:
	cp glical.cm{a,xa} ${PREFIX}/lib/glical/
	cp glical ${PREFIX}/bin/

uninstall:
	rm -f ${PREFIX}/lib/glical/glical.cm{a,xa}
	rm -f ${PREFIX}/bin/glical

include Makefile.prefix
EOF

cat >> $p/configure <<EOF
#!/bin/sh
if [ "$1" == "-prefix" ] && [ "$2" != "" ]
then
  echo "PREFIX=\"$2\"" > Makefile.prefix
else
  exit 1
fi
EOF
chmod a+x $p/configure

tar cvzf $p.tar.gz $p

######################################################################
# generate the opam package

p=glical.$V
mkdir $p

cat > $p/descr <<EOF
Glical: glancing at iCalendar data.
+ A library to glance at iCalendar data using OCaml.
+ A command-line tool to play with iCalendar data.
EOF
cat > $p/opam <<EOF
maintainer: "philippe.wang@gmail.com"
name: "Glical"
opam-version: "1"
license: "ISC"
authors: [ "Philippe Wang <philippe.wang@gmail.com>" ]
homepage: "https://github.com/pw374/glical"
build: [
  ["./configure" "-prefix" prefix]
  [make "build"]
  [make "install"]
]
remove : [
  ["./configure" "-prefix" prefix]
  [make "uninstall"]
]
ocaml-version [ >= "4.01.0" ]
tags: [
  "org:ocamllabs"
]
EOF

cat > $p/url <<EOF
archive: "http://pw374.github.io/distrib/glical/$p.tar.gz"
checksum: "$(md5 < $p.tar.gz)"
EOF
