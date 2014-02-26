#!/bin/bash

set -x

V="$(cat version)"

if [[ "$V" != "" ]]
then
    rm -fr glical-$V* glical.$V*
else
    echo "Error: The version is unset."
    exit 1
fi

######################################################################
# generate the distribution package

p=glical-$V
mkdir $p

cp Makefile.lib $p/

cat > $p/Makefile <<\EOF
build:glical.cma glical.cmxa	

install:glical.cma glical.cmxa
	mkdir -p ${PREFIX}/lib/glical/
	cp $+ ${PREFIX}/lib/glical/
#	cp glical ${PREFIX}/bin/

uninstall:
	rm -f ${PREFIX}/lib/glical/glical.cm{a,xa}
	-rmdir ${PREFIX}/lib/glical
#	rm -f ${PREFIX}/bin/glical

include Makefile.prefix

include Makefile.lib

EOF

cat >> $p/configure <<\EOF
#!/bin/bash
if [[ "$1" == "-prefix" ]] && [[ "$2" != "" ]]
then
  echo "PREFIX=\"$2\"" > Makefile.prefix
else
  >&2 echo "Warning: \$@ is $@; \$1 is $1; and \$2 is $2."
  >&2 echo "Warning: I'm using /usr as the prefix."
  >&2 echo "Warning: Re-run configure with -prefix path if you want another a prefix."
  echo "PREFIX=\"/usr\"" > Makefile.prefix
  exit 1
fi
EOF
chmod a+x $p/configure

cp glical.ml glical_kernel.ml glical.mli glical_cat.ml glical_kernel.mli $p/

tar cvzf $p.tar.gz $p

######################################################################
# generate the opam package

P=glical.$V
mkdir $P

cat > $P/descr <<\EOF
Glical: glancing at iCalendar data.
A library to glance at iCalendar data using OCaml.
EOF
cat > $P/opam <<\EOF
maintainer: "philippe.wang@gmail.com"
name: "glical"
opam-version: "1"
license: "ISC"
authors: [ "Philippe Wang <philippe.wang@gmail.com>" ]
homepage: "https://github.com/pw374/glical"
build: [
  ["./configure" "-prefix" prefix]
  [make "build"]
  [make "install"]
]
remove: [
  ["./configure" "-prefix" prefix]
  [make "uninstall"]
]
ocaml-version: [ >= "3.12.1" ]
tags: [
  "org:ocamllabs"
]
EOF

cat > $P/url <<EOF
archive: "http://pw374.github.io/distrib/glical/$p.tar.gz"
checksum: "$(md5 < $p.tar.gz)"
EOF

cat <<EOF
# TODO:
mkdir -p ../pw374.github.io/distrib/glical
cp $p.tar.gz ../pw374.github.io/distrib/glical/
cd "$PWD/../pw374.github.io/distrib/glical/" && git add $p.tar.gz && git commit $p.tar.gz -m 'add $p.tar.gz'

mkdir -p "$PWD/../opam-repository/packages/glical/"
cp -r "$PWD/$P" "$PWD/../opam-repository/packages/glical/"
cd "$PWD/../opam-repository/packages/glical/"

EOF
