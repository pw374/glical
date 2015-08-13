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

DESCR='Glical: glancing at iCalendar data.'

cat > META <<EOF
version = "$V"
description = "$DESCR."
archive(byte) = "glical.cma"
archive(native) = "glical.cmxa"
exists_if = "glical.cma"
EOF

cat > $p/Makefile <<\EOF
build:glical.cma glical.cmxa	

install:glical.cma glical.cmxa glical.a glical.mli glical.cmi glical.cmo glical.cmx glical_kernel.cmi glical_kernel.mli glical_kernel.cmo glical_kernel.cmx
	mkdir -p ${PREFIX}/lib/glical/
	ocamlfind install -destdir ${PREFIX}/lib glical $+ META
#	cp $+ ${PREFIX}/lib/glical/
#	cp glical ${PREFIX}/bin/

uninstall:
	ocamlfind remove -destdir ${PREFIX}/lib glical
#	rm -f ${PREFIX}/lib/glical/glical.cm{a,xa}
#	-rmdir ${PREFIX}/lib/glical
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

cp glical.ml glical_kernel.ml glical.mli glical_cat.ml glical_kernel.mli META $p/

tar cvzf $p.tar.gz $p

######################################################################
# generate the opam package

P=glical.$V
mkdir $P

cat > $P/descr <<EOF
$DESCR
A library to glance at iCalendar data using OCaml.
EOF
cat > $P/opam <<\EOF
maintainer: "philippe.wang@gmail.com"
name: "glical"
opam-version: "1.2"
license: "ISC"
authors: [ "Philippe Wang <philippe.wang@gmail.com>" ]
homepage: "https://github.com/pw374/glical"
build: [
  ["./configure" "-prefix" prefix]
  [make "build"]
]
install: [
  [make "install"]
]
remove: [
  ["./configure" "-prefix" prefix]
  [make "uninstall"]
]
depends: [
  "ocamlfind"
]
tags: [
  "org:ocamllabs"
]
available: [ocaml-version >= "3.12.1"]
dev-repo: "https://github.com/pw374/glical.git"
bug-reports: "https://github.com/pw374/glical/issues"
EOF

cat > $P/url <<EOF
archive: "http://pw374.github.io/distrib/glical/$p.tar.gz"
checksum: "$(md5 < $p.tar.gz)"
EOF

cat <<EOF
###############################################################################
# TODO:
mkdir -p ../pw374.github.io/distrib/glical
cp $p.tar.gz ../pw374.github.io/distrib/glical/
cd "$PWD/../pw374.github.io/distrib/glical/" && git add $p.tar.gz && git commit $p.tar.gz -m 'add $p.tar.gz' && git push

mkdir -p "$PWD/../opam-repository/packages/glical/"
cp -r "$PWD/$P" "$PWD/../opam-repository/packages/glical/"
cd "$PWD/../opam-repository/packages/glical/"
git pull
git add "$P"
git push pw374
###############################################################################
EOF
