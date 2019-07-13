get_cabal_version() { cat $1/$1.cabal | grep '^Version: ' | sed -e 's/^Version: //g'; }

mk_release_name() { echo "$1-$(get_cabal_version $1)-x86_64-macos.tar.bz2"; }
