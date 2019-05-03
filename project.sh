#!/usr/bin/env bash

CABAL_FLAGS=""

cmd="$1"

shift

case "$cmd" in
  install)
    cabal new-install \
      --symlink-bindir=$HOME/.local/bin \
      -j8 --overwrite-policy=always --disable-documentation \
      exe:hw-json
      $CABAL_FLAGS "$@"
    ;;

  build)
    cabal new-build all -j8 \
      --disable-tests --disable-benchmarks \
      $CABAL_FLAGS "$@"
    ;;
  
  exec)
    cabal new-exec "$(echo *.cabal | cut -d . -f 1)" "$@"
    ;;

  test)
    cabal new-test -j8 --enable-tests --disable-documentation \
      $CABAL_FLAGS "$@"
    ;;

  bench)
    cabal new-bench -j8 \
      $CABAL_FLAGS "$@"
    ;;

  repl)
    cabal new-repl \
      $CABAL_FLAGS "$@"
    ;;

  clean)
    cabal new-clean
    ;;

  publish)
    for x in */*.cabal; do
      (
        echo "== $(dirname $x) =="
        cd $(dirname $x)
        cabal check
        cabal v2-sdist
      )
    done

    cabal upload --publish dist-newstyle/sdist/*.tar.gz
    ;;
  
  *)
    echo "Unrecognised command: $cmd"
    exit 1
    ;;
esac

# haskell-ide-engine work-around
for x in $(find dist-newstyle -name setup-config | grep '/opt/setup-config$' | sed 's|/opt/setup-config$||g'); do
  ( cd $x
    ln -fs opt/setup-config setup-config
  )
done

