#!/bin/bash

export PRESUBMIT_STUDENT_BASE=Formative1

HASKELL_DEPS="mtl" # space-separated list of packages
CMD="env -u LC_CTYPE runhaskell presubmit-res/presubmit.hs"
export PRESUBMIT_VERBOSE=1

# Check whether this script is running in bash. Otherwise, replace the current
# shell with bash.

if [ -z "$BASH" ]
then
    exec bash $0
fi

# Invoke tcsh, load the right modules, then transfer control to the right
# Haskell script running in the right environment.

# The LC_CTYPE thing was because funny character encodings or terminal types
# once screwed up our presubmit. In that case it was because Python3 takes
# character encodings from the environment, even for scripts it runs, so I
# suppose with the current Haskell-only script that shouldn't be an issue.

if hostname -f | grep cca-\[ul\]g04 > /dev/null; then
    if ghc --version | grep 'version 7.6.3' > /dev/null; then
        echo "Correct version of GHC not found. Try running 'module load ghc'."
        exit 1
    elif ghc --version | grep 'version 8.0\.' > /dev/null; then
        true
    else
        echo "Unexpected version of GHC found; expected 8.0.*."
        echo "Please complain to the teaching assistants."
        exit 1
    fi
    export PRESUBMIT_IN_LAB=yes
else
    echo Warning: you are not executing this on a lab machine. This presubmit test may give wonky results.
    echo Warning: you are not executing this on a lab machine. This presubmit test may give wonky results.
    echo Warning: you are not executing this on a lab machine. This presubmit test may give wonky results.
    echo
    export PRESUBMIT_IN_LAB=no
fi

if test -d presubmit-res
then
    true
else
    echo
    echo "I can't find a presubmit-res directory here in $(pwd)."
    echo "Are you in the right directory?"
    exit 1
fi

# Check if the right Haskell packages are installed to execute the presubmit.
# Only an approximate test; the regexp is slightly hacky.

HASKELL_DEPS_REGEXP=$(echo $HASKELL_DEPS | tr ' ' '\n' | sort | tr '\n' ' ' | sed 's/ /.*/g')

if ghc-pkg list | sort | sed 's/-.*//;' | tr '\n' ' ' \
       | grep "$HASKELL_DEPS_REGEXP" > /dev/null; then

    export PRESUBMIT=yesplease

    $CMD
else
    echo "It seems that you don't have the right Haskell packages installed"
    echo "for the presubmit. Try the following commands:"
    echo
    echo "    cabal update"
    echo "    cabal install $HASKELL_DEPS"
    exit 1
fi

