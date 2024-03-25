#!/bin/bash -
#===============================================================================
#
#          FILE: intstall.sh
#
#         USAGE: run "./install.sh [options]" from ORION master directory
#
#   DESCRIPTION: A utility script that builds ORION project
#===============================================================================

# DEBUGGING
set -e
set -C # noclobber

# INTERNAL VARIABLES AND INITIALIZATIONS
readonly PROJECT="ORION"
readonly DIR=$(pwd)
readonly PROGRAM=`basename "$0"`

function usage () {
    echo "Install script of $PROJECT"
    echo "Usage:"
    echo
    echo "$PROGRAM --help|-?"
    echo "    Print this usage output and exit"
    echo
    echo "$PROGRAM --build  |-b"
    echo "    Build the whole project via CMake"
    echo
    echo "$PROGRAM --compile|-c <type>"
    echo "    Compile with build <type> (DEBUG, RELEASE)"
    echo
    echo "$PROGRAM --setvars|-s"
    echo "    Set the project paths in the environment variables"
    echo
}


function download_extra () {
  [[ -f doxygen ]] && return
  if [[ $1 == linux ]]; then
    url=https://www.doxygen.nl/files/doxygen-1.10.0.linux.bin.tar.gz
    curl -O $url
    tar zxf *.tgz
  fi
}


function define_path () {
  rm -f .setvars.sh

  echo 'export ORIONDIR='$DIR >> .setvars.sh
  if [[ $SHELL == *"zsh"* ]]; then
    echo 'vts2tec () { '$DIR'/bin/app/vts2tec $@; }' >> .setvars.sh
    RCFILE=$HOME/.zshrc
  elif [[ $SHELL == *"bash"* ]]; then
    echo 'function vts2tec () { '$DIR'/bin/app/vts2tec $@; }' >> .setvars.sh
    RCFILE=$HOME/.bashrc
  fi
  echo 'export -f vts2tec' >> .setvars.sh
  grep -v "ORION" $RCFILE > tmpfile && mv tmpfile $RCFILE
  echo 'source '$DIR'/.setvars.sh' >> $RCFILE
  source $RCFILE --force
}

function build_project () {
  # download Doxygen
  #./doxygen .Doxyfile
  cd lib/TecIO
  ./build.sh
  cd ../../
  rm -rf bin build && mkdir -p build
  cd build
  cmake .. -DUSE_OPENMP=OFF -DCMAKE_BUILD_TYPE=RELEASE
  make -j
}

function compile () {
  mkdir -p build
  cd build
  cmake .. -DCMAKE_BUILD_TYPE=$TYPE
  make -j
}

SETVARS=0
BUILD=0
TYPE=0

# RETURN VALUES/EXIT STATUS CODES
readonly E_BAD_OPTION=254

# PROCESS COMMAND-LINE ARGUMENTS
if [ $# -eq 0 ]; then
  usage
  exit 0
fi

while test $# -gt 0; do
  if [ x"$1" == x"--" ]; then
    # detect argument termination
    shift
    break
  fi
  case $1 in

    --build | -b )
      shift
      BUILD=1
      ;;

    --compile | -c )
      shift
      TYPE="$1"
      ;;

    --setvars | -s )
      shift
      SETVARS=1
      ;;

    -? | --help )
      usage
      exit
      ;;

    -* )
      echo "Unrecognized option: $1" >&2
      usage
      exit $E_BAD_OPTION
      ;;

    * )
      break
      ;;
  esac
done

if [ "$SETVARS" != "0" ]; then
  define_path
elif [ "$BUILD" != "0" ]; then
  define_path
  build_project
elif [ "$TYPE" != "0" ]; then
  compile
else
  usage
fi
