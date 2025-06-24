#!/bin/bash

set -e  # Exit on any command failure
set -u  # Treat unset variables as an error

PROGRAM=$(basename "$0")
readonly DIR=$(pwd)
VERBOSE=false

function usage() {
    cat <<EOF

Install script for ORION

Usage:
  $PROGRAM [GLOBAL_OPTIONS] COMMAND [COMMAND_OPTIONS]

Global Options:
  -h       , --help         Show this help message and exit
  -v       , --verbose      Enable verbose output

Commands:
  build                     Perform the full build
    --use-tecio             Use TecIO

  compile                   Compile the program using the CMakePresets file

  setvars                   Set project paths in environment variables

EOF
    exit 1
}


function log() {
    if [ "$VERBOSE" = true ]; then
        echo "$1"
    fi
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
  log "Defining paths..."
  rm -f .setvars.sh

  echo 'export ORIONDIR='$DIR >> .setvars.sh
  if [[ $SHELL == *"zsh"* ]]; then
    echo 'ORION () { '$DIR'/bin/app/converter $@; }' >> .setvars.sh
    RCFILE=$HOME/.zshrc
  elif [[ $SHELL == *"bash"* ]]; then
    echo 'function ORION () { '$DIR'/bin/app/converter $@; }' >> .setvars.sh
    RCFILE=$HOME/.bashrc
  fi
  echo 'export -f ORION' >> .setvars.sh
  grep -v "ORION" $RCFILE > tmpfile && mv tmpfile $RCFILE
  echo 'source '$DIR'/.setvars.sh' >> $RCFILE
  source $RCFILE --force
}


# Create default CMakePresets.json if it doesn't exist
function write_presets() {
  cat <<EOF > CMakePresets.json
{
  "version": 3,
  "cmakeMinimumRequired": {
    "major": 3,
    "minor": 23
  },
  "configurePresets": [
    {
      "name": "default",
      "description": "Default preset",
      "binaryDir": "\${sourceDir}/build",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "${BUILD_TYPE}",
        "USE_TECIO": "${USE_TECIO}"
      }
    }
  ]
}
EOF
  echo "CMakePresets.json created with default settings."
}


# Default global values
COMMAND=""
BUILD_TYPE="RELEASE"
USE_TECIO=false

# Define allowed options for each command using regular arrays
CMD_OPTIONS_BUILD=("--use-tecio")

# Parse options with getopts
while getopts "hv:-:" opt; do
    case "$opt" in
        -)
            case "$OPTARG" in
                verbose) VERBOSE=true ;;
                help) usage ;;
                *) echo "Error: Unknown global option '--$OPTARG'"; usage ;;
            esac
            ;;
        h) usage ;;
        v) VERBOSE=true ;;
        *) echo "Error: Unknown global option '-$opt'"; usage ;;
    esac
done
shift $((OPTIND -1))

# Ensure a command was provided
if [[ $# -eq 0 ]]; then
    echo "Error: No command provided!"
    usage
fi

COMMAND="$1"
shift

# Parse command-specific options
while [[ $# -gt 0 ]]; do
    case "$1" in
        --use-tecio)
            [[ "$COMMAND" == "build" ]] || { echo "Error: --use-tecio is only valid for 'build' command"; exit 1; }
            USE_TECIO=true
            ;;
        *)
            echo "Error: Unknown option '$1' for command '$COMMAND'. Valid options: ${CMD_OPTIONS_$COMMAND[@]}"
            exit 1
            ;;
    esac
    shift
done


# Execute the selected command
case "$COMMAND" in
    build)
        log "Building project"
        # download Doxygen
        #./doxygen .Doxyfile
        define_path
        rm -rf build
        cmake -B build -DUSE_TECIO=$USE_TECIO -DCMAKE_BUILD_TYPE=$BUILD_TYPE || exit 1
        cmake --build build || exit 1
        write_presets
        ;;
    compile)
        if [[ -z "$BUILD_TYPE" ]]; then
            echo "Error: --build-type is required for 'compile' command!"
            exit 1
        fi
        log "Compiling with build type: $BUILD_TYPE"
        cd $DIR/build
        cmake .. -DCMAKE_BUILD_TYPE=$BUILD_TYPE -DUSE_TECIO=ON
        make
        ;;
    setvars)
        log "Setting project environment variables"
        define_path
        ;;
    *)
        echo "Error: Unknown command '$COMMAND'"
        usage
        ;;
esac