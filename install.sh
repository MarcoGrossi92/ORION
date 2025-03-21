#!/bin/bash

set -e  # Exit on any command failure
set -u  # Treat unset variables as an error

PROGRAM=$(basename "$0")
readonly DIR=$(pwd)
VERBOSE=false
CMD_OPTIONS=()  # Replace with a regular array

function usage() {
    cat <<EOF

Install script for ORION

Usage:
  $PROGRAM [GLOBAL_OPTIONS] COMMAND [COMMAND_OPTIONS]

Global Options:
  -h       , --help            Show this help message and exit
  -v       , --verbose         Enable verbose output

Commands:
  build                        Perform the full build

  compile                      Compile the program
    --build-type=<build>       Set build type (release, debug, testing, default: release)

  setvars                      Set project paths in environment variables

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


# Default global values
COMMAND=""
BUILD_TYPE=""

# Define allowed options for each command using regular arrays
CMD_OPTIONS_COMPILE=("--build-type")

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
        --build-type=*)
            [[ "$COMMAND" == "compile" ]] || { echo "Error: --build-type is only valid for 'compile' command"; exit 1; }
            BUILD_TYPE="${1#*=}"
            ;;
        *)
            echo "Error: Unknown option '$1' for command '$COMMAND'. Valid options: ${CMD_OPTIONS[$COMMAND]}"
            exit 1
            ;;
    esac
    shift
done


# Execute the selected command
case "$COMMAND" in
    build)
        if [[ -z "$OS_TYPE" || -z "$MASTER_TYPE" ]]; then
            echo "Error: --os and --master are required for the 'build' command!"
            exit 1
        fi
        log "Building project"
        # download Doxygen
        #./doxygen .Doxyfile
        define_path
        rm -rf bin build && mkdir -p build
        cd $DIR/lib/TecIO
        ./build.sh
        mkdir -p $DIR/build
        cd $DIR/build
        cmake .. -DCMAKE_BUILD_TYPE=RELEASE -DUSE_TECIO=ON
        make
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