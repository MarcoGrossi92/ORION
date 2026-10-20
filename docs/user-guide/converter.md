# Command-line Converter

The ORION command-line converter is a standalone utility for converting scientific data between Tecplot, VTK, and PLOT3D formats without writing any code.

## Installation

The converter is built automatically when installing ORION from source:

```bash
./install.sh build --compilers=gnu --use-tecio
```

The executable is located at `bin/app/converter`. Without `--use-tecio` the converter has no binary Tecplot (`.szplt`)
support: asked for one, it stops with an error (see Troubleshooting).

### The `ORION` command

To run the converter from any directory under the name `ORION`, which this documentation uses, source the script of the
ORION folder in your (bash) shell:

```bash
source scripts/setvars.sh
```

It defines the shell function `ORION`, which runs `bin/app/converter`, for the current shell. A shell function is not
inherited by the programs a script starts (GNU `parallel`, for instance): they call the executable, `$ORIONDIR/bin/app/converter`
(`ORIONDIR` is exported by the script). [Installation](../getting-started/installation.md#environment-setup) shows how to
make the command permanent, or to put the executable in your `PATH` instead.

## Basic Usage

### Command Syntax

```bash
ORION --in-file=<file> --out-file=<file> [OPTIONS]
```

The format of each file is that of its extension: the text after the last dot of its name (a dot in a directory of the
path, such as `cases.dat/out.vtm`, is not an extension).

| Extension | Format | Input | Output |
|-----------|--------|:-----:|:------:|
| `.dat`, `.tec` | Tecplot ASCII | ✅ | ✅ |
| `.szplt` | Tecplot binary (TecIO build) | ✅ | ✅ |
| `.vtm` | VTK multi-block: the `.vtm` file and the `.vts` block files next to it | ✅ | ✅ |
| `.p3d` | PLOT3D grid | ✅ (ASCII, Fortran unformatted, C binary) | ✅ (ASCII) |

### Arguments

| Argument | Description | Default |
|----------|-------------|---------|
| `--in-file=<file>` | Path to the input file | required |
| `--out-file=<file>` | Path to the output file | required |
| `--in-format=<format>` | Format of the block files of a `.vtm` input: `ascii`, `binary` or `raw` | taken from the files |
| `--out-format=<format>` | Format of the block files of a `.vtm` output (`ascii`, `binary` or `raw`) or of a `.p3d` output (`ascii`) | `binary` for `.vtm`, `ascii` for `.p3d` |
| `--help`, `-h` | Display help message | — |

The options can be given in any order. An option that the file cannot use is refused (for instance
`--out-format=binary` for a `.p3d` file) and nothing is read or written. The Tecplot formats follow the extension of the
file, and a PLOT3D input is read in whatever layout it is in, detected from the file.

## Conversion Examples

**Tecplot to VTK**

```bash
mkdir vtk
ORION --in-file=simulation.tec --out-file=vtk/visualization.vtm
```

For Tecplot binary files (`.szplt`):

```bash
ORION --in-file=results.szplt --out-file=vtk/results.vtm
```

The converter writes the `.vtm` file and one block file for each block of the input, `Block1.vts`, `Block2.vts`, ..., in
the directory of the `.vtm` file: write each output in a directory of its own. The block files are in binary
(`--out-format=binary`, the default), in ASCII or in raw appended data.

The variables stay where the Tecplot file holds them: variables at the nodes are written as point data of the VTK files,
variables at the cells as cell data.

The variables keep their names (those of the `VARIABLES` list, or of the arrays of VTK files), with the blanks, commas,
tabs and quotes of a name replaced by underscores, since a list of names cannot hold them (`"Mass fraction"` becomes
`Mass_fraction`); a variable without a name is called `var1`, `var2`, .... The blocks are named `Block1`, `Block2`, ....
PLOT3D files have no variables. A Tecplot file in POINT packing (`F=POINT`, one point per line) is read as
any other: its variables are at the points, and they are point data of the VTK files.

A `.dat` file is read and written as a `.tec` file, in ASCII: with one, two or three coordinates and any number of
variables, in BLOCK or POINT packing when read and in BLOCK packing when written.

**VTK to Tecplot**

```bash
ORION --in-file=visualization.vtm --out-file=data.tec
```

The converter reads the `.vtm` file and the block files it lists, from the directory of the `.vtm` file, and takes their
format (`ascii`, `binary` or `raw`) from the files; `--in-format` gives it when the files cannot tell. The variables stay
where the VTK files hold them: point data are written at the nodes, cell data at the cells.

!!! warning "VTK input"
    The converter reads the files that ORION writes: the `.vts` block files (`StructuredGrid`) with the `Points`
    array before the data arrays. A `.vtm` file written by another program that lists its data arrays first is refused with
    an error. A `.vtk` file is not one of the formats of the converter.

**PLOT3D to Tecplot or VTK**

```bash
ORION --in-file=grid.p3d --out-file=grid.tec
ORION --in-file=grid.p3d --out-file=grid/grid.vtm
```

**Tecplot or VTK to PLOT3D**

```bash
ORION --in-file=data.tec --out-file=grid.p3d
ORION --in-file=visualization.vtm --out-file=grid.p3d
```

!!! warning "PLOT3D Files"
    ORION can work only with grid files: a PLOT3D file has no variables, so a conversion to PLOT3D writes the grid and
    says (`WARNING the variables are not written`) that it leaves them out. A PLOT3D grid has two or three coordinates
    in every block: a field of one coordinate (a line of points) is refused, and PLOT3D files are written in ASCII only.

## Batch Conversion

**Using Shell Loops**

Convert multiple files:

```bash
# Convert all .tec files in directory to VTK, in a directory for each file
for file in *.tec; do
    name="${file%.tec}"
    mkdir -p "$name"
    ORION --in-file="$file" --out-file="$name/$name.vtm"
done
```

**Parallel Processing**

For large datasets, use GNU parallel:

```bash
# Install parallel
sudo apt install parallel  # Ubuntu/Debian
brew install parallel      # macOS

# Convert files in parallel
ls *.tec | parallel -j 4 'mkdir -p {.} && $ORIONDIR/bin/app/converter --in-file={} --out-file={.}/{.}.vtm'
```

**Batch Script Example**

Create a conversion script:

```bash
#!/bin/bash
# convert_batch.sh - Convert multiple Tecplot files to VTK

INPUT_DIR="./tecplot_files"
OUTPUT_DIR="./vtk_files"
CONVERTER="/path/to/ORION/bin/app/converter"

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Convert all .tec files
for input_file in "$INPUT_DIR"/*.tec; do
    filename=$(basename "$input_file" .tec)
    mkdir -p "$OUTPUT_DIR/$filename"

    echo "Converting $filename..."
    $CONVERTER --in-file="$input_file" --out-file="$OUTPUT_DIR/$filename/$filename.vtm" > /dev/null

    if [ $? -eq 0 ]; then
        echo "  ✓ Success"
    else
        echo "  ✗ Failed"
    fi
done

echo "Batch conversion complete!"
```

Usage:
```bash
chmod +x convert_batch.sh
./convert_batch.sh
```

## Troubleshooting

When the converter cannot read its input or write its output, it prints a line that begins with `ERROR` and ends with the
exit status 1, without writing the output file:

| Message | Cause |
|---------|-------|
| `ERROR no input file given`, `ERROR no output file given` | `--in-file=` or `--out-file=` is missing |
| `ERROR unsupported input format: <file>` | The extension of the file is none of `.dat .tec .szplt .p3d .vtm` |
| `ERROR unsupported input format: <format>` (or `output`) | `--in-format=` or `--out-format=` is not one the file can have |
| `ERROR read Tecplot file` | The Tecplot file cannot be opened or read (the reader prints the reason on the standard error) |
| `ERROR read VTK files` | The `.vtm` file or a block file it lists cannot be opened or read, or `--in-format` is not their format |
| `ERROR read PLOT3D file: <reason>` | The PLOT3D file matches no layout, or more than one: the reason says which |
| `ERROR write Tecplot file`, `ERROR write VTK files` | The output could not be written: a directory that does not exist, no permission, a full disk (the Tecplot writer prints the reason on the standard error) |
| `ERROR write PLOT3D file` | The mesh is not a PLOT3D grid (one coordinate, blocks with a different number of coordinates), or the file could not be written (the writer prints the reason on the standard error) |

### File Not Found

`ERROR read ... file` for a file that does not exist.

**Solution:** Check file path and permissions:
```bash
ls -l input_file.tec
file input_file.tec
```

### Wrong VTK Format

`ERROR read VTK files` for a `.vtm` file whose block files exist.

**Solution:** The converter takes the format of the block files from the files. If you gave `--in-format`, remove it, or
give the format the files are in (`ascii`, `binary` or `raw`).

### Out of Memory

The converter reads the whole field into memory and then writes it, so it needs room for the field (and, for a conversion
between formats, no more than that). A conversion that does not fit stops with the error of the system.

**Solution:**

- Check dataset size and available RAM
- Convert on a machine with more memory, or increase system swap space

### TecIO Not Available

```
You can not write in binary format without compiling against TecIO
```

A converter built without TecIO stops with error termination: this message (`read` in place of `write` for a binary input
file), after `ERROR STOP` and followed by a backtrace with gfortran, and a nonzero exit status, without writing the output
file.

**Solution:** Rebuild with TecIO:
```bash
./install.sh build --compilers=gnu --use-tecio
```

### Output Directory

The converter does not create directories: the directory of the output file must exist, and be writable. A `.vtm`
output also writes its block files (`Block1.vts`, ...) there. Otherwise the conversion ends with the `ERROR write ...`
line of the format and the exit status 1, and writes nothing.

**Solution:** Create the directory before the conversion, and check its permissions:
```bash
mkdir -p vtk
ls -ld vtk/

# Fix permissions if needed
chmod u+w vtk/
```
