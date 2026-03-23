# Command-line Converter

The ORION command-line converter is a standalone utility for converting scientific data between Tecplot, VTK, and PLOT3D formats without writing any code.

## Installation

The converter is built automatically when installing ORION from source:

```bash
./install.sh build --compiler=gnu --use-tecio
```

The executable is located at `bin/app/ORION`.

### Adding to PATH

To use the converter from any directory:

```bash
# Temporary (current session only)
export PATH="/path/to/ORION/bin/app:$PATH"

# Permanent (add to ~/.bashrc or ~/.zshrc)
echo 'export PATH="/path/to/ORION/bin/app:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

Or use the provided script:

```bash
./install.sh setvars
```

## Basic Usage

### Command Syntax

```bash
ORION --input-format <format> --input-file <file> \
      --output-format <format> --output-file <file> \
      [OPTIONS]
```

### Required Arguments

| Argument | Description | Values |
|----------|-------------|--------|
| `--input-format` | Input file format | `tecplot`, `vtk`, `plot3d` |
| `--input-file` | Path to input file | Any valid file path |
| `--output-format` | Output file format | `tecplot`, `vtk`, `plot3d` |
| `--output-file` | Path to output file | Any valid file path |

### Optional Arguments

| Argument | Description | Default |
|----------|-------------|---------|
| `--help`, `-h` | Display help message | — |
| `--verbose`, `-v` | Enable verbose output | Off |

## Conversion Examples

**Tecplot to VTK**

```bash
ORION --input-format tecplot --input-file simulation.tec \
      --output-format vtk --output-file visualization.vtk
```

For Tecplot binary files (`.plt`, `.szplt`):

```bash
ORION --input-format tecplot --input-file results.szplt \
      --output-format vtk --output-file results.vtk
```

**VTK to Tecplot**

```bash
ORION --input-format vtk --input-file mesh.vtk \
      --output-format tecplot --output-file mesh.tec
```

**PLOT3D to Tecplot**

```bash
ORION --input-format plot3d --input-file grid.p3d \
      --output-format tecplot --output-file combined.tec
```

!!! warning "PLOT3D Files"
    ORION can work only with grid files.

**PLOT3D to VTK**

```bash
ORION --input-format plot3d --input-file grid.p3d \
      --output-format vtk --output-file output.vtk
```

!!! warning "PLOT3D Files"
    ORION can work only with grid files.

## Batch Conversion

**Using Shell Loops**

Convert multiple files:

```bash
# Convert all .tec files in directory to VTK
for file in *.tec; do
    output="${file%.tec}.vtk"
    ORION --input-format tecplot --input-file "$file" \
          --output-format vtk --output-file "$output"
done
```

**Parallel Processing**

For large datasets, use GNU parallel:

```bash
# Install parallel
sudo apt install parallel  # Ubuntu/Debian
brew install parallel      # macOS

# Convert files in parallel
ls *.tec | parallel -j 4 \
    'ORION --input-format tecplot --input-file {} \
           --output-format vtk --output-file {.}.vtk'
```

**Batch Script Example**

Create a conversion script:

```bash
#!/bin/bash
# convert_batch.sh - Convert multiple Tecplot files to VTK

INPUT_DIR="./tecplot_files"
OUTPUT_DIR="./vtk_files"
CONVERTER="/path/to/ORION/bin/app/ORION"

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Convert all .tec files
for input_file in "$INPUT_DIR"/*.tec; do
    filename=$(basename "$input_file" .tec)
    output_file="$OUTPUT_DIR/${filename}.vtk"
    
    echo "Converting $filename..."
    $CONVERTER --input-format tecplot --input-file "$input_file" \
               --output-format vtk --output-file "$output_file"
    
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

### File Not Found

```
Error: Cannot open input file
```

**Solution:** Check file path and permissions:
```bash
ls -l input_file.tec
file input_file.tec
```

### Format Detection Failed

```
Error: Unable to detect input format
```

**Solution:** Explicitly specify format:
```bash
ORION --input-format tecplot --input-file data.tec \
      --output-format vtk --output-file output.vtk
```

### Memory Error

```
Error: Insufficient memory
```

**Solution:**

- Check dataset size and available RAM
- Process data in chunks if possible
- Increase system swap space

### TecIO Not Available

```
Error: Binary Tecplot format not supported
```

**Solution:** Rebuild with TecIO:
```bash
./install.sh build --compiler=gnu --use-tecio
```

### Permission Denied

```
Error: Cannot write output file
```

**Solution:** Check output directory permissions:
```bash
# Check write permission
ls -ld output_directory/

# Fix permissions if needed
chmod +w output_directory/
```