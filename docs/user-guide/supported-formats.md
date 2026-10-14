# Supported File Formats

ORION supports multiple scientific data formats commonly used in computational fluid dynamics (CFD) and scientific visualization. This page provides detailed information about each supported format.

## Format Overview

| Format | Type | Extension(s) | Read | Write | Notes |
|--------|------|--------------|------|-------|-------|
| **Tecplot ASCII** | Text | `.dat`, `.tec` | ✅ | ✅ | Human-readable, slower I/O |
| **Tecplot Binary** | Binary | `.plt`, `.szplt` | ✅¹ | ✅¹ | Compact, fast I/O |
| **VTK** | Text/Binary | `.vtm`, `.vts`  | ✅² | ✅ | Open standard, widely supported |
| **PLOT3D** | Text | `.p3d` | ✅ | ✅ | NASA legacy format |

¹ Requires building with `--use-tecio` flag

² Multi-block structured grids in the layout ORION writes them; block files written by VTK or ParaView, which list their
data arrays before the points, are refused (see [VTK Format](#vtk-format))

## Tecplot Format

Tecplot is a proprietary data visualization format widely used in aerospace and CFD applications.

**Format Types:**
- **ASCII** - Human-readable text format
- **Binary** - Compact binary format (PLT, SZPLT)

**Recommendations:**
- Use ASCII for small files and debugging
- Use PLT for general production work
- Use SZPLT for large datasets requiring compression

### File Structure

**Header:**
```
VARIABLES = "X", "Y", "Z", "Pressure", "Temperature"
ZONE T="Block 1", I=50, J=50, K=50, F=POINT
```

**Data:**
- Point or cell-centered
- Structured grids
- Multi-block support
- Time-dependent data

### Supported Features

| Feature | ASCII | Binary | Notes |
|---------|-------|--------|-------|
| **Structured grids** | ✅ | ✅ | IJK-ordered data |
| **Multi-block** | ✅ | ✅ | Multiple zones |
| **Variable data** | ✅ | ✅ | Scalar and vector fields |
| **Time series** | ✅ | ✅ | Multiple solution times |
| **Boundary conditions** | ❌ | ❌ | Not yet supported |
| **Unstructured** | ❌ | ❌ | Not supported |

## VTK Format

VTK (Visualization Toolkit) is an open-source data format used by ParaView, VisIt, and other visualization tools.

**Format Types:**
- **Legacy formats** - Text/Binary format (`.vtk`)
- **XML formats** - Modern VTK formats (`.vtm`, `.vts`, `.vtu`)

**Files that ORION reads:** `vtk_read_structured_multiblock` reads multi-block structured grids in the layout in which
`vtk_write_structured_multiblock` writes them: a `.vtm` file whose `Block` element lists one `DataSet` per line, and `.vts`
block files of one `StructuredGrid` piece whose `Points` array (named `Points`, `Float64`) comes before its `PointData` or
`CellData` arrays (`Float64`, one component each), in ASCII, base64 binary (a 4-byte length, not compressed) or raw appended
data. A block file that lists its data arrays before its `Points`, as VTK and ParaView write them, is refused with an error
before any block is read: files written by VTK or ParaView are not read (see the
[Fortran API](fortran-api.md#dimensions-of-what-a-reader-returns)).

### File Structure

**Header:**
```
# vtk DataFile Version 3.0
ORION generated VTK file
ASCII
DATASET STRUCTURED_GRID
DIMENSIONS 50 50 50
POINTS 125000 float
```

**Data:**
- Structured and unstructured grids
- Point and cell data
- Multiple scalar/vector fields

### Supported Grid Types

| Grid Type | Support | VTK Dataset Type |
|-----------|---------|------------------|
| **Structured grid** | ✅ | `STRUCTURED_GRID` |
| **Rectilinear grid** | ✅ | `RECTILINEAR_GRID` |
| **Polydata** | ❌ | `POLYDATA` (not implemented) |

## PLOT3D Format

PLOT3D is a NASA-developed format for structured CFD data, still widely used in aerospace applications.

!!! warning "PLOT3D Files"
    ORION can work only with grid files.

**Format Components:**
- **Grid file** (`.p3d`) - Mesh coordinates

### File Structure

**Grid File (.p3d):**
```
Number of blocks
Block 1: I, J, K dimensions
Block 2: I, J, K dimensions
...
X coordinates for all blocks
Y coordinates for all blocks
Z coordinates for all blocks
```

## Format Comparison

### Feature Matrix

| Feature | Tecplot | VTK | PLOT3D |
|---------|---------|-----|--------|
| **Multi-block** | ✅ | ✅ | ✅ |
| **Grid + Solution** | ✅ | ✅ | ❌ |
| **Binary I/O** | ✅ | ✅ | ❌ |
| **Compression** | ✅ | ✅ | ❌ |
| **Metadata** | ✅ | ✅ | ❌ |
| **Time series** | ✅ | ✅ | ❌ |
| **Open format** | ❌ | ✅ | ✅ |

### Use Case Recommendations

**Use Tecplot when:**

- Working with commercial CFD software
- Need advanced visualization features
- Require time-dependent data

**Use VTK when:**

- Need open-source compatibility
- Using ParaView or VisIt
- Sharing data openly

**Use PLOT3D when:**

- Working with legacy NASA codes
- Need compact structured grid format

## Conversion Between Formats

**Example Tecplot → VTK:**
```bash
ORION --input-format tecplot --input-file data.dat \
      --output-format vtk --output-file output.vtk
```

See the [converter guide](converter.md) for more details

| Conversion | Mesh | Variables | Metadata |
|------------|------|-----------|----------|
| **Tec → VTK** | ✅ | ✅ | ⚠️ |
| **VTK → Tec** | ✅ | ✅ | ⚠️ |
| **P3D → Tec** | ✅ | ❌ | ❌ |
| **Tec → P3D** | ✅ | ❌ | ❌ |
| **VTK → P3D** | ✅ | ❌ | ❌ |
| **P3D → VTK** | ✅ | ❌ | ❌ |

**Note:** Some metadata may be lost during conversion between formats with different capabilities.

## Future Format Support

**Need a format not listed here?** Open a feature request on [GitHub](https://github.com/MarcoGrossi92/ORION/issues/new) to let us know!

Planned additions:

- **HDF5** - Hierarchical data format
- **CGNS** - CFD General Notation System

See the [GitHub issues](https://github.com/MarcoGrossi92/ORION/issues) for feature requests and status.

## Format Documentation

**Tecplot:**
- [Tecplot Data Format Guide](https://www.tecplot.com/documentation/)
- TecIO SDK Documentation

**VTK:**
- [VTK File Formats](https://vtk.org/wp-content/uploads/2015/04/file-formats.pdf)
- [VTK User's Guide](https://www.vtk.org/documentation/)

**PLOT3D:**
- [NASA PLOT3D Manual](https://www.grc.nasa.gov/www/wind/valid/plot3d.html)
- [PLOT3D User's Manual (PDF)](https://ntrs.nasa.gov/citations/19900013774)

---