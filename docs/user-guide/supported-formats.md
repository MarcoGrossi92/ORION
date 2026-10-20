# Supported File Formats

ORION supports multiple scientific data formats commonly used in computational fluid dynamics (CFD) and scientific visualization. This page provides detailed information about each supported format.

## Format Overview

| Format | Type | Extension(s) | Read | Write | Notes |
|--------|------|--------------|------|-------|-------|
| **Tecplot ASCII** | Text | `.dat`, `.tec` | ✅ | ✅ | Human-readable, slower I/O |
| **Tecplot Binary** | Binary | `.szplt`, `.plt` | ✅¹ (`.szplt`) | ✅¹ (`.szplt`, `.plt`) | Compact, fast I/O |
| **VTK** | Text/Binary | `.vtm` + `.vts` | ✅² | ✅ | Open standard, widely supported |
| **PLOT3D** | Text/Binary | any (`.p3d`, `.x`, `.xyz`, `.g`, ...) | ✅³ | ✅⁴ | NASA legacy format, grids only |

¹ Requires building with `--use-tecio`. `.szplt` files are read and written; `.plt` files are written only (the reader refuses them)

² Multi-block structured grids in the layout ORION writes them; block files written by VTK or ParaView, which list their
data arrays before the points, are refused (see [VTK Format](#vtk-format))

³ ASCII, Fortran unformatted and C binary; the layout is detected from the file (Fortran `p3d_read_multiblock`, Python `read_P3D`)

⁴ ASCII only, Fortran API and converter

## Tecplot Format

Tecplot is a proprietary data visualization format widely used in aerospace and CFD applications.

**Format Types:**
- **ASCII** - Human-readable text format
- **Binary** - Compact binary format (PLT, SZPLT)

**Recommendations:**
- Use ASCII for small files and debugging
- Use SZPLT for large datasets, and for any binary file that ORION must read back: a PLT file can be written but not read
- Use PLT to hand a file to Tecplot that SZPLT does not suit

### File Structure

**Header** (as ORION writes it):
```
VARIABLES = x y z u
ZONE  T = Block1, I=50, J=50, K=50, DATAPACKING=BLOCK, VARLOCATION=([1-3]=NODAL,[4]=CELLCENTERED), SOLUTIONTIME=0.0E+000, STRANDID=1
```

**Data:**
- Variables at the nodes or at the cells, all at one of them in a file (`orion%tec%node`). ORION writes `DATAPACKING=BLOCK`
  (all the values of a variable, then the next one, one value per line) and reads `BLOCK` and `POINT` packing (`F=POINT`,
  one point per line, the coordinates and then the variables; a `POINT` zone needs all its variables at the nodes)
- Structured (ordered) zones of one, two or three coordinates
- Multi-block support: one zone per block
- A solution time and a strand per file (`solutiontime`, `strandid`)

### Supported Features

| Feature | ASCII | Binary | Notes |
|---------|-------|--------|-------|
| **Structured grids** | ✅ | ✅ | IJK-ordered data |
| **Multi-block** | ✅ | ✅ | Multiple zones |
| **Variable data** | ✅ | ✅ | Scalar variables, at the nodes or at the cells |
| **Solution time** | ✅ | ✅ | `solutiontime`, with its sign; `strandid` of the last zone read from an ASCII file |
| **Ghost cells** | Written | Written | With `orion%tec%bc` and variables at the cells, the arrays of a block hold a layer of cells around it (`mesh` from index `-1`) and the writer writes it; not read |
| **Unstructured** | ❌ | ❌ | Not supported |

## VTK Format

VTK (Visualization Toolkit) is an open-source data format used by ParaView, VisIt, and other visualization tools.

**What ORION handles:** multi-block structured grids as VTK XML files: a `.vtm` file (`vtkMultiBlockDataSet`) that lists
one `.vts` file (`StructuredGrid`) for each block. `vtk_write_structured_multiblock` writes them and
`vtk_read_structured_multiblock` reads them (both are used by the converter). The block files are written in one of three
formats, `orion%vtk%format`: `ascii`, `binary` (base64, the default) or `raw` (appended data). The module `Lib_VTK` also
holds the lower-level routines of Lib_VTK_IO that these two functions are built on.

**Files that ORION reads:** `vtk_read_structured_multiblock` reads multi-block structured grids in the layout in which
`vtk_write_structured_multiblock` writes them: a `.vtm` file whose `Block` element lists one `DataSet` per line, and `.vts`
block files of one `StructuredGrid` piece whose `Points` array (named `Points`, `Float64`) comes before its `PointData` or
`CellData` arrays (`Float64`, one component each), in ASCII, base64 binary (a 4-byte length, not compressed) or raw appended
data. A block file that lists its data arrays before its `Points`, as VTK and ParaView write them, is refused with an error
before any block is read: files written by VTK or ParaView are not read (see the
[Fortran API](fortran-api.md#dimensions-of-what-a-reader-returns)).

### File Structure

**Multi-block file** (`.vtm`):
```xml
<?xml version="1.0"?>
<VTKFile type="vtkMultiBlockDataSet" version="1.0" byte_order="LittleEndian">
  <vtkMultiBlockDataSet>
    <Block index="0">
      <DataSet index="0" file="Block1.vts"/>
    </Block>
  </vtkMultiBlockDataSet>
</VTKFile>
```

**Block file** (`.vts`, here in the `ascii` format, a block of 5 x 5 x 5 nodes with one variable, `Temperature`, at the
cells; the values are left out):
```xml
<?xml version="1.0"?>
<VTKFile type="StructuredGrid" version="0.1" byte_order="LittleEndian">
  <StructuredGrid WholeExtent="+0 +4 +0 +4 +0 +4">
    <Piece Extent="+0 +4 +0 +4 +0 +4">
      <Points>
        <DataArray type="Float64" NumberOfComponents="3" Name="Points" format="ascii">
          ...
        </DataArray>
      </Points>
      <CellData>
        <DataArray type="Float64" Name="Temperature" NumberOfComponents="1" format="ascii">
          ...
        </DataArray>
      </CellData>
    </Piece>
  </StructuredGrid>
</VTKFile>
```

**Data:**
- Structured grids of one, two or three coordinates (a mesh of two coordinates is written as two planes of nodes with
  `z = 0`, one of one coordinate as lines with `y = z = 0`; the reader takes them back)
- Variables at the nodes (`PointData`) or at the cells (`CellData`), one component each
- A time (`TIME`), a step number (`CYCLE`) and named real scalars, as field data of every block file

### Supported Grid Types

| Grid Type | Support | VTK Dataset Type |
|-----------|---------|------------------|
| **Structured grid** | ✅ | `StructuredGrid` (`.vts`) |
| **Rectilinear grid** | Low-level routines of `Lib_VTK` only | `RectilinearGrid` |
| **Polydata, unstructured grid** | ❌ | not implemented |

## PLOT3D Format

PLOT3D is a NASA-developed format for structured CFD data, still widely used in aerospace applications.

!!! warning "PLOT3D Files"
    ORION can work only with grid files.

**Format Components:**
- **Grid file** (`.p3d`) - Mesh coordinates

### File Structure

**Grid file (multi-grid, 3D):**
```
Number of blocks
I, J, K dimensions of each block
For each block: X of all its nodes, then Y, then Z (I fastest), then IBLANK when present
```

A single-grid file has no number of blocks; a 2D file has I, J dimensions and X, Y coordinates.

### Layouts read

ORION reads grid files in every common layout, detected from the file itself, whatever its extension. The Fortran (`p3d_read_multiblock`) and Python (`read_P3D`) readers apply the same rules:

| Feature | Read |
|---------|------|
| ASCII, any number of values per line, repeat counts `r*c`, `D` exponents | ✅ |
| Fortran unformatted (4- or 8-byte record markers, records over 2 GiB) | ✅ |
| C binary (stream, no record markers) | ✅ |
| Little or big endian | ✅ |
| 4- or 8-byte integers, 4- or 8-byte reals | ✅ |
| Multi-grid or single grid | ✅ |
| 3D or 2D | ✅ |
| IBLANK | ✅ (read past) |
| Whole or planes (a Fortran record per K plane) | ✅ |
| Solution (Q) and function files | ❌ |

A file whose size fits no layout, or fits more than one, is refused with a message (see the Fortran API).

## Format Comparison

### Feature Matrix

What ORION does with each format:

| Feature | Tecplot | VTK | PLOT3D |
|---------|---------|-----|--------|
| **Multi-block** | ✅ | ✅ | ✅ |
| **Grid + Solution** | ✅ | ✅ | ❌ |
| **Binary I/O** | ✅ | ✅ | Read only |
| **Compression** | ✅ (`.szplt`) | ❌ (ORION writes none) | ❌ |
| **Metadata** (names, time, step, named scalars) | ✅ | ✅ | ❌ |
| **Solution time** | ✅ | ✅ | ❌ |
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
converter --in-file=data.dat --out-file=vtk/data.vtm
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