# User Guide

Welcome to the ORION user guide! This section provides comprehensive documentation for using ORION's command-line converter and programming APIs.

## Quick Navigation

<div class="grid cards" markdown>

-   :material-file-swap:{ .lg .middle } __Supported Formats__

    ---

    Learn about file formats ORION supports

    [:octicons-arrow-right-24: Format details](supported-formats.md)

-   :material-console:{ .lg .middle } __Command-line Converter__

    ---

    Convert files without writing code

    [:octicons-arrow-right-24: Converter guide](converter.md)

-   :material-code-braces:{ .lg .middle } __Fortran API__

    ---

    High-performance library for Fortran

    [:octicons-arrow-right-24: Fortran documentation](fortran-api.md)

-   :material-language-python:{ .lg .middle } __Python API__

    ---

    Convenient scripting interface

    [:octicons-arrow-right-24: Python documentation](python-api.md)

</div>

## Overview

ORION provides three ways to work with scientific data:

### 1. Command-line Converter

The fastest way to convert between formats without writing any code:

```bash
ORION --input-format tecplot --input-file data.dat \
      --output-format vtk --output-file output.vtk
```

**Best for:**

- Quick one-off conversions
- Batch processing with shell scripts
- Integration into existing workflows

**Learn more:** [Converter Guide](converter.md)

### 2. Fortran API

Native Fortran library for IO applications in external codes:

```fortran
use Lib_ORION_data
use Lib_Tecplot

type(ORION_data) :: IOfield
integer :: E_IO

E_IO = tec_read_structured_multiblock(filename='data.dat', orion=IOfield)
```

**Best for:**

- Integration with Fortran solvers
- Direct memory management
- All format support (Tecplot, VTK, PLOT3D)

**Learn more:** [Fortran API Guide](fortran-api.md)

### 3. Python API

Convenient Python interface for scripting and analysis:

```python
from ORION import read_TEC

x, y, z, var, varnames = read_TEC('data.dat')
```

!!! note "Limited IO"
    Python API may be used only for **reading**.

**Best for:**

- Post-processing and analysis
- Data visualization

**Learn more:** [Python API Guide](python-api.md)

## Supported File Formats

ORION supports the following scientific data formats:

| Format | Description | Fortran | Python | Command-line |
|--------|-------------|---------|--------|--------------|
| **Tecplot ASCII** | Text-based format | ✅ Read/Write | ✅ Read/Write | ✅ |
| **Tecplot Binary** | Binary format | ✅ Read/Write¹ | ❌ | ✅¹ |
| **VTK** | Visualization Toolkit format | ✅ Read/Write | ❌ | ✅ |
| **PLOT3D** | NASA structured grid format | ✅ Read/Write | ❌ | ✅ |

¹ Requires building with `--use-tecio` flag

**Learn more:** [Supported Formats](supported-formats.md)

## Advanced Topics

### Multi-Block Data

All ORION interfaces support multi-block structured grids:

- **Fortran:** Iterate over blocks with array indexing
- **Python:** Index into block dimension of NumPy arrays
- **Converter:** Automatically handles multi-block files

## Troubleshooting

### Common Issues

**"Format not supported"**

- Check that format is spelled correctly
- Verify file extension matches format
- For binary Tecplot, ensure TecIO is enabled

**"Out of memory"**

- Use the converter for very large files
- In Fortran, deallocate arrays after use
- In Python, process data in chunks

**More help:** Each guide has a dedicated troubleshooting section.

## Best Practices

### For Efficient Workflows

1. **Use the right tool:** Converter for conversions, APIs for processing
2. **Cache conversions:** Don't reconvert the same file repeatedly
3. **Choose formats wisely:** Binary formats are faster for large data
4. **Test with small data:** Verify logic before running on large files

### For Code Quality

1. **Check error codes:** Always verify I/O operations succeeded
2. **Document your code:** Explain why, not just what
3. **Add tests:** Ensure your code works as expected
4. **Use version control:** Track changes to your scripts

### For Performance

1. **Minimize I/O:** Read once, process multiple times
2. **Use binary formats:** Faster than ASCII for large datasets
3. **Vectorize operations:** Use array operations instead of loops (Python)

## Additional Resources

### Examples
- **Examples:** Complete working examples in `src/lib/test`

### External Resources
- [Tecplot File Format Guide](https://www.tecplot.com)
- [VTK File Formats](https://vtk.org/wp-content/uploads/2015/04/file-formats.pdf)
- [PLOT3D Format Documentation](https://www.grc.nasa.gov/www/wind/valid/plot3d.html)

## What's Next?

Ready to dive deeper? Here are your next steps:

1. **[Learn the converter](converter.md)** - Start with command-line conversion
2. **[Explore Fortran API](fortran-api.md)** - High-performance programming
3. **[Try Python API](python-api.md)** - Convenient scripting
