# Acknowledgements

ORION is built upon several open-source projects and commercial products.

## Lib_VTK_IO

**VTK I/O Library for Fortran**

- **Repository:** [github.com/victorsndvg/Lib_VTK_IO](https://github.com/victorsndvg/Lib_VTK_IO)
- **License:** BSD-3-Clause
- **Contribution:** Provided the foundation for VTK format reading and writing capabilities

Lib_VTK_IO delivered robust, well-tested routines for handling VTK structured and unstructured data formats. ORION extends and adapts these capabilities to fit into a unified multi-format framework.

**Key contributors to Lib_VTK_IO:**
- Víctor Sande
- And the broader Lib_VTK_IO community

## OFF (Open source Finite volume Fluid dynamics code)

**Tecplot Format Handling**

- **Repository:** [github.com/szaghi/OFF](https://github.com/szaghi/OFF)
- **License:** GPL v3.0
- **Contribution:** Provided basic Tecplot format reading and writing routines

OFF's Tecplot handling routines formed the basis for ORION's Tecplot ASCII support. The clean, modular design of OFF made it an excellent starting point for building ORION's format abstraction layer.

**Key contributors to OFF:**
- Stefano Zaghi
- And the OFF development team


## TecIO Library

**Tecplot Binary Format Support**

- **Provider:** Tecplot, Inc.
- **Purpose:** Reading and writing Tecplot binary formats (.plt, .szplt)
- **License:** Tecplot License Agreement

TecIO enables ORION to work with high-performance binary Tecplot formats, essential for large-scale CFD workflows.

## Documentation Tools

### MkDocs

**Static Site Generator**

- **Website:** [mkdocs.org](https://www.mkdocs.org)
- **License:** BSD-2-Clause
- **Contribution:** Documentation site generation

MkDocs transforms ORION's documentation into a beautiful, searchable website.

### Material for MkDocs

**Modern Documentation Theme**

- **Website:** [squidfunk.github.io/mkdocs-material](https://squidfunk.github.io/mkdocs-material/)
- **License:** MIT
- **Contribution:** Professional documentation theme with advanced features

Material for MkDocs provides the sleek, modern interface for ORION's documentation.

<!-- ## Institutional Support

While ORION is an independent open-source project, it has benefited from:

- Academic research environments
- Computational resources for testing
- Access to various CFD datasets for validation -->

## License Compliance

ORION respects all licenses of dependencies and foundations:

- **GPL v3.0** - ORION's license, compatible with OFF
- **BSD Licenses** - Lib_VTK_IO, NumPy, CMake
- **MIT License** - Material for MkDocs
- **Tecplot License** - TecIO library

See the [License](license.md) page for ORION's full license text.

---
