#!/bin/bash

cd $ORIONDIR/bin/test
rm -f XML* *.dat *.plt

ulimit -s unlimited

./vtk_fullpower -strgr
$ORIONDIR/bin/app/vts2tec --out-format=ascii

./vtk_write
./tecplot_write
