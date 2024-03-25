#!/bin/bash

cd $ORIONDIR/bin/test
rm -f XML* *.dat *.plt

ulimit -s unlimited

echo
echo '--- VTK2TEC ---'
./vtk_fullpower -strgr
$ORIONDIR/bin/app/vts2tec --out-format=ascii

echo
echo '--- VTK reading ---'
./vtk_write

echo
echo '--- Tecplot writing ---'
./tecplot_write

echo
echo '--- Tecplot reading (ascii) ---'
./tecplot_read
