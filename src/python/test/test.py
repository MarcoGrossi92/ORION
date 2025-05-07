from ORION import read_TEC, write_TEC

[x,y,z,var] = read_TEC('solfile.dat')

var[0][0] *= 10

write_TEC('out.tec',x,y,z,var)