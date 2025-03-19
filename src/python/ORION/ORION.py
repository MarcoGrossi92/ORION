import re
import numpy as np

def count_lines_before_float(file_path):
    line_count = 0

    with open(file_path, 'r') as file:
        for line in file:
            line_count += 1

            # Check if the line contains only a floating-point number
            try:
                float_value = float(line.strip())
                # If we successfully convert the line to a float, we've found a line with a sole floating number
                break
            except ValueError:
                # The line does not contain a floating-point number, continue to the next line

                # Additionally, if the line starts with a keyword (e.g., "VARIABLES", "Zone T"),
                # we consider it a non-data line and continue searching for the floating-point number
                if line.strip().startswith(("VARIABLES", "Zone T", "SOLUTIONTIME", "DATAPACKING", "VARLOCATION")):
                    continue

    return line_count


def read_variables(file_path):
    with open(file_path, 'r') as file:
        head_text = file.read()

    # Extracting variables
    variables_match = re.search(r'VARIABLES\s*=\s*(.*?)[\s,]*$', head_text, re.DOTALL)
    if variables_match:
        variables_str = variables_match.group(1)
        variables = [var for var in re.findall(r'"(.*?)"|[^\s,]+', variables_str) if var]
        num_variables = len(variables)
    else:
        num_variables = 0
        variables = []

    return {
        'number': num_variables,
        'name': variables,
    }


def read_dimensions(file_path):
    
    dim = []

    with open(file_path, 'r') as file:
        lines = file.readlines()

    for line in lines:
        if 'I=' in line:
            pattern = r'\b\d+\b'
            numbers = re.findall(pattern, line)
            numbers = [int(num) for num in numbers]
            dim.append(numbers[0:3])

    return dim


def read_geometry(file_path,Nx,Ny,Nz,jumpline):
    MESH = open(file_path,"r")
    MESH = MESH.readlines()

    Ni = Nx+1; Nj = Ny+1; Nk = Nz+1

    line = jumpline-2
    xn = np.zeros((Ni, Nj, Nk))
    yn = np.zeros((Ni, Nj, Nk))
    zn = np.zeros((Ni, Nj, Nk))

    for k in range(Nk):
        for j in range(Nj):
            for i in range(Ni):
                line += 1
                #print(line)
                #print(MESH[line])
                xn[i,j,k] = float(MESH[line])

    for k in range(Nk):
        for j in range(Nj):
            for i in range(Ni):
                line += 1
                yn[i,j,k] = float(MESH[line])

    for k in range(Nk):
        for j in range(Nj):
            for i in range(Ni):
                line += 1
                zn[i,j,k] = float(MESH[line])

    xc = np.zeros((Nx, Ny, Nz))
    yc = np.zeros((Nx, Ny, Nz))
    zc = np.zeros((Nx, Ny, Nz))
    for k in range(Nz):
        for j in range(Ny):
            for i in range(Nx):
                xc[i,j,k] = 0.125*(xn[i,j,k]+xn[i+1,j,k]+xn[i,j+1,k]+xn[i,j,k+1]+xn[i+1,j+1,k]+xn[i+1,j,k+1]+xn[i,j+1,k+1]+xn[i+1,j+1,k+1])
                yc[i,j,k] = 0.125*(yn[i,j,k]+yn[i+1,j,k]+yn[i,j+1,k]+yn[i,j,k+1]+yn[i+1,j+1,k]+yn[i+1,j,k+1]+yn[i,j+1,k+1]+yn[i+1,j+1,k+1])
                zc[i,j,k] = 0.125*(zn[i,j,k]+zn[i+1,j,k]+zn[i,j+1,k]+zn[i,j,k+1]+zn[i+1,j+1,k]+zn[i+1,j,k+1]+zn[i,j+1,k+1]+zn[i+1,j+1,k+1])

    return xn, yn, zn, xc, yc, zc


def read_field(file_path,N,Nx,Ny,Nz,jumpline):
    MESH = open(file_path,"r")
    MESH = MESH.readlines()

    Ni = Nx; Nj = Ny; Nk = Nz
    var = []
    line = jumpline-2
        
    for _ in range(N):
        xaux = []
        for _ in range(Ni*Nj*Nk):
            line += 1
            x = float(MESH[line])
            xaux.append(x)
        # Reshape the list into a 3D array
        xx = np.array(xaux).reshape(Ni, Nj, Nk)
        var.append(xx)

    return var


def read_TEC(file_path):

    variables = read_variables(file_path)
    lines_before_float = count_lines_before_float(file_path)
    dimensions = read_dimensions(file_path)

    # Displaying the result
    print("Number of Variables:", variables['number'])
    print("Variables:", variables['name'])
    print("Block Dimensions:", dimensions)
    print()

    xb = []; yb = []; zb = []; vb = []

    # Read mesh
    jump = 0
    Nb = len(dimensions)
    Nvar = variables['number']-3
    x = []; y = []; z = []
    for b in range(Nb):
        Nx = dimensions[b][0]-1
        Ny = dimensions[b][1]-1
        Nz = dimensions[b][2]-1
        if b==0: jump += lines_before_float
        [xn,yn,zn,x,y,z] = read_geometry(file_path,Nx,Ny,Nz,jump)
        xb.append(x); yb.append(y); zb.append(z)
        jump += 3*(Nx+1)*(Ny+1)*(Nz+1)
        var = read_field(file_path,Nvar,Nx,Ny,Nz,jump)
        vb.append(var)
        jump += Nvar*Nx*Ny*Nz+1

    return xb, yb, zb, vb


def read_nodes_TEC(file_path):

    variables = read_variables(file_path)
    lines_before_float = count_lines_before_float(file_path)
    dimensions = read_dimensions(file_path)

    # Displaying the result
    print("Number of Variables:", variables['number'])
    print("Variables:", variables['name'])
    print("Block Dimensions:", dimensions)
    print()

    xb = []; yb = []; zb = []

    # Read mesh
    jump = 0
    Nb = len(dimensions)
    Nvar = variables['number']-3
    x = []; y = []; z = []
    for b in range(Nb):
        Nx = dimensions[b][0]-1
        Ny = dimensions[b][1]-1
        Nz = dimensions[b][2]-1
        if b==0: jump += lines_before_float
        [xn,yn,zn,x,y,z] = read_geometry(file_path,Nx,Ny,Nz,jump)
        xb.append(xn); yb.append(yn); zb.append(zn)
        jump += 3*(Nx+1)*(Ny+1)*(Nz+1)

    return xb, yb, zb
