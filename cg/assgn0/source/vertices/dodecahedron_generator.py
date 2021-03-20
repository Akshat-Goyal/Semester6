import math
from random import uniform as uf

def print_vertex(vertex, filename):
    with open(filename, "w") as f:
        # print("float vertices[] = {", file=f)
        print(len(vertex), len(vertex[0]), file=f)
        for v in vertex:
            # print("\t", end="", file=f)
            for i in v:
                print(str('%.2f'%(i)) + " ", end="", file=f)
                # print(str('%.2f'%(i)) + "f, ", end="", file=f)
            print(file=f)
        # print("};", file=f)

def texture_decagonal_prism():
    sides = 10
    angle = 2 * math.pi / sides
    x = 0.5
    h = x * math.sin(angle / 2)

    v2d = []
    for i in range(sides):
        theta = i * angle
        v2d.append([x * math.cos(theta), x * math.sin(theta)])
    v2d.append(v2d[0])

    vertex = []
    for i in range(len(v2d) - 1):
        vertex.append([v2d[i][0], v2d[i][1], h, 0.0, 0.0])
        vertex.append([0.0, 0.0, h, 0.5, 1.0])
        vertex.append([v2d[i + 1][0], v2d[i + 1][1], h, 1, 0])

    for i in range(len(v2d) - 1):
        vertex.append([v2d[i][0], v2d[i][1], -h, 0.0, 0.0])
        vertex.append([0.0, 0.0, -h, 0.5, 1.0])
        vertex.append([v2d[i + 1][0], v2d[i + 1][1], -h, 1.0, 0.0])

    for i in range(len(v2d) - 1):
        vertex.append([v2d[i][0], v2d[i][1], h, 0.0, 0.0])
        vertex.append([v2d[i + 1][0], v2d[i + 1][1], h, 0.5, 1.0])
        vertex.append([v2d[i][0], v2d[i][1], -h, 1.0, 0.0])

        vertex.append([v2d[i][0], v2d[i][1], -h, 0.0, 0.0])
        vertex.append([v2d[i + 1][0], v2d[i + 1][1], -h, 0.5, 1.0])
        vertex.append([v2d[i + 1][0], v2d[i + 1][1], h, 1.0, 0.0])

    print_vertex(vertex, "texture_decagonal_prism.txt")

def decagonal_prism():
    sides = 10
    angle = 2 * math.pi / sides
    x = 0.5
    h = x * math.sin(angle / 2)

    v2d = []
    for i in range(sides):
        theta = i * angle
        v2d.append([x * math.cos(theta), x * math.sin(theta)])
    v2d.append(v2d[0])

    vertex = []
    col = [uf(0, 1), uf(0, 1), uf(0, 1)]
    for i in range(len(v2d) - 1):
        vertex.append([v2d[i][0], v2d[i][1], h, col[0], col[1], col[2]])
        vertex.append([0.0, 0.0, h, col[0], col[1], col[2]])
        vertex.append([v2d[i + 1][0], v2d[i + 1][1], h, col[0], col[1], col[2]])

    for i in range(len(v2d) - 1):
        vertex.append([v2d[i][0], v2d[i][1], -h, col[0], col[1], col[2]])
        vertex.append([0.0, 0.0, -h, col[0], col[1], col[2]])
        vertex.append([v2d[i + 1][0], v2d[i + 1][1], -h, col[0], col[1], col[2]])

    for i in range(len(v2d) - 1):
        scol = [uf(0, 1), uf(0, 1), uf(0, 1)]

        vertex.append([v2d[i][0], v2d[i][1], h, scol[0], scol[1], scol[2]])
        vertex.append([v2d[i + 1][0], v2d[i + 1][1], h, scol[0], scol[1], scol[2]])
        vertex.append([v2d[i][0], v2d[i][1], -h, scol[0], scol[1], scol[2]])

        vertex.append([v2d[i][0], v2d[i][1], -h, scol[0], scol[1], scol[2]])
        vertex.append([v2d[i + 1][0], v2d[i + 1][1], -h, scol[0], scol[1], scol[2]])
        vertex.append([v2d[i + 1][0], v2d[i + 1][1], h, scol[0], scol[1], scol[2]])

    print_vertex(vertex, "decagonal_prism.txt")

def undecagonal_pyramid():
    sides = 11
    angle = 2 * math.pi / sides
    x = 0.5
    h = 0.5

    v2d = []
    for i in range(sides):
        theta = i * angle
        v2d.append([x * math.cos(theta), x * math.sin(theta)])
    v2d.append(v2d[0])

    vertex = []
    col = [uf(0, 1), uf(0, 1), uf(0, 1)]
    for i in range(len(v2d) - 1):
        vertex.append([v2d[i][0], v2d[i][1], -h, col[0], col[1], col[2]])
        vertex.append([0.0, 0.0, -h, col[0], col[1], col[2]])
        vertex.append([v2d[i + 1][0], v2d[i + 1][1], -h, col[0], col[1], col[2]])

    for i in range(len(v2d) - 1):
        tcol = [uf(0, 1), uf(0, 1), uf(0, 1)]
        vertex.append([v2d[i][0], v2d[i][1], -h, tcol[0], tcol[1], tcol[2]])
        vertex.append([0.0, 0.0, h, tcol[0], tcol[1], tcol[2]])
        vertex.append([v2d[i + 1][0], v2d[i + 1][1], -h, tcol[0], tcol[1], tcol[2]])

    print_vertex(vertex, "undecagonal_pyramid.txt")

def hexagonal_dipyramid():
    sides = 6
    angle = 2 * math.pi / sides
    x = 0.5
    h = 0.5

    v2d = []
    for i in range(sides):
        theta = i * angle
        v2d.append([x * math.cos(theta), x * math.sin(theta)])
    v2d.append(v2d[0])

    vertex = []
    for i in range(len(v2d) - 1):
        tcol = [uf(0, 1), uf(0, 1), uf(0, 1)]
        vertex.append([v2d[i][0], v2d[i][1], 0.0, tcol[0], tcol[1], tcol[2]])
        vertex.append([0.0, 0.0, h, tcol[0], tcol[1], tcol[2]])
        vertex.append([v2d[i + 1][0], v2d[i + 1][1], 0.0, tcol[0], tcol[1], tcol[2]])

    for i in range(len(v2d) - 1):
        tcol = [uf(0, 1), uf(0, 1), uf(0, 1)]
        vertex.append([v2d[i][0], v2d[i][1], 0.0, tcol[0], tcol[1], tcol[2]])
        vertex.append([0.0, 0.0, -h, tcol[0], tcol[1], tcol[2]])
        vertex.append([v2d[i + 1][0], v2d[i + 1][1], 0.0, tcol[0], tcol[1], tcol[2]])

    print_vertex(vertex, "hexagonal_dipyramid.txt")


if __name__ == "__main__":
    texture_decagonal_prism()
    decagonal_prism()
    undecagonal_pyramid()
    hexagonal_dipyramid()