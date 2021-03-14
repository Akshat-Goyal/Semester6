import math
from random import uniform as uf

def print_vertex(vertex):
    # print("float vertices[] = {")
    print(len(vertex), len(vertex[0]))
    for v in vertex:
        # print("\t", end="")
        for i in v:
            print(str('%.2f'%(i)) + " ", end="")
            # print(str('%.2f'%(i)) + "f, ", end="")
        print()
    # print("};")

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

    print_vertex(vertex)

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

    print_vertex(vertex)

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

    print_vertex(vertex)


if __name__ == "__main__":
    # decagonal_prism()
    # undecagonal_pyramid()
    hexagonal_dipyramid()