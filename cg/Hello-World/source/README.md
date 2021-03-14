# Problem: Modelling

Three dodecahedrons are:
1. Decagonal Prism
2. Undecagonal Pyramid
3. Hexagonal Dipyramid

Coordinates are calculated using python code in `dodecahedron.py` and saved in <object_type>.txt file. Each face is given one random color.

`main.c` file takes the input for object type and reads the coordinates from <object_type>.txt file.

# Problem: Animating

1. Six keys to move the camera along the three different axes. The camera always face the object.
    - W - Front
    - S - Back
    - A - Left
    - D - Right
    - Q - Up
    - E - Down

2. Six keys to move the object along the three different axes.
    - T - Front
    - G - Back
    - F - Left
    - H - Right
    - R - Up
    - Y - Down

3. Three different keys to move to three different pre-decided positions/orientations of the camera. After each ’teleportation’, camera should face the object.
    - V - Moves camera to (0, 0, 3)
    - B - Moves camera to (0, 0, -3)
    - N - Moves camera to (3, 0, 0)

4. Key to make the object spin about any one axis.
    - O - Spins object about (1, 0, 0) local axis

5. Key to make the the camera spin about the object.
    - C - Recenters the object to origin, camera rotates around object.

6. ESC - Exits window.

# Running the Code

The code uses `glad`, `glfw`, `glm` libraries.

On running `main.c` file, code prints:
```
Press:
1 for Decagonal Prism
2 for Undecagonal Pyramid
3 for Hexagonal Dipyramid
Any other key for Exit
```
You need to press key to select object type to be shown on window. After that window opens and you can press other keys mentioned above to move the camera and object.