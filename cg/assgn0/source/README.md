# Problem: Modelling

Three dodecahedrons are:
1. Decagonal Prism
2. Undecagonal Pyramid
3. Hexagonal Dipyramid

There are two variants of Decagonal Prism using Texture and Color. Rest 2 objects use color.


# BONUS PART

- Texture on Decagonal Prism is done.


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


# File Structure

- `source/vertices/dodecahedron_generator.py` - generates 4 files in `source/vertices` folder. 
    - `decagonal_prism.txt` - Decagonal Prism + Color coordinates.
    - `undecagonal_pyramid.txt` - Undecagonal Pyramid + Color coordinates.
    - `hexagonal_dipyramid.txt` - Hexagonal Dipyramid + Color coordinates.
    - `texture_decagonal_prism.txt` - Decagonal Prism + Texture coordinates.
    Each face of object is given one random color in first three files. Texture is used instead of color in 4th file.

- `source/main.h` - header file, contains structs, functions, shaders.

- `source/main.cpp` - contains main function. 

- `source/texture.cpp` - contains texture function.


# Running the Code

- Add `glad`, `glfw`, `glm` libraries in `Hello-World/libraries`.

- Run the following commands.
    ```
    mkdir build
    cd build
    cmake ..
    make
    ./assgn0
    ```

- On executing binary file, code prints:
    ```
    Press:
    1 for Decagonal Prism
    2 for Undecagonal Pyramid
    3 for Hexagonal Dipyramid
    4 for Bonus Part: Texture Decagonal Prism
    Any other key for Exit
    ```
    You need to press key to select object type to be shown on window. After that window opens and you can press other keys mentioned above to move the camera and object.