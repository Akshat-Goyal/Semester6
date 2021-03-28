import bpy
from random import randrange, uniform
from random import randint 
import sys, os

bpy.ops.object.select_all(action = 'SELECT')
bpy.ops.object.delete(use_global = False, confirm = False)

rack_count = 4
shelf_spec = (6.0, 1.0, 0.06)
shelf_height = 1.6

for i in range(0, rack_count):
    bpy.ops.mesh.primitive_cube_add(location = (0, 0, i * shelf_height), scale = shelf_spec)
    
bpy.ops.mesh.primitive_cube_add(location = ((shelf_spec[0] / 2), 0, (rack_count * shelf_height) / 2), scale = (shelf_spec[2], shelf_spec[1], rack_count * shelf_height))
bpy.ops.mesh.primitive_cube_add(location = (-(shelf_spec[0] / 2), 0, (rack_count * shelf_height) / 2), scale = (shelf_spec[2], shelf_spec[1], rack_count * shelf_height))


box_count = rack_count
box_spec = (2, 0.5, 1)

for i in range(0, box_count):
    x = uniform(-(shelf_spec[0] / 2) + (box_spec[0] / 2), (shelf_spec[0] / 2) - (box_spec[0] / 2))
    y = uniform(-(shelf_spec[1] / 2) + (box_spec[1] / 2), (shelf_spec[1] / 2) - (box_spec[1] / 2))
    z = i * shelf_height + shelf_spec[2] + (box_spec[2] / 2)
    bpy.ops.mesh.primitive_cube_add(location = (x, y, z), scale = box_spec)
