import bpy
from random import randrange
import random
import sys, os

bpy.ops.object.select_all(action='SELECT')
bpy.ops.object.delete(use_global=False, confirm=False)


def append_zero(num):
    return "." + str(num).zfill(3)


all_box_loc = 'E:\Blender Tutorial\objects\primitives\BoxModels\\'
rack_loc = 'E:\Blender Tutorial\objects\primitives\Racks\modal.dae'

offset_x = -0.5
z_positions = [0.3, 1.63, 2.98, 4.35]
y_positions = [-2.3, -1.2, 0, 1.5]

subscript = 0
#x_coord = x_start

def make_racks(rack_location, subscript):
    imported_object = bpy.ops.wm.collada_import(filepath=rack_loc)

    model = "Rack"
    change = append_zero(subscript)

    name = model
    # if subscript > 0:
    #     name = model + change
    # else:
    #     name = model

    bpy.data.objects[name].location.x += rack_location[0]
    bpy.data.objects[name].location.y += rack_location[1]
    bpy.data.objects[name].location.z += rack_location[2]
    boxes = ["BoxA", "BoxB", "BoxD", "BoxF", "BoxH"]

    for rows in z_positions:
        for cols in y_positions:
            model = random.choice(boxes)
            model_temp = model
#            model = model+(append_zero(box_count[model_temp]))

            change = append_zero(box_count[model_temp])
            if box_count[model_temp] > 0:
                model = model + change
            else:
                model = model

            box_count[model_temp] += 1
            final_model_location = all_box_loc + model_temp + "/model.dae"
            print(final_model_location)
            imported_object = bpy.ops.wm.collada_import(
                filepath=final_model_location)
            bpy.data.objects[model].location.x = offset_x + rack_location[0]
            bpy.data.objects[model].location.y = cols + rack_location[1]
            bpy.data.objects[model].location.z = rows + rack_location[2]

    os.mkdir('E:\Blender Tutorial/objects/primitives/CustomRacks/rack_' + str(subscript))
    
    bpy.ops.wm.collada_export(
        filepath='E:\Blender Tutorial/objects/primitives/CustomRacks/rack_' + str(subscript) + '/model.dae')


num_racks = 1

for i in range(num_racks):

    bpy.ops.object.select_all(action='SELECT')
    bpy.ops.object.delete(use_global=False, confirm=False)

    box_count = {"BoxA": 0, "BoxB": 0, "BoxC": 0, "BoxD": 0,
                 "BoxF": 0, "BoxG": 0, "BoxH": 0, "BoxI": 0}
    
    subscript = i
    make_racks([0, 0, 0], subscript)
    
# update scene, if needed
dg = bpy.context.evaluated_depsgraph_get()
dg.update()
# bpy.ops.wm.collada_export(filepath='./rendered_warehouse/'+sys.argv[4]+'.dae')
# bpy.ops.export_scene.fbx(filepath='./rendered_warehouse/' + sys.argv[4]+'.fbx', path_mode='RELATIVE')
