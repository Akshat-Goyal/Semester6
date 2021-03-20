#ifndef __MAIN_H__
#define __MAIN_H__


// settings
const unsigned int SCR_WIDTH = 800;
const unsigned int SCR_HEIGHT = 600;

const char *vertexShaderSource ="#version 330 core\n"
    "layout (location = 0) in vec3 aPos;\n"
    "layout (location = 1) in vec3 aColor;\n"
    "out vec3 ourColor;\n"
    "uniform mat4 M;\n"
    "uniform mat4 V;\n"
    "uniform mat4 P;\n"
    "void main()\n"
    "{\n"
    "   gl_Position = P * V * M * vec4(aPos, 1.0);\n"
    "   ourColor = aColor;\n"
    "}\0";

const char *fragmentShaderSource = "#version 330 core\n"
    "out vec4 FragColor;\n"
    "in vec3 ourColor;\n"
    "void main()\n"
    "{\n"
    "   FragColor = vec4(ourColor, 1.0f);\n"
    "}\n\0";

const char *texture_vertexShaderSource ="#version 330 core\n"
    "layout (location = 0) in vec3 aPos;\n"
    "layout (location = 1) in vec2 aTexCoord;\n"
    "out vec2 TexCoord;\n"
    "uniform mat4 M;\n"
    "uniform mat4 V;\n"
    "uniform mat4 P;\n"
    "void main()\n"
    "{\n"
    "   gl_Position = P * V * M * vec4(aPos, 1.0);\n"
    "   TexCoord = vec2(aTexCoord.x, aTexCoord.y);\n"
    "}\0";

const char *texture_fragmentShaderSource = "#version 330 core\n"
    "out vec4 FragColor;\n"
    "in vec2 TexCoord;\n"
    "uniform sampler2D texture1;\n"
    "void main()\n"
    "{\n"
    "   FragColor = texture(texture1, TexCoord);\n"
    "}\n\0";

struct Object{
    bool type = 0;
    int n = 0, m = 0;
    float* vertices = NULL;

    bool select_type(){
        std::cout << "Press:\n";
        std::cout << "1 for Decagonal Prism\n";
        std::cout << "2 for Undecagonal Pyramid\n";
        std::cout << "3 for Hexagonal Dipyramid\n";
        std::cout << "4 for Bonus Part: Texture Decagonal Prism\n";
        std::cout << "Any other key for Exit\n";
        int key = 3;
        std::cin>>key;
        char filename[128];
        switch(key){
            case 1: strcpy(filename, "../source/vertices/decagonal_prism.txt"); break;
            case 2: strcpy(filename, "../source/vertices/undecagonal_pyramid.txt"); break;
            case 3: strcpy(filename, "../source/vertices/hexagonal_dipyramid.txt"); break;
            case 4: strcpy(filename, "../source/vertices/texture_decagonal_prism.txt"); type = 1; break;
            default: exit(0);
        }
        get_vertices(filename);
        return type;
    }

    void get_vertices(char filename[]){
        try{
            std::fstream file;
            file.open(filename);
            file >> n >> m;
            int size = n * m;
            vertices = (float*) malloc(sizeof(float) * size);
            for(int i = 0; i < size; i++){
                file >> vertices[i];
            }
            file.close();
        }
        catch(...){
            std::cout << "Failed to open " << filename << " file\n";
        }
    }
};

struct Coordinates{
    float deltaTime, lastFrame;
    glm::mat4 model, view, object_view, camera_view, projection, rot;
    glm::vec3 cameraPos, cameraFront, cameraTarget, cameraUp, cameraRight;

    Coordinates(unsigned int shaderProgram){
        model = glm::mat4(1.0f);
        model = glm::rotate(model, glm::radians(-90.0f), glm::vec3(1.0f, 0.0f, 0.0f)); 

        object_view = glm::mat4(1.0f);
        // // note that we're translating the scene in the reverse direction of where we want to move
        cameraPos = glm::vec3(0.0f, 0.0f,  3.0f);
        cameraFront = glm::vec3(0.0f, 0.0f, -1.0f);
        cameraTarget = glm::vec3(0.0f, 0.0f, 0.0f);
        cameraUp = glm::vec3(0.0f, 1.0f,  0.0f);
        cameraRight = glm::normalize(glm::cross(cameraFront, cameraUp));
        camera_view = glm::lookAt(cameraPos, cameraTarget, cameraUp);
        view = object_view * camera_view;

        projection = glm::perspective(glm::radians(45.0f), 800.0f / 600.0f, 0.1f, 100.0f);

        // assign value to M, V, P
        int modelLoc = glGetUniformLocation(shaderProgram, "M");
        glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(model));
        int viewLoc = glGetUniformLocation(shaderProgram, "V");
        glUniformMatrix4fv(viewLoc, 1, GL_FALSE, glm::value_ptr(view));
        int projectionLoc = glGetUniformLocation(shaderProgram, "P");
        glUniformMatrix4fv(projectionLoc, 1, GL_FALSE, glm::value_ptr(projection));

        rot = glm::mat4(1.0f);
        rot = glm::rotate(rot, glm::radians(-5.0f), glm::vec3(0.0f, 1.0f, 0.0f)); 
    }

    // process all input: query GLFW whether relevant keys are pressed/released this frame and react accordingly
    // ---------------------------------------------------------------------------------------------------------
    void processInput(GLFWwindow *window, unsigned int shaderProgram){
        float currentFrame = glfwGetTime();
        deltaTime = currentFrame - lastFrame;
        lastFrame = currentFrame;

        // float cameraSpeed = 0.05f; // adjust accordingly
        float cameraSpeed = 2.5f * deltaTime;
        if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS)
            glfwSetWindowShouldClose(window, true);
        // six different keys to move the camera along the three different axes. The camera should not change
        // orientation so as to always face the object when this happens.
        if (glfwGetKey(window, GLFW_KEY_W) == GLFW_PRESS){
            cameraPos += cameraFront * cameraSpeed;
            camera_view = glm::lookAt(cameraPos, cameraTarget, cameraUp);
        }
        if (glfwGetKey(window, GLFW_KEY_S) == GLFW_PRESS){
            cameraPos -= cameraFront * cameraSpeed;
            camera_view = glm::lookAt(cameraPos, cameraTarget, cameraUp);
        }
        if (glfwGetKey(window, GLFW_KEY_A) == GLFW_PRESS){
            cameraPos -= cameraRight * cameraSpeed;
            camera_view = glm::lookAt(cameraPos, cameraTarget, cameraUp);
        }
        if (glfwGetKey(window, GLFW_KEY_D) == GLFW_PRESS){
            cameraPos += cameraRight * cameraSpeed;
            camera_view = glm::lookAt(cameraPos, cameraTarget, cameraUp);
        }
        if (glfwGetKey(window, GLFW_KEY_Q) == GLFW_PRESS){
            cameraPos += cameraUp * cameraSpeed;
            camera_view = glm::lookAt(cameraPos, cameraTarget, cameraUp);
        }
        if (glfwGetKey(window, GLFW_KEY_E) == GLFW_PRESS){
            cameraPos -= cameraUp * cameraSpeed;
            camera_view = glm::lookAt(cameraPos, cameraTarget, cameraUp);
        }
        // Assign six different keys to move the object along the three different axes.
        if (glfwGetKey(window, GLFW_KEY_T) == GLFW_PRESS){
            object_view = glm::translate(object_view, glm::vec3(0.0f, 0.0f, -1.0f) * cameraSpeed);
        }
        if (glfwGetKey(window, GLFW_KEY_G) == GLFW_PRESS){
            object_view = glm::translate(object_view, -glm::vec3(0.0f, 0.0f, -1.0f) * cameraSpeed);
        }
        if (glfwGetKey(window, GLFW_KEY_F) == GLFW_PRESS){
            object_view = glm::translate(object_view, -glm::vec3(1.0f, 0.0f, 0.0f) * cameraSpeed);
        }
        if (glfwGetKey(window, GLFW_KEY_H) == GLFW_PRESS){
            object_view = glm::translate(object_view, glm::vec3(1.0f, 0.0f, 0.0f) * cameraSpeed);
        }
        if (glfwGetKey(window, GLFW_KEY_R) == GLFW_PRESS){
            object_view = glm::translate(object_view, glm::vec3(0.0f, 1.0f, 0.0f) * cameraSpeed);
        }
        if (glfwGetKey(window, GLFW_KEY_Y) == GLFW_PRESS){
            object_view = glm::translate(object_view, -glm::vec3(0.0f, 1.0f, 0.0f) * cameraSpeed);
        }
        // Assign three different keys to move to three different pre-decided positions/orientations of the camera.
        // Note that over here, after each ’teleportation’ we wish to face the object.
        if (glfwGetKey(window, GLFW_KEY_V) == GLFW_PRESS){
            cameraPos = glm::vec3(0.0f, 0.0f, 3.0f);
            cameraTarget = glm::vec3(object_view * model * glm::vec4(0.0f, 0.0f, 0.0f, 1.0f));
            camera_view = glm::lookAt(cameraPos, cameraTarget, cameraUp);
        }
        if (glfwGetKey(window, GLFW_KEY_B) == GLFW_PRESS){
            cameraPos = glm::vec3(0.0f, 0.0f, -3.0f);
            cameraTarget = glm::vec3(object_view * model * glm::vec4(0.0f, 0.0f, 0.0f, 1.0f));
            camera_view = glm::lookAt(cameraPos, cameraTarget, cameraUp);
        }
        if (glfwGetKey(window, GLFW_KEY_N) == GLFW_PRESS){
            cameraPos = glm::vec3(3.0f, 0.0f, 0.0f);
            cameraTarget = glm::vec3(object_view * model * glm::vec4(0.0f, 0.0f, 0.0f, 1.0f));
            camera_view = glm::lookAt(cameraPos, cameraTarget, cameraUp);
        }
        // Assign a key to make the object spin about any one axis.
        if (glfwGetKey(window, GLFW_KEY_O) == GLFW_PRESS){
            model = glm::rotate(model, glm::radians(-60.0f) * cameraSpeed, glm::vec3(1.0f, 0.0f, 0.0f)); 
        }
        // Assign a key to make the the camera spin about the object.
        if (glfwGetKey(window, GLFW_KEY_C) == GLFW_PRESS){
            object_view = glm::mat4(1.0f);
            cameraTarget = glm::vec3(0.0f, 0.0f, 0.0f);
            cameraPos = glm::vec3(rot * glm::vec4(cameraPos, 1.0f));
            camera_view = glm::lookAt(cameraPos, cameraTarget, cameraUp);
        }

        // updating M, V
        int modelLoc = glGetUniformLocation(shaderProgram, "M");
        glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(model));
        view = camera_view * object_view;
        int viewLoc = glGetUniformLocation(shaderProgram, "V");
        glUniformMatrix4fv(viewLoc, 1, GL_FALSE, glm::value_ptr(view));
    }
};

void framebuffer_size_callback(GLFWwindow* window, int width, int height);

// glfw: whenever the window size changed (by OS or user resize) this callback function executes
// ---------------------------------------------------------------------------------------------
void framebuffer_size_callback(GLFWwindow* window, int width, int height)
{
    // make sure the viewport matches the new window dimensions; note that width and 
    // height will be significantly larger than specified on retina displays.
    glViewport(0, 0, width, height);
}

#endif