#include <glad/glad.h>
#include <GLFW/glfw3.h>

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>

#include <iostream>
#include <fstream>
#include <string.h>

void framebuffer_size_callback(GLFWwindow* window, int width, int height);

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

struct Object{
    int n = 0, m = 0;
    float* vertices = NULL;

    void select_type(){
        std::cout << "Press:\n";
        std::cout << "1 for Decagonal Prism\n";
        std::cout << "2 for Undecagonal Pyramid\n";
        std::cout << "3 for Hexagonal Dipyramid\n";
        std::cout << "Any other key for Exit\n";
        int key = 3;
        std::cin>>key;
        char filename[128];
        switch(key){
            case 1: strcpy(filename, "../source/decagonal_prism.txt"); break;
            case 2: strcpy(filename, "../source/undecagonal_pyramid.txt"); break;
            case 3: strcpy(filename, "../source/hexagonal_dipyramid.txt"); break;
            default: exit(0);
        }
        get_vertices(filename);
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

int main()
{
    Object obj;
    // selecting object type
    obj.select_type();

    // glfw: initialize and configure
    // how to handle the windows n all are done with this
    // ------------------------------
    glfwInit();
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

#ifdef __APPLE__
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
#endif

    // glfw window creation
    // --------------------
    GLFWwindow* window = glfwCreateWindow(SCR_WIDTH, SCR_HEIGHT, "LearnOpenGL", NULL, NULL);
    if (window == NULL)
    {
        std::cout << "Failed to create GLFW window" << std::endl;
        glfwTerminate();
        return -1;
    }
    glfwMakeContextCurrent(window);
    glfwSetFramebufferSizeCallback(window, framebuffer_size_callback);

    // glad: load all OpenGL function pointers connect to opengl implementaion from driver
    // ---------------------------------------
    if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress))
    {
        std::cout << "Failed to initialize GLAD" << std::endl;
        return -1;
    }

    // build and compile our shader program
    // ------------------------------------
    // vertex shader
    unsigned int vertexShader = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vertexShader, 1, &vertexShaderSource, NULL);
    glCompileShader(vertexShader);
    // check for shader compile errors
    int success;
    char infoLog[512];
    glGetShaderiv(vertexShader, GL_COMPILE_STATUS, &success);
    if (!success)
    {
        glGetShaderInfoLog(vertexShader, 512, NULL, infoLog);
        std::cout << "ERROR::SHADER::VERTEX::COMPILATION_FAILED\n" << infoLog << std::endl;
    }

    // fragment shader
    unsigned int fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fragmentShader, 1, &fragmentShaderSource, NULL);
    glCompileShader(fragmentShader);
    // check for shader compile errors
    glGetShaderiv(fragmentShader, GL_COMPILE_STATUS, &success);
    if (!success)
    {
        glGetShaderInfoLog(fragmentShader, 512, NULL, infoLog);
        std::cout << "ERROR::SHADER::FRAGMENT::COMPILATION_FAILED\n" << infoLog << std::endl;
    }

    // link shaders
    unsigned int shaderProgram = glCreateProgram();
    glAttachShader(shaderProgram, vertexShader);
    glAttachShader(shaderProgram, fragmentShader);
    glLinkProgram(shaderProgram);
    // check for linking errors
    glGetProgramiv(shaderProgram, GL_LINK_STATUS, &success);
    if (!success) {
        glGetProgramInfoLog(shaderProgram, 512, NULL, infoLog);
        std::cout << "ERROR::SHADER::PROGRAM::LINKING_FAILED\n" << infoLog << std::endl;
    }
    glDeleteShader(vertexShader);
    glDeleteShader(fragmentShader);

    // set up vertex data (and buffer(s)) and configure vertex attributes
    // ------------------------------------------------------------------
    unsigned int VBO, VAO;
    glGenVertexArrays(1, &VAO);
    glGenBuffers(1, &VBO);
    // first parameter is for number of buffer objects to create
    // bind the Vertex Array Object first, then bind and set vertex buffer(s), and then configure vertex attributes(s).
    glBindVertexArray(VAO);

    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    glBufferData(GL_ARRAY_BUFFER, obj.n * obj.m * sizeof(float), obj.vertices, GL_STATIC_DRAW);

    // position attribute
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)0);
    glEnableVertexAttribArray(0);
    // color attribute
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)(3 * sizeof(float)));
    glEnableVertexAttribArray(1);

    // You can unbind the VAO afterwards so other VAO calls won't accidentally modify this VAO, but this rarely happens. Modifying other
    // VAOs requires a call to glBindVertexArray anyways so we generally don't unbind VAOs (nor VBOs) when it's not directly necessary.
    // glBindVertexArray(0);

    // as we only have a single shader, we could also just activate our shader once beforehand if we want to 
    glUseProgram(shaderProgram);

    // initialize M, V, P matrix
    Coordinates coordinates(shaderProgram);

    // render loop
    // -----------
    while (!glfwWindowShouldClose(window))
    {
        // take input and update MVP
        // -----
        coordinates.processInput(window, shaderProgram);

        // render
        // ------
        glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
        // depth
        glEnable(GL_DEPTH_TEST);
        // clear color and depth buffer
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        // render the triangle
        glBindVertexArray(VAO);
        glDrawArrays(GL_TRIANGLES, 0, obj.n);

        // glfw: swap buffers and poll IO events (keys pressed/released, mouse moved etc.)
        // -------------------------------------------------------------------------------
        glfwSwapBuffers(window);
        glfwPollEvents();
    }

    // optional: de-allocate all resources once they've outlived their purpose:
    // ------------------------------------------------------------------------
    glDeleteVertexArrays(1, &VAO);
    glDeleteBuffers(1, &VBO);
    glDeleteProgram(shaderProgram);
    free(obj.vertices);

    // glfw: terminate, clearing all previously allocated GLFW resources.
    // ------------------------------------------------------------------
    glfwTerminate();
    return 0;
}

// glfw: whenever the window size changed (by OS or user resize) this callback function executes
// ---------------------------------------------------------------------------------------------
void framebuffer_size_callback(GLFWwindow* window, int width, int height)
{
    // make sure the viewport matches the new window dimensions; note that width and 
    // height will be significantly larger than specified on retina displays.
    glViewport(0, 0, width, height);
}