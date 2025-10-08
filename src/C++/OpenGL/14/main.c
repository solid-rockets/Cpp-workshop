#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <stdio.h>
#include <unistd.h>
#include "main.h"

int winHeight = 600;
int winWidth = 800;

double player_head = 0.0;
double player_yaw  = 0.0;
double player_x    = 0.0;
double player_y    = 0.0;
double player_z    = -5.0;

double mov_delta = 0.1;
double rot_delta = 0.5;

void setupCamera()
{
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glFrustum(-0.25, 0.25, -0.25, 0.25, 0.25, 100.0);

    glRotated(-player_yaw, 0.0, 1.0, 0.0); // Yaw next.
    glRotated(-player_head, 1.0, 0.0, 0.0); // Heading first.
    glTranslated(-player_x, -player_y, -player_z);
}

void updateOrientation(GLFWwindow *window)
{
    if(glfwGetKey(window, GLFW_KEY_A))
        player_x -= mov_delta;

    if(glfwGetKey(window, GLFW_KEY_D))
        player_x += mov_delta;

    if(glfwGetKey(window, GLFW_KEY_W))
        player_z -= mov_delta;

    if(glfwGetKey(window, GLFW_KEY_S))
        player_z += mov_delta;

    if(glfwGetKey(window, GLFW_KEY_Q))
        player_y += mov_delta;

    if(glfwGetKey(window, GLFW_KEY_E))
        player_y -= mov_delta;

    if(glfwGetKey(window, GLFW_KEY_U))
        player_head -= rot_delta;

    if(glfwGetKey(window, GLFW_KEY_J))
        player_head += rot_delta;

    if(glfwGetKey(window, GLFW_KEY_H))
        player_yaw -= rot_delta;

    if(glfwGetKey(window, GLFW_KEY_K))
        player_yaw += rot_delta;
}

int main(void)
{

    /* Initialize the library */
    if (!glfwInit())
        return -1;

    // Start working with windows/monitors.
    GLFWwindow* window;

    const GLFWvidmode* mode = glfwGetVideoMode(glfwGetPrimaryMonitor());

    winHeight = mode->height;
    winWidth  = mode->width;

    /* Create a windowed mode window and its OpenGL context */
    window = glfwCreateWindow(winWidth,
                              winHeight,
                              "Hello World",
                              NULL, //glfwGetPrimaryMonitor(), // Activating the method call will start the app in fullscreen mode.
                              NULL);
    if (!window)
    {
        glfwTerminate();
        return -1;
    }

    /* Make the window's context current */
    glfwMakeContextCurrent(window);

    // Load functions through GLEW.
    // NOTE: IT APPEARS this call can only be made after
    //       (1) a new window is created and (2) it is made to be
    //       the current context.
    int glewError = glewInit();
    if(glewError != GLEW_OK)
    {
        printf("GLEW initialization error occured: %d\n", glewError);
        return glewError;
    }

    // Enable extra capabilities.
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnable(GL_DEPTH_TEST);

    /* Loop until the user closes the window */
    while (!glfwWindowShouldClose(window))
    {
        // Prepare the window.
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        // Pane down-left.
        glViewport(0, 0, winWidth, winHeight);
        setupCamera();
        drawGraphics();

        // Once all panes are done, increment degrees for the heart.
        incDegrees();

        /* Swap front and back buffers */
        glfwSwapBuffers(window);

        /* Poll for and process events */
        glfwPollEvents();

        // Perform movement.
        updateOrientation(window);

        // Wait before iterating.
        usleep(42); // 24 FPS.
    }

    glfwTerminate();
    return 0;
}
