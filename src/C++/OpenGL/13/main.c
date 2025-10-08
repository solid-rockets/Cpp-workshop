#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <stdio.h>
#include <unistd.h>
#include "main.h"

int winHeight = 600;
int winWidth = 800;

void setupCamera(double Head, double Yaw, double X, double Y, double Z)
{
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glFrustum(-0.25, 0.25, -0.25, 0.25, 0.25, 100.0);

    glRotated(-Yaw, 0.0, 1.0, 0.0); // Yaw next.
    glRotated(-Head, 1.0, 0.0, 0.0); // Heading first.
    glTranslated(-X, -Y, -Z);
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

    int halfHeight = winHeight / 2;
    int halfWidth = winWidth / 2;

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
        glViewport(0, 0, halfWidth, halfHeight);
        setupCamera(50.0, 0.0, 0.0, -1.35, 1.25);
        drawGraphics();

        // Pane down-right.
        glViewport(halfWidth, 0, halfWidth, halfHeight);
        setupCamera(0.0, 90.0, 2.0, 0.0, 0.0);
        drawGraphics();

        // Pane up-left.
        glViewport(0, halfHeight, halfWidth, halfHeight);
        setupCamera(0.0, 0.0, 0.0, 0.0, 1.0);
        drawGraphics();

        // Pane up-right.
        glViewport(halfWidth, halfHeight, halfWidth, halfHeight);
        setupCamera(0.0, 180.0, 0.0, 0.0, -2.0);
        drawGraphics();

        // Once all panes are done, increment degrees for the heart.
        incDegrees();

        /* Swap front and back buffers */
        glfwSwapBuffers(window);

        /* Poll for and process events */
        glfwPollEvents();

        // Wait before iterating.
        usleep(42); // 24 FPS.
    }

    glfwTerminate();
    return 0;
}
