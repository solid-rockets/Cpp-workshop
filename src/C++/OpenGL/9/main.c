#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <stdio.h>
#include <unistd.h>
#include "main.h"

int winHeight = 600;
int winWidth = 800;

int main(void)
{
    GLFWwindow* window;


    int halfHeight = winHeight / 2;
    int halfWidth = winWidth / 2;

    /* Initialize the library */
    if (!glfwInit())
        return -1;

    /* Create a windowed mode window and its OpenGL context */
    window = glfwCreateWindow(winWidth, 
                              winHeight, 
                              "Hello World", 
                              NULL, 
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

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glFrustum(-0.25, 0.25, -0.25, 0.25, 0.25, 100.0);

    /* Loop until the user closes the window */
    while (!glfwWindowShouldClose(window))
    {
        // Prepare the window.
        glClear(GL_COLOR_BUFFER_BIT);

        // Pane down-left.
        glViewport(0, 0, halfWidth, halfHeight);
        drawGraphics();

        // Pane down-right.
        glViewport(halfWidth, 0, halfWidth, halfHeight);
        drawGraphics();
        
        // Pane up-left.
        glViewport(0, halfHeight, halfWidth, halfHeight);
        drawGraphics();

        // Pane up-right.
        glViewport(halfWidth, halfHeight, halfWidth, halfHeight);
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
