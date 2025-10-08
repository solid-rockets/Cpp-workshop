#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <stdio.h>
#include <unistd.h>
#include "main.h"

int main(void)
{
    GLFWwindow* window;

    /* Initialize the library */
    if (!glfwInit())
        return -1;

    /* Create a windowed mode window and its OpenGL context */
    window = glfwCreateWindow(640, 480, "Hello World", NULL, NULL);
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
    //glOrtho(-1.0, 1.0, -1.0, 1.0, 1.0, -5.0);
    //glFrustum(-1.0, 1.0, -1.0, 1.0, 0.25, 100.0);
    glFrustum(-0.25, 0.25, -0.25, 0.25, 0.25, 100.0);

    /* Loop until the user closes the window */
    while (!glfwWindowShouldClose(window))
    {
        /* Call the drawing function */
        drawGraphics();

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
