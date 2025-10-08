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

    // Enable extra capabilities.
    glEnableClientState(GL_VERTEX_ARRAY);

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
