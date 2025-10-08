#include <GLFW/glfw3.h>
#include "main.h"

void drawGraphics()
{
    glClear(GL_COLOR_BUFFER_BIT);

    // Draw right half.
    glBegin(GL_TRIANGLE_FAN);
    glColor3f(0.0, 0.5, 0.9);
    
    glVertex2f(0.0, -0.4);
    glVertex2f(0.3, -0.1);
    glVertex2f(0.3,  0.1);
    glVertex2f(0.2,  0.2);
    glVertex2f(0.1,  0.2);
    glVertex2f(0.0,  0.1);

    glEnd();

    // Draw left half.
    glBegin(GL_TRIANGLE_FAN);
    glColor3f(0.9, 0.5, 0.0);
    
    glVertex2f( 0.0, -0.4);
    glVertex2f( 0.0,  0.1);
    glVertex2f(-0.1,  0.2);
    glVertex2f(-0.2,  0.2);
    glVertex2f(-0.3,  0.1);
    glVertex2f(-0.3, -0.1);
    
    glEnd();
}
