#include <GLFW/glfw3.h>
#include "main.h"

#define A_arr 0
#define B_arr 1
#define C_arr 2
#define D_arr 3
#define E_arr 4
#define F_arr 5
#define G_arr 6
#define H_arr 7
#define I_arr 8
#define J_arr 9

const GLdouble vert_arr[] =
{0.0, -0.4,
 0.3, -0.1,
 0.3,  0.1,
 0.2,  0.2,
 0.1,  0.2,
 0.0,  0.1,
 -0.1, 0.2,
 -0.2, 0.2,
 -0.3, 0.1,
 -0.3, -0.1};

static GLdouble degrees = 1.0;

void drawGraphics()
{
    glClear(GL_COLOR_BUFFER_BIT);

    glVertexPointer(2, GL_DOUBLE, 0, vert_arr);

    // Draw right half.
    glBegin(GL_TRIANGLE_FAN);
    glColor3f(0.0, 0.5, 0.9);
    
    glArrayElement(A_arr);
    glArrayElement(B_arr);
    glArrayElement(C_arr);
    glArrayElement(D_arr);
    glArrayElement(E_arr);
    glArrayElement(F_arr);

    glEnd();

    // Draw left half.
    glBegin(GL_TRIANGLE_FAN);
    glColor3f(0.9, 0.5, 0.0);
    
    glArrayElement(A_arr);
    glArrayElement(F_arr);
    glArrayElement(G_arr);
    glArrayElement(H_arr);
    glArrayElement(I_arr);
    glArrayElement(J_arr);
    
    glEnd();
    
    glRotated(degrees, 0.0, 1.0, 0.0);
}
