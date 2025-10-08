#include <GLFW/glfw3.h>
#include "main.h"

const GLdouble A_arr[] = {0.0, -0.4};
const GLdouble B_arr[] = {0.3, -0.1};
const GLdouble C_arr[] = {0.3,  0.1};
const GLdouble D_arr[] = {0.2,  0.2};
const GLdouble E_arr[] = {0.1,  0.2};
const GLdouble F_arr[] = {0.0,  0.1};
const GLdouble G_arr[] = {-0.1, 0.2};
const GLdouble H_arr[] = {-0.2, 0.2};
const GLdouble I_arr[] = {-0.3, 0.1};
const GLdouble J_arr[] = {-0.3, -0.1};

static GLdouble degrees = 1.0;

void drawGraphics()
{
    glClear(GL_COLOR_BUFFER_BIT);

    // Draw right half.
    glBegin(GL_TRIANGLE_FAN);
    glColor3f(0.0, 0.5, 0.9);
    
    glVertex2dv(A_arr);
    glVertex2dv(B_arr);
    glVertex2dv(C_arr);
    glVertex2dv(D_arr);
    glVertex2dv(E_arr);
    glVertex2dv(F_arr);

    glEnd();

    // Draw left half.
    glBegin(GL_TRIANGLE_FAN);
    glColor3f(0.9, 0.5, 0.0);
    
    glVertex2dv(A_arr);
    glVertex2dv(F_arr);
    glVertex2dv(G_arr);
    glVertex2dv(H_arr);
    glVertex2dv(I_arr);
    glVertex2dv(J_arr);
    
    glEnd();
    
    glRotated(degrees, 0.0, 1.0, 0.0);
}
