#include <GLFW/glfw3.h>
#include "main.h"

#define i_A 0
#define i_B 1
#define i_C 2
#define i_D 3
#define i_E 4
#define i_F 5
#define i_G 6
#define i_H 7
#define i_I 8
#define i_J 9

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

const GLubyte right_index_arr[] = {i_A, i_B, i_C, i_D, i_E, i_F};
const GLubyte left_index_arr[]  = {i_A, i_F, i_G, i_H, i_I, i_J};

static GLdouble degrees = 1.0;

void drawGraphics()
{
    glClear(GL_COLOR_BUFFER_BIT);

    glVertexPointer(2, GL_DOUBLE, 0, vert_arr);

    // Draw right half.
    glColor3f(0.0, 0.5, 0.9);

    glDrawElements(GL_TRIANGLE_FAN, 6, GL_UNSIGNED_BYTE, right_index_arr);

    // Draw left half.
    glColor3f(0.9, 0.5, 0.0);
   
    glDrawElements(GL_TRIANGLE_FAN, 6, GL_UNSIGNED_BYTE, left_index_arr);

    // Do rotation.
    glRotated(degrees, 0.0, 1.0, 0.0);
}
