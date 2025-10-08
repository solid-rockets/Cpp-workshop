#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <stdio.h>
#include "main.h"

#define i_TOP    0
#define i_BOTTOM 1

#define i_RIGHT 2
#define i_LEFT  3

#define i_FRONT 4
#define i_BACK  5

const GLdouble vert_arr[] =
{0.0,  0.3,  0.0,
 0.0, -0.3,  0.0,

 0.3,  0.0,  0.0,
-0.3,  0.0,  0.0,

 0.0,  0.0,  0.3,
 0.0,  0.0, -0.3};

const GLubyte first_index_arr[]  = {i_FRONT, i_BOTTOM, i_RIGHT, i_TOP};
const GLubyte second_index_arr[] = {i_RIGHT, i_BOTTOM, i_BACK,  i_TOP};
const GLubyte third_index_arr[]  = {i_BACK,  i_BOTTOM, i_LEFT,  i_TOP};
const GLubyte fourth_index_arr[] = {i_LEFT,  i_BOTTOM, i_FRONT, i_TOP};

static GLdouble degrees = 0.0;
static GLdouble view_angle = 45.0;

void drawGraphics()
{
    // First, clear the buffer.
    //glClear(GL_COLOR_BUFFER_BIT);

    // Start working on the matrix.
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    //glTranslated(0.0, 0.0, -1.0); // This is the final operation applied to all.
    glRotated(view_angle, 1.0, 0.0, 0.0);

    // Draw the tile, since the matrix for it is ready.
    glColor3d(0.5, 0.5, 0.5);
    glBegin(GL_TRIANGLE_FAN);
    glVertex3d( 0.5,-0.5, 0.5);
    glVertex3d( 0.5,-0.5,-0.5);
    glVertex3d(-0.5,-0.5,-0.5);
    glVertex3d(-0.5,-0.5, 0.5);
    glEnd();

    // Draw the heart.
    glRotated(degrees, 0.0, 1.0, 0.0);

    glVertexPointer(3, GL_DOUBLE, 0, vert_arr);

    // Draw the sides.
    glColor3d(1.0, 0.0, 0.0);
    glDrawElements(GL_TRIANGLE_FAN, 4, GL_UNSIGNED_BYTE, first_index_arr);

    glColor3d(0.0, 1.0, 0.0);
    glDrawElements(GL_TRIANGLE_FAN, 4, GL_UNSIGNED_BYTE, second_index_arr);

    glColor3d(0.0, 0.0, 1.0);
    glDrawElements(GL_TRIANGLE_FAN, 4, GL_UNSIGNED_BYTE, third_index_arr);

    glColor3d(1.0, 1.0, 1.0);
    glDrawElements(GL_TRIANGLE_FAN, 4, GL_UNSIGNED_BYTE, fourth_index_arr);
}

void incDegrees()
{
    // Increment degrees.
    degrees += 1.0;

    if(degrees > 360.0)
        degrees -= 360.0;
}
