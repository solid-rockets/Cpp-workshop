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

const GLdouble vert_arr[] = {
    0.0,  0.3,  0.0,
    0.0, -0.3,  0.0,

    0.3,  0.0,  0.0,
   -0.3,  0.0,  0.0,

    0.0,  0.0,  0.3,
    0.0,  0.0, -0.3
 };

 const GLdouble platform_vert_arr[] = {
     0.5, -0.5,  0.5,
     0.5, -0.5, -0.5,
    -0.5, -0.5, -0.5,
    -0.5, -0.5,  0.5
 };

 const GLdouble color_arr[] = {
    1.0, 1.0, 1.0,
    1.0, 1.0, 1.0,
    1.0, 1.0, 1.0,
    1.0, 1.0, 1.0
 };

const GLubyte first_index_arr[]  = {i_FRONT, i_BOTTOM, i_RIGHT, i_TOP};
const GLubyte second_index_arr[] = {i_RIGHT, i_BOTTOM, i_BACK,  i_TOP};
const GLubyte third_index_arr[]  = {i_BACK,  i_BOTTOM, i_LEFT,  i_TOP};
const GLubyte fourth_index_arr[] = {i_LEFT,  i_BOTTOM, i_FRONT, i_TOP};

const GLubyte platform_index_arr[] = {0, 1, 2, 3}; // 4 elements.

static GLdouble degrees = 0.0;
static GLdouble view_angle = 45.0;

void drawOrientationMarks()
{
    glColor3d(1.0, 0.0, 0.0);
    glBegin(GL_LINES);
    glVertex3d(0.0, 0.0, 0.0);
    glVertex3d(2.0, 0.0, 0.0);
    glEnd();

    glColor3d(0.0, 1.0, 0.0);
    glBegin(GL_LINES);
    glVertex3d(0.0, 0.0, 0.0);
    glVertex3d(0.0, 2.0, 0.0);
    glEnd();

    glColor3d(0.0, 0.0, 1.0);
    glBegin(GL_LINES);
    glVertex3d(0.0, 0.0, 0.0);
    glVertex3d(0.0, 0.0, 2.0);
    glEnd();
}

void drawGraphics()
{
    // First, clear the buffer.
    glLineWidth(5.0);

    // Start working on the matrix.
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    glRotated(view_angle, 1.0, 0.0, 0.0);

    // Draw the tile, since the matrix for it is ready.
    glVertexPointer(3, GL_DOUBLE, 0, platform_vert_arr);

    glColor3d(0.5, 0.5, 0.5);
    glDrawElements(GL_TRIANGLE_FAN, 4, GL_UNSIGNED_BYTE, platform_index_arr);

    //glColor3d(1.0, 1.0, 1.0);
    glEnableClientState(GL_COLOR_ARRAY);
    glColorPointer(3, GL_DOUBLE, 0, color_arr);
    glDrawElements(GL_LINE_LOOP, 4, GL_UNSIGNED_BYTE, platform_index_arr);
    glDisableClientState(GL_COLOR_ARRAY);

    // Draw the sides.
    glRotated(degrees, 0.0, 1.0, 0.0);

    glVertexPointer(3, GL_DOUBLE, 0, vert_arr);

    glColor3d(1.0, 0.0, 0.0);
    glDrawElements(GL_TRIANGLE_FAN, 4, GL_UNSIGNED_BYTE, first_index_arr);
    glColor3d(0.25, 0.25, 0.25);
    glDrawElements(GL_LINE_LOOP, 4, GL_UNSIGNED_BYTE, first_index_arr);

    glColor3d(0.0, 1.0, 0.0);
    glDrawElements(GL_TRIANGLE_FAN, 4, GL_UNSIGNED_BYTE, second_index_arr);
    glColor3d(0.25, 0.25, 0.25);
    glDrawElements(GL_LINE_LOOP, 4, GL_UNSIGNED_BYTE, second_index_arr);

    glColor3d(0.0, 0.0, 1.0);
    glDrawElements(GL_TRIANGLE_FAN, 4, GL_UNSIGNED_BYTE, third_index_arr);
    glColor3d(0.25, 0.25, 0.25);
    glDrawElements(GL_LINE_LOOP, 4, GL_UNSIGNED_BYTE, third_index_arr);

    glColor3d(1.0, 1.0, 1.0);
    glDrawElements(GL_TRIANGLE_FAN, 4, GL_UNSIGNED_BYTE, fourth_index_arr);
    glColor3d(0.25, 0.25, 0.25);
    glDrawElements(GL_LINE_LOOP, 4, GL_UNSIGNED_BYTE, fourth_index_arr);

    // Draw the orientation marks.
    glPushMatrix();
    glLoadIdentity();
    glLineWidth(1.0);
    drawOrientationMarks();
    glPopMatrix();
}

void incDegrees()
{
    // Increment degrees.
    degrees += 1.0;

    if(degrees > 360.0)
        degrees -= 360.0;
}
