#include <raylib.h>
#include <math.h>
#include <stdio.h>

#define WIDTH  320
#define HEIGHT 240
#define TARGET_FPS 24
#define SCALE 30
#define DELTA_RAD 0.1

// DEFINITIONS
// TODO: replace fors with forRange
#define newline() putchar('\n')
#define forRange(var, from, to) for(int var = 0; var < to; var++)

#define VLEN 3
#define MLEN (VLEN*3)

typedef struct {
  // Unit world
  double* rotation;    // Ptr, so calculate somewhere else.
  double position[3];    // Ptr, so calculate somewhere else.
  double depthFactor;

  // Screen world
  double screen[2];
} RotationState;

// GLOBAL STATE


// LOGIC
double dot(double* v1, double* v2) {
  return
    v1[0] * v2[0] +
    v1[1] * v2[1] +
    v1[2] * v2[2];
}

void mulMV(double* m, double* v, double* o) {
  forRange(i, 0, VLEN)
    o[i] = dot(&m[VLEN * i],  v);
}

void clearM(double* o) {
  forRange(i, 0, MLEN)
    o[i] = 0;
}

double* initM(double* o) {
  forRange(r, 0, VLEN) {
    forRange(c, 0, VLEN) {
      if(r == c)
        o[r * VLEN + c] = 1;
      else
        o[r * VLEN + c] = 0;
    }
  }

  return o;
}

double* copyM(double* a, double* b) {
  forRange(i, 0, MLEN)
    b[i] = a[i];

  return b;
}

void mulMM(double* a, double* b, double* o) {
  // Allowing this function to have b == o is useful, and saves some memory,
  // but ultimately all the copying to a temp matrix adds a lot of computational
  // overhead not may overload smaller processors.
  // Let me experiment with the concept of matrix stacks in the future.
  double t[16];
  forRange(i, 0, MLEN) t[i] = 0;

  forRange(c, 0, VLEN) {
    double vb[VLEN] = {
      b[c],
      b[c+VLEN],
      b[c+VLEN*2]
    };

    forRange(r, 0, VLEN) {
      double* va = &a[r * VLEN];

      t[r*VLEN + c] = dot(va, vb);
    }
  }

  forRange(i, 0, MLEN) o[i] = t[i];
}

void printV(double* a) {
  printf("%f %f %f\n", a[0], a[1], a[2]);
}

void printM(double* a) {
  forRange(i, 0, MLEN)
    printV(&a[i]);
}

void adjustToScreen(double* v, double* s, double* o) {
  auto width  = s[0];
  auto height = s[1];

  // Same scale is to make things square, not rectangular.
  o[0] = v[0] *  height + width/2;
  o[1] = v[1] * -height + height/2; // NOTE: neg. y for vertical inversion
}

void addVectors(double* v, double* t, double* o) {
  forRange(i, 0, VLEN)
    o[i] = v[i] + t[i];
}

void scaleVector(double* v, double s, double* o) {
  forRange(i, 0, VLEN)
    o[i] = v[i] * s;
}

void drawLineFromVectors
  (RotationState &state, double* v1, double* v2, Color color) {
  double o1[VLEN];
  double o2[VLEN];

  // Unit world.
  mulMV(state.rotation, v1, o1);
  mulMV(state.rotation, v2, o2);

  addVectors(o1, state.position, o1);
  addVectors(o2, state.position, o2);

  scaleVector(o1, state.depthFactor / abs(o1[2]), o1);
  scaleVector(o2, state.depthFactor / abs(o2[2]), o2);

  // Screen world
  adjustToScreen(o1, state.screen, o1);
  adjustToScreen(o2, state.screen, o2);

  DrawLine(o1[0], o1[1], o2[0], o2[1], color);
}

double* prepRotY(double* mat, double rad) {
  initM(mat);

  mat[0] =  cos(rad);
  mat[2] =  sin(rad);
  mat[6] = -sin(rad);
  mat[8] =  cos(rad);

  return mat;
}

double* prepRotX(double* mat, double rad) {
  initM(mat);

  mat[4] =  cos(rad);
  mat[5] = -sin(rad);
  mat[7] =  sin(rad);
  mat[8] =  cos(rad);

  return mat;
}

int main() {
  // Let's get some display rolling here...
  double heart_arr[8][VLEN] = {
    { .0,  .25, .0},

    {-.25, .5,  .0},
    {-.5,  .25, .0},
    {-.5,  .0,  .0},

    { .0, -.5,  .0},

    { .5,  .0,  .0},
    { .5,  .25, .0},
    { .25, .5,  .0},
  };

  double arrows[4][VLEN] = {
    { 0,  0,  0},
    {.2,  0,  0},
    { 0, .2,  0},
    { 0,  0, .2}
  };
  Color colors[3] = { RED, GREEN, BLUE};

  double cube[8][VLEN] = {
    { .4, .4, 0},
    {-.4, .4, 0},
    {-.4,-.4, 0},
    { .4,-.4, 0},

    { .4, .4, -1},
    {-.4, .4, -1},
    {-.4,-.4, -1},
    { .4,-.4, -1}
   };


  InitWindow(WIDTH, HEIGHT, "vectors");
  SetTargetFPS(TARGET_FPS);

  double emptyMats[6][MLEN];

  for(double d = 0.0; !WindowShouldClose(); d += DELTA_RAD) {
    int top = 0; // Very basic stack

    double* rotaY  = initM(emptyMats[top++]);
    double* rotaX  = initM(emptyMats[top++]);
    double* globalY= initM(emptyMats[top++]);

    double* heartMat = initM(emptyMats[top++]);
    double* otherMat = initM(emptyMats[top++]);

    prepRotY(rotaY, d);
    prepRotX(rotaX, PI/4);
    prepRotY(globalY, PI/4);

    mulMM(rotaX,   rotaY,   heartMat);
    mulMM(globalY, heartMat, heartMat);
    mulMM(globalY, rotaX,   otherMat);

    RotationState state = {
      NULL,
      {0, 0, -1.25},
      1,
      {WIDTH, HEIGHT}
    };

    BeginDrawing();
      ClearBackground(BLACK);

      // Draw the arrows
      state.rotation = otherMat;
      forRange(i, 1, 4)
        drawLineFromVectors(
          state,
          arrows[0],
          arrows[i],
          colors[i-1]
        );

      // Draw the heart.
      state.rotation = heartMat;
      forRange(i, 0, 8)
        drawLineFromVectors(
          state,
          heart_arr[i],
          heart_arr[(i+1)%8],
          GREEN);

      // Draw the cube.
      state.rotation = otherMat;
      forRange(i, 0, 4) // Front square
        drawLineFromVectors(
          state,
          cube[i],
          cube[(i+1)%4],
          BLUE
        );
      forRange(i, 0, 4) // Front->back connecting lines
        drawLineFromVectors(
          state,
          cube[i],
          cube[i+4],
          BLUE
        );
      forRange(i, 0, 4) // Back square
        drawLineFromVectors(
          state,
          cube[4+i],
          cube[4+(i+1)%4],
          BLUE
        );

    EndDrawing();
  }
}
