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
  for(int i = 0; i < VLEN; i++)
    o[i] = dot(&m[VLEN * i],  v);
}

void clearM(double* o) {
  for(int i = 0; i < MLEN; i++)
    o[i] = 0;
}

double* initM(double* o) {
  for(int r = 0; r < VLEN; r++) {
    for(int c = 0; c < VLEN; c++) {
      if(r == c)
        o[r * VLEN + c] = 1;
      else
        o[r * VLEN + c] = 0;
    }
  }

  return o;
}

double* copyM(double* a, double* b) {
  for(int i = 0; i < MLEN; i++)
    b[i] = a[i];

  return b;
}

void mulMM(double* a, double* b, double* o) {
  for(int c = 0; c < VLEN; c++) {
    double vb[VLEN] = {
      b[c],
      b[c+VLEN],
      b[c+VLEN*2]
    };

    for(int r = 0; r < VLEN; r++) {
      double* va = &a[r * VLEN];

      o[r*VLEN + c] = dot(va, vb);
    }
  }
}

void printV(double* a) {
  printf("%f %f %f\n", a[0], a[1], a[2]);
}

void printM(double* a) {
  for(int i = 0; i < MLEN; i += VLEN) {
    printV(&a[i]);
  }
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

void drawLineFromVectors
  (RotationState &state, double* v1, double* v2, Color color) {
  double o1[VLEN];
  double o2[VLEN];

  // Unit world.
  mulMV(state.rotation, v1, o1);
  mulMV(state.rotation, v2, o2);

  // TODO: fix perspective

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

  InitWindow(WIDTH, HEIGHT, "vectors");
  SetTargetFPS(TARGET_FPS);

  double emptyMats[5][MLEN]; // Destruction managed automatically (stack)

  for(double d = 0.0; !WindowShouldClose(); d += DELTA_RAD) {
    double* rotaY  = initM(emptyMats[0]);
    double* rotaX  = initM(emptyMats[1]);
    double* accMat = initM(emptyMats[2]);

    prepRotY(rotaY, d);
    prepRotX(rotaX, 0);

    mulMM(rotaX, rotaY, accMat);

    RotationState state = {
      accMat,
      {0, 0, -3},
      0,
      {WIDTH, HEIGHT}
    };

    BeginDrawing();
      ClearBackground(BLACK);
      for(int i = 0; i < 8; i++)
        drawLineFromVectors(
          state,
          heart_arr[i],
          heart_arr[(i+1)%8],
          GREEN);
    EndDrawing();
  }
}
