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
  double* rotaMatrix;    // Ptr, so calculate somewhere else.
  double translation[3]; // Initialize when creating struct.
  double scale[3];
  double depthFactor;
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

void scaleVectorToScreen(double* v, double* s, double* o) {
  forRange(i, 0, 2)
    o[i] = v[i] * s[i];
}

void translateVector(double* v, double* t, double* o) {
  forRange(i, 0, VLEN)
    o[i] = v[i] + t[i];
}

void drawLineFromVectors
  (RotationState &state, double* v1, double* v2, Color color) {
  double o1[VLEN];
  double o2[VLEN];

  mulMV(state.rotaMatrix, v1, o1);
  mulMV(state.rotaMatrix, v2, o2);

  scaleVectorToScreen(o1, state.scale, o1);
  scaleVectorToScreen(o2, state.scale, o2);

  translateVector(o1, state.translation, o1);
  translateVector(o2, state.translation, o2);

  // TODO: apply perspective.

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
    { 0.0, 1.0, 0.0},

    {-1.0, 2.0, 0.0},
    {-2.0, 1.0, 0.0},
    {-2.0, 0.0, 0.0},

    { 0.0,-2.0, 0.0},

    { 2.0, 0.0, 0.0},
    { 2.0, 1.0, 0.0},
    { 1.0, 2.0, 0.0},
  };

  InitWindow(WIDTH, HEIGHT, "vectors");
  SetTargetFPS(TARGET_FPS);

  double emptyMats[5][MLEN]; // Destruction managed automatically (stack)

  for(double d = 0.0; !WindowShouldClose(); d += DELTA_RAD) {
    double* rotaY  = initM(emptyMats[0]);
    double* rotaX  = initM(emptyMats[1]);
    double* accMat = initM(emptyMats[2]);

    prepRotY(rotaY, d);
    prepRotX(rotaX, PI/4);

    mulMM(rotaX, rotaY, accMat);

    printf("Rotation Y\n");
    printM(rotaY);
    newline();

    printf("Rotation X\n");
    printM(rotaX);
    newline();

    printf("Acc mat\n");
    printM(accMat);
    newline();

    RotationState state = {
      accMat,
      {WIDTH/2, HEIGHT/2, 0},
      {SCALE, -SCALE, 0},
      0
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
