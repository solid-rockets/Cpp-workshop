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
  double* rotaMatrix; // Ptr, so calculate somewhere else.
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

// TODO: fix this
//void mulMM(double* a, double* b, double* o) {
//  double t[16];
//  transposeM(b, t);
//
//  for(int c = 0; c < VLEN; c++) {
//    for(int r = 0; r < VLEN; r++) {
//      o[r*4 + c] = dot(&a[r*VLEN], &b[c*4]);
//    }
//  }
//}

void printV(double* a) {
  printf("%f %f %f\n", a[0], a[1], a[2]);
}

void printM(double* a) {
  for(int i = 0; i < MLEN; i += VLEN) {
    printV(&a[i]);
  }
}

double* genBasicM() { // TODO: bring a ptr from outside, don't generate new.
  auto arr = new double[MLEN]; // This is a bad idea - manual pointer mgmt sucks.

  for(auto i = 0; i < MLEN; i++)
    arr[i] = i * 1.0;

  return arr;
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
//
  printf("Vectors\n");
  printV(v1);
  printV(v2);
  printV(o1);
  printV(o2);
  newline();

  // TODO: apply perspective.

  DrawLine(o1[0], o1[1], o2[0], o2[1], color);
}

double* prepRotY(double* mat, double deg) {
  initM(mat);

  mat[0] =  cos(deg);
  mat[2] =  sin(deg);
  mat[6] = -sin(deg);
  mat[8] =  cos(deg);

  return mat;
}

int main() {
  // Let's get some display rolling here...
  double heart_arr[8][VLEN] = { // How would this be organized in memory?
    { 0.0, 1.0, 0.0},

    {-1.0, 2.0, 0.0},
    {-2.0, 1.0, 0.0},
    {-2.0, 0.0, 0.0},

    { 0.0,-2.0, 0.0},

    { 2.0, 0.0, 0.0},
    { 2.0, 1.0, 0.0},
    { 1.0, 2.0, 0.0},
  };

  InitWindow(WIDTH, HEIGHT, "snek");
  SetTargetFPS(TARGET_FPS);

  double emptyMats[5][MLEN]; // Why not? Destruction managed automatically (stack)

  for(double d = 0.0; !WindowShouldClose(); d += DELTA_RAD) { // TODO: d->angle
    double* rotationM = initM(emptyMats[0]);

    prepRotY(rotationM, d);

    printf("Rotation matrix\n");
    printM(rotationM);
    newline();

    RotationState state = {
      rotationM,
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
