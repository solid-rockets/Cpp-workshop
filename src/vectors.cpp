#include <raylib.h>
#include <math.h>
#include <stdio.h>

#define WIDTH  320
#define HEIGHT 240
#define TARGET_FPS 24
#define SCALE 30
#define DELTA_RAD 0.1

// DEFINITIONS
#define newline() putchar('\n')

typedef double VECTOR[ 4]; // Making types for these was a mistake... use arrays
typedef double MATRIX[16]; // Row-major order.

// GLOBAL STATE


// LOGIC
double dot(VECTOR v1, VECTOR v2) {
  return
    v1[0] * v2[0] +
    v1[1] * v2[1] +
    v1[2] * v2[2] +
    v1[3] * v2[3];
}

void mulMV(MATRIX m, VECTOR v, VECTOR o) {
  o[0] = dot(&m[0],  v);
  o[1] = dot(&m[4],  v);
  o[2] = dot(&m[8],  v);
  o[3] = dot(&m[12], v);
}

void transposeM(MATRIX m, MATRIX o) {
  for(int r = 0; r < 4; r++) {
    for(int c = 0; c < 4; c++) {
      if(r != c) {
        auto val = m[r * 4 + c];
        o[c * 4 + r] = val;
      }
    }
  }
}

void clearM(MATRIX o) {
  for(int i = 0; i < 16; i++)
    o[i] = 0;
}

double* initM(MATRIX o) {
  for(int r = 0; r < 4; r++) {
    for(int c = 0; c < 4; c++) {
      if(r == c)
        o[r * 4 + c] = 1;
      else
        o[r * 4 + c] = 0;
    }
  }

  return o;
}

double* copyM(MATRIX a, MATRIX b) {
  for(int i = 0; i < 16; i++)
    b[i] = a[i];

  return b;
}

void mulMM(MATRIX a, MATRIX b, MATRIX o) {
  double t[16];
  transposeM(b, t);

  for(int c = 0; c < 4; c++) {
    for(int r = 0; r < 4; r++) {
      o[r*4 + c] = dot(&a[r*4], &b[c*4]);
    }
  }
}

void printV(VECTOR a) {
  printf("%f %f %f %f\n", a[0], a[1], a[2], a[3]);
}

void printM(MATRIX a) {
  for(int i = 0; i < 16; i += 4) {
    printV(&a[i]);
  }
}

double* genBasicM() { // TODO: bring a ptr from outside, don't generate new.
  auto arr = new double[16]; // This is a bad idea - manual pointer mgmt sucks.

  for(auto i = 0; i < 16; i++)
    arr[i] = i * 1.0;

  return arr;
}

void drawLineFromVectors(MATRIX mat, VECTOR v1, VECTOR v2, Color color) {
  double o1[4];
  double o2[4];

  mulMV(mat, v1, o1);
  mulMV(mat, v2, o2);

  DrawLine(o1[0], o1[1], o2[0], o2[1], color);
}

double* prepScaleM(MATRIX mat, int width, int height) {
  initM(mat); // Pastes identity into matrix 1st; that way we can keep all dims.

  mat[0] = width;
  mat[5] = height;

  return mat;
}

double* prepTransM(MATRIX mat, double x, double y, double z) {
  initM(mat);

  mat[3]  = x;
  mat[7]  = y;
  mat[11] = z;

  return mat;
}

double* prepRotY(MATRIX mat, double deg) {
  initM(mat);

  mat[0]  =  cos(deg);
  mat[2]  =  sin(deg);
  mat[8]  = -sin(deg);
  mat[10] =  cos(deg);

  return mat;
}

int main() {
  // [[0, 1, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]]^2
  auto A = genBasicM();
  auto B = genBasicM();
  auto C = genBasicM();

  VECTOR r = {1, 1, 0, 0};
  VECTOR o = {0, 0, 0, 0};

  clearM(C);

  printf("Matrix A\n");
  printM(A);
  newline();

  printf("Matrix B\n");
  printM(B);
  newline();

  printf("A * B\n");
  mulMM(A, B, C);
  printM(C);
  newline();

  printf("Vector r\n");
  printV(r);
  newline();

  printf("A * r\n");
  mulMV(A, r, o);
  printV(o);
  newline();

  // Let's get some display rolling here...
  double heart_arr[8][4] = { // How would this be organized in memory?
    { 0.0, 1.0, 0.0, 1.0},

    {-1.0, 2.0, 0.0, 1.0},
    {-2.0, 1.0, 0.0, 1.0},
    {-2.0, 0.0, 0.0, 1.0},

    { 0.0,-2.0, 0.0, 1.0},

    { 2.0, 0.0, 0.0, 1.0},
    { 2.0, 1.0, 0.0, 1.0},
    { 1.0, 2.0, 0.0, 1.0},
  };

  InitWindow(WIDTH, HEIGHT, "snek");
  SetTargetFPS(TARGET_FPS);

  double emptyMats[5][16]; // Why not? Destruction managed automatically (stack)

  for(double d = 0.0; !WindowShouldClose(); d += DELTA_RAD) {
    double* scaleMat = prepScaleM(emptyMats[0], SCALE, -SCALE);
    double* transMat = prepTransM(emptyMats[1], WIDTH / 2, HEIGHT / 2, 0);
    double* rotaMat  = prepRotY(emptyMats[2], d);
    double* tempMat  = initM(emptyMats[3]);
    double* resMat   = initM(emptyMats[4]);

    mulMM(rotaMat, scaleMat, tempMat);
    mulMM(transMat, tempMat, resMat);

    BeginDrawing();
      ClearBackground(BLACK);
      for(int i = 0; i < 8; i++)
        drawLineFromVectors(
          resMat,
          heart_arr[i],
          heart_arr[(i+1)%8],
          GREEN);
    EndDrawing();
  }
}
