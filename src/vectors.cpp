#include <raylib.h>
#include <math.h>
#include <stdio.h>

// DEFINITIONS
typedef double VECTOR[ 4];
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

void initM(MATRIX o) {
  for(int r = 0; r < 4; r++) {
    for(int c = 0; c < 4; c++) {
      if(r == c)
        o[r * 4 + c] = 1;
      else
        o[r * 4 + c] = 0;
    }
  }
}

void mulMM(MATRIX a, MATRIX b, MATRIX o) {
  for(int c2 = 0; c2 < 4; c2++) {
    for(int r = 0; r < 4; r++) {
      for(int c = 0; c < 4; c++) {
        auto v_a = a[r * 4 + c];
        auto v_b = b[c * 4 + c2];

        o[r * 4 + c2] += (v_a * v_b);
      }
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

double* genRandM() {
  auto arr = new double[16];

  for(auto i = 0; i < 16; i++)
    arr[i] = i * 1.0;

  return arr;
}

int main() {
  // [[0, 1, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14, 15]]^2
  auto A = genRandM();
  auto B = genRandM();
  auto C = genRandM();

  clearM(C);

  printM(A);
  putchar('\n');

  printM(B);
  putchar('\n');

  mulMM(A, B, C);
  printM(C);
}
