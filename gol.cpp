#include <raylib.h>
#include <cstdio>
#include <cstdlib>
#include <ctime>

#define INIT_TARGET_FPS 10

#define BLOCK_DIV    1
#define BLOCK_SIZE   20
#define BOARD_WIDTH  40
#define BOARD_HEIGHT 40

// DEFINITIONS
enum CELL {
  EMPTY = 0,
  ALIVE = 1,
  NEXT  = 3
};


// GLOBAL STATE
bool isGameStopped = false;
bool mustRandomize = false;

// LOGIC
Color Cell2Color(int val) {
  switch(val) {
    case CELL::EMPTY : return BLACK;
    case CELL::ALIVE : return SKYBLUE;
    default          : return BLACK;
  }
}

int CalcPos(int c, int r) {
  return r * BOARD_WIDTH + c;
}

void PopulateBoard(char* board) {
  for(int r = 0; r < BOARD_HEIGHT; r++) {
    for(int c = 0; c < BOARD_WIDTH; c++) {
      auto p = CalcPos(c, r);
      board[p] = CELL::EMPTY;
    }
  }
}

void RandomizeBoard(char* board) {
  mustRandomize = false;

  for(int r = 0; r < BOARD_HEIGHT; r++) {
    for(int c = 0; c < BOARD_WIDTH; c++) {
      auto p = CalcPos(c, r);

      if(rand() % 2)
        board[p] = CELL::EMPTY;
      else
        board[p] = CELL::ALIVE;
    }
  }
}

void DrawBoard(char* board) {
  // TODO: use cell divider.
  for(int r = 0; r < BOARD_HEIGHT; r++) {
    for(int c = 0; c < BOARD_WIDTH; c++) {
      auto p = r * BOARD_WIDTH + c;
      auto color = Cell2Color(board[p]);

      auto posX = c * BLOCK_SIZE;
      auto posY = r * BLOCK_SIZE;

      DrawRectangle(posX, posY, BLOCK_SIZE, BLOCK_SIZE, color);
    }
  }
}

void ProcessInputs() {
  if(IsKeyPressed(KEY_R))
    mustRandomize = true;

  if(IsKeyPressed(KEY_S))
    isGameStopped != isGameStopped;
}

int main() {
  // Initialize all variables.
  srand(time(0));
  auto board = new char[BOARD_WIDTH * BOARD_HEIGHT];
  PopulateBoard(board);

  // Proceed with drawing, etc.
  InitWindow(BLOCK_SIZE * BOARD_WIDTH, BLOCK_SIZE * BOARD_HEIGHT, "snek");
  SetTargetFPS(INIT_TARGET_FPS);

  // Game loop
  while(!WindowShouldClose()) {
    BeginDrawing();
      ClearBackground(BLACK);
      DrawBoard(board);
    EndDrawing(); // This polls events automatically.

    ProcessInputs();

    if(!isGameStopped) {
      // TODO: actually run the simulation
      if(mustRandomize) RandomizeBoard(board);
    }
  }

  // Cleanup.
  CloseWindow();
  delete board; // PREVENT MEMORY LEAKS!!!

  return 0;
}
