#include <raylib.h>
#include <cstdio>
#include <cstdlib>
#include <ctime>

// TODO: allow user to change the speed of the simulation.
#define INIT_TARGET_FPS 10
#define CURSOR_DELAY 30

#define BLOCK_PDNG  1
#define BLOCK_SIZE 20
#define WIDTH  40
#define HEIGHT 40

// DEFINITIONS
enum CELL { // TODO: document the flags better.
  EMPTY = 0,
  ALIVE = 1,
  NEXT  = 2  // This is a flag that must be set.
};


// GLOBAL STATE
bool isGameStopped  = false;
bool mustRandomize  = false;
bool mustToggleCell = false;

typedef struct {
  int x;
  int y;
  int delay; // Will show cursor for this many frames.
} CURSOR;

CURSOR cursor = { // Basically, a struct is syn. sugar for a variable prefix.
  WIDTH  / 2,
  HEIGHT / 2,
  0
};

// LOGIC
Color Cell2Color(int val) {
  switch(val) {
    case CELL::EMPTY : return BLACK;
    case CELL::ALIVE : return SKYBLUE;
    default          : return BLACK;
  }
}

int CalcPos(int c, int r) {
  return r * WIDTH + c;
}

void PopulateBoard(char* board) {
  for(int r = 0; r < HEIGHT; r++) {
    for(int c = 0; c < WIDTH; c++) {
      auto p = CalcPos(c, r);
      board[p] = CELL::EMPTY;
    }
  }
}

void RandomizeBoard(char* board) {
  mustRandomize = false;

  for(int r = 0; r < HEIGHT; r++) {
    for(int c = 0; c < WIDTH; c++) {
      auto p = CalcPos(c, r);

      if(rand() % 2)
        board[p] = CELL::EMPTY;
      else
        board[p] = CELL::ALIVE;
    }
  }
}

void DrawBoard(char* board) {
  for(int r = 0; r < HEIGHT; r++) {
    for(int c = 0; c < WIDTH; c++) {
      auto p = CalcPos(c, r);
      auto color = Cell2Color(board[p]);

      auto posX = c * BLOCK_SIZE;
      auto posY = r * BLOCK_SIZE;

      DrawRectangle(
        posX + BLOCK_PDNG, // Block padding added.
        posY + BLOCK_PDNG,
        BLOCK_SIZE - BLOCK_PDNG * 2,
        BLOCK_SIZE - BLOCK_PDNG * 2,
        color);
    }
  }
}

void UpdateCursor(int dx, int dy) {
  cursor.x += dx;
  cursor.y += dy;

  if(cursor.x < 0) cursor.x = 0;
  if(cursor.y < 0) cursor.y = 0;
  if(cursor.x >= WIDTH)  cursor.x = WIDTH  - 1;
  if(cursor.y >= HEIGHT) cursor.y = HEIGHT - 1;

  cursor.delay = CURSOR_DELAY;
}

void ToggleCell(char* board) {
  auto p = CalcPos(cursor.x, cursor.y);

  board[p] = CELL::ALIVE;
  mustToggleCell = false;

  cursor.delay = CURSOR_DELAY;
}

void DrawCursor() {
  if(!cursor.delay) return;
  // TODO: make it match the square within
  DrawRectangleLines(
    cursor.x * BLOCK_SIZE,
    cursor.y * BLOCK_SIZE,
    BLOCK_SIZE,
    BLOCK_SIZE,
    ORANGE
  );
}

void ProcessInputs() {
  // TODO: use a swich
  if(IsKeyPressed(KEY_W)) UpdateCursor( 0,-1);
  if(IsKeyPressed(KEY_S)) UpdateCursor( 0, 1);
  if(IsKeyPressed(KEY_A)) UpdateCursor(-1, 0);
  if(IsKeyPressed(KEY_D)) UpdateCursor( 1, 0);

  // TODO: implement a key for emptying the board

  if(IsKeyPressed(KEY_ENTER))
    mustToggleCell = true;

  if(IsKeyPressed(KEY_R))
    mustRandomize = true;

  if(IsKeyPressed(KEY_SPACE))
    isGameStopped = !isGameStopped;
}

void UpdateSingleCell(char* board, int p) {
  int isL = p % WIDTH == 0;
  int isR = p % WIDTH == WIDTH - 1;
  int isU = p < WIDTH;
  int isD = p > WIDTH * (HEIGHT - 1);

  int count = 0;

  // For reference:
  // 123
  // 4p5
  // 678
  // By using the mask 0x1 I check the actual state of being alive,
  // while completely ignoring the NEXT flag's state.
  // TODO: this will read from outside the board; use ternary ops! with a macro?
  count += !isL & !isU & (board[p - WIDTH - 1] & 0x1); // Up-left corner check.
  count +=        !isU & (board[p - WIDTH    ] & 0x1);
  count += !isR & !isU & (board[p - WIDTH + 1] & 0x1);

  count += !isL        & (board[p         - 1] & 0x1);
  count += !isR        & (board[p         + 1] & 0x1);

  count += !isL & !isD & (board[p + WIDTH - 1] & 0x1);
  count +=        !isD & (board[p + WIDTH    ] & 0x1);
  count += !isR & !isD & (board[p + WIDTH + 1] & 0x1); // Down-right corner.

  auto cellVal = board[p];

  if(cellVal == CELL::EMPTY) {
    if(count == 3)
      board[p] |= CELL::NEXT;
  } else if(cellVal == CELL::ALIVE ) {
    if(count == 2 || count == 3)
      board[p] |= CELL::NEXT;
  }
}

void UpdateCells(char* board) {
  for(int r = 0; r < HEIGHT; r++) {
    for(int c = 0; c < WIDTH; c++) {
      auto p = CalcPos(c, r);

      UpdateSingleCell(board, p);
    }
  }
}

void AdvanceCells(char* board) {
  auto totalSize = HEIGHT * WIDTH;
  for(int i = 0; i < totalSize; i++)
    board[i] >>= 1; // NEXT state flows down to LIFE flag.
}

void UpdateMisc() {
  if(cursor.delay > 0) cursor.delay--;
}

int main() {
  // Initialize all variables.
  srand(time(0));
  auto board = new char[WIDTH * HEIGHT];
  PopulateBoard(board);

  // Proceed with drawing, etc.
  InitWindow(BLOCK_SIZE * WIDTH, BLOCK_SIZE * HEIGHT, "gol");
  SetTargetFPS(INIT_TARGET_FPS);

  // Game loop
  while(!WindowShouldClose()) {
    BeginDrawing();
      ClearBackground(BLACK);
      DrawBoard(board);
      DrawCursor();
    EndDrawing(); // This polls events automatically.

    ProcessInputs();
    UpdateMisc();

    if(mustToggleCell)
      ToggleCell(board);

    if(mustRandomize)
      RandomizeBoard(board);

    if(!isGameStopped) {
      UpdateCells(board);
      AdvanceCells(board);
    }
  }

  // Cleanup.
  CloseWindow();
  delete board; // PREVENT MEMORY LEAKS!!!

  return 0;
}
