#include <raylib.h>
#include <cstdio>
#include <cstdlib>
#include <ctime>

#define INIT_TARGET_FPS 5
#define FPS_DELTA 1
#define ADD_BODY_LEN 5

#define BLOCK_SIZE   20
#define BOARD_WIDTH  40
#define BOARD_HEIGHT 40

// DEFINITIONS
enum CELL {
  EMPTY = 0, // Keep it as 0 because CELL is used for direction, too.
  WALL,
  FOOD,

  BODY,
  UP,
  DOWN,
  LEFT,
  RIGHT
};

typedef struct {
  typedef struct {
    int x;
    int y;
    CELL dir;
  } BodyPart;

  BodyPart head;
  BodyPart tail;

  CELL next;
  int wait;
} Snek;

int GenerateStartPos(int length) {
  int midPoint = length / 2;
  int quarterPoint = midPoint / 2;
  int offset = (rand() % midPoint) - quarterPoint;

  return midPoint + offset;
}

// GLOBAL STATE
int isGameLost = 0;
int nextTargetFPS = INIT_TARGET_FPS;

// LOGIC
CELL GenerateStartDir() {
  int randDir = rand() % 4;

  switch(randDir) {
    case 0: return CELL::UP;
    case 1: return CELL::DOWN;
    case 2: return CELL::LEFT;
    case 3: return CELL::RIGHT;
  }

  return CELL::UP;
}

Snek* GenerateSnek() {
  Snek* snek = new Snek;

  snek->head.x = GenerateStartPos(BOARD_WIDTH);
  snek->head.y = GenerateStartPos(BOARD_HEIGHT);
  snek->head.dir = GenerateStartDir();

  snek->tail.x = snek->head.x;
  snek->tail.y = snek->head.y;
  snek->tail.dir = snek->head.dir;

  snek->next = CELL::EMPTY;
  snek->wait = ADD_BODY_LEN;

  return snek;
}

Color Cell2Color(int val) {
  switch(val) {
    case CELL::EMPTY : return BLACK;
    case CELL::WALL  : return SKYBLUE;
    case CELL::FOOD  : return YELLOW;

    case CELL::BODY  : return GREEN;
    case CELL::UP    : return GREEN;
    case CELL::DOWN  : return GREEN;
    case CELL::LEFT  : return GREEN;
    case CELL::RIGHT : return GREEN;

    default          : return BLACK;
  }
}

int CalcPos(int c, int r) {
  return r * BOARD_WIDTH + c;
}

void PopulateBoard(char* board) {
  for(int r = 0; r < BOARD_HEIGHT; r++) {
    for(int c = 0; c < BOARD_WIDTH; c++) {
      // The board is a single-dimensional array.
      auto p = CalcPos(c, r);

      auto isUDBorder = r == 0 || r == BOARD_HEIGHT - 1;
      auto isLRBorder = c == 0 || c == BOARD_WIDTH - 1;
      auto isBorder = isUDBorder || isLRBorder;

      if(isBorder) {
        board[p] = CELL::WALL;
      } else {
        board[p] = CELL::EMPTY;
      }
    }
  }
}
// DrawRectangle(int posX, int posY, int width, int height, Color color);
void DrawBoard(char* board) {
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

void DrawSnek(Snek* snek) {
  DrawRectangle(
    snek->head.x * BLOCK_SIZE,
    snek->head.y * BLOCK_SIZE,
    BLOCK_SIZE,
    BLOCK_SIZE,
    GREEN
  );

  DrawRectangle(
    snek->tail.x * BLOCK_SIZE,
    snek->tail.y * BLOCK_SIZE,
    BLOCK_SIZE,
    BLOCK_SIZE,
    GREEN
  );
}

void ProcessInputs(Snek* snek) {
  if(IsKeyPressed(KEY_W))
    snek->next = CELL::UP;

  if(IsKeyPressed(KEY_S))
    snek->next = CELL::DOWN;

  if(IsKeyPressed(KEY_A))
    snek->next = CELL::LEFT;

  if(IsKeyPressed(KEY_D))
    snek->next = CELL::RIGHT;
}

void GenerateFood(char* board) {
  for(int isPlaced = 0; !isPlaced; ) {
    auto pos = rand() % (BOARD_WIDTH * BOARD_HEIGHT);

    if(board[pos] == CELL::EMPTY) {
      board[pos] = CELL::FOOD;
      isPlaced = -1;
    }
  }
}

int CanTurn(CELL dir, CELL nextDir) {
  if(
    (dir == CELL::LEFT  && nextDir == CELL::RIGHT) ||
    (dir == CELL::RIGHT && nextDir == CELL::LEFT)  ||
    (dir == CELL::UP    && nextDir == CELL::DOWN)  ||
    (dir == CELL::DOWN  && nextDir == CELL::UP)
  ) {
    return 0;
  } else {
    return -1;
  }
}

void AdvanceSnek(char* board, Snek* snek) {
  // First, move the head.
  if(snek->next && CanTurn(snek->head.dir, snek->next)) {
    snek->head.dir = snek->next;

    auto headPos = CalcPos(snek->head.x, snek->head.y);
    board[headPos] = snek->next;

    snek->next = CELL::EMPTY;
  }

  switch(snek->head.dir) {
    case CELL::UP    : snek->head.y -= 1; break;
    case CELL::DOWN  : snek->head.y += 1; break;
    case CELL::LEFT  : snek->head.x -= 1; break;
    case CELL::RIGHT : snek->head.x += 1; break;
  }

  auto headPos = CalcPos(snek->head.x, snek->head.y);
  auto headVal = board[headPos];
  switch(headVal) {
    case CELL::FOOD:
      SetTargetFPS(nextTargetFPS += FPS_DELTA);
      GenerateFood(board);
      snek->wait = ADD_BODY_LEN;
      break;
    case CELL::BODY:
    case CELL::WALL:
      isGameLost = 1;
    default:
      break;
  }

  board[headPos] = CELL::BODY;

  // Tail is next.
  if(snek->wait > 0) {
    snek->wait -= 1;
  } else {
    switch(snek->tail.dir) {
      case CELL::UP    : snek->tail.y -= 1; break;
      case CELL::DOWN  : snek->tail.y += 1; break;
      case CELL::LEFT  : snek->tail.x -= 1; break;
      case CELL::RIGHT : snek->tail.x += 1; break;
    }

    auto tailPos = CalcPos(snek->tail.x, snek->tail.y);
    auto cellVal = board[tailPos];

    // The tail follows the body, so it'll only find BODY or direction.
    if(cellVal != CELL::BODY)
      snek->tail.dir = (CELL) cellVal;

    board[tailPos] = CELL::EMPTY;
  }
}

int main() {
  // Initialize all variables.
  srand(time(0));
  auto board = new char[BOARD_WIDTH * BOARD_HEIGHT];
  PopulateBoard(board);
  GenerateFood(board);

  auto snek = GenerateSnek();

  // Proceed with drawing, etc.
  InitWindow(BLOCK_SIZE * BOARD_WIDTH, BLOCK_SIZE * BOARD_HEIGHT, "snek");
  SetTargetFPS(nextTargetFPS);

  // Game loop
  while(!WindowShouldClose()) {
    // Draw everything based on latest logic.
    if(!isGameLost) {
      BeginDrawing();
      ClearBackground(BLACK);

      DrawBoard(board);
      DrawSnek(snek);

      EndDrawing(); // This polls events automatically.

      // Perform other game logic after drawing.
      ProcessInputs(snek);
      AdvanceSnek(board, snek);
    } else {
      EnableEventWaiting();

      BeginDrawing();
      DrawText("GAME OVER - press any key",
        2 * BLOCK_SIZE,
        2 * BLOCK_SIZE,
        2 * BLOCK_SIZE,
        RED
      );
      EndDrawing();

      break;
    }
  }

  // Cleanup.
  CloseWindow();

  return 0;
}
