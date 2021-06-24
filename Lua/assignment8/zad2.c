#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <time.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

typedef enum {
    NONE = -1,
    CIRCLE,
    CROSS,
    TIE
} state_t;

#define THROW_ERROR(LUA, ...) \
    do { \
        fprintf(stderr, __VA_ARGS__); \
        lua_close(LUA); \
        exit(1); \
    } while(0)

int
stack_dump(lua_State* lua) {
    int top = lua_gettop(lua);

    for(int i = 1; i <= top; ++i) {
        int type = lua_type(lua, i);

        switch(type) {
            case LUA_TBOOLEAN:
                printf("%d | %d -> %s (boolean)\n", i, -top+i-1, lua_toboolean(lua, i) ? "true" : "false");
                break;
            case LUA_TNUMBER:
                printf("%d | %d -> %g (number)\n", i, -top+i-1, lua_tonumber(lua, i));
                break;
            case LUA_TSTRING:
                printf("%d | %d -> '%s' (string)\n", i, -top+i-1, lua_tostring(lua, i));
                break;
            default:
                printf("%d | %d -> (%s)\n", i, -top+i-1, lua_typename(lua, lua_type(lua, i)));
                break;
        }
    }

    putchar('\n');

    return 0;
}

lua_State*
load_player(const char* filename) {
    lua_State* lua = luaL_newstate();
    luaL_openlibs(lua);

    if(luaL_loadfile(lua, filename) || lua_pcall(lua, 0, 0, 0))
        THROW_ERROR(lua, "Could not load player1 file: %s\n", lua_tostring(lua, -1));

    lua_getglobal(lua, "AI");
    if(!lua_isfunction(lua, -1)) {
        THROW_ERROR(lua, "AI is not function (%s)\n", filename);
    }

    lua_pop(lua, -1);
    return lua;
}

void
push_board(lua_State* lua, state_t board[3][3]) {
    lua_newtable(lua);

    for(int j = 0; j < 3; ++j) {
        lua_pushinteger(lua, j + 1);
        lua_newtable(lua);

        for(int i = 0; i < 3; ++i) {
            lua_pushinteger(lua, i + 1);

            switch(board[j][i]) {
                case NONE:
                    lua_pushstring(lua, " ");
                    break;
                case CIRCLE:
                    lua_pushstring(lua, "O");
                    break;
                case CROSS:
                    lua_pushstring(lua, "X");
                    break;
            }

            lua_settable(lua, -3);
        }

        lua_settable(lua, -3);
    }
}

void
one_step(lua_State* lua, state_t board[3][3], state_t symbol) {
    lua_getglobal(lua, "AI");
    lua_pushstring(lua, symbol == CIRCLE ? "O" : "X");
    push_board(lua, board);
    lua_pcall(lua, 2, 2, 0);

    lua_Integer y = lua_tointeger(lua, -1) - 1;
    lua_Integer x = lua_tointeger(lua, -2) - 1;

    if(x > 3 || x < 0 || y > 3 || y < 0) {
        THROW_ERROR(lua, "Invalid coordinates: (%llu, %llu)\n", x, y);
    }

    if(board[y][x] != NONE) {
        THROW_ERROR(lua, "(%llu, %llu) is already taken!", x, y);
    }

    board[y][x] = symbol;

    lua_pop(lua, 2);
}

int
check_board(state_t board[3][3]) {
    for(int j = 0; j < 3; ++j)
        for(int i = 0; i < 3; ++i)
            if(board[j][i] == NONE) return NONE;

    for(int i = 0; i < 3; ++ i) {
        if(board[0][i] != NONE && board[0][i] == board[1][i] && board[1][i] == board[2][i])
            return board[0][i];

        if(board[i][0] != NONE && board[i][0] == board[i][1] && board[i][1] == board[i][2])
            return board[i][0];
    }

    if(board[0][0] != NONE && board[0][0] == board[1][1] && board[1][1] == board[2][2])
        return board[0][0];

    if(board[2][0] != NONE && board[2][0] == board[1][1] && board[1][1] == board[0][2])
        return board[2][0];

    return TIE;
}

int
main(int argc, char** argv) {
    if(argc != 3) {
        printf("Usage: %s <player1> <player2>\n", argv[0]);
        return -1;
    }

    lua_State* player1 = load_player(argv[1]);
    lua_State* player2 = load_player(argv[2]);

    srand(time(NULL));

    const int SAMPLE_NUM = 50;
    state_t board[3][3] = {NONE};

    int won[2] = {0, 0};
    int who[2] = {0, 1};

    for(int i = 0; i < SAMPLE_NUM; ++i) {
        memset(board, NONE, sizeof(state_t) * 9);

        who[0] = rand() & 1;
        who[1] = !who[0];

        bool end = false;

        while(!end) {
            for(int j = 0; j < 2; ++ j) {
                one_step(who[j] ? player2 : player1, board, (state_t)j);

                int p;
                if((p = check_board(board)) != NONE) {
                    if(p != TIE) {
                        won[who[p]] ++;
                    }

                    end = true;
                    break;
                }
            }
        }
    }

    int max = won[0] + won[1];

    printf("%s: %.2f%%\n", argv[1], won[0], max, won[0] * 100.0 / max);
    printf("%s: %.2f%%\n", argv[2], won[1], max, won[1] * 100.0 / max);

    // lua_close(player1);
    // lua_close(player2);
}
