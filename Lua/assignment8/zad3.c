#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#define THROW_ERROR(...) \
    do { \
        fprintf(stderr, __VA_ARGS__); \
        lua_close(lua); \
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

void
display_map(lua_State* lua, const char* variable_name) {
    lua_getglobal(lua, variable_name);

    if(!lua_istable(lua, -1)) {
        printf("There is no level called %s\n", &variable_name[6]);
        lua_pop(lua, -1);
        return;
    }

    int height = lua_rawlen(lua, -1);

    for(int j = 1; j <= height; ++j) {
        lua_pushinteger(lua, j);
        lua_gettable(lua, -2);

        int width = lua_rawlen(lua, -1);

        for(int i = 1; i <= width; ++i) {
            lua_pushinteger(lua, i);
            lua_gettable(lua, -2);

            printf("%s ", lua_tostring(lua, -1));
            lua_pop(lua, 1);
        }

        putchar('\n');
        lua_pop(lua, 1);
    }

    lua_pop(lua, 1);
}

void
display_all_maps(lua_State* lua) {

    lua_pushglobaltable(lua);
    lua_pushnil(lua);

    while(lua_next(lua, -2)) {
        const char* key = lua_tostring(lua, -2);

        if(strstr(key, "level_") != key) {
            lua_pop(lua, 1);
            continue;
        }

        printf("map: %s\n", key);
        display_map(lua, key);
        putchar('\n');

        lua_pop(lua, 1);
    }

    lua_pop(lua, 1);
}

int
main(void) {
    static char* FNAME = "zad3.lua";

    lua_State* lua = luaL_newstate();
    luaL_openlibs(lua);

    if(luaL_loadfile(lua, FNAME) || lua_pcall(lua, 0, 0, 0))
        THROW_ERROR("Could not load config file: %s\n", lua_tostring(lua, -1));

    char map_name[64];

    for(;;) {
        scanf("%63s", map_name);

        if(strstr(map_name, "*ALL") != NULL) {
            display_all_maps(lua);
        } else {
            char variable_name[72];
            snprintf(variable_name, 72, "level_%s", map_name);

            display_map(lua, variable_name);
        }
    }
}
