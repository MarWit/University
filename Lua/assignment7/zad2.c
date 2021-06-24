#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#define LUA_TINTEGER (LUA_TNUMBER | (1 << 10))

#define THROW_ERROR(...) \
    do { \
        fprintf(stderr, __VA_ARGS__); \
        lua_close(lua); \
        exit(1); \
    } while(0)

typedef struct {
    char* title;
    bool windowed;
    char* geometry;
    bool active;
    char* display;
    int transparent;
    double opacity;
    char* window_class;
    char* background;
    int delay;
} config_t;

int
stack_dump(lua_State* lua) {
    int top = lua_gettop(lua);

    for(int i = 1; i <= top; ++i) {
        int type = lua_type(lua, i);

        switch(type) {
            case LUA_TNIL:
                printf("%d | %d -> nil (nil)\n", i, -top+i-1);
                break;
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
load_constants(lua_State* lua) {
    lua_pushnumber(lua, 1920);
    lua_setglobal(lua, "WIDTH");

    lua_pushnumber(lua, 1080);
    lua_setglobal(lua, "HEIGHT");

    lua_pushstring(lua, ":0");
    lua_setglobal(lua, "DISPLAY");
}

void
get_field(lua_State* lua, char* field, int type, void** output) {
    lua_getfield(lua, 1, field);

    if(lua_type(lua, -1) != (type & 0xff)) {
        printf("%s", lua_typename(lua, lua_type(lua, -1)));
        THROW_ERROR("%s has invalid type, should be %s\n", field, lua_typename(lua, type & 0xff));
    }

    switch(type) {
        case LUA_TSTRING:
            *output = strdup(lua_tostring(lua, -1));
            break;
        case LUA_TNUMBER:
            *((double*)output) = lua_tonumber(lua, -1);
            break;
        case LUA_TINTEGER:
            *((int*)output) = lua_tointeger(lua, -1);
            break;
        case LUA_TBOOLEAN:
            *((bool*)output) = lua_toboolean(lua, -1);
            break;
        default:
            break;
    }
}

config_t*
load_config(lua_State* lua) {
    config_t* config = calloc(1, sizeof(config_t));

    if(!lua_getglobal(lua, "config"))
        THROW_ERROR("config table is not defined");

    if(lua_type(lua, 1) != LUA_TTABLE)
        THROW_ERROR("config is not a table");

    get_field(lua, "title",        LUA_TSTRING,  (void**) &config->title);
    get_field(lua, "windowed",     LUA_TBOOLEAN, (void**) &config->windowed);
    get_field(lua, "geometry",     LUA_TSTRING,  (void**) &config->geometry);
    get_field(lua, "active",       LUA_TBOOLEAN, (void**) &config->active);
    get_field(lua, "display",      LUA_TSTRING,  (void**) &config->display);
    get_field(lua, "transparent",  LUA_TBOOLEAN, (void**) &config->transparent);
    get_field(lua, "opacity",      LUA_TNUMBER,  (void**) &config->opacity);
    get_field(lua, "window_class", LUA_TSTRING,  (void**) &config->window_class);
    get_field(lua, "background",   LUA_TSTRING,  (void**) &config->background);
    get_field(lua, "delay",        LUA_TINTEGER, (void**) &config->delay);

    lua_pop(lua, -1);

    return config;
}

char*
str_replace(const char *s, const char *what, const char *with) {
    char *result;
    int what_len = strlen(what);
    int with_len = strlen(with);
    int i, cnt = 0;

    for(i = 0; s[i] != '\0'; ++i) {
        if(strstr(&s[i], what) == &s[i]) {
            cnt++;
            i += what_len - 1;
        }
    }

    result = (char*) malloc(i + cnt * (with_len - what_len) + 1);

    i = 0;
    while(*s) {
        if (strstr(s, what) == s) {
            strcpy(&result[i], with);
            i += with_len;
            s += what_len;
        } else result[i++] = *s++;
    }

    result[i] = '\0';
    return result;
}

char*
resolve_path(char* path) {
    static char* HOME = "/home/marwit";

    return str_replace(path, "$HOME", HOME);
}

int
main(void) {
    static char* FNAME = "zad2.lua";

    lua_State* lua = luaL_newstate();
    luaL_openlibs(lua);
    load_constants(lua);

    if(luaL_loadfile(lua, FNAME) || lua_pcall(lua, 0, 0, 0))
        THROW_ERROR("Could not load config file: %s\n", lua_tostring(lua, -1));

    config_t* config = load_config(lua);

    lua_getglobal(lua, "config"); // At this point its save to just call it
    char *new_path = resolve_path(config -> background);
    lua_pushstring(lua, new_path);
    lua_setfield(lua, -2, "background");

    char *new_value;
    get_field(lua, "background", LUA_TSTRING, (void**) &new_value);
    printf("New value = %s\n", new_value);


    free(new_value);
    free(new_path);
    free(config);
    lua_close(lua);

    return 0;
}
