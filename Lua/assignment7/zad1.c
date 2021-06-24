#include "stdio.h"

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

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
                printf("%d | %d -> (%s)", i, -top+i-1, lua_typename(lua, lua_type(lua, i)));
                break;
        }
    }

    putchar('\n');

    return 0;
}

int
main(void) {
    lua_State* lua = luaL_newstate();

    lua_pushnumber(lua, 3.5);
    stack_dump(lua);
    lua_pushstring(lua, "hello");
    stack_dump(lua);
    lua_pushnil(lua);
    stack_dump(lua);
    lua_pushvalue(lua,  -2);
    stack_dump(lua);
    lua_remove(lua, 1);
    stack_dump(lua);
    lua_insert(lua,  -2);
    stack_dump(lua);

    lua_close(lua);

    return 0;
}
