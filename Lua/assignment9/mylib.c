#include <stdlib.h>
#include <stdbool.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

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

static int
l_summation(lua_State* lua) {
    int n = lua_gettop(lua);
    lua_Number output = 0;

    for(int i = 1; i <= n; ++i) {
        output += luaL_checknumber(lua, i);
    }

    lua_pushnumber(lua, output);
    return 1;
}

static int
l_reverse(lua_State* lua) {
    lua_Integer s = 1, e = lua_rawlen(lua, 1);

    while(s <= e) {
        lua_pushinteger(lua, e);
        lua_pushinteger(lua, s);
        lua_gettable(lua, 1);

        lua_pushinteger(lua, s);
        lua_pushinteger(lua, e);
        lua_gettable(lua, 1);

        lua_settable(lua, 1);
        lua_settable(lua, 1);

        s ++; e --;
    }

    return 1;
}

static int
l_reduce(lua_State* lua) {
    lua_Integer len = lua_rawlen(lua, 2);
    bool start = lua_gettop(lua) > 2;

    if(!start && !len) {
        return 0;
    }

    if(!start) {
        lua_pushinteger(lua, 1);
        lua_gettable(lua, 2);
    } else {
        lua_pushvalue(lua, 3);
    }

    for(int i = start ? 1 : 2; i <= len; ++i) {
        lua_pushvalue(lua, 1);
        lua_pushinteger(lua, i);
        lua_gettable(lua, 2);
        lua_pushvalue(lua, -3);

        lua_pcall(lua, 2, 1, 0);
        lua_replace(lua, -2);
    }

    return 1;
}

static int
l_filter(lua_State* lua) {
    lua_Integer len = lua_rawlen(lua, 2);
    lua_newtable(lua); int table = lua_gettop(lua);

    for(int j = 1, i = 1; i <= len; ++i) {
        lua_pushvalue(lua, 1);
        lua_pushinteger(lua, i);
        lua_gettable(lua, 2);


        lua_pcall(lua, 1, 1, 0);

        bool check = lua_toboolean(lua, -1);
        lua_pop(lua, 1);

        if(check) {
            lua_pushinteger(lua, j ++);
            lua_pushinteger(lua, i);
            lua_gettable(lua, 2);
            lua_settable(lua, table);
        }
    }

    return 1;
}

// splitAt(table, i1, i2, i3, ...)

/*
static int
l_split_at(lua_State* lua) {
    const lua_Integer len = lua_rawlen(lua, 1);
    const int nargs = lua_gettop(lua);
    // printf("nargs: %d\n", nargs);
    luaL_argcheck(lua, nargs >= 2, 2, "function should have at least 2 arguments");

    int *index_list = calloc(nargs - 1, sizeof(int));

    for(int i = 2, j = 0; i <= nargs; ++i, ++j) {
        const int index = luaL_checkinteger(lua, i);
        luaL_argcheck(lua, index >= 1, i, "index should be greater or equal to 1");

        if(j > 0)
            luaL_argcheck(lua, index > index_list[j - 1], i, "index should be greater than previous");

        if(index > len) {
            if(i > 0)
                index_list[j] = len;

            break;
        }

        index_list[j] = index;
    }

    for(int i = 0; i < nargs - 1; ++ i) {
        lua_newtable(lua); const int table = lua_gettop(lua);
        if(!index_list[i]) continue;

        int j = (!i ? 1 : index_list[i-1] + 1);
        int border = index_list[i] ? index_list[i] : len;

        // printf("j=%d, border=%d\n", j, border);

        for(int k = 1; j <= border; ++j, ++k) {
            lua_pushinteger(lua, k);
            lua_pushinteger(lua, j);
            lua_gettable(lua, 1);
            lua_settable(lua, table);
        }
    }

    free(index_list);

    return nargs - 1;
}
*/

static int
l_split_at(lua_State* lua) {
    const lua_Integer len = lua_rawlen(lua, 1);
    const int nargs = lua_gettop(lua);
    luaL_argcheck(lua, nargs >= 2, 2, "function should have at least 2 arguments");

    int cur = 1;
    for(int i = 2; i <= nargs; ++ i) {
        const int length = luaL_checkinteger(lua, i);
        lua_newtable(lua); const int table = lua_gettop(lua);

        if(cur > len) continue;

        const int border = cur + length > len ? len : cur + length;

        for(int k = 1; cur < border; ++cur, ++k) {
            lua_pushinteger(lua, k);
            lua_pushinteger(lua, cur);
            lua_gettable(lua, 1);
            lua_settable(lua, table);
        }
    }

    if(cur < len) {
        lua_newtable(lua); const int table = lua_gettop(lua);

        for(int k = 1; cur <= len; ++cur, ++k) {
            lua_pushinteger(lua, k);
            lua_pushinteger(lua, cur);
            lua_gettable(lua, 1);
            lua_settable(lua, table);
        }
    }

    return nargs;
}

static const luaL_Reg mylib[] = {
    {"summation", l_summation},
    {"reverse", l_reverse},
    {"reduce", l_reduce},
    {"filter", l_filter},
    {"splitAt", l_split_at},
    {NULL, NULL}
};

LUALIB_API int
luaopen_mylib(lua_State* lua) {
    lua_newtable(lua);
    luaL_setfuncs(lua, mylib, 0);
    return 1;
}
