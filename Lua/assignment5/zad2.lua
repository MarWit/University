package.path = '../?.lua;'..package.path
utils = require 'utils.lib'

function zip(...)
    local values = {...}
    if next(values) == nil then
        return next, {}, nil
    end

    local min = #values[1]

    for i=2,#values do
        local len = #values[i]
        min = (min < len) and min or len
    end

    return function(state)
        if state[2] <= min then
            local values = state[1]
            local k = state[2]
            local out = {}

            state[2] = k + 1

            for i=1,#values do
                out[i] = values[i][k]
            end

            return table.unpack(out)
        end
    end, {values, 1, min}
end

some_sequence = {'a', 'b', 'c', 'd'}
tests = {
    {{{'a', 40}, {'b', 50}, {'c', 60}}, {{'a', 'b', 'c', 'd'}, {40, 50, 60}}},
    {{{'a', 'a'}, {'b', 'b'}, {'c', 'c'}, {'d', 'd'}}, {some_sequence, some_sequence}},
    {{}, {}}
}

function test_util(input)
    out = {}
    _f, _s, _var = zip(table.unpack(input))

    while true do
        var1, var2 = _f(_s, _var)
        _var = var1

        if _var == nil then
            return out
        end

        table.insert(out, {var1, var2})
    end
end

utils.run_tests(tests, test_util, true)

