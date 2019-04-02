package.path = '../?.lua;'..package.path
utils = require 'utils.lib'

function chain(...)
    return function(state)
        if next(state[1]) == nil then return nil end

        local values, t, i, n = table.unpack(state)
        if n == -1 then
            n = #values[t]
            state[4] = n
        end

        if i + 1 <= n then
            state[3] = i + 1
            return values[t][i + 1]
        else
            local values_len = #values
            if t + 1 <= values_len then
                next_t = -1

                for j=t + 1,values_len do
                    n = #values[j]
                    if n ~= 0 then
                        next_t = j
                        break
                    end
                end

                if next_t == -1 then
                    return
                end

                t = next_t
                i = 1

                state[2] = t
                state[3] = 1
                state[4] = n

                return values[t][i]
            end
        end
    end, {{...}, 1, 0, -1}, nil
end

tests = {
    {{'a', 'b', 'c', 40, 50, 6, 7}, {{'a', 'b', 'c'}, {40, 50}, {}, {6, 7}}},
    {{}, {{}, {}, {}, {}}},
    {{'a', 'b', 'c', 'd', 'e'}, {{'a'}, {'b'}, {'c'}, {'d'}, {'e'}}},
    {{}, {}}
}

function test_util(input)
    return utils.from_iter(chain(table.unpack(input)))
end

utils.run_tests(tests, test_util)
