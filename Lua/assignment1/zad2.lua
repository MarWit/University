function map(input, func)
    assert(type(func), 'function')

    local output = {}

    for i=1,#input do
        output[i] = func(input[i])
    end

    return output
end

function display(value)
    if type(value) ~= 'table' then
        return tostring(value)
    end

    local out = '{'

    for i=1,#value do
        out = out..display(value[i])

        if i ~= #value then
            out = out..', '
        end
    end

    return out .. '}'
end

function compare_array(a, b)
    if #a ~= #b then return false end

    for i, v in ipairs(a) do
        if v ~= b[i] then
            return false
        end
    end

    return true
end

tests = {
    {
        function(v) return v + 2 end,
        {1, 2, 3, 4, 5},
        {3, 4, 5, 6, 7},
    },
    {
        function(v) return v .. "!" end,
        {"Hi", "Hey", "Hello"},
        {"Hi!", "Hey!", "Hello!"}
    }
}

for i, test in ipairs(tests) do
    f, input, out_correct = unpack(test)
    input_copy = { unpack(input) }

    out = map(input, f)
    is_ok = (compare_array(out, out_correct) and compare_array(input, input_copy))

    print("Expected output: " .. display(out_correct))
    print("Actual output: " .. display(out))
    print("Result: " .. (is_ok and "Ok!\n" or "Fail.\n"))
end

