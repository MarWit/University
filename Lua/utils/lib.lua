local utils = {}

function utils.tuple_iter(tuples)
    local i = 0
    local len = #tuples
    return function()
        i = i + 1
        if i <= len then
            return unpack(tuples[i])
        end
    end
end

-- NOTE: Only works for shallow tables
function utils.array_eq(a, b, deepcmp)
    if #a ~= #b then
        return false
    end

    for i=1,#a do
        if type(a[i]) ~= type(b[i]) then
            return false
        end

        if deepcmp and type(a[i]) == 'table' then
            if #a[i] ~= #b[i] or not utils.array_eq(a[i], b[i], true) then
                return false
            end
        elseif a[i] ~= b[i] then
            return false
        end
    end

    return true
end

function utils.prettify(input)
    if type(input) == 'string' then
        return "'" .. input .. "'"
    end

    if type(input) ~= 'table' then
        return tostring(input)
    end

    local output = { '{' }

    for i=1,#input do
        output[#output + 1] = utils.prettify(input[i])

        if i ~= #input then
            output[#output + 1] = ', '
        end
    end

    output[#output + 1] = '}'

    return table.concat(output)
end

function utils.from_iter(iterable, is_table)
    local output = {}

    if is_table then
        for k, v in iterable do
            output[k] = v
        end
    else
        for v in iterable do
            table.insert(output, v)
        end
    end

    return output
end

function utils.run_tests(tests, func, deepcmp)
    local function helper(good_output, ...)
        return good_output, func(...)
    end

    local len = #tests
    io.write(('Running %d tests.\n\n'):format(len))

    local passed = 0

    for i, test in ipairs(tests) do
        io.write(('Test %d/%d...'):format(i, len))

        local good_output, output = helper(unpack(test))
        local result

        if type(output) == 'table' then
            -- XXX: Change it later to table_eq
            result = utils.array_eq(output, good_output, deepcmp)
        else
            result = (output == good_output)
        end

        if result then
            io.write('OK!\n')
            passed = passed + 1
        else
            io.write('Fail. ')
            io.write(('Expected: %s, got %s.\n'):format(utils.prettify(good_output), utils.prettify(output)))
        end
    end

    io.write(('%d/%d (%.2f%%) tests passed.\n'):format(passed, len, 100. * passed/len))
    io.flush()
end

return utils
