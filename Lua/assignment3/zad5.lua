package.path = '../?.lua;'..package.path
require 'utils.lib'

function string.split(str, delim)
    local delim = delim or ' '
    local pattern = string.format('([^%s]*)%s', delim, delim)

    output = {}

    for s in (str..delim):gmatch(pattern) do
        table.insert(output, s)
    end

    return output
end

tests = {
    {{'', 'test', 'string', '', ''}, ' test string  ', nil, },
    {{'test', '12', '5', '', 'xyz', ''}, 'test,12,5,,xyz,', ','},
    {{'', '', '', '', '', ''}, '#####', '#'},
}

utils.run_tests(tests, string.split)
