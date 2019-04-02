package.path = '../?.lua;'..package.path
utils = require 'utils.lib'

function utf8.normalize(str)
    local output = ''
    local str_len = utf8.len(str)

    for i=1,str_len do
        local start_i = utf8.offset(str, i)
        local end_i = (i == str_len) and #str or utf8.offset(str, i+1) - 1

        if end_i == start_i then
            output = output .. str:sub(start_i, start_i)
        end
    end

    return output
end

tests = {
    {'Ksiyc', 'Księżyc'},
    {'Gegka', 'Gżegżółka'},
    {'Jakie dusze zdanie', 'Jakieś dłuższe zdanie'},
}

utils.run_tests(tests, utf8.normalize)
