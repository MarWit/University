package.path = '../?.lua;'..package.path
utils = require 'utils.lib'

function utf8.reverse(str)
    local output = ''
    local str_len = utf8.len(str)

    for i=str_len,1,-1 do
        local start_i = utf8.offset(str, i)
        local end_i = (i == str_len) and #str or utf8.offset(str, i+1) - 1

        output = output .. str:sub(start_i, end_i)
    end

    return output
end

tests = {
    {'cyżęisK', 'Księżyc'},
    {'akłóżgeżG', 'Gżegżółka'},
    {'einadz ezsżułd śeikaJ', 'Jakieś dłuższe zdanie'},
}

utils.run_tests(tests, utf8.reverse)
