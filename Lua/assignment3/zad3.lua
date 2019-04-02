package.path = '../?.lua;'..package.path
utils = require 'utils.lib'

function utf8.sub(str, i, j)
    iter = 1
    output = ''

    for c in str:gmatch(utf8.charpattern) do
        if iter >= i then
            output = output .. c
        end

        if j and iter == j then
            break
        end

        iter = iter + 1
    end

    return output
end

tests = {
    {'życ', 'Księżyc', 5, 7},
    {'żegż', 'Gżegżółka', 2, 5},
    {'ś dłuż', 'Jakieś dłuższe zdanie', 6, 11},
    {'życ:\nN', 'Księżyc:\nNów', 5, 10},
    {'łąść', 'żźćęłąść', 5, nil},
}

utils.run_tests(tests, utf8.sub)


