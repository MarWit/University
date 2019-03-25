package.path = '../?.lua;'..package.path
require 'utils.lib'

function unpath(filepath, delim)
    delim = delim or '/'
    PATH_PATTERN = string.format('([^%s]+)%s', delim, delim)
    FILENAME_PATTERN = string.format('%s?([^%s]*)%%.([^%s]*)$', delim, delim, delim)

    components = utils.from_iter(filepath:gmatch(PATH_PATTERN))
    components[#components+1] = { filepath:match(FILENAME_PATTERN) }

    return components
end

tests = {
    {{'home', 'marwit', 'Studia', 'Lua', 'assignment4', {'zad3', 'lua'}}, '/home/marwit/Studia/Lua/assignment4/zad3.lua'},
    {{{'file.name', 'ext'}}, 'file.name.ext'},
    {{'K:', 'hidden-name', 'Teaching', '2016_Lua', '[Lab]', {'Lecture 04', 'pdf'}}, 'K:\\hidden-name\\Teaching\\2016_Lua\\[Lab]\\Lecture 04.pdf', '\\'},
}

utils.run_tests(tests, unpath, true)
