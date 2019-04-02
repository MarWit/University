package.path = '../?.lua;'..package.path
utils = require 'utils.lib'

function check_expression(input)
    -- Strip parenthesis
    local check = input:match"^%s*%(%s*(.*)%s*%)%s*$"
    if check ~= nil then
        return check_expression(check)
    end

    -- LHS is expression
    local check = { input:match"^%s*(%b())%s*[%*%+%-/]%s*(.*)%s*$" }
    if #check == 2 then
        return check_expression(check[1]) and check_expression(check[2])
    end

    -- LHS is number
    local check = input:match"^%s*[%+%-]?%s*%d+%.?%d*%s*[%*%+%-/]%s*(.*)%s*$"
    if check ~= nil then
        return check_expression(check)
    end

    -- It's just number..
    local check = input:match"^%s*[%+%-]?%s*%d+%.?%d*%s*$"
    return check ~= nil
end

tests = {
    {true, '-2+ 4.503'},
    {true, '(2*3.5*4)- (+12)/3'},
    {false, '2 * (   3 - 4 + 2'},
    {true, '(2+(3+(4+(5+(6+(7+8*(9)))))))'},
}

utils.run_tests(tests, check_expression)
