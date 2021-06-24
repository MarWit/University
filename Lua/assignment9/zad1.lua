package.path = '../?.lua;'..package.path
utils = require 'utils.lib'

lib = require 'mylib'

print(lib.summation(1, 2, 3, 4, 5))

tab = {'a', 'b', 'c', 'd', 'e'}
lib.reverse(tab)

tab = {1, 2, 3, 4, 5}
sum = lib.reduce(function(a, b) return a+b end, tab, 10)

print(('sum = %s'):format(sum))

new_tab = lib.filter(function(i) return i > 2 end, tab)

for _, v in ipairs(new_tab) do
    print(v)
end

tab = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
new_tab = {lib.splitAt(tab, 3, 2, 2)}

print(utils.prettify(tab))
print(utils.prettify(new_tab))
