package.path = '../?.lua;'..package.path
utils = require 'utils.lib'

-- Silly hacks
getmetatable('').__ipairs = function(s)
    local i, n = 0, #s
    return function()
        i = i + 1
        if i <= n then
            return i, s:sub(i, i)
        end
    end
end

-- Utils

-- XXX: Undefined behavior for types other than table or string
function concat(a, x)
    if type(x) == 'table' then
        local tab = {a}
        for i=1,#x do
            table.insert(tab, x[i])
        end

        return tab
    elseif type(x) == 'string' then
        return a .. x
    else
        return nil
    end
end

-- XXX: Undefined behavior for types other than table or string
function head(t)
    if type(t) == 'table' then
        return t[1] or ''
    elseif type(t) == 'string' then
        return t:sub(1, 1)
    else
        return nil
    end
end

-- XXX: Undefined behavior for types other than table or string
function tail(t, offset)
    offset = offset or 2

    if type(t) == 'table' then
        local tab = {}
        for i=offset,#t do
            table.insert(tab, t[i])
        end

        return tab
    elseif type(t) == 'string' then
        return t:sub(offset)
    else
        return nil
    end
end

local Trie = {}
setmetatable(Trie, Trie)

Trie.prototype = {}

function Trie.prototype:constructor(values)
    local object = {childs = {}, value = nil, len = 0}
    setmetatable(object, Trie)

    if #values == 1 then
        object.value = values[1]
    else
        for _, v in ipairs(values) do
            object:add(v)
        end
    end

    return object
end

function Trie.prototype:add(value)
    local iter = self

    if self:find(value) then
        return
    end

    self.len = self.len + 1

    for i, v in ipairs(value) do
        local rest = tail(value, i+1)

        if iter.childs[v] == nil then
            if iter.value == nil and next(iter.childs) == nil then
                iter.value = concat(v, rest)
                break
            end

            if iter.value ~= nil then
                -- iter.childs[iter.value:sub(1,1)] = Trie{iter.value:sub(2)}
                iter.childs[head(iter.value)] = Trie{tail(iter.value)}
            end

            local old_value = iter.value
            iter.value = nil

            if old_value == nil or v ~= head(old_value) then
                iter.childs[v] = Trie{rest}
                break
            end

        end

        iter = iter.childs[v]
        if rest == '' then
            iter.childs[''] = Trie{''}
            break
        end
    end
end

function Trie.prototype:find(value)
    local iter = self

    for i, v in ipairs(value) do
        local rest = tail(value, i+1)

        if iter.value == concat(v, rest) then
            return true
        end

        if not iter.childs[v] then
            return false
        end

        iter = iter.childs[v]

        if rest == '' and iter.childs[''] then
            return true
        end
    end

    return false
end

function Trie.prototype:merge(trie)
    local iter1, iter2 = nil, nil
    local stack = {{self, trie}}

    while #stack > 0 do
        iter1, iter2 = table.unpack(stack[#stack])
        stack[#stack] = nil

        for k, v in pairs(iter2.childs) do
            if not iter1.childs[k] then
                if iter1.value and iter1.value:sub(1, 1) == k then
                    iter1.childs[k] = Trie{iter1.value:sub(2)}
                    iter1.value = nil

                    stack[#stack + 1] = {iter1.childs[k], iter2.childs[k]}
                else
                    iter1.childs[k] = v
                    self.len = self.len + v.len
                end
            else
                stack[#stack + 1] = {iter1.childs[k], iter2.childs[k]}
            end
        end

        if iter2.value then
            local len = iter1.len
            iter1:add(iter2.value)
            self.len = self.len + iter1.len - len
        end

    end
end

function Trie.prototype:traverse()
    local stack = {{'', self}}
    local prefix = ''

    while #stack > 0 do
        prefix, iter = table.unpack(stack[#stack])
        stack[#stack] = nil

        if iter.value ~= nil then
            print(utils.prettify(prefix), utils.prettify(iter.value))
        else
            for k, v in pairs(iter.childs) do
                if v.value then
                    -- print(prefix .. k, v.value)
                    -- print(utils.prettify(concat(prefix, k)), utils.prettify(v.value))
                    print(prefix, k, utils.prettify(v.value))

                    if v.childs[''] then
                        -- print(prefix .. k)
                        print(utils.prettify(concat(prefix, k)))
                    end
                else
                    -- stack[#stack + 1] = {prefix .. k, v}
                    stack[#stack + 1] = {concat(prefix, k), v}
                end
            end
        end
    end
end

function Trie.prototype:size()
    return self.len
end

function Trie:__add(other)
    if getmetatable(other) == Trie then
        self:merge(other)
    else
        self:add(other)
    end

    return self
end

function Trie:__len()
    return self:size()
end

Trie.__index = Trie.prototype
Trie.__call  = Trie.prototype.constructor

local trie = Trie{}
local trie2 = Trie{}

(trie+'kotek')
(trie+'piesek')
trie2:add('kot')
trie2:add('pies')
trie2:add({2,3,4})

print(trie:size())
print(trie2:size())

trie:traverse()
print('')
trie2:traverse()

trie:merge(trie2)

print('  ')
trie:traverse()

print(#trie)
