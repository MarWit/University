local CVector = {}
setmetatable(CVector, CVector)

CVector.prototype = {}

function CVector.prototype:constructor(vec)
    local object = {}
    setmetatable(object, CVector)

    if getmetatable(vec) == CVector then
        min, max = 0, vec.len - 1
    else
        min, max = 1, #vec
    end

    iter = 0
    for i=min,max do
        object[iter] = vec[i]
        iter = iter + 1
    end

    object.len = iter
    return object
end

function CVector.prototype:at(i)
    return self[i]
end

function CVector.prototype:clear()
    for i=0,object.len - 1 do
        self[i] = nil
    end

    self.len = 0
end

function CVector.prototype:empty()
    return self.len == 0
end

function CVector.prototype:insert(i, e)
    if type(e) == 'table' then
        local min, max = 0, 0

        if getmetatable(e) == CVector then
            min, max = 0, e.len - 1
        else
            min, max = 1, #e
        end

        width = max - min + 1
        for k=self.len + width - 1, i + width, -1 do
            self[k] = self[k - width]
        end

        for k=min, max do
            self[i] = e[k]
            i = i + 1
        end

        self.len = self.len + width
    else
        for k=self.len,i+1,-1 do
            self[k] = self[k-1]
        end

        self[i] = e
        self.len = self.len + 1
    end
end

function CVector.prototype:size()
    return self.len
end

function CVector.prototype:push_back(value)
    self[self.len] = value
    self.len = self.len + 1
end

function CVector.prototype:pop_back()
    if self.len > 0 then
        self.len = self.len - 1
        local value = self[self.len]
        self[self.len] = nil

        return value
    end

end

function CVector:__tostring()
    local values = {}

    for i=0,self.len-1 do
        values[#values+1] = tostring(self[i])
    end

    return '{' .. table.concat(values, ', ') .. '}'
end

CVector.__index = CVector.prototype
CVector.__call  = CVector.prototype.constructor

local v = CVector{'a', 'd', 'e'}
print(v)
local w = CVector(v)
print(w)
print(v:empty(), CVector{}:empty())
v:insert(1, {2, 3})
print(v)
v:pop_back()
print(v)
print (v:size())
v:push_back(nil)
print(v)
print (v:size())
