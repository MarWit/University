local Vector = {}
setmetatable(Vector, Vector)

Vector.prototype = {}

-- Static methods

function Vector.zeros(n)
    local values = {}

    for i=1, n do
        values[i] = 0
    end

    return Vector(values)
end

-- Class methods

function Vector.prototype:constructor(values)
    local object = { table.unpack(values) }
    object.len = #object
    setmetatable(object, Vector)

    return object
end

function Vector.prototype:copy()
    local values = {}

    for i=1,self.len do
        values[i] = self[i]
    end

    return Vector(values)
end

function Vector.prototype:sum()
    local sum = 0
    for i=1,self.len do
        sum = sum + self[i]
    end

    return sum
end

function Vector.prototype:map(func)
    local object = self:copy()

    for i=1,object.len do
        object[i] = func(i, object[i])
    end

    return object
end

-- Operator overload methods

function Vector:__add(other)
    local values = {}

    -- TODO: Check size of arrays
    for i=1, self.len do
        values[i] = self[i] + other[i]
    end

    return Vector(values)
end


function Vector:__sub(other)
    local values = {}

    -- TODO: Check size of arrays
    for i=1, self.len do
        values[i] = self[i] - other[i]
    end

    return Vector(values)
end

function Vector:__div(other)
    if getmetatable(self) ~= Vector then
        self, other = other, self
    end

    if type(other) ~= 'number' then
        return
    end

    local values = {}
    for i=1, self.len do
        values[i] = self[i] / other
    end

    return Vector(values)
end

function Vector:__mul(other)
    if getmetatable(self) ~= Vector then
        self, other = other, self
    end

    if type(other) == 'number' then
        return self:map(function(_, v) return v * other end)
    elseif getmetatable(other) == Vector then
        return self:map(function(i, v) return v * other[i] end):sum()
    end
end

function Vector:__unm()
    local vec = self:copy()
    return vec:map(function(v) return -v end)
end

function Vector:__len()
    local squares = 0

    for i=1, self.len do
        squares = squares + self[i] * self[i]
    end

    return math.sqrt(squares)
end

function Vector:__eq(other)
    if self.len ~= other.len then
        return false
    end

    for i=1, self.len do
        if self[i] ~= other[i] then
            return false
        end
    end

    return true
end

function Vector:__ipairs()
    return function(state)
        local vec, i = table.unpack(state)
        state[2] = i + 1

        if i > vec.len then
            return
        end

        local basis = Vector.zeros(vec.len)
        basis[i] = 1

        return basis, vec[i]
    end, {self, 1}, nil
end

function Vector:__tostring()
    local copy = {}

    for i=1,self.len do
        copy[#copy+1] = self[i]
    end

    return "(" .. table.concat(copy, ", ") .. ")"
end

Vector.__call = Vector.prototype.constructor
Vector.__index = Vector.prototype

local vec1 = Vector{1, 2, 3}
local vec2 = Vector{3, 2, 1}
local vec3 = vec1 + vec2
local vec4 = vec1 - vec2
local vec5 = -vec1
local vec6 = 2 * vec1
local vec7 = vec1 / 1.5
local vec8 = vec1 * vec2
local vec9 = #Vector{4, 3}

print(('%s + %s = %s'):format(vec1, vec2, vec3))
print(('%s - %s = %s'):format(vec1, vec2, vec4))
print(('-%s = %s'):format(vec1, vec5))
print(('2 * %s = %s'):format(vec1, vec6))
print(('%s / 1.5 = %s'):format(vec1, vec7))
print(('%s * %s = %s'):format(vec1, vec2, vec8))
print(('#%s = %s'):format(Vector{4, 3}, vec9))
print(('(%s + %s) == (%s + %s) => %s'):format(vec1, vec2, vec2, vec1, (vec1 + vec2) == (vec2 + vec1)))

for b, v in ipairs(vec1) do
    print(('%s -> %d'):format(b, v))
end
