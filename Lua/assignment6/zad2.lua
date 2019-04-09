local Frac = {}
setmetatable(Frac, Frac)

Frac.prototype = {}

-- Util functions

local min = math.min
local max = math.max

function gcd(a, b)
    while b ~= 0 do
        local t = b
        b = a % b
        a = t
    end

    return a
end

function lcm(a, b)
    return a * b / gcd(a, b)
end

-- Class methods

function Frac.prototype:constructor(a, b)
    local object = {0, a, b}
    setmetatable(object, Frac)

    return object:simplify()
end

function Frac.prototype:simplify()
    local sign = false

    if self[1] < 0 then
        sign = true
        self[1] = -self[1]
    end

    if self[2] < 0 then
        sign = true
        self[2] = -self[2]
    end

    while self[2] ~= 0 and self[2] >= self[3] do
        self[1] = self[1] + 1
        self[2] = self[2] - self[3]
    end

    local g = gcd(self[2], self[3])
    if g > 1 then
        self[2] = self[2] // g
        self[3] = self[3] // g
    end

    if sign then
        if self[1] ~= 0 then
            self[1] = -self[1]
        else
            self[2] = -self[2]
        end
    end

    return self
end

function Frac.prototype:flip()
    local b = self[1] * self[3] + self[2]
    local a = self[3]

    return Frac(a, b)
end

function Frac.prototype:tofloat()
    return self[1] + self[2] / self[3]
end

-- Operator overload

function Frac:__add(other)
    -- XXX: Assuming both self and other are Frac

    local a = 0
    local b = 0
    local c = 0

    c = self[1] + other[1]
    b = self[3] * other[3]
    a = c * b + self[2] * other[3] + other[2] * self[3]

    return Frac(a, b)
end

function Frac:__sub(other)
    -- XXX: Assuming both self and other are Frac

    local a = 0
    local b = 0
    local c = 0

    c = self[1] - other[1]
    b = self[3] * other[3]
    a = c * b + self[2] * other[3] - other[2] * self[3]

    return Frac(a, b)
end

function Frac:__mul(other)
    -- XXX: Assuming both self and other are Frac

    local a = (self[1] * self[3] + self[2]) * (other[1] * other[3] + other[2])
    local b = self[3] * other[3]

    return Frac(a, b)
end

function Frac:__div(other)
    -- XXX: Assuming both self and other are Frac

    return self * other:flip()
end

function Frac:__unm()
    return Frac(-(self[1] * self[3] + self[2]), self[3])
end

function Frac:__eq(other)
    return self[1] == other[1] and self[2] == other[2] and self[3] == other[3]
end

function Frac:__lt(other)
    if self[1] < other[1] then
        return true
    end

    if self[2] * other[3] < other[2] * self[3] then
        return true
    end

    return false
end

function Frac:__le(other)
    return (self < other) or (self == other)
end

function Frac:__concat(other)
    return ('%s%s'):format(self, other)
end

function Frac:__tostring()
    local out = {}
    out[#out+1] = (self[1] ~= 0) and self[1] or nil
    out[#out+1] = (self[2] ~= 0) and (self[2] .. '/' .. self[3]) or nil

    return table.concat(out, ' i ')
end

Frac.__index = Frac.prototype
Frac.__call = Frac.prototype.constructor

local frac1 = Frac(2, 3)
print(frac1)

local frac2 = Frac(11, 4)
print(frac2)

local frac3 = frac1 + frac2
print(frac3)

local frac4 = frac1 - frac2
print(frac4)

local frac5 = frac1 * frac2
print(frac5)

local frac6 = frac1 / frac2
print(frac6)

local frac7 = -frac1
print(frac7)


print( 'Wynik: ' .. (Frac(2, 3) + Frac(3, 4)) )
print( Frac.tofloat(Frac(2, 3) * Frac(3, 4)) )
print( Frac(2, 3) < Frac(3, 4) )
print( Frac(2, 3) == Frac(8, 12) )
