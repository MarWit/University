function moveinto(input, i1, j1, i2, output)
    if output == nil then
        output = input
    end

    local len = j1 - i1 + 1
    local old_len = table.maxn(output)

    for i=old_len,i2,-1 do
        output[i+len] = output[i]
    end

    for i=0,len - 1 do
        output[i+i2] = input[i1+i]
    end

    return old_len + len
end

function eq_table(tab1, tab2)
    if #tab1 ~= #tab2 then
        return false
    end

    for i=1,#tab1 do
        if tab1[i] ~= tab2[i] then
            return false
        end
    end

    return true
end

local tab = {1,2,3,6,7,8}
local tab2 = {4,5}

moveinto(tab2, 1, 2, 4, tab)
assert(eq_table(tab, {1,2,3,4,5,6,7,8}))

local tab = {1, nil, 3, 7, nil, 8}
local tab2 = {3, 4, nil, 6, 7}

moveinto(tab2, 2, 4, 4, tab)
assert(eq_table(tab, {1,nil,3,4,nil,6,7,nil,8}))

local tab = {1,2,3}

moveinto(tab, 1, 3, 1)
moveinto(tab, 1, 6, 1)
assert(eq_table(tab, {1,2,3,1,2,3,1,2,3,1,2,3}))
