
math.randomseed (os.time())
local function rng () return math.random (3) end
AI = function ( mysymbol , board )

    if board[2][2] == ' ' then return 2, 2 end

 
    for i=1, 3 do
        if board[1][i] == board[2][i] and board[1][i] ~= ' ' and board[3][i] == ' ' then return 3, i end
        if board[2][i] == board[3][i] and board[2][i] ~= ' ' and board[1][i] == ' ' then return 1, i end
        if board[1][i] == board[3][i] and board[1][i] ~= ' ' and board[2][i] == ' ' then return 2, i end
        
        if board[i][1] == board[i][2] and board[i][3] ~= ' ' and board[i][3] == ' ' then return i, 3 end
        if board[i][2] == board[i][3] and board[i][1] ~= ' ' and board[i][1] == ' ' then return i, 1 end
        if board[i][1] == board[i][3] and board[i][2] ~= ' ' and board[i][2] == ' ' then return i, 2 end
    end

    if board[1][1] == board[2][2] and board[1][1] ~= ' ' and board[3][3] == ' ' then return 3, 3 end
    if board[1][1] == board[3][3] and board[1][1] ~= ' ' and board[2][2] == ' ' then return 2, 2 end
    if board[3][1] == board[2][2] and board[3][1] ~= ' ' and board[1][3] == ' ' then return 1, 3 end
    if board[3][1] == board[1][3] and board[3][1] ~= ' ' and board[2][2] == ' ' then return 2, 2 end
    while true do
    local x , y = rng () , rng ()
        if board [ x ][ y ] == ' ' then
            return x , y
        end
    end
end