local osym = {X = "O", O = "X"}

local function winner(board)
    for j = 1,3 do
        for i = 1,3 do
            if board[j][i] == " " then return " " end
        end
    end

    for i = 1,3 do
        if board[1][i] ~= NONE and board[1][i] == board[2][i] and board[2][i] == board[3][i] then
            return board[1][i]
        end

        if board[i][1] ~= NONE and board[i][1] == board[i][2] and board[i][2] == board[i][3] then
            return board[i][1]
        end
    end

    if board[1][1] ~= NONE and board[1][1] == board[2][2] and board[2][2] == board[3][3] then
        return board[1][1]
    end

    if board[3][1] ~= NONE and board[3][1] == board[2][2] and board[2][2] == board[1][3] then
        return board[3][1]
    end

    return "T";
end

local function copy_board(board)
    local new_board = {}

    for i=1,3 do
        table.insert(new_board, { table.unpack(board[i]) })
    end

    return new_board
end

local function minmax(board, symbol, scores)
    local win = winner(board)
    if win ~= " " then
        if win == "T" then return 0 end
        return scores[symbol] * scores[win]
    end

    local score = -2;
    local move = nil

    for j=1,3 do
        for i=1,3 do
            if board[j][i] == " " then
                local new_board = copy_board(board)
                new_board[j][i] = symbol
                local new_score = -minmax(new_board, osym[symbol], scores)
                if new_score > score then
                    score = new_score
                    move = {i, j}
                end
            end
        end
    end

    if move == nil then
        return 0
    end

    return score
end

AI = function(mysymbol, board)
    local scores = {}
    scores[mysymbol] = 1
    scores[osym[mysymbol]] = -1

    local score = -2
    local move = nil


    for j=1,3 do
        for i=1,3 do
            if board[j][i] == " " then
                local new_board = copy_board(board)
                new_board[j][i] = symbol
                local new_score = -minmax(new_board, osym[mysymbol], scores)
                if new_score > score then
                    score = new_score
                    move = {i, j}
                end
            end
        end
    end

    return move[1], move[2]
end
