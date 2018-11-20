function do_move(board, color)
    local moves = board:valid_moves( color )
    local len = #moves

    if len == 0 then
        return nil
    end

    local idx = math.random(len)
    local item = moves[ idx ]

    return item[ 1 ], item[ 2 ]
end
