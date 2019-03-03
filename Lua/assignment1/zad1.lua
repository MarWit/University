#!/usr/bin/luajit

function printf(input)
    local function aux(value)
        if type(value) ~= 'table' then
            return tostring(value)
        end

        out = '{'

        for i=1,#value do
            out = out..aux(value[i])

            if i ~= #value then
                out = out..', '
            end
        end

        return out..'}'
    end

    out = aux(input)
    print(out)

    return out
end

function check(input, correct_out, out)
    print( "Excepted output: " .. correct_out )
    print( "Output: " .. out )
    print( "Result: " .. ((correct_out == out) and "OK!\n" or "Fail.\n") )
end

tests = {
    {{'ala', 'ma', 127, 'kotów'}, "{ala, ma, 127, kotów}"},
    {{'to są', {}, {2, 'tablice'}, 'zagnieżdżone?', {true}}, "{to są, {}, {2, tablice}, zagnieżdżone?, {true}}"},
    {{{{{{{{}}}}}}}, "{{{{{{{}}}}}}}"},
    {{12.0, 13.4, 2, {}, 7}, "{12, 13.4, 2, {}, 7}"}
}

for _, test in ipairs(tests) do
    check(test[1], test[2], printf(test[1]))
end
