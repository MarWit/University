local window_width  = 0.2 * WIDTH
local window_height = 0.3 * HEIGHT

config = {
    title        = 'Title',
    windowed     = true,
    geometry     = ('%dx%d+0+0'):format(window_width, window_height),
    active       = true,
    display      = DISPLAY,
    transparent  = true,
    opacity      = 0.9,
    delay        = 1000,
    window_class = 'some_class',
    background   = '$HOME/pretty_pictures/cats/simba.jpg'
}
