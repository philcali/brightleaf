msg = wait(0, p)
if type(msg) = "roVideoPlayerEvent" then
    if debug then print "video event"
    if msg.isFullResult()
        if debug then print "video finished"
        return 9
    end if
elseif type(msg) = "roUniversalControlEvent" then
    if debug then print "button press "; msg.GetInt()
    HandleButton(msg.GetInt())
elseif msg = invalid then
    if debug print "timeout"
    return 6
end if
