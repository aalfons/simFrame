setClass("MySampleControl",
    # definition of additional properties
    contains = "VirtualSampleControl")

setMethod("setup",
    signature(x = "data.frame", control = "MySampleControl"),
    function(x, control) {
        # method definition
    })
