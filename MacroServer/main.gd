extends Node

var peer = null
var loaded_files = null

func _ready():
    loaded_files = []

    var port_number = int(OS.get_environment("GDLISP_PORT_NUMBER"))

    peer = StreamPeerTCP.new()
    peer.big_endian = true
    peer.connect_to_host("127.0.0.1", port_number)

func _process(_delta):
    if peer.get_available_bytes() > 0:
        var json_result = JSON.parse(peer.get_string())
        if json_result.error == OK:
            var payload = json_result.result
            run_command(payload)
        else:
            # TODO Send error back
            push_error("Invalid JSON " + json_result.error_string)

func run_command(payload):
    var cmd = payload['command']
    var args = payload['args']
    match cmd:
        "quit":
            peer.put_string("Acknowledged\nQuitting...")
            get_tree().quit()
        "ping":
            peer.put_string("pong")
        "eval":
            var input = args[0]
            var result = eval(input)
            peer.put_string(pretty(result))
        "load":
            var input = args[0]
            var idx = len(loaded_files)
            loaded_files.push_back(load(input))
            peer.put_string(pretty(idx))

# Funny hack, thanks Godot Q&A! :)
#
# https://godotengine.org/qa/339/does-gdscript-have-method-to-execute-string-code-exec-python?show=362#a362
func eval(input):
    var script = GDScript.new()
    script.set_source_code("func eval(MAIN):\n    return " + input)
    script.reload()

    var obj = Reference.new()
    obj.set_script(script)

    return obj.eval(self)

# I'll probably end up migrating this to GDLisp.gd proper at some
# point, but for now, here it is.
func pretty(value):
    if value == null:
        return "()"
    elif value is GDLisp.Cons:
        return "({} . {})".format([pretty(value.car), pretty(value.cdr)], "{}")
    elif value is Array:
        var s = "["
        var first = true
        for x in value:
            if not first:
                s += " "
            s += pretty(x)
            first = false
        return s + "]"
    elif value is String:
        var s = "\""
        for x in value.to_ascii(): # TODO Unicode support
            # TODO More escaping
            match x:
                34:
                    s += "\\\""
                92:
                    s += "\\\\"
                _:
                    s += char(x)
        return s + "\""
    elif value is GDLisp.Symbol:
        return value.contents
    elif value is bool and value:
        return "#t"
    elif value is bool and not value:
        return "#f"
    elif value is int or value is float:
        return str(value)
    elif value is Vector2:
        return "V{{} {}}".format([pretty(value.x), pretty(value.y)], "{}")
    elif value is Vector3:
        return "V{{} {} {}}".format([pretty(value.x), pretty(value.y), pretty(value.z)], "{}")
    else:
        return str(value)
