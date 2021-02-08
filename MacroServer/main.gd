extends Node

var peer = null

func _ready():
    peer = StreamPeerTCP.new()
    peer.big_endian = true
    peer.connect_to_host("127.0.0.1", 61992)

func _process(_delta):
    if peer.get_available_bytes() > 0:
        var cmd = peer.get_string()
        run_command(cmd)

func run_command(cmd):
    match cmd:
        "quit":
            peer.put_string("Acknowledged\nQuitting...")
            get_tree().quit()
        "ping":
            peer.put_string("pong")
        "eval":
            var input = peer.get_string()
            var result = eval(input)
            peer.put_string(str(result))

# Funny hack, thanks Godot Q&A! :)
#
# https://godotengine.org/qa/339/does-gdscript-have-method-to-execute-string-code-exec-python?show=362#a362
func eval(input):
    var script = GDScript.new()
    script.set_source_code("func eval():\n    return " + input)
    script.reload()

    var obj = Reference.new()
    obj.set_script(script)

    return obj.eval()
