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
