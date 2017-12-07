.PHONY: all

default: all

server:
	ocamlbuild chat_server.ml -pkg lwt -pkg lwt.unix -pkg str chat_server.byte
chatbox:
	ocamlbuild -use-ocamlfind -pkgs 'lablgtk2.auto-init lwt lwt_glib cohttp cohttp-lwt-unix str yojson' 'chatbox.byte'

all: server chatbox

clean:
	ocamlbuild -clean
	rm chatbox.byte chat_server.byte
