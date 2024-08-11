install:
	mkdir -p ~/.config/emacs
	cp init.el ~/.config/emacs/.
	cp config.org ~/.config/emacs/.
	cd ~/.config/emacs

get_updates:
	cp ~/.config/emacs/config.org .

install-tasks:
	cp ./OrgFiles/Tasks.org ~/OrgFiles/.
