sync:
	@if test -e $(HOME)/.config/sxhdk; then cp -R ~/.config/sxhkd ./.config/; fi
	@if test -e $(HOME)/.config/bspwm; then cp -R ~/.config/bspwm ./.config/; fi
	cp -R ~/.config/nvim ./.config/
	@if test -e $(HOME)/.config/alacritty; then \
		cp ~/.config/alacritty/alacritty.toml ./.config/alacritty/; \
	else \
		cp ~/.alacritty.toml ./.config/alacritty/alacritty.toml; \
	fi
	cp ~/.emacs .emacs
	@if test -e $(HOME)/.psqlrc; then cp ~/.psqlrc .psqlrc; fi
	cp ~/.inputrc .inputrc
	cp ~/.tmux.conf .tmux.conf
	cp ~/.gitignore .gitignore
	cp ~/.gitconfig .gitconfig
	cp ~/bin/git-pr bin/
