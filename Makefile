TARGET := `guile -c "(display (string-append (car %load-path) \"/language\"))"`


all:
	@echo "Just type \"sudo make install\""

install:
	@cp -fr simple $(TARGET) 