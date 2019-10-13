CC = kotlinc
CFLAGS = -include-runtime -d

submission = hw6.tar.gz
submission_files = everyNth.kt Makefile

default:
	$(CC) everyNth.kt $(CFLAGS) everyNth.jar

check: default
	java -jar everyNth.jar

clean:
	@rm -f everyNth.jar hw6.tar.gz

dist:
	@tar -czf $(submission) $(submission_files)
