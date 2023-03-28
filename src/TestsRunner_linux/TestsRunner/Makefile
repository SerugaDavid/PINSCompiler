build:clean
	mkdir .build/
	cd .build/ && find ../src/ -type f -name "*.java" | xargs javac -cp ".:../lib/ArgPar-0.1.jar" -d .

test:build
	cd .build/ && java -cp ".:../lib/*" App 

clean:
	rm -rf .build
