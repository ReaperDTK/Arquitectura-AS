
OUT=./bin

chat:
	erlc -o $(OUT) ./src/*/*.erl

clean:
	rm ./bin/*.beam

run:
	erl -pz $(OUT) -name $(name)
