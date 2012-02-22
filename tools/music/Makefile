all: music

music: iTunes.8
			8l -o music iTunes.8

iTunes.8: iTunes.go
	8g iTunes.go

clean:
	rm -rf *.8 music