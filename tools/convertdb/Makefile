all: convertdb

convertdb: couch.8 convertdb.8
			8l -o convertdb convertdb.8

convertdb.8: convertdb.go
	8g convertdb.go

couch.8: couch.go
	8g couch.go

clean:
	rm -rf *.8 convertdb	
