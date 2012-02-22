/* This program is free software. It comes without any warranty, to
 * the extent permitted by applicable law. You can redistribute it
 * and/or modify it under the terms of the Do What The Fuck You Want
 * To Do Public License, Version 3, as published by Vincent Ambo. See
 * included COPYING file for more details. */

package main

import( "fmt"
	    "exec"
		"strings"
		"http"
		"url"
		"flag"
		"os"
		"time"
)

var authkey, host, c_artist, c_title string

func init(){
	flag.StringVar(&authkey, "key", "none", "http auth key")
	flag.StringVar(&host, "host", "http://localhost:8080", "host")
}

func main(){
	flag.Parse()
	fmt.Println("Music updater launching. Update occurs once per minute.")
	go updaterThread()

	var cc string
	for {
		fmt.Println("Type \"exit\" to quit")
		fmt.Scanf("%s", &cc)
		switch(cc) {
			case "exit":
				os.Exit(1)
			default:
				fmt.Println("Type \"exit\" to quit")

		}
	}
}

func updaterThread(){
	rValues := make(url.Values)
	rValues.Add("artist", "")
	rValues.Add("title", "")
	rValues.Add("key", authkey)

	for {
		title, artist := getTrack()
		if (title != c_title) || (artist != c_artist) {
			fmt.Println("Updating to: " + title + " - " + artist)
			c_artist = artist; c_title = title	
			rValues.Set("artist", artist)
			rValues.Set("title", title)
			_, err := http.PostForm(fmt.Sprint(host + "/setsong"), rValues)
			if err != nil {
				fmt.Println(err.String())
			}
		}
		time.Sleep(60000000000)
	}
}

func getTrack() (title, artist string){
	a, err := exec.Command("./gettitle").Output()
	if err != nil {
		fmt.Println("err: " + err.String())
		title = ""
		artist = ""
	} else {
		trackInfo := strings.Split(string(a), "\n")
		title = trackInfo[0]
		artist = trackInfo[1]
	}
	return
}