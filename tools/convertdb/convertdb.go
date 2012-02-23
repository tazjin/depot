package main

import (
	"strconv"
	"fmt"
	"io/ioutil"
	"json"
	"./couch"
	"os"
	"time"
)

//old
type OldComment struct {
	Author string
	Text   string
	Date   string
}

type OldEntry struct {
	Id       string
	Title    string
	Author   string
	Text     string
	Mtext    string
	Comments []OldComment
}

//old
type Comment struct {
	Author string 	`json:"cauthor"`
	Text   string 	`json:"ctext"`
	Date   int64 	`json:"cdate"`
}

type Entry struct {
	Id       string `json:"_id"`
	Year     int    `json:"year"`
	Month    int    `json:"month"`
	Day      int    `json:"day"`
	Lang     string `json:"lang"`
	Title    string `json:"title"`
	Author   string `json:"author"`
	Text     string `json:"text"`
	Mtext    string `json:"mtext"`
	Comments []Comment 	`json:"comments"`
}

func main() {
	getAllByYear("2011", 8, 12)
	getAllByYear("2012", 1, 2)
}

func getAllByYear(year string, minm, maxm int){
	db, _ := couch.NewDatabase("127.0.0.1", "5984", "tazblog")
	for i:=minm;i<=maxm;i++{
		dirList, err := ioutil.ReadDir(fmt.Sprintf("data/%s/%02d/", year, i))
		if err != nil {
			fmt.Println(err.String())
			os.Exit(1)
		}
		for d:=len(dirList)-1; d>-1; d--{
			content, cErr := ioutil.ReadFile(fmt.Sprintf("data/%s/%02d/%s", year, i, dirList[d].Name))
			if cErr != nil {
				fmt.Println(cErr)
				os.Exit(1)
			}
			var oEntry OldEntry
			jErr := json.Unmarshal(content, &oEntry)
			if jErr != nil {
				fmt.Println(jErr.String())
				os.Exit(1)
			}
			nEntry := convertEntry(oEntry, fmt.Sprintf("data/%s/%02d/%s", year, i, dirList[d].Name))
			eId, _, err := db.Insert(nEntry)
			if err != nil {
				fmt.Println(err.String())
				os.Exit(1)
			}
			fmt.Println("Inserted " + eId)
		}		
	}
}

func convertEntry(oEntry OldEntry, p string) Entry{
	var nEntry Entry
	nComments := make([]Comment, len(oEntry.Comments))
	for i:=0;i<len(oEntry.Comments);i++{
		nComments[i].Author = oEntry.Comments[i].Author
		nComments[i].Text = oEntry.Comments[i].Text
		nComments[i].Date = parseDumbTime(oEntry.Comments[i].Date)
	}

	nEntry.Id 		= oEntry.Id[3:]
	nEntry.Year, _  = strconv.Atoi(p[5:9])
	nEntry.Month, _ = strconv.Atoi(p[10:12])
	nEntry.Day, _	= strconv.Atoi(p[13:15])
	nEntry.Title  	= oEntry.Title
	nEntry.Author 	= oEntry.Author
	nEntry.Mtext 	= oEntry.Mtext
	nEntry.Text 	= oEntry.Text
	nEntry.Comments = nComments
	nEntry.Lang 	= "DE"

	return nEntry
}

func parseDumbTime(ct string) int64 {
	x, err := time.Parse("[Am 02.01.2006 um 15:04 Uhr]", ct)
	if err != nil {
		fmt.Println(err.String())
		os.Exit(1)
	}

	return x.Seconds()
}