# MangaDownloader

Currently a Quick & Dirty script to download mangas from mangareader.net website. Retries download
of pages if they fail, also supports resuming downloading of already partially downloaded files.

## Usage

`MangaDownloader.exe http://www.mangareader.net/one-piece`

Shows a List of available Chapters

	One Piece
	1: One Piece 1
	2: One Piece 2
	3: One Piece 3
	4: One Piece 4
	5: One Piece 5
	6: One Piece 6
	7: One Piece 7

`MangaDownloader.exe http://www.mangareader.net/one-piece 7`

Downloads entry 7

`MangaDownloader.exe http://www.mangareader.net/one-piece 3 6`

Downloads entries 3 to 6

`MangaDownloader.exe http://www.mangareader.net/one-piece all`

Downloads all chapters of the manga

`MangaDownloader.exe http://www.mangareader.net/one-piece 100 end`

Downloads all chapters starting from entry 100. 

## Destination

All files are currently downloaded relative from where the command was executed. The following file-structure is used `<MangaTitle>/<ChapterName>/<PageNo>.<FileExtension>`

	One Piece/One Piece 3/1.jpg
	One Piece/One Piece 3/2.jpg
	One Piece/One Piece 3/3.jpg
	...
