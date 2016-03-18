namespace MangaDownloader

type Manga = {
    Title: string
    Uri:   Uri
}

type Chapter = {
    Manga: Manga
    Title: string
    Uri:   Uri
}

type Page = {
    Chapter: Chapter
    Number:  int
    Uri:     Uri
}

type Image = {
    Page: Page
    Uri:  Uri
}
