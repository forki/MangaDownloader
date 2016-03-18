namespace MangaDownloader

type Manga = {
    Title: string
    Uri:   Uri
}
with static member create title uri = {Title=title; Uri=uri}

type Chapter = {
    Manga: Manga
    Title: string
    Uri:   Uri
}
with static member create manga title uri = {Manga=manga; Title=title; Uri=uri}

type Page = {
    Chapter: Chapter
    Number:  int
    Uri:     Uri
}
with static member create chapter no uri = {Chapter=chapter; Number=no; Uri=uri}

type Image = {
    Page: Page
    Uri:  Uri
}
with static member create page uri = {Page=page; Uri=uri}


type Manga with
    member o.createChapter = Chapter.create o
    member o.createChapterWithRelativeUri(title,str) = 
        str |> Uri.tryCreate o.Uri |> Option.map (o.createChapter title)
type Chapter with
    member o.createPage = Page.create o
    member o.createPageWithRelativeUri(no,str) =
        str |> Uri.tryCreate o.Uri |> Option.map (o.createPage no)
type Page with
    member o.createImage = Image.create o