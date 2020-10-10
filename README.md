# Experimental stuff

This is probably not worthy of your time yet. It is an attempt to scratch two itches in one. Firstly I wanted a way to view football scores and tables in the terminal. Secondly I wanted to be able to use (Elm)[https://elm-lang.org/] to create terminal apps (Elm is designed for use in Web applications targetting the browser).

# Status

It's working for me, there are definitely a few rough edges around the Elm-terminal api. You can see the todos below that everything is far from perfect, but I honestly think it's pretty usable.

# Football-data.org

This uses football-data.org for the scores and tables and basically all the data it ever gets.
To actually use this you will need your own key, which you can get from (football-data)[https://www.football-data.org/].
Once you have your key you need to make a source file in `src/Private/Key.elm` that looks like:

```
module Private.Key exposing (key)


key : String
key =
    "your secret api key"
```


# Todo

- [x] I need to actually get the screen size in rows, so I will also need a resize event.
- [x] I want to draw the screen but show the menu at the top or bottom (probably top to make it easy)
- [x] I'd like to see the dates of games, this would mean the output would have to format it first and then take the number of lines the screen requires, but perhaps that's a good thing to do generically anyway.
- [x] I'd like to know if a game is currently in progress or finished.
- [x] It would be great if we could add a little colour, eg for games in progress.
- [x] Add the 'form' to the table with colours
- [ ] I'd like to properly justify the matches, to do this you will have to add 'info' rows to 'Table'
- [ ] When we go to the matches if we could automatically scroll down to the something like the current date
- [ ] Document more the way this works with Elm and Node.tty
