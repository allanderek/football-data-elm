# Experimental stuff

This is probably not worthy of your time yet. It is an attempt to scratch two itches in one. Firstly I wanted a way to view football scores and tables in the terminal. Secondly I wanted to be able to use [Elm]( https://elm-lang.org/ ) to create terminal apps (Elm is designed for use in Web applications targetting the browser).

# Status

It's working for me, there are definitely a few rough edges around the Elm-terminal api. You can see the todos below that everything is far from perfect, but I honestly think it's pretty usable.

# Football-data.org

This uses football-data.org for the scores and tables and basically all the data it ever gets.
To actually use this you will need your own key, which you can get from [football-data](https://www.football-data.org/).
Once you have your key you need to make a source file in `src/Private/Key.elm` that looks like:

```
module Private.Key exposing (key)


key : String
key =
    "your secret api key"
```

# Matches scrolling

Sometimes as you scroll the list of matches they appear to move slightly left/right. This is due to the fact that we only render the lines that are on the screen, so sometimes that changes the justification slightly. We could solve that by just *always* rendering *all* of the lines and *then* choosing which ones to display.

Another option to optimise this, would be to render all the matches when we download them, and then we only have to pick out which ones to show each time we scroll. We would have to remember to re-render this 'memoised' list of matches stored on the model whenever there was a screen resize. At the moment the whole thing seems to run fast enough for me.

# Todo

- [x] I need to actually get the screen size in rows, so I will also need a resize event.
- [x] I want to draw the screen but show the menu at the top or bottom (probably top to make it easy)
- [x] I'd like to see the dates of games, this would mean the output would have to format it first and then take the number of lines the screen requires, but perhaps that's a good thing to do generically anyway.
- [x] I'd like to know if a game is currently in progress or finished.
- [x] It would be great if we could add a little colour, eg for games in progress.
- [x] Add the 'form' to the table with colours
- [x] I'd like to properly justify the matches, to do this you will have to add 'info' rows to 'Table'
- [x] When we go to the matches if we could automatically scroll down to something like the current date. I did this easier, I just scroll down to the first 'SCHEDULED' match.
- [ ] Document more the way this works with Elm and Node.tty
- [ ] I should still get the *actual* timezone we're in, but that is going to be tricky because we normally use the webapi to get the timezone, might be a way around it with node.
- [ ] Dates which are close to the current date should say something more human like, for example, on Monday-Friday of the week 12-16 of October 2020, rather than 17/10/2020 state 'Saturday'
- [ ] Maybe have a working indicator, particular for when you refresh the current screen (by simply hitting the short cut for the current screen.
- [ ] Tables for the UCL, there is more than one and we should display them all.
