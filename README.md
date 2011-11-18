# Crafty Erlang

Introductory presentation on Erlang, focusing on programming idioms, thinking in Erlang, and getting started with simple projects.

Presented using S9/Slideshow.

To install S9/Slideshow and the S6 theme:

    sudo gem install slideshow
    slideshow -f http://github.com/geraldb/slideshow-s6-syntax-highlighter/raw/master/s6syntax.txt

To generate the slides:

    slideshow -t s6syntax.txt crafty_erlang

This will generate a crafty_erlang.html file, which you can just open in a browser.


## Associated scripts
* **bowling_game.erl** Algorithm for scoring a bowling game.
* **bowling_game_test.erl** Unit tests for same.
* **bowling_store.erl** Data store for players and rolls in a bowling game; returns new total score when roll added.
* **bowling_web.erl** Web app for keeping score in a bowling game.
* **kvstore.erl** Key-value storage module; no longer part of presentation.
* **rest_client.erl** Command-line app which talks to the bowling_web REST client.
* **scorekeeper.erl** Command-line app for keeping score in a bowling game.
* **start_server.erl** Script for starting up bowling_web app.
* **uniq_id.erl** Simplest possible data manager process.
* **web_test.erl** Automated test of bowling_web REST requests.
