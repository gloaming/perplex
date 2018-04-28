# perplex

## Building

In order to play the game, you'll need a word list.
Currently you'll have to manually download one from [SCOWL](http://app.aspell.net/create) (or any other source) and place it in the top folder as `words.txt`

Then build with [stack](https:///haskellstack.org):
````
stack build && stack exec perplex
````
## Playing
Find words in the grid by moving in adjacent or diagonal steps, without using any tile twice. Type a word and hit `space` or `enter` to record it - how many can you find?

When you can't take any more, press `shift+enter` to see all words and generate a new grid.

