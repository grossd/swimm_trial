# A graph DB

  - Nodes and links are primary citizens				[OK]
  - Links can be between links						[OK]
  - Links can be ternary
    - Are they such immediately or later?
  - Links and nodes are typed (multiple types, optionally a bitmap).	[OK]
  - We can put backtrackable attributes on nodes and links.		[OK]
  - Types								[OK]

## Main concerns

  - Query-by-type is slow.
    - Option
      - Maintain a type/2 relation between edge/node and type.
      - Create a type lattice.

  - The plugin heat.pl maintains an inverse map for the `heat`
    property to find all nodes with descending heat.

## Demo graph

  - Site:
    - http://networkrepository.com/soc.php
  - Install
    - Download http://nrvis.com/download/data/soc/fb-pages-artist.zip
    - Unzip in a directory `graphs` to get
      - graphs/fb-pages-artist.edges
      - graphs/fb-pages-artist.nodes


## Playing

Playing with a very naive implementation of pagerank, deliberately
designed to stress node property updates.


```
swipl -O play.pl social.pl
?- time(load_graph('fb-pages-artist')).	% 7 sec
?- time(pagerank(1)).			% 2 sec
?- prop(Node, heat, Heat).
```

Or, with a heat map.

```
swipl -O play.pl social.pl heat.pl
?- time(load_graph('fb-pages-artist')).
?- time(pagerank(1)).			% 25 sec
?- max_heat_gen(Heat, Node).
```
