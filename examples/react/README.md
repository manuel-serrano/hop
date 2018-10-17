This example illustrates Web Reactive Programming (WRP) in Hop.
It creates two _reactors_ (`width` and `els`) and two reactive
HTML nodes (the two tables). When the `width` reactor is modified
(via the `enlarge` button) the table borders automatically enlarge. When
the `els` reactor is modified (via the `push` button), new elements
are added to the two tables.
