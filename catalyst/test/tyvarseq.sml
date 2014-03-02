exception Catalyst;

val g' = fn s => (fn (x,y) => (y,x)) s;

val (f,(f',g')) = (fn x => x, (fn y => y, fn z => z));
