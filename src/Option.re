type map('a, 'value) = ('a => 'value, option('a)) => option('value);
type map2('a, 'b, 'value) =
  (('a, 'b) => 'value, option('a), option('b)) => option('value);
type map3('a, 'b, 'c, 'value) =
  (('a, 'b, 'c) => 'value, option('a), option('b), option('c)) =>
  option('value);
type map4('a, 'b, 'c, 'd, 'value) =
  (
    ('a, 'b, 'c, 'd) => 'value,
    option('a),
    option('b),
    option('c),
    option('d)
  ) =>
  option('value);
type map5('a, 'b, 'c, 'd, 'e, 'value) =
  (
    ('a, 'b, 'c, 'd, 'e) => 'value,
    option('a),
    option('b),
    option('c),
    option('d),
    option('e)
  ) =>
  option('value);
type andThen('a, 'b) = ('a => option('b), option('a)) => option('b);
type withDefault('a) = ('a, option('a)) => 'a;

let map: map('a, 'c) =
  (f, optionA) =>
    switch (optionA) {
    | Some(a) => Some(f(a))
    | None => None
    };

let map2: map2('a, 'b, 'c) =
  (f, optionA, optionB) =>
    switch (optionA, optionB) {
    | (Some(a), Some(b)) => Some(f(a, b))
    | (_, _) => None
    };

let map3: map3('a, 'b, 'c, 'd) =
  (f, optionA, optionB, optionC) =>
    switch (optionA, optionB, optionC) {
    | (Some(a), Some(b), Some(c)) => Some(f(a, b, c))
    | (_, _, _) => None
    };

let map4: map4('a, 'b, 'c, 'd, 'e) =
  (f, optionA, optionB, optionC, optionD) =>
    switch (optionA, optionB, optionC, optionD) {
    | (Some(a), Some(b), Some(c), Some(d)) => Some(f(a, b, c, d))
    | (_, _, _, _) => None
    };

let map5: map5('a, 'b, 'c, 'd, 'e, 'f) =
  (f, optionA, optionB, optionC, optionD, optionE) =>
    switch (optionA, optionB, optionC, optionD, optionE) {
    | (Some(a), Some(b), Some(c), Some(d), Some(e)) =>
      Some(f(a, b, c, d, e))
    | (_, _, _, _, _) => None
    };

let andThen: andThen('a, 'b) =
  (f, opt) =>
    switch (opt) {
    | Some(a) => f(a)
    | None => None
    };

let withDefault: withDefault('a) =
  (default, opt) =>
    switch (opt) {
    | Some(value) => value
    | None => default
    };
