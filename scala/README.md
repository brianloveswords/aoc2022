# Advent of Code 2022

## Requirements

- [sbt](https://www.scala-sbt.org/download.html)

## Usage

I usually solve things in the test suite as much as possible and don't really
use the main build so much. Given that, here's my workflow using `day1` as an
example:

- run `sbt`
- in the sbt shell, `~testOnly main.day1`
- iterate in the `./src/scala/test/day1.scala` file until I solve things.

Within `TestSuite` there is a provided `read` function with the signature
`String -> String`. It takes a file name, expected to be in the `inputs/`
folder, and synchronously returns the file contents. I also provide `lines`
which returns `List[String]`.

Note, under normal (production) circumstances, I would return an `IO[String]`
but for the specific context of this set of exercises, I'm treating the input
as something that would otherwise be included at compile time (if it wasn't so
inconvenient) since the per-day input data doesn't change.

## Libraries

I've included a few libraries here:

- [`cats`](https://typelevel.org/cats/)
- [`cats-effect`](https://typelevel.org/cats-effect/)
- [`fs2`](https://fs2.io/)

This is my standard toolkit for managing concurrency and streaming data. It
may not be strictly necessary, but if it makes sense to scatter/gather work
for any of the problems these libraries will come in handy.
