package main

import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.charset.Charset

def id[A](a: A): A = a

val ansiDim = "\u001b[2m"
val ansiBold = "\u001b[1m"
val ansiGreen = "\u001b[32m"
val ansiReset = "\u001b[0m"

trait TestSuite extends CatsEffectSuite with ScalaCheckEffectSuite:
  def read(name: String): String =
    Files.readString(Paths.get(s"./inputs/${name}"), Charset.forName("UTF-8"))

  def lines(name: String): List[String] =
    read(name).trim.split("\n").toList
