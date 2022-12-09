package main

import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.charset.Charset

def id[A](a: A): A = a

trait TestSuite extends CatsEffectSuite with ScalaCheckEffectSuite:
  def read(name: String): String =
    Files.readString(Paths.get(s"./inputs/${name}"), Charset.forName("UTF-8"))

  def lines(name: String): List[String] =
    read(name).trim.split("\n").toList
