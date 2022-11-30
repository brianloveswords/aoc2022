package day1

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp

object Main extends IOApp:

  def run(args: List[String]): IO[ExitCode] =
    IO.println("ok").as(ExitCode.Success)
