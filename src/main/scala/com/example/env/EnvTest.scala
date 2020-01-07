package com.example.env

import cats.effect.Sync

final case class ExampleEnv(trackingId: String)

object EnvFTTest extends App {
  import com.example.env.instances.catz._

  def buildProgram[F[_] : Sync]: F[Unit] = {
    import cats.implicits._
    for {
      _ <- Sync[F].delay(println("Starting"))
      _ <- Sync[F].delay(println("Finished"))
    } yield ()
  }

  implicit val provideContextEnv: ProvideEnv[Context[ExampleEnv]] = new ProvideEnv[Context[ExampleEnv]] {
    override def provide: Context[ExampleEnv] = Context.initUnsafe(ExampleEnv("init"))
  }

  buildProgram[Env[ExampleEnv, ?]].unsafeRunSync(ExampleEnv("init"))
}


object EnvTest extends App {
  val program: Env[ExampleEnv, Unit] = for {
    _ <- EnvCtx(println("Starting"))
    _ <- EnvCtx.setContext(ExampleEnv("request B"))
    newCtx <- EnvCtx.context
    _ <- EnvCtx(println(s"Read $newCtx"))
    _ <- EnvCtx(println("Finished"))
  } yield ()

  program.unsafeRunSync(ExampleEnv("request A"))
}