package com.example.env

import zio.internal.PlatformLive
import zio.{FiberFailure, FiberRef, RIO, Runtime, UIO, ZIO}

sealed trait Exit[+A] {
  def toEither: Either[Throwable, A]
}
object Exit {
  final case class Success[+A](value: A) extends Exit[A] {
    override def toEither: Either[Throwable, A] = Right(value)
  }

  sealed trait Failure extends Exit[Nothing]

  final case class Error[+E <: Throwable](error: E) extends Exit.Failure {
    override def toEither: Either[Throwable, Nothing] = Left(error)
  }

  final case class Termination(error: Throwable) extends Exit.Failure {
    override def toEither: Either[Throwable, Nothing] = Left(error)
  }

  def fromZIOExit[E <: Throwable, A](result: zio.Exit[E, A]): Exit[A] = result match {
    case zio.Exit.Success(v) => Success(v)
    case zio.Exit.Failure(failure) => fromZIOFailure(failure)
  }

  def fromZIOFailure[E <: Throwable](failure: zio.Exit.Cause[E]): Failure =
    failure.failureOrCause match {
      case Left(err) => Error(err)
      case Right(cause) =>
        val unchecked = cause.defects
        val exceptions = if (cause.interrupted) {
          new InterruptedException :: unchecked
        } else {
          unchecked
        }

        val compound = exceptions match {
          case e :: Nil => e
          case _ => FiberFailure(cause)
        }

        Termination(compound)
    }
}

trait ProvideEnv[E] {
  def provide: E
}

trait Env[Ctx, +A] {
  def run: RIO[Context[Ctx], A]

  // basic transformations
  def map[B](f: A => B): Env[Ctx, B]
  def flatMap[B](f: A => Env[Ctx, B]): Env[Ctx, B]
  def void: Env[Ctx, Unit] = map(_ => ())

  // execute
  def unsafeRunSync(env: Ctx): A
  def unsafeRunEmptySync(implicit provideEnv: ProvideEnv[Ctx]): A
}

trait Context[Ctx] {
  def ctx: Context.Service[Ctx]
}

object Context {
  trait Service[Ctx] {
    val read: UIO[Ctx]
    def set[R, E, A](value: Ctx): UIO[Unit]
    def update[R, E, A](f: Ctx => Ctx): UIO[Ctx]
  }

  def initUnsafe[Ctx](value: Ctx): Context[Ctx] = {
    Runtime({}, PlatformLive.Default).unsafeRun(Context.init(value))
  }

  def init[Ctx](value: Ctx): UIO[Context[Ctx]] = {
    makeService(value).map {service =>
      new Context[Ctx] {
        override def ctx: Service[Ctx] = service
      }
    }
  }

  def makeService[Ctx](value: Ctx): UIO[Context.Service[Ctx]] =
    FiberRef.make(value).map { fiberRef =>
      new Context.Service[Ctx] {
        val read: UIO[Ctx] = fiberRef.get
        def set[R, E, A](value: Ctx): UIO[Unit] = fiberRef.set(value)
        def update[R, E, A](f: Ctx => Ctx): UIO[Ctx] = fiberRef.update(f)
      }
    }

  // helper methods
  def read[Ctx]: ZIO[Context[Ctx], Nothing, Ctx] = ZIO.accessM[Context[Ctx]](_.ctx.read)
  def update[Ctx, R <: Context[Ctx], E, A](f: Ctx => Ctx): ZIO[R, Nothing, Ctx] =
    ZIO.accessM[R](_.ctx.update(f))
  def set[Ctx, R <: Context[Ctx]](value: Ctx): ZIO[R, Nothing, Unit] = ZIO.accessM[R](_.ctx.set(value))
}

private final class EnvCtx[Ctx, +A] (zio: RIO[Context[Ctx], A]) extends Env[Ctx, A] {
  val runtime = Runtime({}, PlatformLive.Default)

  override def run: RIO[Context[Ctx], A] = zio

  override def map[B](f: A => B): Env[Ctx, B] = new EnvCtx(zio.map(f))
  override def flatMap[B](f: A => Env[Ctx, B]): Env[Ctx, B] = new EnvCtx[Ctx, B](zio.flatMap(a => f(a).run))

  override def unsafeRunSync(env: Ctx): A = {
    val exit = Exit.fromZIOExit(runtime.unsafeRunSync(zio.provideSomeM(Context.init(env))))
    exit match {
      case Exit.Success(value) => value
      case Exit.Error(throwable) => throw throwable
      case Exit.Termination(throwable) => throw throwable
    }
  }
  override def unsafeRunEmptySync(implicit provideEnv: ProvideEnv[Ctx]): A = {
    unsafeRunSync(provideEnv.provide)
  }
}

object EnvCtx {
  def apply[R, A](body: => A): Env[R, A] = ofZIO(ZIO(body))
  def pure[R, A](x: A): Env[R, A] = ofZIO(ZIO.succeed(x))
  def delay[R, A](x: A): Env[R, A] = ofZIO(ZIO.effect(x))

  def context[R]: Env[R, R] = ofZIO(Context.read[R])
  def updateContext[Ctx, A](f: Ctx => Ctx): Env[Ctx, Ctx] = ofZIO(Context.update(f))
  def setContext[Ctx, A](value: Ctx): Env[Ctx, Unit] = ofZIO(Context.set(value))

  def ofZIO[R, A](zio: RIO[Context[R], A]): Env[R, A] = new EnvCtx(zio)
}

