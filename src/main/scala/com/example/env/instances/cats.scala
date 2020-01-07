package com.example.env.instances

import cats.arrow.FunctionK
import cats.effect._
import cats.{Monad, MonadError, effect}
import com.example.env.EnvCtx.ofZIO
import com.example.env.{Context, Env, EnvCtx, ProvideEnv}
import zio.internal.PlatformLive
import zio.{RIO, Runtime}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{FiniteDuration, TimeUnit}

object catz extends CatsInstance

private[env] abstract class CatsInstance {
  import zio.interop.catz._

  implicit def emptyRuntime[Ctx](implicit provideEmptyEnv: ProvideEnv[Ctx]): Runtime[Ctx] = {
    Runtime[Ctx](provideEmptyEnv.provide, PlatformLive.Default)
  }

  implicit def contextShift[Ctx]: ContextShift[Env[Ctx, ?]] = new EnvContextShift()
  implicit def timer[Ctx]: EnvTimer[Ctx] = new EnvTimer
  implicit def concurrentEffect[Ctx](implicit runtime: Runtime[Context[Ctx]]): effect.ConcurrentEffect[Env[Ctx, ?]] =
    new EnvConcurrentEffect[Ctx]
}

class EnvTimer[Ctx] extends Timer[Env[Ctx, ?]] {
  import zio.interop.catz.implicits._

  private val timer = ioTimer[Throwable]

  override def clock: Clock[Env[Ctx, ?]] = {
    val clock = timer.clock
    new Clock[Env[Ctx, ?]] {
      override def monotonic(unit: TimeUnit): Env[Ctx, Long] = ofZIO(clock.monotonic(unit))
      override def realTime(unit: TimeUnit): Env[Ctx, Long] = ofZIO(clock.realTime(unit))
    }
  }

  override def sleep(timespan: FiniteDuration): Env[Ctx, Unit] = ofZIO(timer.sleep(timespan))
}

class EnvContextShift[Ctx](implicit contextShift: ContextShift[RIO[Context[Ctx], ?]]) extends ContextShift[Env[Ctx, ?]] {
  override def shift: Env[Ctx, Unit] = ofZIO(contextShift.shift)
  override def evalOn[A](ec: ExecutionContext)(fa: Env[Ctx, A]): Env[Ctx, A] = ofZIO(contextShift.evalOn(ec)(fa.run))
}

class EnvConcurrentEffect[Ctx](implicit concurrentEffect: ConcurrentEffect[RIO[Context[Ctx], ?]]) extends EnvConcurrent[Ctx] with ConcurrentEffect[Env[Ctx, ?]] {
  override def runCancelable[A](fa: Env[Ctx, A])(cb: Either[Throwable, A] => effect.IO[Unit]): SyncIO[CancelToken[Env[Ctx, ?]]] = {
    concurrentEffect.runCancelable(fa.run)(cb).map(ofZIO)
  }

  override def runAsync[A](fa: Env[Ctx, A])(cb: Either[Throwable, A] => effect.IO[Unit]): SyncIO[Unit] = {
    concurrentEffect.runAsync(fa.run)(cb)
  }

  override def toIO[A](fa: Env[Ctx, A]): effect.IO[A] = concurrentEffect.toIO[A](fa.run)
}

class EnvConcurrent[Ctx](implicit concurrent: Concurrent[RIO[Context[Ctx], ?]]) extends EnvAsync with Concurrent[Env[Ctx, ?]] {

  private def toFiber: FunctionK[RIO[Context[Ctx], ?], Env[Ctx, ?]] = new FunctionK[RIO[Context[Ctx], ?], Env[Ctx, ?]] {
    override def apply[A](fa: RIO[Context[Ctx], A]): Env[Ctx, A] = ofZIO(fa)
  }

  override def start[A](fa: Env[Ctx, A]): Env[Ctx, cats.effect.Fiber[Env[Ctx, ?], A]] =
    ofZIO(concurrent.start(fa.run).map(fiber => fiber.mapK(toFiber)))

  override def race[A, B](fa: Env[Ctx, A], fb: Env[Ctx, B]): Env[Ctx, Either[A, B]] = ofZIO(concurrent.race(fa.run, fb.run))

  override def racePair[A, B](fa: Env[Ctx, A], fb: Env[Ctx, B]): Env[Ctx, Either[(A, cats.effect.Fiber[Env[Ctx, ?], B]), (cats.effect.Fiber[Env[Ctx, ?], A], B)]] = {
    ofZIO {
      concurrent.racePair(fa.run, fb.run).map {
        case Left((a, fiber)) => Left((a, fiber.mapK(toFiber)))
        case Right((fiber, b)) => Right((fiber.mapK(toFiber), b))
      }
    }
  }

  override def cancelable[A](k: (Either[Throwable, A] => Unit) => CancelToken[Env[Ctx, ?]]): Env[Ctx, A] = {
    ofZIO(concurrent.cancelable((k2: Either[Throwable, A] => Unit) => k(k2).run))
  }
}

class EnvAsync[Ctx](implicit async: Async[RIO[Context[Ctx], ?]]) extends EnvSync with Async[Env[Ctx, ?]] {
  override final def async[A](k: (Either[Throwable, A] => Unit) => Unit): Env[Ctx, A] = ofZIO(async.async(k))
  override final def asyncF[A](k: (Either[Throwable, A] => Unit) => Env[Ctx, Unit]): Env[Ctx, A] = ofZIO {
    async.asyncF((k2: Either[Throwable, A] => Unit) => k(k2).run)
  }
  override final def never[A]: Env[Ctx, A] = ofZIO(async.never)
}

class EnvSync[Ctx](implicit sync: Sync[RIO[Context[Ctx], ?]]) extends EnvBracket with Sync[Env[Ctx, ?]] {
  override final def suspend[A](thunk: => Env[Ctx, A]): Env[Ctx, A] = ofZIO(sync.suspend(thunk.run))
  override final def delay[A](thunk: => A): Env[Ctx, A] = ofZIO(sync.delay(thunk))
}

class EnvBracket[Ctx](implicit bracket: Bracket[RIO[Context[Ctx], ?], Throwable]) extends EnvMonadError with Bracket[Env[Ctx, ?], Throwable] {
  override def bracketCase[A, B](acquire: Env[Ctx, A])(use: A => Env[Ctx, B])
                                (release: (A, ExitCase[Throwable]) => Env[Ctx, Unit]): Env[Ctx, B] = ofZIO {
    bracket.bracketCase(acquire.run)(use(_).run) {
      case (a, ec) => release(a, ec).run
    }
  }
}

class EnvMonadError[Ctx](implicit monadError: MonadError[RIO[Context[Ctx], ?], Throwable]) extends EnvMonad[Ctx] with MonadError[Env[Ctx, ?], Throwable] {
  override def handleErrorWith[A](fa: Env[Ctx, A])(f: Throwable => Env[Ctx, A]): Env[Ctx, A] = ofZIO {
    monadError.handleErrorWith(fa.run)(err => f(err).run)
  }
  override def raiseError[A](e: Throwable): Env[Ctx, A] = ofZIO {
    monadError.raiseError(e)
  }
}

class EnvMonad[Ctx](implicit monad: Monad[RIO[Context[Ctx], ?]]) extends cats.Monad[Env[Ctx, ?]] {
  override def pure[A](x: A): Env[Ctx, A] = EnvCtx.pure(x)
  override def map[A, B](fa: Env[Ctx, A])(f: A => B): Env[Ctx, B] = fa.map(f)
  override def flatMap[A, B](fa: Env[Ctx, A])(f: A => Env[Ctx, B]): Env[Ctx, B] = fa.flatMap(f)
  override def tailRecM[A, B](a: A)(f: A => Env[Ctx, Either[A, B]]): Env[Ctx, B] = ofZIO(monad.tailRecM(a)((a: A) => f(a).run))
}
