package stockschecker.repositories

import fs2.Stream
import stockschecker.domain.{Command, CreateCommand}

trait CommandRepository[F[_]]:
  def streamActive: Stream[F, Command]
  def create(cmd: CreateCommand): F[Command]
  def update(cmd: Command): F[Command]
